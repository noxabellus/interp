//! Contains the universal Value implementation

#![allow(clippy::unusual_byte_groupings,)]

use std::{
	hash::{ Hash, Hasher },
	cmp::Ordering,
	fmt::{ self, Display, Debug, Formatter }
};

use super::{
	object::{ self, FnCastable, FnCastMut, FunctionKind },
	typeinfo::TypeID,
};



const SIGN_MASK: u64 = 0b_1_00000000000_0_000_000000000000000000000000000000000000000000000000;

macro_rules! tda { ($expr: expr) => { ($expr << 48u64) }; }
macro_rules! tdb { ($expr: expr) => { tda!($expr) | SIGN_MASK }; }

/// Indicates which kind of type of data is held by a Value
#[repr(u64)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum TypeDiscriminator {
	Real = 0,

	Integer = tda!(1),
	Character = tda!(2),
	Boolean = tda!(3),
	Nil = tda!(4),
	TypeID = tda!(5),

	Record = tda!(6),
	Array = tda!(7),
	Map = tdb!(1),
	String = tdb!(2),
	Function = tdb!(3),

	Userdata = tdb!(4),
}



impl Display for TypeDiscriminator {
	fn fmt (&self, f: &mut Formatter<'_>) -> fmt::Result {
		write!(f, "{}", match self {
			Self::Real => "Real",
			Self::Integer => "Integer",
			Self::Character => "Character",
			Self::Boolean => "Boolean",
			Self::Nil => "Nil",
			Self::TypeID => "TypeID",
			Self::Record => "Record",
			Self::Array => "Array",
			Self::Map => "Map",
			Self::String => "String",
			Self::Function => "Function",
			Self::Userdata => "Userdata",
		})
	}
}


pub use internal::Value;


#[cfg(feature="nantag")]
mod internal {
	use super::*;
	use std::mem::transmute;

	/// A nan-tagged Value representing any data in the VM
	#[repr(transparent)]
	#[derive(Clone, Copy)]
	pub struct Value(u64);

	#[cfg(feature="nantag_clear_unusable_data_bits")]
	macro_rules! convert_data {
		($data:expr, $td:expr) => {
			Value((($data as u64) & Value::DATA_MASK) | (Value::NAN_MASK | ($td as u64)))
		};
	}

	#[cfg(not(feature="nantag_clear_unusable_data_bits"))]
	macro_rules! convert_data {
		($data:expr, $td:expr) => {
			Value(($data as u64) | (Value::NAN_MASK | ($td as u64)))
		};
	}

	impl Value {
		const SIGN_MASK: u64 = self::SIGN_MASK;
		const NAN_MASK:  u64 = 0b_0_11111111111_1_000_000000000000000000000000000000000000000000000000;
		const TYPE_MASK: u64 = 0b_0_00000000000_0_111_000000000000000000000000000000000000000000000000;
		const DATA_MASK: u64 = 0b_0_00000000000_0_000_111111111111111111111111111111111111111111111111;
		const SIGNED_TYPE_MASK: u64 = Self::SIGN_MASK | Self::TYPE_MASK;


		const fn get_nan_segment  (&self) -> u64 { self.0 & Self::NAN_MASK  }
		const fn get_type_segment (&self) -> u64 { self.0 & Self::TYPE_MASK }
		const fn get_data_segment (&self) -> u64 { self.0 & Self::DATA_MASK }
		const fn get_signed_type_segment (&self) -> u64 { self.0 & Self::SIGNED_TYPE_MASK }


		/// Get the data portion of a non-real value
		pub const fn get_data_bits (&self) -> u64 {
			self.get_data_segment()
		}

		/// Note that the use of this function may be slower
		/// than the various `is_`*`ty`* functions unless matching on many variants,
		/// because it requires an internal branch
		pub fn get_type_discriminator (&self) -> TypeDiscriminator {
			if (self.get_nan_segment() == Self::NAN_MASK) & (self.get_type_segment() != 0) {
				unsafe { transmute(self.get_signed_type_segment()) }
			} else {
				TypeDiscriminator::Real
			}
		}

		const fn is_td (&self, td: TypeDiscriminator) -> bool {
			const fn nan_discriminator (td: TypeDiscriminator) -> u64 { (td as u64) | Value::NAN_MASK }

			(self.0 & nan_discriminator(td)) == nan_discriminator(td)
		}

		
		/// Determine if a Value is of type `Real`
		pub const fn is_real (&self) -> bool { (self.get_nan_segment() != Self::NAN_MASK) | (self.get_type_segment() == 0) }

		/// Determine if a Value is of type `Integer`
		pub const fn is_integer (&self) -> bool { self.is_td(TypeDiscriminator::Integer) }

		/// Determine if a Value is of type `Character`
		pub const fn is_character (&self) -> bool { self.is_td(TypeDiscriminator::Character) }

		/// Determine if a Value is of type `Boolean`
		pub const fn is_boolean (&self) -> bool { self.is_td(TypeDiscriminator::Boolean) }

		/// Determine if a Value is of type `Nil`
		pub const fn is_nil (&self) -> bool { self.is_td(TypeDiscriminator::Nil) }

		/// Determine if a Value is of type `TypeID`
		pub const fn is_type_id (&self) -> bool { self.is_td(TypeDiscriminator::TypeID) }

		/// Determine if a Value is of type `Record`
		pub const fn is_record (&self) -> bool { self.is_td(TypeDiscriminator::Record) }

		/// Determine if a Value is of type `Array`
		pub const fn is_array (&self) -> bool { self.is_td(TypeDiscriminator::Array) }

		/// Determine if a Value is of type `Map`
		pub const fn is_map (&self) -> bool { self.is_td(TypeDiscriminator::Map) }

		/// Determine if a Value is of type `String`
		pub const fn is_string (&self) -> bool { self.is_td(TypeDiscriminator::String) }

		/// Determine if a Value is of type `Function`
		pub const fn is_function (&self) -> bool { self.is_td(TypeDiscriminator::Function) }

		/// Determine if a Value is of *subtype* `Closure`
		pub fn is_procedure (&self) -> bool { self.as_function().map(|p| FnCastable::get_kind(p) == FunctionKind::Procedure).unwrap_or(false) }

		/// Determine if a Value is of *subtype* `Closure`
		pub fn is_closure (&self) -> bool { self.as_function().map(|p| FnCastable::get_kind(p) == FunctionKind::Closure).unwrap_or(false) }
		
		/// Determine if a Value is of *subtype* `Foreign`
		pub fn is_foreign (&self) -> bool { self.as_function().map(|p| FnCastable::get_kind(p) == FunctionKind::Foreign).unwrap_or(false) }

		/// Determine if a Value is of type `Userdata`
		pub const fn is_userdata (&self) -> bool { self.is_td(TypeDiscriminator::Userdata) }


		/// Extract the internal `Real` in a Value
		/// # Safety
		/// Does not check that the Value actually contains the designated type
		pub unsafe fn as_real_unchecked (&self) -> f64 { f64::from_bits(self.0) }

		/// Extract the internal `Integer` in a Value
		/// # Safety
		/// Does not check that the Value actually contains the designated type
		pub unsafe fn as_integer_unchecked (&self) -> i32 { self.get_data_segment() as _ }

		/// Extract the internal `Character` in a Value
		/// # Safety
		/// Does not check that the Value actually contains the designated type
		pub unsafe fn as_character_unchecked (&self) -> char { std::char::from_u32_unchecked(self.get_data_segment() as u32) }

		/// Extract the internal `Boolean` in a Value
		/// # Safety
		/// Does not check that the Value actually contains the designated type
		pub unsafe fn as_boolean_unchecked (&self) -> bool { self.get_data_segment() == 1 }

		/// Extract the internal `Type` in a Value
		/// # Safety
		/// Does not check that the Value actually contains the designated type
		pub unsafe fn as_type_id_unchecked (&self) -> TypeID { TypeID(self.get_data_segment() as u16) }

		/// Extract the internal `Record` in a Value
		/// # Safety
		/// Does not check that the Value actually contains the designated type
		pub unsafe fn as_record_unchecked (&self) -> *mut object::Record { self.get_data_segment() as _ }

		/// Extract the internal `Array` in a Value
		/// # Safety
		/// Does not check that the Value actually contains the designated type
		pub unsafe fn as_array_unchecked (&self) -> *mut object::Array { self.get_data_segment() as _ }

		/// Extract the internal `Map` in a Value
		/// # Safety
		/// Does not check that the Value actually contains the designated type
		pub unsafe fn as_map_unchecked (&self) -> *mut object::Map { self.get_data_segment() as _ }

		/// Extract the internal `String` in a Value
		/// # Safety
		/// Does not check that the Value actually contains the designated type
		pub unsafe fn as_string_unchecked (&self) -> *mut object::String { self.get_data_segment() as _ }

		/// Extract the internal `Function` in a Value
		/// # Safety
		/// Does not check that the Value actually contains the designated type
		pub unsafe fn as_function_unchecked (&self) -> *mut object::Function { self.get_data_segment() as _ }

		/// Extract the internal `Closure` in a Value
		/// # Safety
		/// Does not check that the Value actually contains the designated type
		pub unsafe fn as_procedure_unchecked (&self) -> *mut object::Procedure { self.as_function_unchecked() as _ }

		/// Extract the internal `Closure` in a Value
		/// # Safety
		/// Does not check that the Value actually contains the designated type
		pub unsafe fn as_closure_unchecked (&self) -> *mut object::Closure { self.as_function_unchecked() as _ }
		
		/// Extract the internal `Foreign` in a Value
		/// # Safety
		/// Does not check that the Value actually contains the designated type
		pub unsafe fn as_foreign_unchecked (&self) -> *mut object::Foreign { self.as_function_unchecked() as _ }
		
		/// Extract the internal `Userdata` in a Value
		/// # Safety
		/// Does not check that the Value actually contains the designated type
		pub unsafe fn as_userdata_unchecked (&self) -> *mut object::Userdata { self.get_data_segment() as _ }


		/// Extract the internal `Real` in a Value
		pub fn as_real (&self) -> Option<f64> { if self.is_real() { Some(unsafe { self.as_real_unchecked() }) } else { None } }

		/// Extract the internal `Integer` in a Value
		pub fn as_integer (&self) -> Option<i32> { if self.is_integer() { Some(unsafe { self.as_integer_unchecked() }) } else { None } }

		/// Extract the internal `Character` in a Value
		pub fn as_character (&self) -> Option<char> { if self.is_character() { Some(unsafe { self.as_character_unchecked() }) } else { None } }

		/// Extract the internal `Boolean` in a Value
		pub fn as_boolean (&self) -> Option<bool> { if self.is_boolean() { Some(unsafe { self.as_boolean_unchecked() }) } else { None } }

		/// Extract the internal `TypeID` in a Value
		pub fn as_type_id (&self) -> Option<TypeID> { if self.is_type_id() { Some(unsafe { self.as_type_id_unchecked() }) } else { None } }

		/// Extract the internal `Record` in a Value
		pub fn as_record (&self) -> Option<*mut object::Record> { if self.is_record() { Some(unsafe { self.as_record_unchecked() }) } else { None } }

		/// Extract the internal `Array` in a Value
		pub fn as_array (&self) -> Option<*mut object::Array> { if self.is_array() { Some(unsafe { self.as_array_unchecked() }) } else { None } }

		/// Extract the internal `Map` in a Value
		pub fn as_map (&self) -> Option<*mut object::Map> { if self.is_map() { Some(unsafe { self.as_map_unchecked() }) } else { None } }

		/// Extract the internal `String` in a Value
		pub fn as_string (&self) -> Option<*mut object::String> { if self.is_string() { Some(unsafe { self.as_string_unchecked() }) } else { None } }

		/// Extract the internal `Function` in a Value
		pub fn as_function (&self) -> Option<*mut object::Function> { if self.is_function() { Some(unsafe { self.as_function_unchecked() }) } else { None } }

		/// Extract the internal `Procedure` in a Value
		pub fn as_procedure (&self) -> Option<*mut object::Procedure> { self.as_function().and_then(FnCastMut::as_procedure_mut) }

		/// Extract the internal `Closure` in a Value
		pub fn as_closure (&self) -> Option<*mut object::Closure> { self.as_function().and_then(FnCastMut::as_closure_mut) }
		
		/// Extract the internal `Foreign` in a Value
		pub fn as_foreign (&self) -> Option<*mut object::Foreign> { self.as_function().and_then(FnCastMut::as_foreign_mut) }

		/// Extract the internal `Userdata` in a Value
		pub fn as_userdata (&self) -> Option<*mut object::Userdata> { if self.is_userdata() { Some(unsafe { self.as_userdata_unchecked() }) } else { None } }


		/// Create a Value wrapping for data of `Real` type
		pub fn from_real (data: f64) -> Self { unsafe { transmute(data) } }

		/// Create a Value wrapping for data of `Integer` type
		pub fn from_integer (data: i32) -> Self { convert_data!(data, TypeDiscriminator::Integer) }

		/// Create a Value wrapping for data of `Character` type
		pub fn from_character (data: char) -> Self { convert_data!(data, TypeDiscriminator::Character) }

		/// Create a Value wrapping for data of `Boolean` type
		pub fn from_boolean (data: bool) -> Self { convert_data!(data, TypeDiscriminator::Boolean) }

		/// Create a Value wrapping for data of `Nil` type
		pub fn from_nil () -> Self { Self(Self::NAN_MASK | (TypeDiscriminator::Nil as u64)) }

		/// Create a Value wrapping for data of `TypeID` type
		pub fn from_type_id (data: TypeID) -> Self { convert_data!(data.0, TypeDiscriminator::TypeID) }

		/// Create a Value wrapping for data of `Record` type
		pub fn from_record (data: *mut object::Record) -> Self { convert_data!(data, TypeDiscriminator::Record) }

		/// Create a Value wrapping for data of `Array` type
		pub fn from_array (data: *mut object::Array) -> Self { convert_data!(data, TypeDiscriminator::Array) }

		/// Create a Value wrapping for data of `Map` type
		pub fn from_map (data: *mut object::Map) -> Self { convert_data!(data, TypeDiscriminator::Map) }

		/// Create a Value wrapping for data of `String` type
		pub fn from_string (data: *mut object::String) -> Self { convert_data!(data, TypeDiscriminator::String) }

		/// Create a Value wrapping for data of `Function` type
		pub fn from_function (data: *mut object::Function) -> Self { convert_data!(data, TypeDiscriminator::Function) }

		// /// Create a Value wrapping for data of `Closure` type
		// pub fn from_closure (data: *mut object::Closure) -> Self { convert_data!(data, TypeDiscriminator::Closure) }

		/// Create a Value wrapping for data of `Userdata` type
		pub fn from_userdata (data: *mut object::Userdata) -> Self { convert_data!(data, TypeDiscriminator::Userdata) }

		// /// Create a Value wrapping for data of `Foreign` type
		// pub fn from_foreign (data: *mut object::Foreign) -> Self { convert_data!(data, TypeDiscriminator::Foreign) }
	}
}


#[cfg(not(feature="nantag"))]
mod internal {
	use super::*;

	/// An enum Value representing any data in the VM
	#[repr(C)]
	#[derive(Clone, Copy)]
	pub struct Value {
		discriminant: TypeDiscriminator,
		data: ValueData
	}

	#[repr(C)]
	#[derive(Clone, Copy)]
	union ValueData {
		bits: u64,

		real: f64,
		integer: i32,
		character: char,
		boolean: bool,
		nil: (),
		type_id: TypeID,

		record: *mut object::Record,
		array: *mut object::Array,
		map: *mut object::Map,
		string: *mut object::String,
		function: *mut object::Function,
		closure: *mut object::Closure,
		userdata: *mut object::Userdata,
		foreign: *mut object::Foreign,
	}


	impl Value {
		/// Get the data portion of a non-real value
		pub fn get_data_bits (&self) -> u64 {
			unsafe { self.data.bits }
		}

		/// Note that the use of this function may be slower
		/// than the various `is_`*`ty`* functions unless matching on many variants,
		/// because it requires an internal branch
		pub const fn get_type_discriminator (&self) -> TypeDiscriminator {
			self.discriminant
		}

		const fn is_td (&self, td: TypeDiscriminator) -> bool {
			self.discriminant as u64 == td as u64
		}


		/// Determine if a Value is of type `Real`
		pub const fn is_real (&self) -> bool { self.is_td(TypeDiscriminator::Real) }

		/// Determine if a Value is of type `Integer`
		pub const fn is_integer (&self) -> bool { self.is_td(TypeDiscriminator::Integer) }

		/// Determine if a Value is of type `Character`
		pub const fn is_character (&self) -> bool { self.is_td(TypeDiscriminator::Character) }

		/// Determine if a Value is of type `Boolean`
		pub const fn is_boolean (&self) -> bool { self.is_td(TypeDiscriminator::Boolean) }

		/// Determine if a Value is of type `Nil`
		pub const fn is_nil (&self) -> bool { self.is_td(TypeDiscriminator::Nil) }

		/// Determine if a Value is of type `TypeID`
		pub const fn is_type_id (&self) -> bool { self.is_td(TypeDiscriminator::TypeID) }

		/// Determine if a Value is of type `Record`
		pub const fn is_record (&self) -> bool { self.is_td(TypeDiscriminator::Record) }

		/// Determine if a Value is of type `Array`
		pub const fn is_array (&self) -> bool { self.is_td(TypeDiscriminator::Array) }

		/// Determine if a Value is of type `Map`
		pub const fn is_map (&self) -> bool { self.is_td(TypeDiscriminator::Map) }

		/// Determine if a Value is of type `String`
		pub const fn is_string (&self) -> bool { self.is_td(TypeDiscriminator::String) }

		/// Determine if a Value is of type `Function`
		pub const fn is_function (&self) -> bool { self.is_td(TypeDiscriminator::Function) }

		/// Determine if a Value is of *subtype* `Procedure`
		pub fn is_procedure (&self) -> bool { self.as_function().map(|f| FnCastable::get_kind(f) == FunctionKind::Procedure).unwrap_or(false) }

		/// Determine if a Value is of *subtype* `Closure`
		pub fn is_closure (&self) -> bool { self.as_function().map(|f| FnCastable::get_kind(f) == FunctionKind::Closure).unwrap_or(false) }

		/// Determine if a Value is of *subtype* `Foreign`
		pub fn is_foreign (&self) -> bool { self.as_function().map(|f| FnCastable::get_kind(f) == FunctionKind::Foreign).unwrap_or(false) }

		/// Determine if a Value is of type `Userdata`
		pub const fn is_userdata (&self) -> bool { self.is_td(TypeDiscriminator::Userdata) }


		/// Extract the internal `Real` in a Value
		/// # Safety
		/// Does not check that the Value actually contains the designated type
		pub unsafe fn as_real_unchecked (&self) -> f64 { self.data.real }
		
		/// Extract the internal `Integer` in a Value
		/// # Safety
		/// Does not check that the Value actually contains the designated type
		pub unsafe fn as_integer_unchecked (&self) -> i32 { self.data.integer }
		
		/// Extract the internal `Character` in a Value
		/// # Safety
		/// Does not check that the Value actually contains the designated type
		pub unsafe fn as_character_unchecked (&self) -> char { self.data.character }
		
		/// Extract the internal `Boolean` in a Value
		/// # Safety
		/// Does not check that the Value actually contains the designated type
		pub unsafe fn as_boolean_unchecked (&self) -> bool { self.data.boolean }
		
		/// Extract the internal `TypeID` in a Value
		/// # Safety
		/// Does not check that the Value actually contains the designated type
		pub unsafe fn as_type_id_unchecked (&self) -> TypeID { self.data.type_id }
		
		/// Extract the internal `Record` in a Value
		/// # Safety
		/// Does not check that the Value actually contains the designated type
		pub unsafe fn as_record_unchecked (&self) -> *mut object::Record { self.data.record }
		
		/// Extract the internal `Array` in a Value
		/// # Safety
		/// Does not check that the Value actually contains the designated type
		pub unsafe fn as_array_unchecked (&self) -> *mut object::Array { self.data.array }
		
		/// Extract the internal `Map` in a Value
		/// # Safety
		/// Does not check that the Value actually contains the designated type
		pub unsafe fn as_map_unchecked (&self) -> *mut object::Map { self.data.map }
		
		/// Extract the internal `String` in a Value
		/// # Safety
		/// Does not check that the Value actually contains the designated type
		pub unsafe fn as_string_unchecked (&self) -> *mut object::String { self.data.string }
		
		/// Extract the internal `Function` in a Value
		/// # Safety
		/// Does not check that the Value actually contains the designated type
		pub unsafe fn as_function_unchecked (&self) -> *mut object::Function { self.data.function }
		
		/// Extract the internal `Procedure` in a Value
		/// # Safety
		/// Does not check that the Value actually contains the designated type
		pub unsafe fn as_procedure_unchecked (&self) -> *mut object::Procedure { self.data.function as _ }
		
		/// Extract the internal `Closure` in a Value
		/// # Safety
		/// Does not check that the Value actually contains the designated type
		pub unsafe fn as_closure_unchecked (&self) -> *mut object::Closure { self.data.function as _ }

		/// Extract the internal `Foreign` in a Value
		/// # Safety
		/// Does not check that the Value actually contains the designated type
		pub unsafe fn as_foreign_unchecked (&self) -> *mut object::Foreign { self.data.function as _ }
		
		/// Extract the internal `Userdata` in a Value
		/// # Safety
		/// Does not check that the Value actually contains the designated type
		pub unsafe fn as_userdata_unchecked (&self) -> *mut object::Userdata { self.data.userdata }
		

		/// Extract the internal `Real` in a Value
		pub fn as_real (&self) -> Option<f64> { if self.is_real() { Some(unsafe { self.as_real_unchecked() }) } else { None } }

		/// Extract the internal `Integer` in a Value
		pub fn as_integer (&self) -> Option<i32> { if self.is_integer() { Some(unsafe { self.as_integer_unchecked() }) } else { None } }

		/// Extract the internal `Character` in a Value
		pub fn as_character (&self) -> Option<char> { if self.is_character() { Some(unsafe { self.as_character_unchecked() }) } else { None } }

		/// Extract the internal `Boolean` in a Value
		pub fn as_boolean (&self) -> Option<bool> { if self.is_boolean() { Some(unsafe { self.as_boolean_unchecked() }) } else { None } }

		/// Extract the internal `TypeID` in a Value
		pub fn as_type_id (&self) -> Option<TypeID> { if self.is_type_id() { Some(unsafe { self.as_type_id_unchecked() }) } else { None } }

		/// Extract the internal `Record` in a Value
		pub fn as_record (&self) -> Option<*mut object::Record> { if self.is_record() { Some(unsafe { self.as_record_unchecked() }) } else { None } }

		/// Extract the internal `Array` in a Value
		pub fn as_array (&self) -> Option<*mut object::Array> { if self.is_array() { Some(unsafe { self.as_array_unchecked() }) } else { None } }

		/// Extract the internal `Map` in a Value
		pub fn as_map (&self) -> Option<*mut object::Map> { if self.is_map() { Some(unsafe { self.as_map_unchecked() }) } else { None } }

		/// Extract the internal `String` in a Value
		pub fn as_string (&self) -> Option<*mut object::String> { if self.is_string() { Some(unsafe { self.as_string_unchecked() }) } else { None } }

		/// Extract the internal `Function` in a Value
		pub fn as_function (&self) -> Option<*mut object::Function> { if self.is_function() { Some(unsafe { self.as_function_unchecked() }) } else { None } }

		/// Extract the internal `Procedure` in a Value
		pub fn as_procedure (&self) -> Option<*mut object::Procedure> { self.as_function().and_then(FnCastMut::as_procedure_mut) }

		/// Extract the internal `Closure` in a Value
		pub fn as_closure (&self) -> Option<*mut object::Closure> { self.as_function().and_then(FnCastMut::as_closure_mut) }
		
		/// Extract the internal `Foreign` in a Value
		pub fn as_foreign (&self) -> Option<*mut object::Foreign> { self.as_function().and_then(FnCastMut::as_foreign_mut) }

		/// Extract the internal `Userdata` in a Value
		pub fn as_userdata (&self) -> Option<*mut object::Userdata> { if self.is_userdata() { Some(unsafe { self.as_userdata_unchecked() }) } else { None } }


		/// Create a Value wrapping for data of `Real` type
		pub fn from_real (data: f64) -> Self { Self { discriminant: TypeDiscriminator::Real, data: ValueData { real: data } } }

		/// Create a Value wrapping for data of `Integer` type
		pub fn from_integer (data: i32) -> Self { Self { discriminant: TypeDiscriminator::Integer, data: ValueData { integer: data } } }

		/// Create a Value wrapping for data of `Character` type
		pub fn from_character (data: char) -> Self { Self { discriminant: TypeDiscriminator::Character, data: ValueData { character: data } } }

		/// Create a Value wrapping for data of `Boolean` type
		pub fn from_boolean (data: bool) -> Self { Self { discriminant: TypeDiscriminator::Boolean, data: ValueData { boolean: data } } }

		/// Create a Value wrapping for data of `Nil` type
		pub fn from_nil () -> Self { Self { discriminant: TypeDiscriminator::Nil, data: ValueData { nil: () } } }

		/// Create a Value wrapping for data of `TypeID` type
		pub fn from_type_id (data: TypeID) -> Self { Self { discriminant: TypeDiscriminator::TypeID, data: ValueData { type_id: data } } }

		/// Create a Value wrapping for data of `Record` type
		pub fn from_record (data: *mut object::Record) -> Self { Self { discriminant: TypeDiscriminator::Record, data: ValueData { record: data } } }

		/// Create a Value wrapping for data of `Array` type
		pub fn from_array (data: *mut object::Array) -> Self { Self { discriminant: TypeDiscriminator::Array, data: ValueData { array: data } } }

		/// Create a Value wrapping for data of `Map` type
		pub fn from_map (data: *mut object::Map) -> Self { Self { discriminant: TypeDiscriminator::Map, data: ValueData { map: data } } }

		/// Create a Value wrapping for data of `String` type
		pub fn from_string (data: *mut object::String) -> Self { Self { discriminant: TypeDiscriminator::String, data: ValueData { string: data } } }

		/// Create a Value wrapping for data of `Function` type
		pub fn from_function (data: *mut object::Function) -> Self { Self { discriminant: TypeDiscriminator::Function, data: ValueData { function: data } } }

		/// Create a Value wrapping for data of `Function` type
		pub fn from_procedure (data: *mut object::Procedure) -> Self { Self::from_function(data as _) }

		/// Create a Value wrapping for data of `Function` type
		pub fn from_closure (data: *mut object::Closure) -> Self { Self::from_function(data as _) }

		/// Create a Value wrapping for data of `Function` type
		pub fn from_foreign (data: *mut object::Foreign) -> Self { Self::from_function(data as _) }

		/// Create a Value wrapping for data of `Userdata` type
		pub fn from_userdata (data: *mut object::Userdata) -> Self { Self { discriminant: TypeDiscriminator::Userdata, data: ValueData { userdata: data } } }
	}
}


impl Debug for Value {
	fn fmt (&self, f: &mut Formatter<'_>) -> fmt::Result {
		let td = self.get_type_discriminator();
		match td {
			TypeDiscriminator::Real => write!(f, "Real({})", unsafe { self.as_real_unchecked() }),
			TypeDiscriminator::Integer => write!(f, "Integer({})", unsafe { self.as_integer_unchecked() }),
			TypeDiscriminator::Character => write!(f, "Character({:?})", unsafe { self.as_character_unchecked() }),
			TypeDiscriminator::Boolean => write!(f, "Boolean({})", unsafe { self.as_boolean_unchecked() }),
			TypeDiscriminator::Nil => write!(f, "Nil"),
			TypeDiscriminator::TypeID => write!(f, "TypeID({:?})", unsafe { self.as_type_id_unchecked() }),
			TypeDiscriminator::String => write!(f, "String({:?})", unsafe { &*self.as_string_unchecked() }.data),
			_ => write!(f, "{}(@{:012x})", td, self.get_data_bits())
		}
	}
}

impl PartialEq for Value {
	fn eq (&self, other: &Self) -> bool {
		match (self.get_type_discriminator(), other.get_type_discriminator()) {
			(TypeDiscriminator::Real, TypeDiscriminator::Real) =>  {
				let (a, b) = unsafe { (self.as_real_unchecked(), other.as_real_unchecked()) };

				a == b
			},

			(TypeDiscriminator::Record, TypeDiscriminator::Record) => {
				let (a, b) = unsafe { (&*self.as_record_unchecked(), &*other.as_record_unchecked()) };

				a == b
			},

			(TypeDiscriminator::Array, TypeDiscriminator::Array) => {
				let (a, b) = unsafe { (&*self.as_array_unchecked(), &*other.as_array_unchecked()) };

				a == b
			},

			(TypeDiscriminator::Map, TypeDiscriminator::Map) => {
				let (a, b) = unsafe { (&*self.as_map_unchecked(), &*other.as_map_unchecked()) };

				a == b
			},

			(TypeDiscriminator::String, TypeDiscriminator::String) => {
				let (a, b) = unsafe { (&*self.as_string_unchecked(), &*other.as_string_unchecked()) };

				a == b
			},

			(a, b) => (a == b) && (self.get_data_bits() == other.get_data_bits())
		}
	}
}

impl Eq for Value { }

impl Ord for Value {
	fn cmp (&self, other: &Self) -> Ordering {
		match (self.get_type_discriminator(), other.get_type_discriminator()) {
			(TypeDiscriminator::Real, TypeDiscriminator::Real) => {
				let (a, b) = unsafe { (self.as_real_unchecked(), other.as_real_unchecked()) };

				a.partial_cmp(&b).unwrap_or(Ordering::Equal)
			},

			(TypeDiscriminator::Record, TypeDiscriminator::Record) => {
				let (a, b) = unsafe { (&*self.as_record_unchecked(), &*other.as_record_unchecked()) };

				a.cmp(b)
			},

			(TypeDiscriminator::Array, TypeDiscriminator::Array) => {
				let (a, b) = unsafe { (&*self.as_array_unchecked(), &*other.as_array_unchecked()) };

				a.cmp(b)
			},

			(TypeDiscriminator::Map, TypeDiscriminator::Map) => {
				let (a, b) = unsafe { (&*self.as_map_unchecked(), &*other.as_map_unchecked()) };

				a.cmp(b)
			},

			(TypeDiscriminator::String, TypeDiscriminator::String) => {
				let (a, b) = unsafe { (&*self.as_string_unchecked(), &*other.as_string_unchecked()) };

				a.cmp(b)
			},

			(a, b) => a.cmp(&b).then(self.get_data_bits().cmp(&other.get_data_bits()))
		}
	}
}

impl PartialOrd for Value {
	fn partial_cmp (&self, other: &Self) -> Option<Ordering> {
		Some(self.cmp(other))
	}
}

impl Hash for Value {
	fn hash<H: Hasher> (&self, h: &mut H) {
		let td = self.get_type_discriminator();
		td.hash(h);
		match td {
			TypeDiscriminator::Record => unsafe { &*self.as_record_unchecked() }.hash(h),
			TypeDiscriminator::Array => unsafe { &*self.as_array_unchecked() }.hash(h),
			TypeDiscriminator::Map => unsafe { &*self.as_map_unchecked() }.hash(h),
			TypeDiscriminator::String => unsafe { &*self.as_string_unchecked() }.hash(h),
			_ => self.get_data_bits().hash(h)
		}
	}
}

impl Default for Value {
	fn default () -> Self { Self::from_nil() }
}