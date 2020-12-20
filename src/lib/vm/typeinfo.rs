//! Contains information and ids related to types
use std::{
	mem::{ transmute },
	hash::{ Hash, Hasher },
	collections::{ HashMap, hash_map::DefaultHasher },
};

use macros::unchecked_destructure;
use macros::static_assert;
use super::value::TypeDiscriminator;

/// Identifies which sub-variant of a type is represented by an object (E.g. which Record type, etc)
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeID(pub(crate) u16);

impl TypeID {
	/// The maximum number of types a script can reference
	pub const MAX_TYPES: usize = u16::MAX as _;
}


/// Defines the higher-level type of a type
#[repr(u8)]
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TypeKind {
	Nil,
	Real,
	Integer,
	Character,
	Boolean,
	TypeID,
	String,

	Record,
	Array,
	Map,
	Function,
	Closure,
	Userdata,
	Foreign,
}

impl From<PrimitiveType> for TypeKind {
	fn from (prim: PrimitiveType) -> Self {
		static_assert!(
				 TypeKind::Nil as u8 == PrimitiveType::Nil as u8
			&& TypeKind::Real as u8 == PrimitiveType::Real as u8
			&& TypeKind::Integer as u8 == PrimitiveType::Integer as u8
			&& TypeKind::Character as u8 == PrimitiveType::Character as u8
			&& TypeKind::Boolean as u8 == PrimitiveType::Boolean as u8
			&& TypeKind::TypeID as u8 == PrimitiveType::TypeID as u8
			&& TypeKind::String as u8 == PrimitiveType::String as u8
		);

		// SAFETY: TypeKind and PrimitiveType have the same discriminators,
		// for the subset of TypeKind represented by PrimitiveType; See static_assert above
		unsafe { transmute(prim) }
	}
}

impl From<&TypeInfo> for TypeKind {
	fn from (info: &TypeInfo) -> Self {
		match info {
			TypeInfo::Primitive(prim) => (*prim).into(),
			TypeInfo::Record { .. } => TypeKind::Record,
			TypeInfo::Array(_) => TypeKind::Array,
			TypeInfo::Map(_, _) => TypeKind::Map,
			TypeInfo::Function { kind: FunctionKind::Free, .. } => TypeKind::Function,
			TypeInfo::Function { kind: FunctionKind::Closure, .. } => TypeKind::Closure,
			TypeInfo::Function { kind: FunctionKind::Foreign, .. } => TypeKind::Foreign,
			TypeInfo::Userdata(_) => TypeKind::Userdata,
		}
	}
}

impl From<TypeDiscriminator> for TypeKind {
	fn from (disc: TypeDiscriminator) -> Self {
		match disc {
			TypeDiscriminator::Real => TypeKind::Real,
			TypeDiscriminator::Integer => TypeKind::Integer,
			TypeDiscriminator::Character => TypeKind::Character,
			TypeDiscriminator::Boolean => TypeKind::Boolean,
			TypeDiscriminator::Nil => TypeKind::Nil,
			TypeDiscriminator::TypeID => TypeKind::TypeID,
			TypeDiscriminator::Record => TypeKind::Record,
			TypeDiscriminator::Array => TypeKind::Array,
			TypeDiscriminator::Map => TypeKind::Map,
			TypeDiscriminator::String => TypeKind::String,
			TypeDiscriminator::Function => TypeKind::Function,
			TypeDiscriminator::Closure => TypeKind::Closure,
			TypeDiscriminator::Userdata => TypeKind::Userdata,
			TypeDiscriminator::Foreign => TypeKind::Foreign,
		}
	}
}


/// A freestanding type which contains no references to other types;
/// Note that this is not necessarily a type for a scalar *value*,
/// its a type which can be expressed in terms of its variant alone
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PrimitiveType {
	/// Represents the absence of data
	Nil,
	/// Floating point number
	Real,
	/// Signed 32 bit integer
	Integer,
	/// 24 bit fixed width unicode encoded codepoint
	Character,
	/// True or False
	Boolean,
	/// A reference to a type
	TypeID,
	/// A collection of characters forming a stream of text
	String
}

/// Singifies whether a function is a native function or if it is internal to the VM, whether it is a free function or a closure
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum FunctionKind {
	/// A free function or method, internal to the vm, with no internal state
	Free,
	/// A function internal to the vm, with its own bound internal state
	Closure,
	/// A native machine code function provided by the vm's host
	Foreign,
}

/// Contains type-specific data for values
#[repr(u8)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TypeInfo {
	/// A primitive scalar value
	Primitive(PrimitiveType),
	/// A named type representing a collection of values of various types
	Record {
		/// The types associated with each field
		field_types: Vec<TypeID>,
		/// The names associated with each field
		field_names: Vec<String>,
	},
	/// A contiguous series of values of the same type
	Array(TypeID),
	/// A hashmap table from values of type 0 to type 1
	Map(TypeID, TypeID),
	/// A functional interface, free, method or closure
	Function {
		/// Indicates whether a function is a native function or if it is internal to the VM, whether it is a free function or a closure
		kind: FunctionKind,
		/// The type of value, if any, returned by a function
		return_type: Option<TypeID>,
		/// The type of each parameter passed to a function, if any
		parameter_types: Vec<TypeID>,
	},
	/// A named type representing a native value
	Userdata(String),
}

/// Stores TypeInfo and provides association to unique TypeIDs for each type
pub struct TypeRegistry {
	info: Vec<TypeInfo>,
	records: Vec<(u64, TypeID)>,
	arrays: HashMap<TypeID, TypeID>,
	maps: HashMap<(TypeID, TypeID), TypeID>,
	functions: Vec<(u64, TypeID)>,
	userdata: Vec<(u64, TypeID)>,
}


// This has to be outside of impl TypeRegistry due to const eval problems
const BUILTIN_TYPEINFO: &[TypeInfo] = {
	use TypeInfo::*;
	use PrimitiveType::*;

	&[
		Primitive(Nil),
		Primitive(Real),
		Primitive(Integer),
		Primitive(Character),
		Primitive(Boolean),
		Primitive(TypeID),
		Primitive(String),
	]
};

impl TypeID {
	/// Get the universal TypeID of a PrimitiveType
	pub const fn from_primitive (prim: PrimitiveType) -> TypeID {
		// Primitive types are added in the same order they are declared, so their ids are their discriminants
		{
			macro_rules! check_prim { ($($var:ident),*) => { $( static_assert!(matches!(BUILTIN_TYPEINFO[PrimitiveType::$var as usize], TypeInfo::Primitive(PrimitiveType::$var))); )* } }

			check_prim!(Nil, Real, Integer, Character, Boolean, TypeID, String);
		}

		TypeID(prim as u16)
	}
}

impl TypeRegistry {
	// Though the length is useful information, a properly-created TypeRegistry is never empty because of builtins
	#![allow(clippy::clippy::len_without_is_empty)]

	/// Create a new TypeRegistry and initialize it with builtin types
	pub fn new () -> Self {
		let mut out = Self { .. Default::default() };

		out.load_builtins();

		out
	}

	fn load_builtins (&mut self) {
		for builtin in BUILTIN_TYPEINFO {
			self.register_type(builtin.clone());
		}
	}

	fn register_type (&mut self, info: TypeInfo) -> Option<TypeID> {
		let idx = self.info.len();
		if idx >= TypeID::MAX_TYPES { return None }

		self.info.push(info);

		Some(TypeID(idx as _))
	}

	fn register_type_with <F: FnOnce () -> TypeInfo> (&mut self, f: F) -> Option<TypeID> {
		let idx = self.info.len();
		if idx >= TypeID::MAX_TYPES { return None }

		self.info.push(f());

		Some(TypeID(idx as _))
	}

	/// Get TypeInfo from a TypeID
	/// # Safety
	/// This does not validate the TypeID provided
	pub unsafe fn get_type_unchecked (&self, id: TypeID) -> &TypeInfo {
		self.info.get_unchecked(id.0 as usize)
	}

	/// Get TypeInfo from a TypeID
	pub fn get_type (&self, id: TypeID) -> Option<&TypeInfo> {
		self.info.get(id.0 as usize)
	}

	/// Get the number of unique types stored in a TypeRegistry
	pub fn len (&self) -> usize {
		self.info.len()
	}

	/// Get the universal TypeID of a PrimitiveType
	pub const fn primitive_id (&self, prim: PrimitiveType) -> TypeID {
		TypeID::from_primitive(prim)
	}

	/// Create a new Userdata type
	/// 
	/// Returns None if:
	/// + A Userdata type with the given name already exists
	/// + There are already `TypeID::MAX_TYPES` types registered but a new one needs to be created
	pub fn create_userdata (&mut self, name: &str) -> Option<TypeID> {
		let in_name = name;

		let mut hasher = DefaultHasher::default();
		in_name.hash(&mut hasher);
		let in_hash = hasher.finish();

		for &(existing_hash, existing_id) in self.userdata.iter() {
			if in_hash != existing_hash { continue }
			
			// SAFETY:
			// 1. types are never deleted, so any id inserted into self.functions will be a valid index in self.info
			// 2. only function ids are stored in the functions array, so existing_info will never be anything but a function
			unsafe { unchecked_destructure!(
				self.info.get_unchecked(existing_id.0 as usize),
				TypeInfo::Userdata(existing_name)
				=> if in_name == existing_name {
					return Some(existing_id)
				} else {
					continue
				}
			) }
		}

		let new_id = self.register_type_with(|| TypeInfo::Userdata(name.to_owned()))?;
		self.userdata.push((in_hash, new_id));
		
		Some(new_id)
	}
	
	/// Find a Userdata type by name, if one exists
	pub fn find_userdata (&self, name: &str) -> Option<TypeID> {
		let in_name = name;

		let mut hasher = DefaultHasher::default();
		in_name.hash(&mut hasher);
		let in_hash = hasher.finish();

		for &(existing_hash, existing_id) in self.userdata.iter() {
			if in_hash != existing_hash { continue }
			
			// SAFETY:
			// 1. types are never deleted, so any id inserted into self.functions will be a valid index in self.info
			// 2. only function ids are stored in the functions array, so existing_info will never be anything but a function
			unsafe { unchecked_destructure!(
				self.info.get_unchecked(existing_id.0 as usize),
				TypeInfo::Userdata(existing_name)
				=> if in_name == existing_name {
					return Some(existing_id)
				} else {
					continue
				}
			) }
		}

		None
	}

	/// Create a new Record type
	///
	/// Returns `None` if there are already `TypeID::MAX_TYPES` types registered but a new one needs to be created
	pub fn create_record (&mut self, field_types: &[TypeID], field_names: &[String]) -> Option<TypeID> {
		let (in_field_types, in_field_names) = (field_types, field_names);

		let mut hasher = DefaultHasher::default();

		in_field_types.hash(&mut hasher);
		in_field_names.hash(&mut hasher);

		let in_hash = hasher.finish();

		for &(existing_hash, existing_id) in self.records.iter() {
			if existing_hash == in_hash {
				// SAFETY:
				// 1. types are never deleted, so any id inserted into self.records will be a valid index in self.info
				// 2. only record ids are stored in the records array, so existing_info will never be anything but a record
				unsafe { unchecked_destructure!(
					self.info.get_unchecked(existing_id.0 as usize), 
					TypeInfo::Record { field_types, field_names, .. }
					=> if field_types.as_slice() == in_field_types
					&& field_names.as_slice() == in_field_names {
						return Some(existing_id)
					}
				) }
			}
		}
		
		let new_id = self.register_type_with(|| TypeInfo::Record { field_types: field_types.to_owned(), field_names: field_names.to_owned() })?;
		self.records.push((in_hash, new_id));

		Some(new_id)
	}

	/// Get an id for an anonymous array type, returns an existing id if one exists or creates a new one if necessary
	///
	/// Returns `None` if there are already `TypeID::MAX_TYPES` types registered but a new one needs to be created
	pub fn create_array (&mut self, elem_type: TypeID) -> Option<TypeID> {
		Some(if let Some(&existing_id) = self.arrays.get(&elem_type) {
			existing_id
		} else {
			let new_id = self.register_type(TypeInfo::Array(elem_type))?;
			self.arrays.insert(elem_type, new_id);

			new_id
		})
	}

	/// Get an id for an anonymous map type, returns an existing id if one exists or creates a new one if necessary
	///
	/// Returns `None` if there are already `TypeID::MAX_TYPES` types registered but a new one needs to be created
	pub fn create_map (&mut self, key_type: TypeID, value_type: TypeID) -> Option<TypeID> {
		Some(if let Some(&existing_id) = self.maps.get(&(key_type, value_type)) {
			existing_id
		} else {
			let new_id = self.register_type(TypeInfo::Map(key_type, value_type))?;
			self.maps.insert((key_type, value_type), new_id);

			new_id
		})
	}

	/// Get an id for an anonymous function type, returns an existing id if one exists or creates a new one if necessary
	///
	/// Returns `None` if there are already `TypeID::MAX_TYPES` types registered but a new one needs to be created
	pub fn create_function (&mut self, kind: FunctionKind, return_type: Option<TypeID>, parameter_types: &[TypeID]) -> Option<TypeID> {
		let (in_kind, in_return_type, in_parameter_types) = (&kind, &return_type, parameter_types);

		let mut hasher = DefaultHasher::default();

		in_kind.hash(&mut hasher);
		in_return_type.hash(&mut hasher);
		in_parameter_types.hash(&mut hasher);

		let in_hash = hasher.finish();

		for &(existing_hash, existing_id) in self.functions.iter() {
			if in_hash != existing_hash { continue }

			// SAFETY:
			// 1. types are never deleted, so any id inserted into self.functions will be a valid index in self.info
			// 2. only function ids are stored in the functions array, so existing_info will never be anything but a function
			unsafe { unchecked_destructure!(
				self.info.get_unchecked(existing_id.0 as usize),
				TypeInfo::Function { kind, return_type, parameter_types }
				=> if kind == in_kind
				&& return_type == in_return_type
				&& parameter_types.as_slice() == in_parameter_types {
					return Some(existing_id)
				} else {
					continue
				}
			) }
		}

		let new_id = self.register_type_with(|| TypeInfo::Function { kind, return_type, parameter_types: parameter_types.to_owned() })?;
		self.functions.push((in_hash, new_id));

		Some(new_id)
	}
}

impl Default for TypeRegistry { fn default () -> Self { Self::new() } }


/// Allows generic passing of unowned representations of types to be registered and converted to owned equivalents if necessary
pub trait RegisterableAsType {
	/// Register this as a type in a TypeRegistry
	fn register_as_type (self, registry: &mut TypeRegistry) -> Option<TypeID>;
}


impl RegisterableAsType for PrimitiveType {
	fn register_as_type (self, _: &mut TypeRegistry) -> Option<TypeID> {
		Some(TypeID::from_primitive(self))
	}
}

impl RegisterableAsType for &str {
	fn register_as_type (self, registry: &mut TypeRegistry) -> Option<TypeID> {
		registry.create_userdata(self)
	}
}

impl RegisterableAsType for (TypeID,) {
	fn register_as_type (self, registry: &mut TypeRegistry) -> Option<TypeID> {
		registry.create_array(self.0)
	}
}

impl RegisterableAsType for (TypeID, TypeID) {
	fn register_as_type (self, registry: &mut TypeRegistry) -> Option<TypeID> {
		registry.create_map(self.0, self.1)
	}
}

impl RegisterableAsType for (&[TypeID], &[String]) {
	fn register_as_type (self, registry: &mut TypeRegistry) -> Option<TypeID> {
		registry.create_record(self.0, self.1)
	}
}

impl RegisterableAsType for (FunctionKind, Option<TypeID>, &[TypeID]) {
	fn register_as_type (self, registry: &mut TypeRegistry) -> Option<TypeID> {
		registry.create_function(self.0, self.1, self.2)
	}
}