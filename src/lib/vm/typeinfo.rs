//! Contains information and ids related to types
use std::{
	mem::{ transmute },
	hash::{ Hash },
};

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
		/// The names associated with each field
		field_names: Vec<String>,
		/// The types associated with each field
		field_types: Vec<TypeID>,
	},
	/// A contiguous series of values of the same type
	Array(TypeID),
	/// A hashmap table from values of type 0 to type 1
	Map(TypeID, TypeID),
	/// A functional interface, free, method or closure
	Function {
		/// Indicates whether a function is a native function or if it is internal to the VM, whether it is a free function or a closure
		kind: FunctionKind,
		/// The type of each parameter passed to a function, if any
		parameter_types: Vec<TypeID>,
		/// The type of value, if any, returned by a function
		return_type: Option<TypeID>,
	},
	/// A named type representing a native value
	Userdata(String),
}


/// Stores TypeInfo and provides association to TypeIDs for each type
#[derive(Debug)]
pub struct TypeRegistry {
	info: Vec<TypeInfo>
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

	// TODO unpub
	#![allow(missing_docs)]

	/// Create a new TypeRegistry and initialize it with builtin types
	pub fn new () -> Self {
		let mut out = Self {
			info: Vec::default(),
		};

		out.load_builtins();

		out
	}

	pub fn load_builtins (&mut self) {
		for builtin in BUILTIN_TYPEINFO {
			self.register_type(builtin.clone());
		}
	}

	pub fn id_iter (&self) -> std::iter::Map<std::ops::Range<u16>, impl FnMut(u16) -> TypeID> {
		(0..self.len() as u16).into_iter().map(TypeID)
	}

	pub fn iter (&self) -> std::slice::Iter<TypeInfo> {
		self.info.iter()
	}

	fn next_id (&mut self) -> Option<TypeID> {
		let idx = self.info.len();
		if idx >= TypeID::MAX_TYPES { return None }
		Some(TypeID(idx as _)) 
	}

	pub fn pre_register_type (&mut self) -> Option<TypeID> {
		let id = self.next_id()?;
		self.info.push(TypeInfo::Primitive(PrimitiveType::Nil));
		Some(id)
	}

	pub fn find_type (&self, info: &TypeInfo) -> Option<TypeID> {
		for (idx, existing_info) in self.info.iter().enumerate() {
			if existing_info == info {
				return Some(TypeID(idx as _))
			}
		}

		None
	}

	/// # Panics
	/// Panics if the given preregistered ID is already defined or invalid,
	/// or if the info provided is already registered to another ID
	#[track_caller]
	pub fn define_type (&mut self, pre_registered_id: TypeID, info: TypeInfo) {
		assert!(self.find_type(&info).is_none());
		assert!(pre_registered_id.0 > 0);
		let r = self.info.get_mut(pre_registered_id.0 as usize).unwrap();
		assert!(matches!(r, TypeInfo::Primitive(PrimitiveType::Nil)));
		*r = info;
	}

	/// # Safety
	/// This does not perform any checks to ensure that the given `pre_registered_id` is okay to write to
	pub unsafe fn define_type_unchecked (&mut self, pre_registered_id: TypeID, info: TypeInfo) {
		*self.info.get_unchecked_mut(pre_registered_id.0 as usize) = info;
	}

	pub fn register_type (&mut self, info: TypeInfo) -> Option<TypeID> {
		if let Some(existing_id) = self.find_type(&info) {
			return Some(existing_id)
		}

		let id = self.next_id()?;
		self.info.push(info);
		Some(id)
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
}

impl Default for TypeRegistry { fn default () -> Self { Self::new() } }