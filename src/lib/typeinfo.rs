//! Contains information and ids related to types
use std::{
  intrinsics::likely,
  mem::{ transmute, variant_count },
  hash::{ Hash, Hasher },
  collections::HashMap,
};

use crate::unchecked_destructure;
use crate::static_assert;
use crate::fnv1a::Fnv1a;
use crate::value::TypeDiscriminator;

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

  Record,
  Array,
  Map,
  String,
  Function,
  Closure,
  Userdata,
  Foreign,
}

impl From<PrimitiveKind> for TypeKind {
  fn from (prim: PrimitiveKind) -> Self {
    static_assert!(
         TypeKind::Nil as u8 == PrimitiveKind::Nil as u8
      && TypeKind::Real as u8 == PrimitiveKind::Real as u8
      && TypeKind::Integer as u8 == PrimitiveKind::Integer as u8
      && TypeKind::Character as u8 == PrimitiveKind::Character as u8
      && TypeKind::Boolean as u8 == PrimitiveKind::Boolean as u8
      && TypeKind::TypeID as u8 == PrimitiveKind::TypeID as u8
    );

    // SAFETY: TypeKind and PrimitiveKind have the same discriminators,
    // for the subset of TypeKind represented by PrimitiveKind; See static_assert above
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
      TypeInfo::String => TypeKind::String,
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


/// Signifies which variant of Primitive is represented by a value
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PrimitiveKind {
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
  Primitive(PrimitiveKind),
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
  /// A collection of utf-8 variable width unicode encoded codepoints
  String,
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
  userdata: HashMap<String, TypeID>,
}


// This has to be outside of impl TypeRegistry due to const eval problems
const BUILTIN_TYPEINFO: &[TypeInfo] = {
  use TypeInfo::*;
  use PrimitiveKind::*;

  &[
    Primitive(Nil),
    Primitive(Real),
    Primitive(Integer),
    Primitive(Character),
    Primitive(Boolean),
    Primitive(TypeID),
    String,
  ]
};

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

  fn register_type (&mut self, info: TypeInfo) -> TypeID {
    let idx = self.info.len();
    assert!(idx < TypeID::MAX_TYPES, "Cannot create more than {} types", TypeID::MAX_TYPES);

    self.info.push(info);

    TypeID(idx as _)
  }

  /// Get TypeInfo from a TypeID
  /// # Safety
  /// This does not validate the TypeID provided
  pub unsafe fn get_type_info_unchecked (&self, id: TypeID) -> &TypeInfo {
    self.info.get_unchecked(id.0 as usize)
  }

  /// Get TypeInfo from a TypeID
  pub fn get_type_info (&self, id: TypeID) -> Option<&TypeInfo> {
    self.info.get(id.0 as usize)
  }

  /// Get the number of unique types stored in a TypeRegistry
  pub fn len (&self) -> usize {
    self.info.len()
  }

  /// Get the TypeID of a PrimitiveKind
  pub const fn primitive_id (prim: PrimitiveKind) -> TypeID {
    // Primitive types are added in the same order they are declared, so their ids are their discriminants
    {
      macro_rules! check_prim { ($($var:ident),*) => { $( static_assert!(matches!(BUILTIN_TYPEINFO[PrimitiveKind::$var as usize], TypeInfo::Primitive(PrimitiveKind::$var))); )* } }

      check_prim!(Nil, Real, Integer, Character, Boolean, TypeID);
    }

    TypeID(prim as u16)
  }

  /// Get the TypeID of the String type
  pub const fn string_id () -> TypeID {
    // String is added directly after primitive types so its id is their variant count
    const STRING_ID: usize = variant_count::<PrimitiveKind>();
    static_assert!(matches!(BUILTIN_TYPEINFO[STRING_ID], TypeInfo::String));

    TypeID(STRING_ID as u16)
  }

  /// Create a new Userdata type
  /// 
  /// Returns None if a Userdata type with the given name already exists
  /// # Panics
  /// + There are already `TypeID::MAX_TYPES` TypeIDs registered
  pub fn create_userdata (&mut self, name: String) -> Option<TypeID> {
    if likely(!self.userdata.contains_key(&name)) {
      let new_id = self.register_type(TypeInfo::Userdata(name.clone()));
      self.userdata.insert(name, new_id);
      
      Some(new_id)
    } else {
      None
    }
  }
  
  /// Find a Userdata type by name, if one exists
  pub fn find_userdata (&self, name: &str) -> Option<TypeID> {
    self.userdata.get(name).copied()
  }

  /// Create a new Record type
  /// # Panics
  /// + There are already `TypeID::MAX_TYPES` TypeIDs registered
  pub fn create_record (&mut self, field_types: &[TypeID], field_names: &[String]) -> TypeID {
    let (in_field_types, in_field_names) = (field_types, field_names);

    let mut hasher = Fnv1a::default();

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
            return existing_id
          }
        ) }
      }
    }
    
    let new_id = self.register_type(TypeInfo::Record { field_types: field_types.to_owned(), field_names: field_names.to_owned() });
    self.records.push((in_hash, new_id));

    new_id
  }

  /// Get an id for an anonymous array type, returns an existing id if one exists or creates a new one if necessary
  /// # Panics
  /// + There are already `TypeID::MAX_TYPES` TypeIDs registered and a new one needs to be created
  pub fn create_array (&mut self, elem_type: TypeID) -> TypeID {
    if let Some(&existing_id) = self.arrays.get(&elem_type) {
      existing_id
    } else {
      let new_id = self.register_type(TypeInfo::Array(elem_type));
      self.arrays.insert(elem_type, new_id);

      new_id
    }
  }

  /// Get an id for an anonymous map type, returns an existing id if one exists or creates a new one if necessary
  /// # Panics
  /// + There are already `TypeID::MAX_TYPES` TypeIDs registered and a new one needs to be created
  pub fn create_map (&mut self, key_type: TypeID, value_type: TypeID) -> TypeID {
    if let Some(&existing_id) = self.maps.get(&(key_type, value_type)) {
      existing_id
    } else {
      let new_id = self.register_type(TypeInfo::Map(key_type, value_type));
      self.maps.insert((key_type, value_type), new_id);

      new_id
    }
  }

  /// Get an id for an anonymous function type, returns an existing id if one exists or creates a new one if necessary
  /// # Panics
  /// + There are already `TypeID::MAX_TYPES` TypeIDs registered and a new one needs to be created
  pub fn create_function (&mut self, kind: FunctionKind, return_type: Option<TypeID>, parameter_types: &[TypeID]) -> TypeID {
    let (in_kind, in_return_type, in_parameter_types) = (&kind, &return_type, parameter_types);

    let mut hasher = Fnv1a::default();

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
          return existing_id
        } else {
          continue
        }
      ) }
    }

    let new_id = self.register_type(TypeInfo::Function { kind, return_type, parameter_types: parameter_types.to_owned() });
    self.functions.push((in_hash, new_id));

    new_id
  }
}

impl Default for TypeRegistry { fn default () -> Self { Self::new() } }