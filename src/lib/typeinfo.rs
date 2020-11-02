//! Contains information and ids related to types

use std::{
  hash::{ Hash, Hasher },
  collections::HashMap,
};

use crate::fnv1a::Fnv1a;
use crate::value::TypeDiscriminator;

/// Identifies which sub-variant of a type is represented by an object (E.g. which Record type, etc)
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeID(pub(crate) u16);


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
  Userdata,
  Foreign,
}

impl From<PrimitiveType> for TypeKind {
  fn from (prim: PrimitiveType) -> Self {
    unsafe { std::mem::transmute(prim) }
  }
}

impl From<&TypeInfo> for TypeKind {
  fn from (info: &TypeInfo) -> Self {
    match info {
      TypeInfo::Primitive(prim) => prim.clone().into(),
      TypeInfo::Record { .. } => TypeKind::Record,
      TypeInfo::Array(_) => TypeKind::Array,
      TypeInfo::Map(_, _) => TypeKind::Map,
      TypeInfo::String => TypeKind::String,
      TypeInfo::Function { is_foreign, .. } => if *is_foreign { TypeKind::Foreign } else { TypeKind::Function },
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
      TypeDiscriminator::Userdata => TypeKind::Userdata,
      TypeDiscriminator::Foreign => TypeKind::Foreign,
    }
  }
}


/// Signifies which variant of Primitive is represented by a value
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
}

/// Contains type-specific data for values
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TypeInfo {
  /// A primitive scalar value
  Primitive(PrimitiveType),
  /// A named type representing a collection of values of various types
  Record {
    /// The unique name of the Record
    name: String,
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
    /// Indicates whether a function is a native function or internal to the VM
    is_foreign: bool,
    /// The type of value, if any, returned by a function
    return_type: Option<TypeID>,
    /// The type of each parameter passed to a function, if any
    parameter_types: Vec<TypeID>,
  },
  /// A named type representing a native value
  Userdata(String),
}

/// Stores TypeInfo and provides association to unique TypeIDs for each type
pub struct TypeStore {
  type_id_counter: u16,
  info: Vec<TypeInfo>,
  arrays: HashMap<TypeID, TypeID>,
  maps: HashMap<(TypeID, TypeID), TypeID>,
  functions: Vec<(u64, TypeID)>,
  records: HashMap<String, TypeID>,
  userdata: HashMap<String, TypeID>,
}

impl TypeStore {
  const BUILTINS: &'static [TypeInfo] = {
    use TypeInfo::*;
    use PrimitiveType::*;

    &[
      Primitive(Nil),
      Primitive(Real),
      Primitive(Integer),
      Primitive(Character),
      Primitive(Boolean),
      String,
    ]
  };

  /// Create a new TypeStore and initialize it with builtin types
  pub fn new () -> Self {
    let mut out = Self {
      type_id_counter: 0,
      info: Vec::default(),
      arrays: HashMap::default(),
      maps: HashMap::default(),
      functions: Vec::default(),
      records: HashMap::default(),
      userdata: HashMap::default()
    };

    out.load_builtins();

    out
  }

  fn load_builtins (&mut self) {
    for builtin in Self::BUILTINS {
      self.register_type(builtin.clone());
    }
  }

  fn register_type (&mut self, info: TypeInfo) -> TypeID {
    assert!(self.type_id_counter < u16::MAX);

    let id = self.type_id_counter;
    self.type_id_counter += 1;

    self.info.push(info);

    TypeID(id)
  }

  /// Get the TypeID of a PrimitiveType
  pub const fn primitive_id (prim: PrimitiveType) -> TypeID {
    TypeID(prim as u16)
  }

  /// Get the TypeID of the String type
  pub const fn string_id () -> TypeID {
    TypeID(std::mem::variant_count::<PrimitiveType>() as u16)
  }

  /// Create a new Userdata type
  /// # Panics
  /// + A Userdata type with the given name already exists
  /// + There are already u16::MAX TypeIDs registered
  pub fn create_userdata (&mut self, name: String) -> TypeID {
    assert!(!self.userdata.contains_key(&name));
    self.register_type(TypeInfo::Userdata(name))
  }
  
  /// Find a Userdata type by name, if one exists
  pub fn find_userdata (&self, name: &str) -> Option<TypeID> {
    self.userdata.get(name).copied()
  }

  /// Create a new Record type
  /// # Panics
  /// + A Record type with the given name already exists
  /// + There are already u16::MAX TypeIDs registered
  pub fn create_record (&mut self, name: String, field_types: Vec<TypeID>, field_names: Vec<String>) -> TypeID {
    assert!(!self.records.contains_key(&name));
    self.register_type(TypeInfo::Record { name, field_types, field_names })
  }
  
  /// Find a Record type by name, if one exists
  pub fn find_record (&self, name: &str) -> Option<TypeID> {
    self.records.get(name).copied()
  }

  /// Get an id for an anonymous array type, returns an existing id if one exists or creates a new one if necessary
  /// # Panics
  /// + There are already u16::MAX TypeIDs registered and a new one needs to be created
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
  /// + There are already u16::MAX TypeIDs registered and a new one needs to be created
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
  /// + There are already u16::MAX TypeIDs registered and a new one needs to be created
  pub fn create_function (&mut self, is_foreign: bool, return_type: Option<TypeID>, parameter_types: &[TypeID]) -> TypeID {
    let (in_is_foreign, in_return_type, in_parameter_types) = (&is_foreign, &return_type, parameter_types);

    let mut hasher = Fnv1a::default();

    in_is_foreign.hash(&mut hasher);
    in_return_type.hash(&mut hasher);
    in_parameter_types.hash(&mut hasher);

    let in_hash = hasher.finish();

    for &(existing_hash, existing_id) in self.functions.iter() {
      if in_hash != existing_hash { continue }

      let existing_info = unsafe { self.info.get_unchecked(existing_id.0 as usize) };

      match existing_info {
        TypeInfo::Function { is_foreign, return_type, parameter_types }
        if is_foreign == in_is_foreign
        && return_type == in_return_type
        && parameter_types.as_slice() == in_parameter_types
        => return existing_id,

        _ => continue
      }
    }

    let new_id = self.register_type(TypeInfo::Function { is_foreign, return_type, parameter_types: parameter_types.to_owned() });
    self.functions.push((in_hash, new_id));

    new_id
  }
}

impl Default for TypeStore { fn default () -> Self { Self::new() } }