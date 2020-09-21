//! Structures and methods relating to vm type information

use std::{
  mem::transmute,
  hash::{ Hash, Hasher },
};

use super::value::ValueData;

/// Describes a type for a value in the VM
pub struct TypeInfo {
  /// The unique ID associated with a type in the VM
  pub id: TypeID,
  /// Variant-specific data for a type in the VM
  pub data: TypeData
}


/// A unique identifier for a type in the VM
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeID(pub(crate) u16);


/// Variant-specific data for a type in the VM
#[repr(C, u16)]
pub enum TypeData {
  /// Indicates the absence of value
  Nil,

  /// A logical control value, true or false
  Bool,

  /// 8 bit unsigned integer
  U8,
  /// 16 bit unsigned integer
  U16,
  /// 32 bit unsigned integer
  U32,
  /// 64 bit unsigned integer
  U64,

  /// 8 bit signed integer
  I8,
  /// 16 bit signed integer
  I16,
  /// 32 bit signed integer
  I32,
  /// 64 bit signed integer
  I64,
  
  /// 32 bit real number
  F32,
  /// 64 bit real number
  F64,

  /// Utf-8 encoded text
  String,

  /// A contiguous series of values
  Array(TypeID),

  /// A hashmap from one value type to another
  Map(TypeID, TypeID),

  /// User defined data structure
  Record(RecordTypeData),

  /// A functional interface
  Function(FunctionTypeData),

  /// A functional interface with associated state
  Closure(FunctionTypeData),

  /// A native function
  Foreign(FunctionTypeData),

  /// A native data structure
  Userdata(UserdataTypeID),
}

impl TypeData {
  /// Convert TypeData to its TypeKind
  pub fn to_kind (&self) -> TypeKind {
    unsafe { transmute(std::mem::discriminant(self)) }
  }

  /// Convert TypeData to its TypeID
  /// # Safety
  /// Only primitive TypeData and their associated TypeKind may be directly transmuted to TypeIDs
  pub unsafe fn to_id (&self) -> TypeID {
    self.to_kind().to_id()
  }
}

/// The discriminant portion of a TypeData.
/// Variant documentation can be found in TypeData
#[repr(u16)]
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TypeKind {
  Nil,

  Bool,

  U8,
  U16,
  U32,
  U64,

  I8,
  I16,
  I32,
  I64,
  
  F32,
  F64,

  String,

  Array,

  Map,

  Record,

  Function,

  Closure,

  Foreign,

  Userdata,
}

impl TypeKind {
  /// Convert TypeKind to its TypeID
  /// # Safety
  /// Only primitive TypeData and their associated TypeKind may be directly transmuted to TypeIDs
  pub unsafe fn to_id (self) -> TypeID {
    transmute(self)
  }
}

impl TypeID {
  /// Use type information to hash a ValueData.
  /// This is required because some ValueData variants will have uninitialized bytes,
  /// and because the floating point variants have various nan values that must be unified into a single hash
  pub fn hash<H: Hasher> (&self, value: &ValueData, h: &mut H) {
    unsafe { 
      match transmute(self.0) {
        TypeKind::Nil => 0u8.hash(h),
        TypeKind::F32 if value.F32.is_nan() => f32::NAN.to_bits().hash(h),
        TypeKind::F64 if value.F64.is_nan() => f64::NAN.to_bits().hash(h),
        TypeKind::U8 | TypeKind::I8 | TypeKind::Bool => value.U8.hash(h),
        TypeKind::U16 | TypeKind::I16 => value.U16.hash(h),
        TypeKind::U32 | TypeKind::I32 | TypeKind::F32 => value.U32.hash(h),
        _ => value.U64.hash(h),
      }
    }
  }

  /// Use type information to compare two ValueData.
  /// This is required because some ValueData variants will have uninitialized bytes,
  /// and because the floating point variants have non-IEE-standard comparison behavior for NAN values
  pub fn eq (&self, a: &ValueData, b: &ValueData) -> bool {
    unsafe {
      match transmute(self.0) {
        TypeKind::Nil => true,
        TypeKind::F32 => (a.F32.is_nan() & b.F32.is_nan()) | (a.F32 == b.F32),
        TypeKind::F64 => (a.F64.is_nan() & b.F64.is_nan()) | (a.F64 == b.F64),
        TypeKind::U8 | TypeKind::I8 | TypeKind::Bool => a.U8 == b.U8,
        TypeKind::U16 | TypeKind::I16 => a.U16 == b.U16,
        TypeKind::U32 | TypeKind::I32 => a.U32 == b.U32,
        _ => a.U64 == b.U64
      }
    }
  }

  /// Produce a hashing function for a specific type
  pub fn get_hasher_func<H: Hasher> (&self) -> fn (&ValueData, h: &mut H) {
    unsafe { 
      match transmute(self.0) {
        TypeKind::Nil => |_, h| 0u8.hash(h),
        TypeKind::F32 => |v, h| if v.F32.is_nan() { f32::NAN.to_bits().hash(h) } else { v.U32.hash(h) },
        TypeKind::F64 => |v, h| if v.F64.is_nan() { f64::NAN.to_bits().hash(h) } else { v.U64.hash(h) },
        TypeKind::U8 | TypeKind::I8 | TypeKind::Bool => |v, h| v.U8.hash(h),
        TypeKind::U16 | TypeKind::I16 => |v, h| v.U16.hash(h),
        TypeKind::U32 | TypeKind::I32 => |v, h| v.U32.hash(h),
        _ => |v, h| v.U64.hash(h),
      }
    }
  }


  /// Produce a comparison function for a specific type
  #[allow(clippy::float_cmp)] // We cant afford approximate comparison for every float, and it isnt always desirable
  pub fn get_eq_func (&self) -> fn (&ValueData, &ValueData) -> bool {
    unsafe {
      match transmute(self.0) {
        TypeKind::Nil => |_,_| true,
        TypeKind::F32 => |a, b| (a.F32.is_nan() & b.F32.is_nan()) | (a.F32 == b.F32),
        TypeKind::F64 => |a, b| (a.F64.is_nan() & b.F64.is_nan()) | (a.F64 == b.F64),
        TypeKind::U8 | TypeKind::I8 | TypeKind::Bool => |a, b| a.U8 == b.U8,
        TypeKind::U16 | TypeKind::I16 => |a, b| a.U16 == b.U16,
        TypeKind::U32 | TypeKind::I32 => |a, b| a.U32 == b.U32,
        _ => |a, b| a.U64 == b.U64
      }
    }
  }
}

/// Information about a user defined type
pub struct RecordTypeData {
  /// The names of fields contained in a user defined data structure
  pub field_names: Vec<String>,
  /// The type ids of fields contained in a user defined data structure
  pub field_types: Vec<TypeID>,
}


/// Information about a functional interface
pub struct FunctionTypeData {
  /// The names of parameters accepted by a functional interface
  pub parameter_names: Vec<String>,
  /// The type ids of parameters accepted by a functional interface
  pub parameter_types: Vec<TypeID>,
  /// The type id of the return value of a functional interface, if it has one
  pub return_type: Option<TypeID>
}


/// A unique identifier for a native type in the VM
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UserdataTypeID(pub(crate) u16);



#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_type_data_to_type_kind () {
    let td = TypeData::Nil;
    let tk = td.to_kind();
    assert_eq!(tk, TypeKind::Nil);

    let td = TypeData::Bool;
    let tk = td.to_kind();
    assert_eq!(tk, TypeKind::Bool);

    let td = TypeData::U8;
    let tk = td.to_kind();
    assert_eq!(tk, TypeKind::U8);

    let td = TypeData::U16;
    let tk = td.to_kind();
    assert_eq!(tk, TypeKind::U16);

    let td = TypeData::U32;
    let tk = td.to_kind();
    assert_eq!(tk, TypeKind::U32);

    let td = TypeData::U64;
    let tk = td.to_kind();
    assert_eq!(tk, TypeKind::U64);

    let td = TypeData::I8;
    let tk = td.to_kind();
    assert_eq!(tk, TypeKind::I8);

    let td = TypeData::I16;
    let tk = td.to_kind();
    assert_eq!(tk, TypeKind::I16);

    let td = TypeData::I32;
    let tk = td.to_kind();
    assert_eq!(tk, TypeKind::I32);

    let td = TypeData::I64;
    let tk = td.to_kind();
    assert_eq!(tk, TypeKind::I64);

    let td = TypeData::F32;
    let tk = td.to_kind();
    assert_eq!(tk, TypeKind::F32);

    let td = TypeData::F64;
    let tk = td.to_kind();
    assert_eq!(tk, TypeKind::F64);

    let td = TypeData::String;
    let tk = td.to_kind();
    assert_eq!(tk, TypeKind::String);
  }

  #[test]
  fn test_value_data_comparison () {
    let comp = unsafe { TypeKind::F32.to_id() }.get_eq_func();
    
    let a = ValueData { F32: f32::NAN };
    let b = ValueData { F32: f32::NAN };
    assert!(comp(&a, &b));
    
    let a = ValueData { F32: 0.0 };
    let b = ValueData { F32: -0.0 };
    assert!(comp(&a, &b));
    
    let a = ValueData { U8: 1 };
    let b = ValueData { I8: 1 };
    assert!(comp(&a, &b));
  }
}