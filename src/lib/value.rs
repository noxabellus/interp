//! Structures and methods related to storing values in the VM


use super::map::Map;

/// The main storage union for values in the VM.
/// Variant docs can be found in TypeData
#[derive(Clone, Copy)]
#[allow(missing_docs, non_snake_case)]
pub union ValueData {
  pub Nil: (),
  pub Bool: bool,
  pub U8: u8,   pub U16: u16, pub U32: u32, pub U64: u64,
  pub I8: i8,   pub I16: i16, pub I32: i32, pub I64: i64,
  pub F32: f32, pub F64: f64,
  pub String: *mut String,
  pub Array: *mut Vec<ValueData>,
  pub Map: *mut Map,
  pub Record: *mut Vec<ValueData>,
  pub Function: *mut Function,
  pub Closure: *mut Closure,
  pub Foreign: *const extern "C" fn () -> (),
  pub Userdata: *const (),
}

/// Placeholder: Will contain function data
#[derive(Debug, Clone)]
pub struct Function;

/// Placeholder: Will contain closure data
#[derive(Debug, Clone)]
pub struct Closure;