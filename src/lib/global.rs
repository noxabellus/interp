//! Global variable storage


use crate::value::Value;


/// A unique identifier for a global variable inside a GlobalRegistry
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct GlobalID(u16);

impl GlobalID {
  /// The maximum number of global variables a script can create
  pub const MAX_GLOBALS: usize = u16::MAX as _;
}


/// Stores global variables
pub struct GlobalRegistry {
  globals: Vec<Value>,
}

impl GlobalRegistry {
  /// Create a new GlobalRegistry
  pub fn new () -> Self {
    Self { .. Default::default() }
  }

  /// Create a new global variable or get an id for an existing one with the provided name
  ///
  /// Returns None if there are already `GlobalID::MAX_GLOBALS` globals
  pub fn create_global (&mut self) -> Option<(GlobalID, &mut Value)> {
    let idx = self.globals.len();
    if idx >= GlobalID::MAX_GLOBALS { return None }

    let id = GlobalID(idx as _);

    self.globals.push(Value::from_nil());

    Some((id, unsafe { self.get_global_unchecked_mut(id) }))
  }


  /// Get a global by id
  /// # Safety
  /// This does not bounds check the id
  pub unsafe fn get_global_unchecked_mut (&mut self, id: GlobalID) -> &mut Value {
    self.globals.get_unchecked_mut(id.0 as usize)
  }

  /// Get a global by id, if the id is valid
  pub fn get_global_mut (&mut self, id: GlobalID) -> Option<&mut Value> {
    self.globals.get_mut(id.0 as usize)
  }

  /// Get a global by id
  /// # Safety
  /// This does not bounds check the id
  pub unsafe fn get_global_unchecked (&self, id: GlobalID) -> &Value {
    self.globals.get_unchecked(id.0 as usize)
  }

  /// Get a global by id, if the id is valid
  pub fn get_global (&self, id: GlobalID) -> Option<&Value> {
    self.globals.get(id.0 as usize)
  }
}

impl Default for GlobalRegistry { fn default () -> Self { Self::new() } }