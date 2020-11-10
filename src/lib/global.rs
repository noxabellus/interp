//! Global variables and their storage

use std::collections::HashMap;

use crate::value::Value;


/// A unique identifier for a global variable inside a GlobalRegistry
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct GlobalID(u16);


/// Stores global variables
pub struct GlobalRegistry {
  globals: Vec<Value>,
  global_names: HashMap<String, GlobalID>,
}

impl GlobalRegistry {
  /// Create a new GlobalRegistry
  pub fn new () -> Self {
    Self {
      globals: Vec::default(),
      global_names: HashMap::default()
    }
  }

  /// Create a new global variable or get an id for an existing one with the provided name
  /// # Panics
  /// + There are already `u16::MAX` globals in the GlobalRegistry but a new global needs to be created
  pub fn create_global (&mut self, name: &str) -> (GlobalID, &mut Value) {
    let id = if let Some(&existing_id) = self.global_names.get(name) {
      existing_id
    } else {
      let idx = self.globals.len();
      assert!(idx <= u16::MAX as _, "Cannot create more than {} globals", u16::MAX);

      let id = GlobalID(idx as u16);

      self.globals.push(Value::from_nil());
      self.global_names.insert(name.to_owned(), id);

      id
    };

    (id, unsafe { self.get_global_unchecked_mut(id) })
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


  /// Find the id of a global by name
  pub fn find_global (&self, name: &str) -> Option<GlobalID> {
    self.global_names.get(name).copied()
  }
}

impl Default for GlobalRegistry { fn default () -> Self { Self::new() } }