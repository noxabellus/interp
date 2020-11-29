//! Modules and their storage

use std::{
  collections::hash_map::{
    HashMap,
    // RawEntryMut
  }
};

// use crate::utils::InsertUnique;

use super::{
  Value,
  TypeID, GlobalID,
};


/// A unique identifier for a module inside a ModuleRegistry
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct ModuleID(u16);

impl ModuleID {
  /// The maximum number of modules a script can create
  pub const MAX_MODULES: usize = u16::MAX as _;
}

/// An ID bound to a name in a Module
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ModuleBinding {
  Global(GlobalID),
  Type(TypeID),
}

/// Binds types and globals declared by a script
#[derive(Default)]
pub struct Module {
  globals: Vec<Value>,
  bindings: HashMap<String, ModuleBinding>
}



impl Module {
  /// Create a new Module
  pub fn new () -> Self {
    Self { .. Default::default() }
  }


  /// Bind a new type to a name in a Module
  ///
  /// Returns false if there was already a binding with the given name
  pub fn export_type (&mut self, name: &str, id: TypeID) -> bool {
    if !self.bindings.contains_key(name) {
      self.bindings.insert(name.to_owned(), ModuleBinding::Type(id));
      true
    } else {
      false
    }
  }


  /// Bind a new global to a name in a Module
  ///
  /// Returns false if:
  /// + There was already a binding with the given name
  pub fn export_global (&mut self, name: &str, id: GlobalID) -> bool {
    if !self.bindings.contains_key(name) {
      self.bindings.insert(name.to_owned(), ModuleBinding::Global(id));
      true
    } else {
      false
    }
  }

  /// Create a new global in a Module
  pub fn create_global (&mut self) -> Option<(GlobalID, &mut Value)> {
    let idx = self.globals.len();
    if idx >= GlobalID::MAX_GLOBALS { return None }
    
    let id = GlobalID(idx as _);
    self.globals.push(Value::from_nil());

    Some((id, unsafe { self.globals.get_unchecked_mut(idx) }))
  }


  /// Lookup a binding by name
  pub fn find_binding (&self, name: &str) -> Option<ModuleBinding> {
    self.bindings.get(name).copied()
  }

  /// Get a global by id
  pub fn get_global (&self, id: GlobalID) -> Option<&Value> {
    self.globals.get(id.0 as usize)
  }

  /// Get a global by id
  /// # Safety
  /// This does not bounds check the id
  pub unsafe fn get_global_unchecked (&self, id: GlobalID) -> &Value {
    self.globals.get_unchecked(id.0 as usize)
  }

  /// Get a global by id
  pub fn get_global_mut (&mut self, id: GlobalID) -> Option<&mut Value> {
    self.globals.get_mut(id.0 as usize)
  }

  /// Get a global by id
  /// # Safety
  /// This does not bounds check the id
  pub unsafe fn get_global_unchecked_mut (&mut self, id: GlobalID) -> &mut Value {
    self.globals.get_unchecked_mut(id.0 as usize)
  }
}


/// Stores modules
#[derive(Default)]
pub struct ModuleRegistry {
  modules: Vec<Module>,
  module_names: HashMap<String, ModuleID>,
}

impl ModuleRegistry {
  /// Create a new ModuleRegistry
  pub fn new () -> Self {
    Self { .. Default::default() }
  }

  /// Create a new module or get an id for an existing one with the provided name
  ///
  /// Returns None if:
  /// + There are `ModuleID::MAX_MODULES`
  /// + A module is already bound to `name`
  pub fn create_module (&mut self, name: &str) -> Option<(ModuleID, &mut Module)> {
    if self.module_names.contains_key(name) {
      None
    } else {
      let idx = self.modules.len();
      if idx >= ModuleID::MAX_MODULES { return None }

      let id = ModuleID(idx as _);

      self.modules.push(Module::default());
      self.module_names.insert(name.to_owned(), id);

      Some((id, unsafe { self.get_module_unchecked_mut(id) }))
    }
  }


  /// Determine how many Modules a ModuleRegistry contains
  pub fn len (&self) -> usize {
    self.modules.len()
  }

  /// Determine if a ModuleRegistry contains no Modules
  pub fn is_empty (&self) -> bool {
    self.modules.is_empty()
  }


  /// Get a module by id
  /// # Safety
  /// This does not bounds check the id
  pub unsafe fn get_module_unchecked_mut (&mut self, id: ModuleID) -> &mut Module {
    self.modules.get_unchecked_mut(id.0 as usize)
  }

  /// Get a module by id, if the id is valid
  pub fn get_module_mut (&mut self, id: ModuleID) -> Option<&mut Module> {
    self.modules.get_mut(id.0 as usize)
  }

  /// Get a module by id
  /// # Safety
  /// This does not bounds check the id
  pub unsafe fn get_module_unchecked (&self, id: ModuleID) -> &Module {
    self.modules.get_unchecked(id.0 as usize)
  }

  /// Get a module by id, if the id is valid
  pub fn get_module (&self, id: ModuleID) -> Option<&Module> {
    self.modules.get(id.0 as usize)
  }


  /// Find the id of a module by name
  pub fn find_module (&self, name: &str) -> Option<ModuleID> {
    self.module_names.get(name).copied()
  }


  /// Find the name of a module by id
  pub fn find_name (&self, id: ModuleID) -> Option<&str> {
    for (k, &v) in self.module_names.iter() {
      if v == id {
        return Some(k)
      }
    }
    
    None
  }
}