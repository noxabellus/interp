//! Modules and their storage

use std::{
  collections::HashMap,
};

use crate::{
  utils::InsertUnique,
  global::GlobalID,
  typeinfo::TypeID,
};


/// A unique identifier for a module inside a ModuleRegistry
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct ModuleID(u16);

impl ModuleID {
  /// The maximum number of modules a script can create
  pub const MAX_MODULES: usize = u16::MAX as _;
}


/// Binds types and globals declared by a script
#[derive(Default)]
pub struct Module {
  types: HashMap<String, TypeID>,
  globals: HashMap<String, GlobalID>,
}



impl Module {
  /// Create a new Module
  pub fn new () -> Self {
    Self { .. Default::default() }
  }


  /// Bind a new type to a name in a Module
  ///
  /// Returns false if there was already a type with the given name
  pub fn register_type (&mut self, name: &str, id: TypeID) -> bool {
    self.types.insert_unique(name, id)
  }

  /// Bind a new global to a name in a Module
  ///
  /// Returns false if there was already a global with the given name
  pub fn register_global (&mut self, name: &str, id: GlobalID) -> bool {
    self.globals.insert_unique(name, id)
  }


  /// Determine how many types are registered in a Module
  pub fn num_types (&self) -> usize {
    self.types.len()
  }

  /// Determine how many globals are registered in a Module
  pub fn num_globals (&self) -> usize {
    self.globals.len()
  }


  /// Determine if a Module has registered any types
  pub fn has_types (&self) -> bool {
    !self.types.is_empty()
  }

  /// Determine if a Module has registered any globals
  pub fn has_globals (&self) -> bool {
    !self.globals.is_empty()
  }


  /// Find a type in a Module by name
  pub fn find_type (&self, name: &str) -> Option<TypeID> {
    self.types.get(name).copied()
  }

  /// Find a global in a Module by name
  pub fn find_global (&self, name: &str) -> Option<GlobalID> {
    self.globals.get(name).copied()
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
  /// + A module with the name `name` already exists
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