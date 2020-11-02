//! The main vm data structure

use std::{
  collections::HashMap,
};

use crate::{
  value::Value,
  stack::Stack,
  typeinfo::TypeStore,
  instruction::Instruction,
  object,
};



/// Data for a function call
pub struct CallFrame {
  /// The function being executed within this CallFrame
  pub function: *mut object::Function,
  /// The number of local variables utilized by this CallFrame
  pub local_count: usize,
}

/// A unique identifier for a global variable inside a Context
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct GlobalID(u16);

/// Contains shared contextual information for all VM Fibers
pub struct Context {
  /// All types known by the VM
  pub types: TypeStore,
  /// All global variables known by the VM
  pub globals: Vec<Value>,
  /// Lookup table to get global variables by name
  pub global_names: HashMap<String, GlobalID>,
}

impl Context {
  /// Create a new Context
  pub fn new () -> Self {
    Self {
      types: TypeStore::default(),
      globals: Vec::default(),
      global_names: HashMap::default()
    }
  }

  /// Create a new global variable or get an id for an existing one with the provided name
  /// # Panics
  /// + There are already `u16::MAX` globals in the Context but a new global needs to be created
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


  /// Get a global by index
  /// # Safety
  /// This does not bounds check the index
  pub unsafe fn get_global_unchecked_mut (&mut self, id: GlobalID) -> &mut Value {
    self.globals.get_unchecked_mut(id.0 as usize)
  }

  /// Get a global by index, if the index is valid
  pub fn get_global_mut (&mut self, id: GlobalID) -> Option<&mut Value> {
    self.globals.get_mut(id.0 as usize)
  }

  /// Get a global by index
  /// # Safety
  /// This does not bounds check the index
  pub unsafe fn get_global_unchecked (&self, id: GlobalID) -> &Value {
    self.globals.get_unchecked(id.0 as usize)
  }

  /// Get a global by index, if the index is valid
  pub fn get_global (&self, id: GlobalID) -> Option<&Value> {
    self.globals.get(id.0 as usize)
  }


  /// Find the index of a global by name
  pub fn find_global (&self, name: &str) -> Option<GlobalID> {
    self.global_names.get(name).copied()
  }
}

impl Default for Context { fn default () -> Self { Self::new() } }


/// The main VM execution data structure containing the stack, type info collection, and other data required to execute code
pub struct Fiber {
  /// The shared Context containing TypeInfo and other shared data
  pub context: *mut Context,
  /// Temporary operand data produced and used by expressions
  pub stack: Stack<Value>,
  /// Call stack frames
  pub frames: Stack<CallFrame>,
  /// All local variables utilized by active CallFrames
  pub locals: Vec<Value>,
  /// The instruction pointer for this Fiber
  pub instruction: *const Instruction
}