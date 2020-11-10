//! The main vm data structure

use crate::{
  value::Value,
  stack::Stack,
  typeinfo::TypeRegistry,
  global::GlobalRegistry,
  object,
};



/// Data for a function call
pub struct CallFrame {
  /// The function or closure being executed within this CallFrame
  pub body: *mut object::Object,
  /// The number of local variables utilized by this CallFrame
  pub local_count: usize,
  /// The instruction pointer for this CallFrame
  pub instruction: usize
}


/// Contains shared contextual information for all VM Fibers
pub struct Context {
  /// All types known by the VM
  pub types: TypeRegistry,
  /// All global variables known by the VM
  pub globals: GlobalRegistry,
}

impl Context {
  /// Create a new Context
  pub fn new () -> Self {
    Self {
      types: TypeRegistry::default(),
      globals: GlobalRegistry::default(),
    }
  }
}

impl Default for Context { fn default () -> Self { Self::new() } }


/// The main VM execution data structure containing the stack and other data required to execute code
pub struct Fiber {
  /// The shared Context containing TypeInfo and other shared data
  pub context: *mut Context,
  /// Temporary operand data produced and used by expressions
  pub stack: Stack<Value>,
  /// Call stack frames
  pub frames: Stack<CallFrame>,
  /// All local variables utilized by active CallFrames
  pub locals: Stack<Value>,
}