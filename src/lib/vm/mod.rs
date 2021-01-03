//! The main vm data structure

pub mod typeinfo;
pub mod value;
pub mod object;
pub mod stack;
pub mod instruction;
pub mod global;
pub mod module;
pub mod context;

pub use self::{
	value::Value,
	stack::Stack,
	typeinfo::{ TypeRegistry, TypeID, TypeInfo, },
	global::{ GlobalID, },
	module::{ ModuleRegistry, ModuleID, Module },
	context::Context,
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



