//! Global variables and their storage

// use std::collections::HashMap;

// use super::Value;


/// A unique identifier for a global variable inside a GlobalRegistry
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct GlobalID(pub(crate) u16);

impl GlobalID {
	/// The maximum number of global variables a script can create
	pub const MAX_GLOBALS: usize = u16::MAX as _;
}