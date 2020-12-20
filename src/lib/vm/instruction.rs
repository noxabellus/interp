//! The instruction set

use std::{
	ptr::read_unaligned,
	mem::{ size_of },
	slice
};

use super::{
	global::GlobalID,
	module::ModuleID,
	typeinfo::{ TypeID, TypeKind }
};



// Using all these newtypes, while it looks a bit messy,
// will allow easy maintenance if the backing types ever need to change

/// Wraps an id for a constant value in bytecode
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConstantID(pub(crate) u16);

impl ConstantID {
	/// The maximum number of constant values per function
	pub const MAX_CONSTANTS: usize = u16::MAX as _;
}

/// Wraps an id for a local variable in a function
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LocalID(pub(crate) u8);

impl LocalID {
	/// The maximum number of local variables per function
	pub const MAX_LOCALS: usize = u8::MAX as _;
}

/// Wraps an id for an upvalue in a closure
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UpvalueID(pub(crate) u8);

impl UpvalueID {
	/// The maximum number of captured values per closure
	pub const MAX_UPVALUES: usize = u8::MAX as _;
}

/// Wraps an id for a field in a record
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FieldID(pub(crate) u8);

impl FieldID {
	/// The maximum number of fields per record
	pub const MAX_FIELDS: usize = u8::MAX as _;
}

/// Wraps an id for a parameter in a function
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ParameterID(pub(crate) u8);

impl ParameterID {
	/// The maximum number of named parameters per function
	pub const MAX_PARAMETERS: usize = u8::MAX as _;
}

/// Wraps an offset for a bytecode jump instruction
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct JumpOffset(pub(crate) i32);

impl JumpOffset {
	/// The largest possible negative jump
	pub const MIN_OFFSET: usize = i32::MIN as _;
	/// The largest possible positive jump
	pub const MAX_OFFSET: usize = i32::MAX as _;
}


/// An enum over every instruction type
///
/// # Safety
/// There is really no efficient way to safely decode instructions genericly.
/// We cannot encode a full enum because of size constraints,
/// and we cannot match on the variant in a decode function to immediately decode data
/// because that would introduce a redundant branch in the interpreter. Because of this,
/// the interpreter itself is the only trusted instruction decoder
#[repr(u8)]
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Instruction {
	// Variable access //

	LoadLocal, // (LocalID),
	StoreLocal, // (LocalID),

	LoadUpvalue, // (UpvalueID),
	StoreUpvalue, // (UpvalueID),

	LoadGlobal, // (ModuleID, GlobalID),
	LoadGlobalDeferred, // (ModuleID)
	StoreGlobal, // (ModuleID, GlobalID),
	StoreGlobalDeferred, // (ModuleID)


	// Collection access //

	GetField, // (FieldID),
	GetFieldDeferred,
	SetField, // (FieldID),
	SetFieldDeferred,

	GetArrayElement,
	SetArrayElement,

	GetMapElement,
	SetMapElement,

	GetStringCodepoint,
	SetStringCodepoint,

	ConcatString,
	ConcatArray,


	// Constructors //

	CreateRecord, // (TypeID, FieldID),
	CreateArray, // (TypeID, i32),
	CreateMap, // (TypeID, u32),
	CreateString,
	CreateClosure,


	// Constant values //

	Constant, // (ConstantID),
	Nil,
	

	// Unary ops //

	NegateReal,
	NegateInteger,

	AbsReal,
	AbsInteger,

	NotInteger,
	NotBoolean,


	// Binary ops //

	AddReal,
	AddInteger,

	SubReal,
	SubInteger,

	MulReal,
	MulInteger,

	DivReal,
	DivInteger,

	RemReal,
	RemInteger,

	PowReal,
	PowInteger,

	AndInteger,
	AndBoolean,
	
	OrInteger,
	OrBoolean,

	XorInteger,

	LShiftInteger,
	RShiftInteger,

	EqReal,
	EqInteger,
	EqCharacter,
	EqBoolean,
	EqTypeID,
	
	NeReal,
	NeInteger,
	NeCharacter,
	NeBoolean,
	NeTypeID,

	GtReal,
	GtInteger,
	GtCharacter,

	LtReal,
	LtInteger,
	LtCharacter,

	GeReal,
	GeInteger,
	GeCharacter,

	LeReal,
	LeInteger,
	LeCharacter,


	// Control flow //

	Call,
	CallForeign,

	Branch, // (JumpOffset),
	ConditionalBranch, // (JumpOffset, JumpOffset),

	Return,

	Panic,


	// Conversion //

	CastRealToInteger,
	CastIntegerToReal,
	CastCharacterToInteger,
	CastIntegerToCharacter,
	CastIntegerToBoolean,
	CastBooleanToInteger,
	// CastDeferred, // not sure what this would do


	// Introspection //

	TypeIDOfLocal, // (LocalID),
	TypeIDOfGlobal, // (GlobalID),
	TypeIDOfGlobalDeferred, // (ModuleID)
	TypeIDOfField, // (FieldID),
	TypeIDOfFieldDeferred,
	TypeIDOfKey,
	TypeIDOfElement,
	TypeIDOfParameter, // (ParameterID),
	TypeIDOfReturn,
	TypeIDOfValue,
	
	ModuleExists,
	GlobalExists, // (ModuleID)
	FieldExists,
	ParameterExists,
	ReturnExists,

	FieldCount,
	ElementCount,
	ParameterCount,

	IsTypeKind, // (TypeKind),
}



/// Allows encoding and decoding a value from an instruction stream
pub trait Codable: Copy {
	/// Encode this value into an instruction stream as raw unaligned bytes
	fn encode (self, code: &mut Vec<u8>) {
		code.copy_from_slice(unsafe { slice::from_raw_parts(&self as *const Self as *const u8, size_of::<Self>()) });
	}

	/// Create an instance of this type by reading from an instruction stream
	/// # Debug Panics
	/// In debug mode, this will check that the given code has enough remaining data, and panic if not
	/// # Safety
	/// This is inherently unsafe, there is no way to check that
	/// the instruction stream contains the specified data,
	/// and the only trusted caller is the interpreter itself
	unsafe fn decode (ip: &mut usize, code: &[u8]) -> Self {
		debug_assert!(*ip + size_of::<Self>() <= code.len());
	
		let val = read_unaligned(code.as_ptr().add(*ip) as *const Self);
	
		*ip += size_of::<Self>();
	
		val
	}
}

impl Codable for Instruction { }
impl Codable for TypeID { }
impl Codable for TypeKind { }
impl Codable for ConstantID { }
impl Codable for LocalID { }
impl Codable for UpvalueID { }
impl Codable for GlobalID { }
impl Codable for ModuleID { }
impl Codable for FieldID { }
impl Codable for ParameterID { }
impl Codable for JumpOffset { }
impl Codable for (JumpOffset, JumpOffset) { }