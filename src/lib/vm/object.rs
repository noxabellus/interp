//! Object value implementations

use std::{
	hash::{ Hash, Hasher, },
	collections::HashMap,
	cmp::Ordering,
	any::Any,
};

use super::{
	value::Value,
	typeinfo::TypeID,
	Fiber,
};


/// Indicates which variant of Object is pointed to
#[repr(u8)]
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ObjectKind {
	Array,
	Map,
	String,
	Record,
	Function,
	// Closure,
	Userdata,
	// Foreign
}

/// Component of all Object type variants,
/// allowing garbage collection and type identification
#[repr(C)]
pub struct Object {
	/// Discriminant for dyncasting Object pointers to variant pointers
	pub kind: ObjectKind,
	/// Discriminant for the type of the object containing to this header
	pub type_id: TypeID,
	/// Linked list pointer to the next allocated Object
	pub next: *mut Object,
}


/// Allows casting between generic Object pointers and specific variants.
/// The only safe implementation of this trait is on *mut/const Object
pub unsafe trait ObjCastable: Sized + Copy {
	/// Get the ObjectKind of an Object
	fn get_kind (self) -> ObjectKind;

	/// Get the TypeID of an Object
	fn get_type_id (self) -> TypeID;
}

/// Allows casting between generic Object pointers and specific variants.
/// The only safe implementation of this trait is on *mut/const Object
pub unsafe trait ObjCast: ObjCastable {
	/// Cast an Object pointer to a pointer to a variant
	/// # Safety
	/// This does not check the ObjectKind discriminant of the Object header
	unsafe fn as_array_unchecked (self) -> *const Array;

	/// Cast an Object pointer to a pointer to a variant
	/// # Safety
	/// This does not check the ObjectKind discriminant of the Object header
	unsafe fn as_map_unchecked (self) -> *const Map;

	/// Cast an Object pointer to a pointer to a variant
	/// # Safety
	/// This does not check the ObjectKind discriminant of the Object header
	unsafe fn as_string_unchecked (self) -> *const String;

	/// Cast an Object pointer to a pointer to a variant
	/// # Safety
	/// This does not check the ObjectKind discriminant of the Object header
	unsafe fn as_record_unchecked (self) -> *const Record;

	/// Cast an Object pointer to a pointer to a variant
	/// # Safety
	/// This does not check the ObjectKind discriminant of the Object header
	unsafe fn as_function_unchecked (self) -> *const Function;

	/// Cast an Object pointer to a pointer to a variant
	/// # Safety
	/// This does not check the ObjectKind discriminant of the Object header
	unsafe fn as_procedure_unchecked (self) -> *const Procedure;

	/// Cast an Object pointer to a pointer to a variant
	/// # Safety
	/// This does not check the ObjectKind discriminant of the Object header
	unsafe fn as_closure_unchecked (self) -> *const Closure;

	/// Cast an Object pointer to a pointer to a variant
	/// # Safety
	/// This does not check the ObjectKind discriminant of the Object header
	unsafe fn as_foreign_unchecked (self) -> *const Foreign;

	/// Cast an Object pointer to a pointer to a variant
	/// # Safety
	/// This does not check the ObjectKind discriminant of the Object header
	unsafe fn as_userdata_unchecked (self) -> *const Userdata;


	/// Cast an Object pointer to a pointer to a variant
	fn as_array (self) -> Option<*const Array> { if self.get_kind() == ObjectKind::Array { Some(unsafe { self.as_array_unchecked() }) } else { None } }

	/// Cast an Object pointer to a pointer to a variant
	fn as_map (self) -> Option<*const Map> { if self.get_kind() == ObjectKind::Map { Some(unsafe { self.as_map_unchecked() }) } else { None } }

	/// Cast an Object pointer to a pointer to a variant
	fn as_string (self) -> Option<*const String> { if self.get_kind() == ObjectKind::String { Some(unsafe { self.as_string_unchecked() }) } else { None } }

	/// Cast an Object pointer to a pointer to a variant
	fn as_record (self) -> Option<*const Record> { if self.get_kind() == ObjectKind::Record { Some(unsafe { self.as_record_unchecked() }) } else { None } }

	/// Cast an Object pointer to a pointer to a variant
	fn as_function (self) -> Option<*const Function> { if self.get_kind() == ObjectKind::Function { Some(unsafe { self.as_function_unchecked() }) } else { None } }
	
	/// Cast an Object pointer to a pointer to a variant
	fn as_procedure (self) -> Option<*const Procedure> { self.as_function().and_then(FnCast::as_procedure) }

	/// Cast an Object pointer to a pointer to a variant
	fn as_closure (self) -> Option<*const Closure> { self.as_function().and_then(FnCast::as_closure) }
	
	/// Cast an Object pointer to a pointer to a variant
	fn as_foreign (self) -> Option<*const Foreign> { self.as_function().and_then(FnCast::as_foreign) }
	
	/// Cast an Object pointer to a pointer to a variant
	fn as_userdata (self) -> Option<*const Userdata> { if self.get_kind() == ObjectKind::Userdata { Some(unsafe { self.as_userdata_unchecked() }) } else { None } }
}

/// Allows casting between generic Object pointers and specific variants.
/// The only safe implementation of this trait is on *mut Object
pub unsafe trait ObjCastMut: ObjCastable {
	/// Cast an Object pointer to a pointer to a variant
	/// # Safety
	/// This does not check the ObjectKind discriminant of the Object header
	unsafe fn as_array_unchecked_mut (self) -> *mut Array;

	/// Cast an Object pointer to a pointer to a variant
	/// # Safety
	/// This does not check the ObjectKind discriminant of the Object header
	unsafe fn as_map_unchecked_mut (self) -> *mut Map;

	/// Cast an Object pointer to a pointer to a variant
	/// # Safety
	/// This does not check the ObjectKind discriminant of the Object header
	unsafe fn as_string_unchecked_mut (self) -> *mut String;

	/// Cast an Object pointer to a pointer to a variant
	/// # Safety
	/// This does not check the ObjectKind discriminant of the Object header
	unsafe fn as_record_unchecked_mut (self) -> *mut Record;

	/// Cast an Object pointer to a pointer to a variant
	/// # Safety
	/// This does not check the ObjectKind discriminant of the Object header
	unsafe fn as_function_unchecked_mut (self) -> *mut Function;

	/// Cast an Object pointer to a pointer to a variant
	/// # Safety
	/// This does not check the ObjectKind discriminant of the Object header
	unsafe fn as_procedure_unchecked_mut (self) -> *mut Procedure;

	/// Cast an Object pointer to a pointer to a variant
	/// # Safety
	/// This does not check the ObjectKind discriminant of the Object header
	unsafe fn as_closure_unchecked_mut (self) -> *mut Closure;
	
	/// Cast an Object pointer to a pointer to a variant
	/// # Safety
	/// This does not check the ObjectKind discriminant of the Object header
	unsafe fn as_foreign_unchecked_mut (self) -> *mut Foreign;

	/// Cast an Object pointer to a pointer to a variant
	/// # Safety
	/// This does not check the ObjectKind discriminant of the Object header
	unsafe fn as_userdata_unchecked_mut (self) -> *mut Userdata;


	/// Cast an Object pointer to a pointer to a variant
	fn as_array_mut (self) -> Option<*mut Array> { if self.get_kind() == ObjectKind::Array { Some(unsafe { self.as_array_unchecked_mut() }) } else { None } }

	/// Cast an Object pointer to a pointer to a variant
	fn as_map_mut (self) -> Option<*mut Map> { if self.get_kind() == ObjectKind::Map { Some(unsafe { self.as_map_unchecked_mut() }) } else { None } }

	/// Cast an Object pointer to a pointer to a variant
	fn as_string_mut (self) -> Option<*mut String> { if self.get_kind() == ObjectKind::String { Some(unsafe { self.as_string_unchecked_mut() }) } else { None } }

	/// Cast an Object pointer to a pointer to a variant
	fn as_record_mut (self) -> Option<*mut Record> { if self.get_kind() == ObjectKind::Record { Some(unsafe { self.as_record_unchecked_mut() }) } else { None } }

	/// Cast an Object pointer to a pointer to a variant
	fn as_function_mut (self) -> Option<*mut Function> { if self.get_kind() == ObjectKind::Function { Some(unsafe { self.as_function_unchecked_mut() }) } else { None } }

	/// Cast an Object pointer to a pointer to a variant
	fn as_procedure_mut (self) -> Option<*mut Procedure> { self.as_function_mut().and_then(FnCastMut::as_procedure_mut) }

	/// Cast an Object pointer to a pointer to a variant
	fn as_closure_mut (self) -> Option<*mut Closure> { self.as_function_mut().and_then(FnCastMut::as_closure_mut) }
	
	/// Cast an Object pointer to a pointer to a variant
	fn as_foreign_mut (self) -> Option<*mut Foreign> { self.as_function_mut().and_then(FnCastMut::as_foreign_mut) }

	/// Cast an Object pointer to a pointer to a variant
	fn as_userdata_mut (self) -> Option<*mut Userdata> { if self.get_kind() == ObjectKind::Userdata { Some(unsafe { self.as_userdata_unchecked_mut() }) } else { None } }
}


unsafe impl ObjCastable for *const Object {
	fn get_kind (self) -> ObjectKind { unsafe { (*self).kind } }
	fn get_type_id (self) -> TypeID { unsafe { (*self).type_id } }
}

unsafe impl ObjCastable for *mut Object {
	fn get_kind (self) -> ObjectKind { unsafe { (*self).kind } }
	fn get_type_id (self) -> TypeID { unsafe { (*self).type_id } }
}

unsafe impl ObjCast for *const Object {
	unsafe fn as_array_unchecked (self) -> *const Array { self as _ }
	unsafe fn as_map_unchecked (self) -> *const Map { self as _ }
	unsafe fn as_string_unchecked (self) -> *const String { self as _ }
	unsafe fn as_record_unchecked (self) -> *const Record { self as _ }
	unsafe fn as_function_unchecked (self) -> *const Function { self as _ }
	unsafe fn as_procedure_unchecked (self) -> *const Procedure { self as _ }
	unsafe fn as_closure_unchecked (self) -> *const Closure { self as _ }
	unsafe fn as_foreign_unchecked (self) -> *const Foreign { self as _ }
	unsafe fn as_userdata_unchecked (self) -> *const Userdata { self as _ }
}

unsafe impl ObjCast for *mut Object {
	unsafe fn as_array_unchecked (self) -> *const Array { self as _ }
	unsafe fn as_map_unchecked (self) -> *const Map { self as _ }
	unsafe fn as_string_unchecked (self) -> *const String { self as _ }
	unsafe fn as_record_unchecked (self) -> *const Record { self as _ }
	unsafe fn as_function_unchecked (self) -> *const Function { self as _ }
	unsafe fn as_procedure_unchecked (self) -> *const Procedure { self as _ }
	unsafe fn as_closure_unchecked (self) -> *const Closure { self as _ }
	unsafe fn as_foreign_unchecked (self) -> *const Foreign { self as _ }
	unsafe fn as_userdata_unchecked (self) -> *const Userdata { self as _ }
}

unsafe impl ObjCastMut for *mut Object {
	unsafe fn as_array_unchecked_mut (self) -> *mut Array { self as _ }
	unsafe fn as_map_unchecked_mut (self) -> *mut Map { self as _ }
	unsafe fn as_string_unchecked_mut (self) -> *mut String { self as _ }
	unsafe fn as_record_unchecked_mut (self) -> *mut Record { self as _ }
	unsafe fn as_function_unchecked_mut (self) -> *mut Function { self as _ }
	unsafe fn as_procedure_unchecked_mut (self) -> *mut Procedure { self as _ }
	unsafe fn as_closure_unchecked_mut (self) -> *mut Closure { self as _ }
	unsafe fn as_foreign_unchecked_mut (self) -> *mut Foreign { self as _ }
	unsafe fn as_userdata_unchecked_mut (self) -> *mut Userdata { self as _ }
}

/// Allows casting between specific variants and generic Object pointers.
/// The only safe implementation of this trait is on *mut/const Procedure, Closure and Foreign
pub unsafe trait IntoObj: Sized + Copy {
	/// Upcast a variant pointer into a Object pointer
	fn as_object (self) -> *const Object;
}

/// Allows casting between specific variants and generic Object pointers.
/// The only safe implementation of this trait is on *mut Procedure, Closure and Foreign
pub unsafe trait IntoObjMut: Sized + Copy {
	/// Upcast a variant pointer into a Object pointer
	fn as_object_mut (self) -> *mut Object;
}

unsafe impl IntoObj for *const Array { fn as_object (self) -> *const Object { self as _ } }
unsafe impl IntoObj for *const Map { fn as_object (self) -> *const Object { self as _ } }
unsafe impl IntoObj for *const String { fn as_object (self) -> *const Object { self as _ } }
unsafe impl IntoObj for *const Record { fn as_object (self) -> *const Object { self as _ } }
unsafe impl IntoObj for *const Function { fn as_object (self) -> *const Object { self as _ } }
unsafe impl IntoObj for *const Userdata { fn as_object (self) -> *const Object { self as _ } }

unsafe impl IntoObj for *mut Array { fn as_object (self) -> *const Object { self as _ } }
unsafe impl IntoObj for *mut Map { fn as_object (self) -> *const Object { self as _ } }
unsafe impl IntoObj for *mut String { fn as_object (self) -> *const Object { self as _ } }
unsafe impl IntoObj for *mut Record { fn as_object (self) -> *const Object { self as _ } }
unsafe impl IntoObj for *mut Function { fn as_object (self) -> *const Object { self as _ } }
unsafe impl IntoObj for *mut Userdata { fn as_object (self) -> *const Object { self as _ } }

unsafe impl IntoObjMut for *mut Array { fn as_object_mut (self) -> *mut Object { self as _ } }
unsafe impl IntoObjMut for *mut Map { fn as_object_mut (self) -> *mut Object { self as _ } }
unsafe impl IntoObjMut for *mut String { fn as_object_mut (self) -> *mut Object { self as _ } }
unsafe impl IntoObjMut for *mut Record { fn as_object_mut (self) -> *mut Object { self as _ } }
unsafe impl IntoObjMut for *mut Function { fn as_object_mut (self) -> *mut Object { self as _ } }
unsafe impl IntoObjMut for *mut Userdata { fn as_object_mut (self) -> *mut Object { self as _ } }


/// Allows casting between generic Function pointers and specific variants.
/// The only safe implementation of this trait is on *mut Function
pub unsafe trait FnCastable: Sized + Copy {
	/// Allows casting between generic Function pointers and specific variants.
	/// The only safe implementation of this trait is on *mut/const Function

	/// Get the FunctionKind of a Function
	fn get_kind (self) -> FunctionKind;

	/// Get the TypeID of a Function
	fn get_type_id (self) -> TypeID;
}

/// Allows casting between generic Function pointers and specific variants.
/// The only safe implementation of this trait is on *mut/const Function
pub unsafe trait FnCast: FnCastable {
	/// Upcast a Function to an Object
	fn as_object (self) -> *const Object;

	/// Cast a Function pointer to a pointer to a variant
	/// # Safety
	/// This does not check the FunctionKind discriminant of the Function header
	unsafe fn as_procedure_unchecked (self) -> *const Procedure;

	/// Cast a Function pointer to a pointer to a variant
	/// # Safety
	/// This does not check the FunctionKind discriminant of the Function header
	unsafe fn as_closure_unchecked (self) -> *const Closure;

	/// Cast a Function pointer to a pointer to a variant
	/// # Safety
	/// This does not check the FunctionKind discriminant of the Function header
	unsafe fn as_foreign_unchecked (self) -> *const Foreign;

	/// Cast a Function pointer to a pointer to a variant
	fn as_procedure (self) -> Option<*const Procedure> { if self.get_kind() == FunctionKind::Procedure { Some(unsafe { self.as_procedure_unchecked() }) } else { None } }

	/// Cast a Function pointer to a pointer to a variant
	fn as_closure (self) -> Option<*const Closure> { if self.get_kind() == FunctionKind::Closure { Some(unsafe { self.as_closure_unchecked() }) } else { None } }
	
	/// Cast a Function pointer to a pointer to a variant
	fn as_foreign (self) -> Option<*const Foreign> { if self.get_kind() == FunctionKind::Foreign { Some(unsafe { self.as_foreign_unchecked() }) } else { None } }
}

/// Allows casting between generic Function pointers and specific variants.
/// The only safe implementation of this trait is on *mut/const Function
pub unsafe trait FnCastMut: FnCastable {
	/// Upcast a Function to an Object
	fn as_object_mut (self) -> *mut Object;

	/// Cast a Function pointer to a pointer to a variant
	/// # Safety
	/// This does not check the FunctionKind discriminant of the Function header
	unsafe fn as_procedure_unchecked_mut (self) -> *mut Procedure;

	/// Cast a Function pointer to a pointer to a variant
	/// # Safety
	/// This does not check the FunctionKind discriminant of the Function header
	unsafe fn as_closure_unchecked_mut (self) -> *mut Closure;

	/// Cast a Function pointer to a pointer to a variant
	/// # Safety
	/// This does not check the FunctionKind discriminant of the Function header
	unsafe fn as_foreign_unchecked_mut (self) -> *mut Foreign;

	/// Cast a Function pointer to a pointer to a variant
	fn as_procedure_mut (self) -> Option<*mut Procedure> { if self.get_kind() == FunctionKind::Procedure { Some(unsafe { self.as_procedure_unchecked_mut() }) } else { None } }

	/// Cast a Function pointer to a pointer to a variant
	fn as_closure_mut (self) -> Option<*mut Closure> { if self.get_kind() == FunctionKind::Closure { Some(unsafe { self.as_closure_unchecked_mut() }) } else { None } }
	
	/// Cast a Function pointer to a pointer to a variant
	fn as_foreign_mut (self) -> Option<*mut Foreign> { if self.get_kind() == FunctionKind::Foreign { Some(unsafe { self.as_foreign_unchecked_mut() }) } else { None } }
}

unsafe impl FnCastable for *const Function {
	fn get_kind (self) -> FunctionKind { unsafe { (*self).kind } }
	fn get_type_id (self) -> TypeID { unsafe { (*self).object_header.type_id } }
}

unsafe impl FnCastable for *mut Function {
	fn get_kind (self) -> FunctionKind { unsafe { (*self).kind } }
	fn get_type_id (self) -> TypeID { unsafe { (*self).object_header.type_id } }
}

unsafe impl FnCast for *const Function {
	fn as_object (self) -> *const Object { self as _ }
	unsafe fn as_procedure_unchecked (self) -> *const Procedure { self as _ }
	unsafe fn as_closure_unchecked (self) -> *const Closure { self as _ }
	unsafe fn as_foreign_unchecked (self) -> *const Foreign { self as _ }
}

unsafe impl FnCast for *mut Function {
	fn as_object (self) -> *const Object { self as _ }
	unsafe fn as_procedure_unchecked (self) -> *const Procedure { self as _ }
	unsafe fn as_closure_unchecked (self) -> *const Closure { self as _ }
	unsafe fn as_foreign_unchecked (self) -> *const Foreign { self as _ }
}

unsafe impl FnCastMut for *mut Function {
	fn as_object_mut (self) -> *mut Object { self as _ }
	unsafe fn as_procedure_unchecked_mut (self) -> *mut Procedure { self as _ }
	unsafe fn as_closure_unchecked_mut (self) -> *mut Closure { self as _ }
	unsafe fn as_foreign_unchecked_mut (self) -> *mut Foreign { self as _ }
}


/// Allows casting between specific variants and generic Function pointers.
/// The only safe implementation of this trait is on *mut/const Procedure, Closure and Foreign
pub unsafe trait IntoFn: Sized + Copy {
	/// Upcast a variant pointer into a Function pointer
	fn as_function (self) -> *const Function;
}

/// Allows casting between specific variants and generic Function pointers.
/// The only safe implementation of this trait is on *mut Procedure, Closure and Foreign
pub unsafe trait IntoFnMut: Sized + Copy {
	/// Upcast a variant pointer into a Function pointer
	fn as_function_mut (self) -> *mut Function;
}


unsafe impl IntoFn for *const Procedure { fn as_function (self) -> *const Function { self as _ } }
unsafe impl IntoFn for *const Closure { fn as_function (self) -> *const Function { self as _ } }
unsafe impl IntoFn for *const Foreign { fn as_function (self) -> *const Function { self as _ } }

unsafe impl IntoFn for *mut Procedure { fn as_function (self) -> *const Function { self as _ } }
unsafe impl IntoFn for *mut Closure { fn as_function (self) -> *const Function { self as _ } }
unsafe impl IntoFn for *mut Foreign { fn as_function (self) -> *const Function { self as _ } }

unsafe impl IntoFnMut for *mut Procedure { fn as_function_mut (self) -> *mut Function { self as _ } }
unsafe impl IntoFnMut for *mut Closure { fn as_function_mut (self) -> *mut Function { self as _ } }
unsafe impl IntoFnMut for *mut Foreign { fn as_function_mut (self) -> *mut Function { self as _ } }


unsafe impl<T> IntoObj for T where T: IntoFn { fn as_object (self) -> *const Object { self.as_function() as _ } }
unsafe impl<T> IntoObjMut for T where T: IntoFnMut { fn as_object_mut (self) -> *mut Object { self.as_function_mut() as _ } }


/// A dynamically sized array (vec) of Values of the same type
#[repr(C)]
pub struct Array {
	/// Contains the object's type id and linked list pointer
	pub header: Object,
	/// Contains the actual value of the object
	pub data: Vec<Value>
}

impl PartialEq for Array {
	fn eq (&self, other: &Self) -> bool {
		self.header.type_id == other.header.type_id && self.data == other.data
	}
}

impl Eq for Array { }

impl Ord for Array {
	fn cmp (&self, other: &Self) -> Ordering {
		let mut base = self.header.type_id.cmp(&other.header.type_id);

		if base == Ordering::Equal {
			base = base.then(self.data.cmp(&other.data));
		}

		base
	}
}

impl PartialOrd for Array {
	fn partial_cmp (&self, other: &Self) -> Option<Ordering> {
		Some(self.cmp(other))
	}
}

impl Hash for Array {
	fn hash<H: Hasher> (&self, h: &mut H) {
		self.header.type_id.hash(h);
		self.data.hash(h);
	}
}


/// A hashmap from values of one type to values of another or the same type
#[repr(C)]
pub struct Map {
	/// Contains the object's type id and linked list pointer
	pub header: Object,
	/// Contains the actual value of the object
	pub data: HashMap<Value, Value>
}

impl PartialEq for Map {
	fn eq (&self, other: &Self) -> bool {
		self.header.type_id == other.header.type_id && self.data == other.data
	}
}

impl Eq for Map { }

impl Ord for Map {
	fn cmp (&self, other: &Self) -> Ordering {
		// No way to order two maps by value because even containing the same key/value pairs, they may not iterate in the same order
		self.header.type_id.cmp(&other.header.type_id).then((self as *const Map).cmp(&(other as *const Map)))
	}
}

impl PartialOrd for Map {
	fn partial_cmp (&self, other: &Self) -> Option<Ordering> {
		Some(self.cmp(other))
	}
}

impl Hash for Map {
	fn hash<H: Hasher> (&self, h: &mut H) {
		self.header.type_id.hash(h);
		// Unstable key/value order makes hashing by value problematic
		(self as *const Map).hash(h);
	}
}


/// Utf8-encoded string
#[repr(C)]
pub struct String {
	/// Contains the object's type id and linked list pointer
	pub header: Object,
	/// Contains the actual value of the object
	pub data: std::string::String
}

impl PartialEq for String {
	fn eq (&self, other: &Self) -> bool {
		self.data == other.data
	}
}

impl Eq for String { }

impl Ord for String {
	fn cmp (&self, other: &Self) -> Ordering {
		self.data.cmp(&other.data)
	}
}

impl PartialOrd for String {
	fn partial_cmp (&self, other: &Self) -> Option<Ordering> {
		Some(self.cmp(other))
	}
}

impl Hash for String {
	fn hash<H: Hasher> (&self, h: &mut H) {
		self.data.hash(h)
	}
}


/// A collection of values of various (predefined) types
#[repr(C)]
pub struct Record {
	/// Contains the object's type id and linked list pointer
	pub header: Object,
	/// The number of fields associated with a Record
	pub num_fields: usize,
	/// The Values associated with each field
	pub fields: *mut Value
}

impl Record {
	/// Get a slice of the fields in a Record
	pub fn fields (&self) -> &[Value] {
		// SAFETY: this is safe as long as the Record instance was constructed properly
		unsafe { std::slice::from_raw_parts(self.fields, self.num_fields) }
	}

	/// Get a mutable slice of the fields in a Record
	pub fn fields_mut (&mut self) -> &mut [Value] {
		// SAFETY: this is safe as long as the Record instance was constructed properly
		unsafe { std::slice::from_raw_parts_mut(self.fields, self.num_fields) }
	}
}

impl PartialEq for Record {
	fn eq (&self, other: &Self) -> bool {
		if other.header.type_id != self.header.type_id { return false }

		for (a, b) in self.fields().iter().zip(other.fields().iter()) {
			if a != b { return false }
		}

		true
	}
}

impl Eq for Record { }

impl Ord for Record {
	fn cmp (&self, other: &Self) -> Ordering {
		let mut base = self.header.type_id.cmp(&other.header.type_id);

		if base == Ordering::Equal {
			for (a, b) in self.fields().iter().zip(other.fields().iter()) {
				base = base.then(a.cmp(b))
			}
		}

		base
	}
}

impl PartialOrd for Record {
	fn partial_cmp (&self, other: &Self) -> Option<Ordering> {
		Some(self.cmp(other))
	}
}

impl Hash for Record {
	fn hash<H: Hasher> (&self, h: &mut H) {
		self.header.type_id.hash(h);

		for f in self.fields().iter() {
			f.hash(h)
		}
	}
}


/// Singifies whether a function is a native function or if it is internal to the VM, whether it is a free function or a closure
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum FunctionKind {
	/// A free function or method, internal to the vm, with no internal state
	Procedure,
	/// A function internal to the vm, with its own bound internal state
	Closure,
	/// A native machine code function provided by the vm's host
	Foreign,
}


/// A wrapper around the standard Object header, providing specific information on the variant of a function
#[repr(C)]
pub struct Function {
	/// Contains the object's type id and linked list pointer
	pub object_header: Object,
	/// Discriminant for the kind of the function containing to this header
	pub kind: FunctionKind,
}

/// A free standing procedure function
#[repr(C)]
pub struct Procedure {
	/// Contains the object's type id and linked list pointer
	pub header: Function,
	/// Contains any constant values used in the bytecode of a Function.
	/// This reduces redundancy and allows garbage collector tracking of object constants
	pub constants: Vec<Value>,
	/// Contains the bytecode for a Function
	pub code: Vec<u8>,
}

/// A closure function
#[repr(C)]
pub struct Closure {
	/// Contains the object's type id and linked list pointer
	pub header: Function,
	/// The procedure function over which this Closure is formed
	pub function: *mut Procedure,
	/// An array of bindings to any captured Values used by a Closure
	pub upvalues: Vec<*mut Value>,
}

/// Wrapper for native functions
#[repr(C)]
pub struct Foreign {
	/// Contains the object's type id and linked list pointer
	pub header: Function,
	/// Contains the actual value of the object
	pub data: extern "C" fn (*mut Fiber) -> ()
}

/// Wrapper for native data
#[repr(C)]
pub struct Userdata {
	/// Contains the object's type id and linked list pointer
	pub header: Object,
	/// Contains the actual value of the object
	pub data: *mut dyn Any
}

