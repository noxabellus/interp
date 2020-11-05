//! Object value implementations

use std::{
  hash::{ Hash, Hasher, },
  collections::HashMap,
  cmp::Ordering,
  any::Any,
};

use crate::{
  value::Value,
  typeinfo::TypeID,
};

/// Component of all Object type variants,
/// allowing garbage collection and type identification
#[repr(C)]
pub struct Header {
  /// Discrimant for the type of the object containing to this header
  pub type_id: TypeID,
  /// Linked list pointer to the next allocated Object
  pub next: *mut Header,
}


/// A dynamically sized array (vec) of Values of the same type
#[repr(C)]
pub struct Array {
  /// Contains the object's type id and linked list pointer
  pub header: Header,
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
  pub header: Header,
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
  pub header: Header,
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
  pub header: Header,
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


/// A free function or closure
#[repr(C)]
pub struct Function {
  /// Contains the object's type id and linked list pointer
  pub header: Header,
  // TODO
}


/// Wrapper for native data
#[repr(C)]
pub struct Userdata {
  /// Contains the object's type id and linked list pointer
  pub header: Header,
  /// Contains the actual value of the object
  pub data: Box<dyn Any>
}

/// Wrapper for native functions
#[repr(C)]
pub struct Foreign {
  /// Contains the object's type id and linked list pointer
  pub header: Header,
  /// Contains the actual value of the object
  pub data: extern "C" fn () -> ()
}