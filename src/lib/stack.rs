//! Contains a stack implementation for the VM

use super::{
  type_info::TypeID,
  value::ValueData,
};


// TODO this really needs to be implemented directly, instead of via Vecs,
// because pretty much every function is performing duplicate validation


/// A LIFO stack implementation with parallel arrays of ValueData and TypeID
pub struct Stack {
  types: Vec<TypeID>,
  values: Vec<ValueData>
}

impl Stack {
  const DEFAULT_CAPACITY: usize = (1024 * 1024) / 8; // 1mb of values

  /// Create a new Stack with the default capacity, 1mb of values
  pub fn new () -> Self {
    Self::with_capacity(Self::DEFAULT_CAPACITY)
  }

  /// Create a new Stack with a given capacity
  pub fn with_capacity (capacity: usize) -> Self {
    Self {
      types: Vec::with_capacity(capacity),
      values: Vec::with_capacity(capacity)
    }
  }

  /// Get the number of values on the Stack
  pub fn len (&self) -> usize { self.values.len() }

  fn last_index (&self) -> Option<usize> { self.len().checked_sub(1) }

  /// Determine if there are any values on the Stack
  pub fn is_empty (&self) -> bool { self.values.is_empty() }

  /// Get the number of values allowed on the Stack
  pub fn capacity (&self) -> usize { self.values.capacity() }

  /// Add a new value onto the top of the Stack.
  /// Returns true on success, false on stack overflow
  pub fn push (&mut self, type_id: TypeID, value_data: ValueData) -> bool {
    if self.len() < self.capacity() {
      self.types.push(type_id);
      self.values.push(value_data);
      true
    } else {
      false
    }
  }

  /// Remove a value from the top of the Stack.
  /// Returns Some((TypeID, ValueData)) on success, None on stack underflow
  pub fn pop (&mut self) -> Option<(TypeID, ValueData)> {
    self.last_index().map(|idx| {
      (self.types.remove(idx), self.values.remove(idx))
    })
  }

  /// Get a reference to the type and value on the top of the Stack, if any exist
  pub fn peek (&self) -> Option<(&TypeID, &ValueData)> {
    self.last_index().map(|idx| unsafe {
      (self.types.get_unchecked(idx), self.values.get_unchecked(idx))
    })
  }

  /// Get a reference to the type and value on the top of the Stack, if any exist
  pub fn peek_mut (&mut self) -> Option<(&mut TypeID, &mut ValueData)> {
    if let Some(idx) = self.last_index() { unsafe {
      Some((self.types.get_unchecked_mut(idx), self.values.get_unchecked_mut(idx)))
    } } else { None }
  }

  /// Duplicate the value on the top of the Stack, if one exists
  /// Returns true on success, false on stack overflow or stack underflow
  pub fn duplicate (&mut self) -> bool {
    if let Some(idx) = self.last_index() { unsafe {
      self.push(*self.types.get_unchecked(idx), *self.values.get_unchecked(idx))
    } } else { false }
  }
}

impl Default for Stack { fn default () -> Self { Self::new() } }