//! A stack implementation without type information

use std::{
  intrinsics::{ likely, unlikely }
};

type Raw = u8;

use super::unsafe_vec::UnsafeVec;


struct StackEntry {
  base: usize,
  ptr: *mut Raw,
}

/// A runtime equivalent of the native stack;
/// Allocates values while maintaining alignment, and keeps an array of pointers to these values for register windows
pub struct Stack {
  data: UnsafeVec<Raw>,
  entries: UnsafeVec<StackEntry>,
}


impl Stack {
  const DEFAULT_MAX_CAPACITY: usize = 1024 * 1024;

  /// Create a new Stack, with its internal vectors pre-allocating a default maximum capacity
  pub fn new () -> Self {
    Self::with_capacity(Self::DEFAULT_MAX_CAPACITY)
  }

  /// Create a new Stack, with its internal vectors pre-allocating a given maximum capacity
  pub fn with_capacity (max_capacity: usize) -> Self {
    unsafe {
      Self {
        data: UnsafeVec::with_capacity(max_capacity),
        entries: UnsafeVec::with_capacity(max_capacity)
      }
    }
  }

  /// Determine how much data capacity is available on the stack
  pub fn remaining_data_capacity (&self) -> usize {
    self.data.remaining_capacity()
  }

  /// Get the number of discreet values on the stack
  pub fn len (&self) -> usize {
    self.entries.len()
  }

  /// Determine if there are any values stored on the stack
  pub fn is_empty (&self) -> bool { 
    self.entries.is_empty()
  }

  /// Allocate space for a value on the Stack and return its index.
  /// Returns None on stack overflow
  pub fn push (&mut self, size: usize, align: usize) -> Option<usize> {
    unsafe {
      let p = self.data.as_ptr();
      let base = self.data.len();
      let base_ptr = p.add(base);
      let align_offset = align - (base_ptr as usize & (align - 1));

      let needed_size = align_offset + size;
      if unlikely(self.remaining_data_capacity() < needed_size) { return None }

      let ptr = base_ptr.add(align_offset);

      let idx = self.entries.push(StackEntry { base, ptr });
      
      self.data.set_len(self.data.len() + needed_size);

      Some(idx)
    }
  }


  /// Get a register window for a relative stack index (an index relative to the top of the stack).
  /// Returns None if the given index is invalid
  pub fn get_window (&self, rel_idx: usize) -> Option<*mut Raw> {
    if self.entries.len() > rel_idx {
      Some(unsafe { self.entries.get(self.len() - 1 - rel_idx).ptr })
    } else {
      None
    }
  }


  /// Remove a value from the Stack and return a temporary pointer to it for finalization.
  /// Returns None on stack underflow
  /// # Safety
  /// The pointer returned is only valid until the next call to push
  pub fn pop (&mut self) -> Option<*mut Raw> {
    unsafe {
      if unlikely(self.is_empty()) { return None }

      let StackEntry { base, ptr } = self.entries.pop_ref();

      self.data.set_len(*base);

      Some(*ptr)
    }
  }


  /// Duplicate the value at the top of the stack if there is one.
  /// Returns None on stack overflow or stack underflow
  /// # Safety
  /// This doesn't ensure that the duplicated value is the same size or alignment
  pub unsafe fn duplicate (&mut self, size: usize, align: usize) -> Option<usize> {
    if unlikely(self.is_empty()) { return None }
    
    let new_data = self.push(size, align);

    if likely(new_data.is_some()) {
      let orig = self.entries.get(self.len() - 2).ptr;
      let copy = self.entries.get(self.len() - 1).ptr;

      std::ptr::copy_nonoverlapping(orig, copy, size);
    }

    new_data
  }
}

impl Default for Stack { fn default () -> Self { Self::new() } }