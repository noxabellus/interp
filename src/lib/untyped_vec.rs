//! C-style type-less vec implementation

use std::{
  mem::ManuallyDrop,
  ptr,
  alloc::{ Layout, alloc, realloc, dealloc, handle_alloc_error },
  intrinsics::{ likely, unlikely }
};


type Raw = u8;


/// Implements Vec operations without type info, c-style
#[derive(Clone)]
pub struct UntypedVec {
  data: *mut Raw,
  value_size: usize,
  value_align: usize,
  length: usize,
  capacity: usize
}

impl UntypedVec {
  /// Create a new UntypedVec and initialize its value size and alignment without allocating
  pub fn new (value_size: usize, value_align: usize) -> Self {
    Self {
      data: ptr::null_mut(),
      value_size,
      value_align,
      length: 0,
      capacity: 0
    }
  }

  /// Create a new UntypedVec, initialize its value size and alignment, and allocate enough memory for a given capacity of values
  pub fn with_capacity(value_size: usize, value_align: usize, capacity: usize) -> Self {
    let mut out = Self::new(value_size, value_align);

    out.allocate_total(capacity);

    out
  }


  /// Ensure enough space to store at least `desired_capacity` values has been allocated in a UntypedVec.
  pub fn allocate_total (&mut self, desired_capacity: usize) { unsafe {
    if desired_capacity <= self.capacity { return }

    let mut capacity;

    let mem;
    if likely(self.capacity > 0) {
      capacity = self.capacity;

      let old_layout = Layout::from_size_align(self.value_size * capacity, self.value_align).unwrap();
      
      while capacity < desired_capacity { capacity *= 2; }

      mem = realloc(self.data as *mut u8, old_layout, self.value_size * capacity);
    } else {
      capacity = 2;
      
      while capacity < desired_capacity { capacity *= 2; }

      mem = alloc(Layout::from_size_align(self.value_size * capacity, self.value_align).unwrap());
    }

    if unlikely(mem.is_null()) {
      let new_layout = Layout::from_size_align(self.value_size * capacity, self.value_align).unwrap();
      handle_alloc_error(new_layout)
    }

    self.data = mem;
    self.capacity = capacity;
  } }

  /// Ensure enough space to store at least `additional_capacity` *more* values has been allocated in a UntypedVec.
  pub fn allocate_additional (&mut self, additional_capacity: usize) {
    self.allocate_total(self.length + additional_capacity);
  }



  /// Append `n` values onto the end of an UntypedVec.
  /// Returns a pointer to the start of the values added.
  /// If `values` is null, no copy occurs
  /// # Safety
  /// The caller must ensure that `values` is valid for reads for up to `n * value_size` bytes
  pub unsafe fn push_n (&mut self, values: *const Raw, n: usize) -> *mut Raw {
    debug_assert!(n != 0);

    self.allocate_additional(n);

    let p = self.end_ptr();

    if !values.is_null() { ptr::copy(values, p, n * self.value_size) }

    self.length += n;
    
    p
  }

  /// Append a value onto the end of an UntypedVec.
  /// Returns a pointer to the value added.
  /// If `value` is null, no copy occurs
  /// # Safety
  /// The caller must ensure that `value` is valid for reads for up to `value_size` bytes
  pub unsafe fn push (&mut self, value: *const Raw) -> *mut Raw {
    self.push_n(value, 1)
  }

  /// Add `n` values at the given index of an UntypedVec.
  /// Returns a pointer to the start of the values added.
  /// If `values` is null, no copy occurs
  /// # Safety
  /// The caller must ensure that `values` is valid for reads for up to `n * value_size` bytes
  pub unsafe fn insert_n (&mut self, idx: usize, values: *const Raw, n: usize) -> *mut Raw {
    debug_assert!(n != 0);

    if unlikely(idx >= self.length) { return self.push_n(values, n) }

    self.allocate_additional(n);
    
    let n_bytes = n * self.value_size;
    
    let p = self.get_unchecked(idx);

    ptr::copy(p, p.add(n_bytes), (self.length - idx) * self.value_size);
    
    if !values.is_null() { ptr::copy(values, p, n_bytes) }

    self.length += n;

    p
  }

  /// Add a value at the given index of an UntypedVec.
  /// Returns a pointer to the value added.
  /// If `value` is null, no copy occurs
  /// # Safety
  /// The caller must ensure that `value` is valid for reads for up to `value_size` bytes
  pub unsafe fn insert (&mut self, idx: usize, value: *const Raw) -> *mut Raw {
    self.insert_n(idx, value, 1)
  }


  /// Add `n` values at the beginning of an UntypedVec.
  /// Returns a pointer to the values added.
  /// If `values` is null, no copy occurs
  /// # Safety
  /// The caller must ensure that `values` is valid for reads for up to `n * value_size` bytes
  pub unsafe fn shift_n (&mut self, values: *const Raw, n: usize) -> *mut Raw {
    self.insert_n(0, values, n)
  }


  /// Add a value at the beginning of an UntypedVec.
  /// Returns a pointer to the value added.
  /// If `value` is null, no copy occurs
  /// # Safety
  /// The caller must ensure that `value` is valid for reads for up to `value_size` bytes
  pub unsafe fn shift (&mut self, values: *const Raw) -> *mut Raw {
    self.insert_n(0, values, 1)
  }


  /// Remove `n` values at the given index of an UntypedVec.
  /// Copies removed data to `values` if it is not null
  /// # Safety
  /// The caller must ensure that `values` is valid for writes for up to `n * value_size` bytes.
  /// The caller must ensure that `idx` through `idx + n` is in range
  pub unsafe fn remove_unchecked (&mut self, idx: usize, values: *mut Raw, n: usize) {
    let p = self.get_unchecked(idx);

    if !values.is_null() { ptr::copy(p, values, n * self.value_size) }
    
    if idx + n < self.length {
      ptr::copy(p.add(n), p, (self.length - (idx + n)) * self.value_size)
    }
    
    self.length -= n;
  }

  /// Remove `n` values at the given index of an UntypedVec.
  /// Copies removed data to `values` if it is not null.
  /// If the index and count is out of range, does nothing and returns false
  /// # Safety
  /// The caller must ensure that `values` is valid for writes for up to `n * value_size` bytes
  pub unsafe fn remove_n (&mut self, idx: usize, values: *mut Raw, n: usize) -> bool {
    if likely(self.length >= idx + n) {
      self.remove_unchecked(idx, values, n);

      true
    } else {
      false
    }
  }

  /// Remove a value at the given index of an UntypedVec.
  /// Copies removed data to `value` if it is not null.
  /// If the index is out of range, does nothing and returns false
  /// # Safety
  /// The caller must ensure that `value` is valid for writes for up to `value_size` bytes
  pub unsafe fn remove (&mut self, idx: usize, value: *mut Raw) -> bool {
    if likely(self.length > idx) {
      self.remove_unchecked(idx, value, 1);

      true
    } else {
      false
    }
  }



  /// Remove `n` values at the given index of an UntypedVec.
  /// Copies removed data to `values` if it is not null
  /// # Safety
  /// The caller must ensure that `values` is valid for writes for up to `n * value_size` bytes.
  /// The caller must ensure that `idx` through `idx + n` is in range
  pub unsafe fn swap_remove_unchecked (&mut self, idx: usize, values: *mut Raw, n: usize) {
    let p = self.get_unchecked(idx);

    let n_bytes = n * self.value_size;
    if !values.is_null() { ptr::copy(p, values, n_bytes) }

    if idx + n < self.length {
      ptr::copy(self.end_ptr().offset(-(n_bytes as isize)), p, n_bytes);
    }

    self.length -= n;
  }

  /// Remove `n` values at the given index of an UntypedVec.
  /// Copies removed data to `values` if it is not null
  /// If the index and count is out of range, does nothing and returns false
  /// # Safety
  /// The caller must ensure that `values` is valid for writes for up to `n * value_size` bytes
  pub unsafe fn swap_remove_n (&mut self, idx: usize, values: *mut Raw, n: usize) -> bool {
    if likely(self.length >= idx + n) {
      self.swap_remove_unchecked(idx, values, n);

      true
    } else {
      false
    }
  }

  /// Remove a value at the given index of an UntypedVec.
  /// Copies removed data to `value` if it is not null.
  /// If the index is out of range, does nothing and returns false
  /// # Safety
  /// The caller must ensure that `value` is valid for writes for up to `value_size` bytes
  pub unsafe fn swap_remove (&mut self, idx: usize, value: *mut Raw) -> bool {
    if likely(self.length > idx) {
      self.swap_remove_unchecked(idx, value, 1);

      true
    } else {
      false
    }
  }


  /// Remove `n` values at the start of an UntypedVec.
  /// Copies removed data to `values` if it is not null
  /// # Safety
  /// The caller must ensure that `values` is valid for writes for up to `n * value_size` bytes.
  /// The caller must ensure that the UntypedVec contains at least `n` elements
  pub unsafe fn unshift_unchecked (&mut self, values: *mut Raw, n: usize) {
    self.remove_unchecked(0, values, n)
  }


  /// Remove `n` values at the start of an UntypedVec.
  /// Copies removed data to `values` if it is not null.
  /// If the count is out of range, does nothing and returns false
  /// # Safety
  /// The caller must ensure that `values` is valid for writes for up to `n * value_size` bytes
  pub unsafe fn unshift_n (&mut self, values: *mut Raw, n: usize) -> bool {
    self.remove_n(0, values, n)
  }

  /// Remove a value at the start of an UntypedVec.
  /// Copies removed data to `value` if it is not null.
  /// If the UntypedVec is empty, does nothing and returns false
  /// # Safety
  /// The caller must ensure that `value` is valid for writes for up to `value_size` bytes
  pub unsafe fn unshift (&mut self, value: *mut Raw) -> bool {
    self.remove(0, value)
  }
  

  /// Returns a pointer to the last `n` elements in an UntypedVec and decrements the length, forgetting the values
  /// # Safety
  /// The returned pointer is only valid until another value is added to the UntypedVec.
  /// It is up to the caller to ensure that there is at least `n` elements in the UntypedVec
  pub unsafe fn pop_n_unchecked (&mut self, n: usize) -> *mut Raw {
    self.length -= n;
    self.end_ptr()
  }

  /// Returns a pointer to the last `n` elements in an UntypedVec and decrements the length, forgetting the values.
  /// Returns None and does nothing if there are not at least `n` elements in the UntypedVec
  /// # Safety
  /// The returned pointer is only valid until another value is added to the UntypedVec.
  pub unsafe fn pop_n (&mut self, n: usize) -> Option<*mut Raw> {
    if likely(self.length >= n) {
      Some(self.pop_n_unchecked(n))
    } else {
      None
    }
  }


  /// Returns a pointer to the last element in an UntypedVec and decrements the length, forgetting the value
  /// # Safety
  /// The returned pointer is only valid until another value is added to the UntypedVec.
  /// It is up to the caller to ensure that there is at least one element in the UntypedVec
  pub unsafe fn pop_unchecked (&mut self) -> *mut Raw {
    self.pop_n_unchecked(1)
  }

  /// Returns a pointer to the last element in an UntypedVec and decrements the length, forgetting the value.
  /// Returns None and does nothing if there is not at least one element in the UntypedVec
  /// # Safety
  /// The returned pointer is only valid until another value is added to the UntypedVec.
  pub unsafe fn pop (&mut self) -> Option<*mut Raw> {
    self.pop_n(1)
  }


  /// Get a pointer to the first byte of value data in an UntypedVec
  pub fn as_ptr (&self) -> *mut Raw {
    let p = self.data;
    unsafe { 
      if likely(!p.is_null()) {
        p.add(self.value_align - (p as usize & (self.value_align - 1)))
      } else {
        p
      }
    }
  }

  /// Replace `n` existing values in an UntypedVec
  /// # Safety
  /// The caller must ensure that `values` is valid for reads for up to `n * value_size` bytes.
  /// The caller must ensure that `idx` through `idx + n` is in range
  pub unsafe fn replace_unchecked (&mut self, idx: usize, values: *const Raw, n: usize) {
    ptr::copy(values, self.get_unchecked(idx), n * self.value_size)
  }

  /// Replace `n` existing values in an UntypedVec.
  /// Returns true if the given index and count were valid and the copy occurred
  /// # Safety
  /// The caller must ensure that `values` is valid for reads for up to `n * value_size` bytes.
  pub unsafe fn replace_n (&mut self, idx: usize, values: *const Raw, n: usize) -> bool {
    if likely(self.length >= idx + n) {
      self.replace_unchecked(idx, values, n);
      true
    }
    else {
      false
    }
  }

  /// Replace an existing value in an UntypedVec.
  /// Returns true if the given index was valid and the copy occurred
  /// # Safety
  /// The caller must ensure that `value` is valid for reads for up to `value_size` bytes.
  pub unsafe fn replace (&mut self, idx: usize, values: *const Raw) -> bool {
    self.replace_n(idx, values, 1)
  }


  /// Swap `n` values in an UntypedVec
  /// # Safety
  /// The caller must ensure that `values` is valid for reads and writes for up to `n * value_size` bytes.
  /// The caller must ensure that `idx` through `idx + n` is in range.
  /// The caller must ensure that `idx` through `idx + n` does not overlap with `values`
  pub unsafe fn swap_unchecked (&mut self, idx: usize, values: *mut Raw, n: usize) {
    ptr::swap_nonoverlapping(values, self.get_unchecked(idx), n * self.value_size)
  }

  /// Swap `n` values in an UntypedVec.
  /// Returns true if the given index and count were valid and the copy occurred
  /// # Safety
  /// The caller must ensure that `values` is valid for reads and writes for up to `n * value_size` bytes.
  /// The caller must ensure that `idx` through `idx + n` does not overlap with `values`
  pub unsafe fn swap_n (&mut self, idx: usize, values: *mut Raw, n: usize) -> bool {
    if likely(self.length >= idx + n) {
      self.swap_unchecked(idx, values, n);
      true
    }
    else {
      false
    }
  }

  /// Swap a value in an UntypedVec.
  /// Returns true if the given index was valid and the copy occurred
  /// # Safety
  /// The caller must ensure that `value` is valid for reads and writes for up to `value_size` bytes.
  /// The caller must ensure that `idx` does not overlap with `value`
  pub unsafe fn swap (&mut self, idx: usize, values: *mut Raw) -> bool {
    self.swap_n(idx, values, 1)
  }



  /// Get a pointer to the byte at a value-multiple index in an UntypedVec
  /// # Safety
  /// This does not range check the given index, nor does it check that the UntypedVec's internal memory has been allocated
  pub unsafe fn get_unchecked (&self, idx: usize) -> *mut Raw {
    self.as_ptr().add(idx * self.value_size)
  }


  /// Get a pointer to the byte at a value-multiple index in an UntypedVec, if it is in range
  pub fn get (&self, idx: usize) -> Option<*mut Raw> {
    unsafe {
      if likely(self.length > idx) { 
        Some(self.as_ptr().add(idx * self.value_size))
      } else {
        None
      }
    }
  }


  /// Get a sentinal pointer just past the end of an UntypedVec's last value
  pub fn end_ptr (&self) -> *mut Raw {
    unsafe { self.as_ptr().add(self.value_size * self.length) }
  }


  /// Get an immutable iterator over the value pointers in an UntypedVec
  pub fn iter (&self) -> UntypedIter {
    UntypedIter {
      cur_ptr: self.as_ptr(),
      end_ptr: self.end_ptr(),
      value_size: self.value_size
    }
  }

  /// Get a mutable iterator over the value pointers in an UntypedVec
  pub fn iter_mut (&self) -> UntypedIterMut {
    UntypedIterMut {
      cur_ptr: self.as_ptr(),
      end_ptr: self.end_ptr(),
      value_size: self.value_size
    }
  }
}

impl Drop for UntypedVec {
  fn drop (&mut self) {
    let old_layout = Layout::from_size_align(self.value_size * self.capacity, self.value_align).unwrap();
    unsafe { dealloc(self.data, old_layout) }
  }
}


/// A constant pointer iterator over an UntypedVec
pub struct UntypedIter {
  cur_ptr: *const Raw,
  end_ptr: *const Raw,
  value_size: usize,
}

impl Iterator for UntypedIter {
  type Item = *const Raw;

  fn next (&mut self) -> Option<*const Raw> { unsafe {
    if likely(self.cur_ptr < self.end_ptr) {
      let out = Some(self.cur_ptr);
      self.cur_ptr = self.cur_ptr.add(self.value_size);
      out
    } else {
      None
    }
  } }
}


/// A mutable pointer iterator over an UntypedVec
pub struct UntypedIterMut {
  cur_ptr: *mut Raw,
  end_ptr: *mut Raw,
  value_size: usize,
}

impl Iterator for UntypedIterMut {
  type Item = *mut Raw;

  fn next (&mut self) -> Option<*mut Raw> { unsafe {
    if likely(self.cur_ptr < self.end_ptr) {
      let out = Some(self.cur_ptr);
      self.cur_ptr = self.cur_ptr.add(self.value_size);
      out
    } else {
      None
    }
  } }
}


/// An owning iterator over an UntypedVec
pub struct UntypedIntoIter {
  inner: ManuallyDrop<UntypedVec>,
  idx: usize
}

impl Iterator for UntypedIntoIter {
  type Item = *mut Raw;
  fn next (&mut self) -> Option<*mut Raw> {
    let out = self.inner.get(self.idx);
    self.idx += 1;
    out
  }
}

impl IntoIterator for UntypedVec {
  type Item = *mut Raw;
  type IntoIter = UntypedIntoIter;

  fn into_iter (self) -> Self::IntoIter {
    UntypedIntoIter {
      inner: ManuallyDrop::new(self),
      idx: 0
    }
  }
}

impl<'i> IntoIterator for &'i UntypedVec {
  type Item = *const Raw;
  type IntoIter = UntypedIter;

  fn into_iter (self) -> Self::IntoIter {
    self.iter()
  }
}

impl<'i> IntoIterator for &'i mut UntypedVec {
  type Item = *mut Raw;
  type IntoIter = UntypedIterMut;

  fn into_iter (self) -> Self::IntoIter {
    self.iter_mut()
  }
}