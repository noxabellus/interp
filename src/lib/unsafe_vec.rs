//! Contains an unsafe / unchecked implementation of Vec

use std::{
  marker::PhantomData,
  mem::{ self, needs_drop, size_of, align_of, MaybeUninit },
  ptr,
  alloc::{ Layout, alloc, realloc, dealloc, handle_alloc_error },
  intrinsics::{ likely, unlikely }
};


/// An unsafe / unchecked implementation of Vec
#[derive(Clone)]
pub struct UnsafeVec<T> {
  data: *mut T,
  capacity: usize,
  length: usize,
}

#[allow(dead_code)]
impl<T> UnsafeVec<T> {

  /// Create a new UnsafeVec with no capacity
  /// # Safety
  /// Using the value created with this function,
  /// without first calling an allocate method
  /// will cause various explosions
  pub unsafe fn new () -> Self {
    Self { data: ptr::null_mut(), capacity: 0, length: 0 }
  }

  /// Create a new UnsafeVec with a given capacity
  /// # Safety
  /// This does not validate the created `alloc::Layout`.
  /// Calls `handle_alloc_error` if allocation fails
  pub unsafe fn with_capacity (capacity: usize) -> Self {
    let layout = Layout::from_size_align_unchecked(size_of::<T>() * capacity, align_of::<T>());

    let data = alloc(layout) as *mut T;

    if unlikely(data.is_null()) { handle_alloc_error(layout) }

    Self { data, capacity, length: 0 }
  }


  /// Ensure enough space to store at least `desired_capacity` elements has been allocated in a UnsafeVec.
  /// # Safety
  /// This does not validate the created `alloc::Layout`.
  /// Calls `handle_alloc_error` if allocation fails
  pub unsafe fn allocate_total (&mut self, desired_capacity: usize) {
    if self.capacity >= desired_capacity { return }

    let mut capacity;

    let mem;
    if likely(self.capacity > 0) {
      capacity = self.capacity;

      let old_layout = Layout::from_size_align_unchecked(size_of::<T>() * capacity, align_of::<T>());
      
      while capacity < desired_capacity { capacity *= 2; }

      mem = realloc(self.data as *mut u8, old_layout, size_of::<T>() * capacity) as *mut T;
    } else {
      capacity = 2;
      
      while capacity < desired_capacity { capacity *= 2; }

      mem = alloc(Layout::from_size_align_unchecked(size_of::<T>() * capacity, align_of::<T>())) as *mut T;
    }

    if unlikely(mem.is_null()) {
      let new_layout = Layout::from_size_align_unchecked(size_of::<T>() * capacity, align_of::<T>());
      handle_alloc_error(new_layout)
    }

    self.data = mem;
    self.capacity = capacity;
  }

  /// Ensure enough space to store at least `additional_capacity` *more* elements has been allocated in a UnsafeVec.
  /// # Safety
  /// See `allocate_total`
  pub unsafe fn allocate_additional (&mut self, additional_capacity: usize) {
    self.allocate_total(self.len() + additional_capacity);
  }


  /// Push a number of values to the end of an UnsafeVec. Does not attempt to copy from the given pointer if it is null
  /// # Safety
  /// This does not compare the current length to the capacity or reallocate
  pub unsafe fn push_n (&mut self, v: *const T, count: usize) -> usize {
    let idx = self.length;
    if !v.is_null() { ptr::copy(v, self.data.add(idx), count) }
    self.length += count;
    idx
  }

  /// Push a single value to the end of the UnsafeVec
  /// # Safety
  /// This does not compare the current length to the capacity or reallocate
  pub unsafe fn push (&mut self, v: T) -> usize {
    let idx = self.length;
    ptr::copy(&v, self.data.add(idx), 1);
    mem::forget(v);
    self.length += 1;
    idx
  }


  /// Insert multiple elements at the given index an UnsafeVec, moving any existing elements from the index forward
  /// # Safety
  /// This does not compare the current length to the capacity or reallocate.
  /// This does not range check the given index or number of values
  pub unsafe fn insert_n (&mut self, idx: usize, values: *const T, n: usize) {
    let p = self.get_ptr(idx);
    ptr::copy(p, p.add(n), self.length - idx);
    
    if !values.is_null() { ptr::copy(values, p, n) }

    self.length += n;
  }

  /// Insert an element at the given index in an UnsafeVec, moving any existing elements from the index forward
  /// # Safety
  /// This does not compare the current length to the capacity or reallocate.
  /// This does not range check the given index
  pub unsafe fn insert (&mut self, idx: usize, v: T) {
    self.insert_n(idx, &v, 1);
    mem::forget(v);
  }


  /// Insert multiple elements at the beginning of an UnsafeVec, moving any existing elements forward
  /// # Safety
  /// This does not compare the current length to the capacity or reallocate.
  /// This does not range check the given index or number of values
  pub unsafe fn shift_n (&mut self, values: *const T, n: usize) {
    self.insert_n(0, values, n)
  }

  /// Insert an element at the beginning of an UnsafeVec, moving any existing elements forward
  /// # Safety
  /// This does not compare the current length to the capacity or reallocate.
  pub unsafe fn shift (&mut self, v: T) {
    self.insert(0, v)
  }


  /// Get a constant reference to a given index offset in an UnsafeVec
  /// # Safety
  /// This does not range-check the index
  pub unsafe fn get (&self, idx: usize) -> &T {
    &*self.get_ptr(idx)
  }

  /// Get a mutable reference to a given index offset in an UnsafeVec
  /// # Safety
  /// This does not range-check the index
  pub unsafe fn get_mut (&mut self, idx: usize) -> &mut T {
    &mut *self.get_ptr(idx)
  }

  /// Set the length of an UnsafeVec to the given value
  /// # Safety
  /// This does not drop or create any values, and does not compare the new length to the capacity or reallocate
  pub unsafe fn set_len (&mut self, new_length: usize) {
    self.length = new_length
  }
  
  /// Get a pointer to the last values in an UnsafeVec and decrement its length
  /// # Safety
  /// This does not attempt to ensure that there is actually the given number of values stored in the UnsafeVec
  pub unsafe fn pop_n (&mut self, n: usize) -> *mut T {
    self.length -= n;
    self.end_ptr()
  }

  /// Get a reference to the last value in an UnsafeVec and decrement its length
  /// # Safety
  /// This does not attempt to ensure that there is actually a value stored in the UnsafeVec
  pub unsafe fn pop_ref (&mut self) -> &mut T {
    &mut *self.pop_n(1)
  }

  /// Extract the last value in an UnsafeVec and decrement its length
  /// # Safety
  /// This does not attempt to ensure that there is actually a value stored in the UnsafeVec
  pub unsafe fn pop (&mut self) -> T {
    ptr::read(self.pop_n(1))
  }


  /// Remove multiple elements from a given index in an UnsafeVec.
  /// Copies removed data into `values` if it is not null.
  /// This moves all values after the given index back `n` elements to close the gap.
  /// See `swap_remove_n` for a more efficient version
  /// # Safety
  /// This does not range check the given index or number of values.
  /// This does not attempt to drop the removed values, or any values overwritten in `values`
  pub unsafe fn remove_n (&mut self, idx: usize, values: *mut T, n: usize) {
    let p = self.get_ptr(idx);

    if !values.is_null() { ptr::copy(p, values, n) }

    if idx + n < self.length {
      ptr::copy(p.add(n), p, self.length - (idx + n))
    }

    self.length -= n;
  }

  /// Remove an element from a given index in an UnsafeVec.
  /// This moves all values after the given index back `n` elements to close the gap.
  /// See `swap_remove` for a more efficient version
  /// # Safety
  /// This does not range check the given index.
  /// This does not attempt to drop the removed values
  pub unsafe fn remove (&mut self, idx: usize) -> T {
    let mut out = MaybeUninit::uninit();

    self.remove_n(idx, out.as_mut_ptr(), 1);

    out.assume_init()
  }


  /// Remove multiple elements from the beginning of an UnsafeVec.
  /// Copies removed data into `values` if it is not null.
  /// This moves all values after `n` elements backward to close the gap.
  /// # Safety
  /// This does not range check the given index or number of values.
  /// This does not attempt to drop the removed values, or any values overwritten in `values`
  pub unsafe fn unshift_n (&mut self, values: *mut T, n: usize) {
    self.remove_n(0, values, n)
  }

  /// Remove an element from the beginning of an UnsafeVec.
  /// This moves all values after `n` elements backward to close the gap.
  /// # Safety
  /// This does not check that there is a value in the UnsafeVec.
  pub unsafe fn unshift (&mut self) -> T {
    self.remove(0)
  }


  /// Remove multiple elements from a given index in an UnsafeVec.
  /// Copies removed data into `values` if it is not null.
  /// This moves values from the end of the UnsafeVec to close the gap.
  /// See `remove_n` for an order-preserving version
  /// # Safety
  /// This does not range check the given index or number of values.
  /// This does not attempt to drop the removed values, or any values overwritten in `values`
  pub unsafe fn swap_remove_n (&mut self, idx: usize, values: *mut T, n: usize) {
    let p = self.get_ptr(idx);

    if !values.is_null() { ptr::copy(p, values, n) }

    if idx + n < self.length {
      ptr::copy(self.end_ptr().offset(-(n as isize)), p, n);
    }

    self.length -= n;
  }


  /// Remove an element from a given index in an UnsafeVec.
  /// This moves values from the end of the UnsafeVec to close the gap.
  /// See `remove` for an order-preserving version
  /// # Safety
  /// This does not range check the given index.
  /// This does not attempt to drop the removed values
  pub unsafe fn swap_remove (&mut self, idx: usize) -> T {
    let mut out = MaybeUninit::uninit();

    self.swap_remove_n(idx, out.as_mut_ptr(), 1);

    out.assume_init()
  }

  /// Get the number of values stored in an UnsafeVec
  pub fn len (&self) -> usize { self.length }
  /// Get the index of the last value stored in an UnsafeVec
  /// # Safety
  /// This does not attempt to ensure that there is actually a value stored in the UnsafeVec
  pub unsafe fn last_idx (&self) -> usize { self.length - 1 }
  /// Determine if there are any values stored in an UnsafeVec
  pub fn is_empty (&self) -> bool { self.length == 0 }
  /// Determine the current allocated storage in number of values for an UnsafeVec
  pub fn capacity (&self) -> usize { self.capacity }
  /// Determine how much of the allocated storage is still available in an UnsafeVec
  pub fn remaining_capacity (&self) -> usize { self.capacity - self.length }



  /// Drop all items from idx to idx + n, if they implement drop
  /// # Safety
  /// This does not check that the value type of the UnsafeVec actually needs drop.
  /// This does not range check the given index or number of values.
  /// This does not change the length or arrangement of the UnsafeVec
  pub unsafe fn drop_range (&mut self, idx: usize, n: usize) {
    for i in idx..(idx + n) {
      ptr::drop_in_place(self.data.add(i));
    }
  }



  /// Get a pointer to a given index offset in an UnsafeVec
  /// # Safety
  /// This does not range-check the index
  pub unsafe fn get_ptr (&self, idx: usize) -> *mut T {
    self.data.add(idx)
  }

  /// Get a sentinal pointer one address past the end of an UnsafeVec
  /// # Safety
  /// This pointer is for flow control, and is not dereferencable
  pub unsafe fn end_ptr (&self) -> *mut T {
    self.data.add(self.length)
  }

  /// Convert an UnsafeVec reference into a pointer to its data
  pub fn as_ptr (&self) -> *mut T { self.data }


  /// Get an immutable iterator over the values in an UnsafeVec
  pub fn iter (&self) -> Iter<T> {
    let cur_ptr = self.as_ptr();
    Iter {
      cur_ptr: self.as_ptr(),
      end_ptr: if !cur_ptr.is_null() { unsafe { self.end_ptr() } } else { cur_ptr },
      _phantom: PhantomData
    }
  }


  /// Get a mutable iterator over the values in an UnsafeVec
  pub fn iter_mut (&mut self) -> IterMut<T> {
    let cur_ptr = self.as_ptr();
    IterMut {
      cur_ptr: self.as_ptr(),
      end_ptr: if !cur_ptr.is_null() { unsafe { self.end_ptr() } } else { cur_ptr },
      _phantom: PhantomData
    }
  }
}

impl<T> Drop for UnsafeVec<T> {
  fn drop (&mut self) {
    unsafe {
      if needs_drop::<T>() {
        self.drop_range(0, self.length)
      }

      dealloc(self.data as _, Layout::from_size_align_unchecked(size_of::<T>() * self.capacity, align_of::<T>()))
    }
  }
}

/// Immutable iterator over the values in an UnsafeVec
pub struct Iter<'i, T> {
  cur_ptr: *const T,
  end_ptr: *const T,
  _phantom: PhantomData<&'i T>,
}

impl<'i, T> Iterator for Iter<'i, T> {
  type Item = &'i T;

  fn next (&mut self) -> Option<&'i T> { unsafe { 
    if likely(self.cur_ptr < self.end_ptr) {
      let out = Some(&*self.cur_ptr);
      self.cur_ptr = self.cur_ptr.add(1);
      out
    } else {
      None
    }
  } }
}

/// Mutable iterator over the values in an UnsafeVec
pub struct IterMut<'i, T> {
  cur_ptr: *mut T,
  end_ptr: *mut T,
  _phantom: PhantomData<&'i mut T>,
}

impl<'i, T> Iterator for IterMut<'i, T> {
  type Item = &'i mut T;

  fn next (&mut self) -> Option<&'i mut T> { unsafe { 
    if likely(self.cur_ptr < self.end_ptr) {
      let out = Some(&mut *self.cur_ptr);
      self.cur_ptr = self.cur_ptr.add(1);
      out
    } else {
      None
    }
  } }
}

/// An owning iterator over an UnsafeVec
pub struct IntoIter<T> {
  inner: UnsafeVec<T>
}

impl<T> Iterator for IntoIter<T> {
  type Item = T;
  fn next (&mut self) -> Option<T> { unsafe {
    if !self.inner.is_empty() { Some(self.inner.unshift()) }
    else { None }
  } }
}


impl<T> IntoIterator for UnsafeVec<T> {
  type Item = T;
  type IntoIter = IntoIter<T>;

  fn into_iter (self) -> Self::IntoIter {
    IntoIter { inner: self }
  }
}


impl<'i, T> IntoIterator for &'i UnsafeVec<T> {
  type Item = &'i T;
  type IntoIter = Iter<'i, T>;

  fn into_iter (self) -> Self::IntoIter {
    self.iter()
  }
}

impl<'i, T> IntoIterator for &'i mut UnsafeVec<T> {
  type Item = &'i mut T;
  type IntoIter = IterMut<'i, T>;

  fn into_iter (self) -> Self::IntoIter {
    self.iter_mut()
  }
}