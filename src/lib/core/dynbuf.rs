//! Contains an implementation of a heap-allocated, dynamically resized buffer like Vec,
//! that enables easy C FFI by being representable as a raw pointer.

use std::{
  ffi::c_void,
  marker::PhantomData,
  borrow::{ Borrow, BorrowMut },
  alloc::{ alloc, realloc, dealloc, Layout },
  mem::{ self, size_of, align_of },
  ptr,
  slice,
  ops,
  fmt,
  io,
  hash
};

use super::{
  maybe::*,
  outcome::*,
};


/// Holds allocation information for a DynBuf
#[repr(C)]
#[derive(Debug)]
pub struct DynBufInfo {
  /// How many elements are currently stored in a DynBuf
  pub length: usize,
  /// How many elements there is currently room allocated for in a DynBuf
  pub capacity: usize,
  /// The method used to clean up interior elements in a DynBuf
  pub dropper: fn (*mut c_void)
}


/// A heap-allocated, dynamically resized buffer like Vec,
/// but enabling easy C FFI by being representable by a raw pointer
#[repr(transparent)]
pub struct DynBuf<T> (*mut T);


impl<T> DynBuf<T> {
  /// Null ptr initialized DynBuf
  pub const NULL: Self = Self(ptr::null_mut());

  /// The minimum number of items to allocate in a DynBuf when it is being initialized
  pub const MINIMUM_ALLOCATION: usize = 16;


  /// Create a new null-initialized DynBuf with no capacity
  pub const fn new () -> Self { Self(ptr::null_mut()) }

  /// Create a new heap-allocated DynBuf with a given capacity.
  /// Note that if `initial_capacity` is `0` no heap allocation will be created, and instead the internal pointer will be initialized to null.
  /// To force a heap allocation of at least `MINIMUM_ALLOCATION`, use `new`/`default` followed by `initialize`
  pub fn with_capacity (initial_capacity: usize) -> Self {
    let mut slf = Self::new();

    slf.allocate_total(initial_capacity);
    
    slf
  }

  /// Ensure a DynBuf has a heap allocation with room for at least `MINIMUM_ALLOCATION` or `initial_capacity` elements, whichever is larger
  pub fn initialize (&mut self, initial_capacity: usize) {
    self.allocate_total(initial_capacity.min(Self::MINIMUM_ALLOCATION));
  }

  /// Determine if a DynBuf is initialized and has a heap allocation
  pub fn is_init (&self) -> bool {
    !self.0.is_null()
  }


  /// Get a pointer to the DynBufInfo struct holding the allocation information of a DynBuf
  /// # Safety
  /// This is only safe if it is known that the DynBuf has already been allocated and is no longer null
  /// 
  /// Note: Do not read/write to this ptr directly, use `std::ptr::read_unaligned`/`std::ptr::write_unaligned`, or use the methods `read_info`/`write_info`
  pub unsafe fn get_info_ptr_unchecked (&self) -> *mut () {
    (self.0 as *mut DynBufInfo).offset(-1) as *mut _
  }

  /// Get a pointer to the DynBufInfo struct holding the allocation information of a DynBuf
  /// # Safety
  /// Note: Do not read/write to this ptr directly, use `std::ptr::read_unaligned`/`std::ptr::write_unaligned`, or use the methods `read_info`/`write_info`
  pub unsafe fn get_info_ptr (&self) -> Maybe<*mut ()> {
    if self.is_init() {
      Just(self.get_info_ptr_unchecked())
    } else {
      Nothing
    }
  }

  /// Read a DynBuf's DynBufInfo into a local copy, if the DynBuf is initialized.
  /// This is necessary because the DynBufInfo is stored at an unaligned address
  /// # Safety
  /// This is only safe if it is known that the DynBuf has already been allocated and is no longer null
  pub unsafe fn read_info_unchecked (&self) -> DynBufInfo {
    ptr::read_unaligned(self.get_info_ptr_unchecked() as *mut _)
  }

  /// Read a DynBuf's DynBufInfo into a local copy, if the DynBuf is initialized.
  /// This is necessary because the DynBufInfo is stored at an unaligned address
  pub fn read_info (&self) -> Maybe<DynBufInfo> {
    unsafe { self.get_info_ptr().map(|p| ptr::read_unaligned(p as *mut _)) }
  }

  /// Write a DynBuf's DynBufInfo from a local copy, if the DynBuf is initialized.
  /// This is necessary because the DynBufInfo is stored at an unaligned address
  /// # Safety
  /// This is only safe if it is known that the DynBuf has already been allocated and is no longer null
  pub unsafe fn write_info (&self, info: DynBufInfo) {
    ptr::write_unaligned(self.get_info_ptr_unchecked() as *mut _, info)
  }


  /// Ensure enough space to store at least `desired_capacity` elements has been allocated in a DynBuf.
  /// Note that if `desired_capacity` is `0` no heap allocation will be created
  pub fn allocate_total (&mut self, desired_capacity: usize) {
    if desired_capacity == 0 { return }

    let mem: *mut DynBufInfo;
    let length;
    let mut capacity;

    if !self.is_init() {
      length = 0;
      capacity = Self::MINIMUM_ALLOCATION;
      while capacity < desired_capacity { capacity *= 2; }

      let layout = Layout::from_size_align(size_of::<T>() * capacity + size_of::<DynBufInfo>(), align_of::<T>()).unwrap();

      mem = unsafe { alloc(layout) as *mut _ };
    } else {
      let info = unsafe { self.read_info_unchecked() };

      length = info.length;
      capacity = info.capacity;
      if desired_capacity <= capacity { return }

      let layout = Layout::from_size_align(size_of::<T>() * capacity + size_of::<DynBufInfo>(), align_of::<T>()).unwrap();
      
      while capacity < desired_capacity { capacity *= 2; }

      mem = unsafe { realloc(self.get_info_ptr_unchecked() as *mut _, layout, size_of::<T>() * capacity + size_of::<DynBufInfo>()) as *mut _ };
    }

    unsafe {
      self.0 = mem.offset(1) as *mut _;

      self.write_info(DynBufInfo {
        length,
        capacity,
        dropper: |buf: *mut c_void| { Self(buf as _); }
      });
    }
  }

  /// Ensure enough space to store at least `additional_capacity` *more* elements has been allocated in a DynBuf.
  /// Note that if `additional_capacity` is `0` this does nothing, including not creating a minimum-sized heap allocation for an uninitialized DynBuf
  pub fn allocate_additional (&mut self, additional_capacity: usize) {
    self.allocate_total(self.len() + additional_capacity);
  }


  /// Get the last valid index in a DynBuf
  /// # Safety
  /// This is only safe if it is known that the DynBuf has already been allocated and is no longer null, and has at least one element
  pub unsafe fn last_index_unchecked (&self) -> usize {
    self.read_info_unchecked().length - 1
  }

  /// Get the last valid index in a DynBuf
  pub fn last_index (&self) -> Maybe<usize> {
    self.read_info().and_then(|i| if i.length > 0 { Just(i.length-1) } else { Nothing })
  }


  /// Get the number of elements currently contained in a DynBuf
  pub fn len (&self) -> usize {
    self.read_info().map(|i| i.length).unwrap_or(0)
  }

  /// Determine if a DynBuf has any elements
  pub fn is_empty (&self) -> bool {
    self.len() == 0
  }

  /// Get the number of elements a DynBuf has currently allocated space for
  pub fn capacity (&self) -> usize {
    self.read_info().map(|i| i.capacity).unwrap_or(0)
  }


  /// Get a sentinal pointer one element past the last element in a DynBuf
  /// # Safety
  /// This is only safe if it is known that the DynBuf has already been allocated and is no longer null
  pub unsafe fn end_ptr_unchecked (&self) -> *mut T {
    self.0.wrapping_add(self.len()) // need wrapping add here because `add` is (supposedly) ub on null ptr
  }

  /// Get a sentinal pointer one element past the last element in a DynBuf
  pub fn end_ptr (&self) -> Maybe<*mut T> {
    self.read_info().map(|i| unsafe { self.0.add(i.length) })
  }


  /// Get a pointer to the last element in a DynBuf
  /// # Safety
  /// This is only safe if it is known that the DynBuf has already been allocated and contains a value
  pub unsafe fn last_ptr_unchecked (&self) -> *mut T {
    self.0.add(self.read_info_unchecked().length - 1)
  }

  /// Get a pointer to the last element in a DynBuf
  pub fn last_ptr (&self) -> Maybe<*mut T> {
    self.read_info().and_then(|i| if i.length > 0 { Just(unsafe { self.0.add(i.length - 1) }) } else { Nothing })
  }

  /// Get an immutable reference to the last element in a DynBuf
  /// # Safety
  /// This is only safe if it is known that the DynBuf has already been allocated and contains a value
  pub unsafe fn last_unchecked (&self) -> &T {
    &*self.last_ptr_unchecked()
  }

  /// Get an immutable reference to the last element in a DynBuf
  /// # Safety
  /// This is only safe if it is known that the DynBuf has already been allocated and contains a value
  pub fn last (&self) -> Maybe<&T> {
    self.last_ptr().map(|p| unsafe { &*p })
  }

  /// Get a mutable reference to the last element in a DynBuf
  /// # Safety
  /// This is only safe if it is known that the DynBuf has already been allocated and contains a value
  pub unsafe fn last_unchecked_mut (&mut self) -> &mut T {
    &mut *self.last_ptr_unchecked()
  }

  /// Get a mutable reference to the last element in a DynBuf
  /// # Safety
  /// This is only safe if it is known that the DynBuf has already been allocated and contains a value
  pub fn last_mut (&mut self) -> Maybe<&mut T> {
    self.last_ptr().map(|p| unsafe { &mut *p })
  }

  
  /// Get a pointer to the first element in a DynBuf
  /// # Safety
  /// This is only safe if it is known that the DynBuf has already been allocated and contains a value
  pub unsafe fn first_ptr_unchecked (&self) -> *mut T {
    self.0
  }
  
  /// Get a pointer to the first element in a DynBuf
  pub fn first_ptr (&self) -> Maybe<*mut T> {
    self.read_info().and_then(|i| if i.length > 0 { Just(self.0) } else { Nothing })
  }
  
  /// Get an immutable reference to the first element in a DynBuf
  /// # Safety
  /// This is only safe if it is known that the DynBuf has already been allocated and contains a value
  pub unsafe fn first_unchecked (&self) -> &T {
    &*self.first_ptr_unchecked()
  }
  
  /// Get an immutable reference to the first element in a DynBuf
  pub fn first (&self) -> Maybe<&T> {
    self.first_ptr().map(|p| unsafe { &*p })
  }
  
  /// Get a mutable reference to the first element in a DynBuf
  /// # Safety
  /// This is only safe if it is known that the DynBuf has already been allocated and contains a value
  pub unsafe fn first_unchecked_mut (&mut self) -> &mut T {
    &mut *self.first_ptr_unchecked()
  }

  /// Get a mutable reference to the first element in a DynBuf
  pub fn first_mut (&mut self) -> Maybe<&mut T> {
    self.first_ptr().map(|p| unsafe { &mut *p })
  }


  /// Get a pointer to the element in a DynBuf at the given index
  /// # Safety
  /// This is only safe if it is known that the DynBuf has already been allocated, and that the given index is in range
  pub unsafe fn get_ptr_unchecked (&self, idx: usize) -> *mut T {
    self.0.add(idx)
  }
  
  /// Get a pointer to the element in a DynBuf at the given index
  pub fn get_ptr (&self, idx: usize) -> Maybe<*mut T> {
    self.read_info().and_then(|i| if i.length > idx { Just(unsafe { self.0.add(idx) }) } else { Nothing })
  }

  /// Get an immutable reference to the element in a DynBuf at the given index
  /// # Safety
  /// This is only safe if it is known that the DynBuf has already been allocated, and that the given index is in range
  pub unsafe fn get_unchecked (&self, idx: usize) -> &T {
    &*self.get_ptr_unchecked(idx)
  }

  /// Get a mutable reference to the element in a DynBuf at the given index
  /// # Safety
  /// This is only safe if it is known that the DynBuf has already been allocated, and that the given index is in range
  pub unsafe fn get_unchecked_mut (&mut self, idx: usize) -> &mut T {
    &mut *self.get_ptr_unchecked(idx)
  }

  /// Get an immutable reference to the element in a DynBuf at the given index
  pub fn get (&self, idx: usize) -> Maybe<&T> {
    self.get_ptr(idx).map(|p| unsafe { &*p })
  }

  /// Get a mutable reference to the element in a DynBuf at the given index
  pub fn get_mut (&mut self, idx: usize) -> Maybe<&mut T> {
    self.get_ptr(idx).map(|p| unsafe { &mut *p })
  }


  /// Insert multiple elements at the given index a DynBuf, moving any existing elements from the index forward
  /// # Safety
  /// Values will be copied as raw data, disregarding any Copy/Clone restrictions,
  /// this is only safe if the values in the input buffer are not dropped or are POD
  pub unsafe fn insert_n (&mut self, idx: usize, values: *const T, n: usize) -> *mut T {
    if n == 0 { return ptr::null_mut() }

    self.allocate_additional(n);

    let mut i = self.read_info_unchecked();

    let p;
    if idx < i.length {
      p = self.get_ptr_unchecked(idx);
      ptr::copy(p, p.add(n), i.length - idx);
    } else {
      p = self.end_ptr_unchecked();
    }
    
    if !values.is_null() { ptr::copy(values, p, n) }

    i.length += n;

    self.write_info(i);

    p
  }

  /// Insert an element at the given index in a DynBuf, moving any existing elements from the index forward
  pub fn insert (&mut self, idx: usize, value: T) {
    unsafe {
      self.insert_n(idx, &value, 1);
      mem::forget(value);
    }
  }


  /// Add multiple elements to the beginning of a DynBuf
  /// # Safety
  /// Values will be copied as raw data, disregarding any Copy/Clone restrictions,
  /// this is only safe if the values in the input buffer are not dropped or are POD
  pub unsafe fn shift_n (&mut self, values: *const T, n: usize) -> *mut T {
    self.insert_n(0, values, n)
  }

  /// Add an element to the beginning of a DynBuf
  pub fn shift (&mut self, value: T) {
    self.insert(0, value)
  }


  /// Add multiple elements to the end of a DynBuf
  /// # Safety
  /// Values will be copied as raw data, disregarding any Copy/Clone restrictions,
  /// this is only safe if the values in the input buffer are not dropped or are POD
  pub unsafe fn push_n (&mut self, values: *const T, n: usize) -> *mut T {
    self.insert_n(self.len(), values, n)
  }

  /// Add an element to the end of a DynBuf
  pub fn push (&mut self, value: T) {
    self.insert(self.len(), value)
  }


  /// Swap the values at the given index in a DynBuf for new ones, if it they exist.
  /// Returns true if the index and length are in range, and the swap took place
  /// # Panics
  /// + The `values` ptr must not overlap the DynBuf's internal memory at the `idx` -> `idx + n` range.
  /// + The `values` ptr must not be null
  /// # Safety
  /// Values will be swapped as raw data, disregarding any Copy/Clone restrictions,
  /// this is only safe if the values in the in/out buffer are manually dropped or are POD
  #[track_caller]
  pub unsafe fn replace_n (&mut self, idx: usize, values: *mut T, n: usize) -> bool {
    if n == 0 { return true }

    if let Just(i) = self.read_info() {
      if idx + n <= i.length {
        let p = self.get_ptr_unchecked(idx);

        assert!(!values.is_null(), "DynBuff::replace_n call cannot swapp with null");
        
        assert!(
          (values as usize) + (n as usize) <= (p as usize) && (values as usize) >= (p as usize) + (n as usize),
          "DynBuff::replace_n call cannot swap overlapping memory"
        );

        ptr::swap_nonoverlapping(p, values, n);

        return true
      }
    }

    false
  }

  /// Swap the value at the given index in a DynBuf for a new one, if it exists.
  /// If the index is not in range, this returns the `new_value` provided instead
  pub fn replace (&mut self, idx: usize, mut value: T) -> T {
    unsafe { self.replace_n(idx, &mut value, 1); }
    value
  }


  /// Drop all items from idx to idx + n, if they implement drop
  /// # Safety
  /// + The caller must ensure that the given range is valid
  /// + The caller must rearrange the DynBuf to prevent use-after-free
  pub unsafe fn drop_range (&mut self, idx: usize, n: usize) {
    if mem::needs_drop::<T>() {
      for i in idx..(idx + n) {
        ptr::drop_in_place(self.0.add(i));
      }
    }
  }


  /// Remove multiple elements from the end of a DynBuf.
  /// Returns true if there were at least `n` elements at the given index in the DynBuf, and the removal took place.
  /// This moves all values after the given index back `n` elements to close the gap.
  /// See `swap_remove_n` for a more efficient Maybe, at the cost of not retaining order
  /// # Safety
  /// If `values` is not null, elements will be copied as raw data, disregarding any Copy/Clone restrictions,
  /// this is only safe if the values in the output buffer are manually dropped or are POD
  pub unsafe fn remove_n (&mut self, idx: usize, values: *mut T, n: usize) -> bool {
    if n == 0 { return true }

    if let Just(mut i) = self.read_info() {
      if idx + n <= i.length {
        let p = self.get_ptr_unchecked(idx);

        if !values.is_null() { ptr::copy(p, values, n) }
        else { self.drop_range(idx, n) }

        if idx + n < i.length {
          ptr::copy(p.add(n), p, i.length - (idx + n))
        }

        i.length -= n;

        self.write_info(i);

        return true
      }
    }

    false
  }

  /// Remove an element at the given index in a DynBuf, if it exists.
  /// This moves all values after the given index back one element to close the gap.
  /// See `swap_remove` for a more efficient Maybe, at the cost of not retaining order
  pub fn remove (&mut self, idx: usize) -> Maybe<T> {
    let mut out = std::mem::MaybeUninit::uninit();

    unsafe { 
      if self.remove_n(idx, out.as_mut_ptr(), 1) {
        Just(out.assume_init())
      } else {
        Nothing
      }
    } 
  }


  /// Remove multiple elements from the end of a DynBuf.
  /// Returns true if there were at least `n` elements in the DynBuf, and the removal took place
  /// # Safety
  /// If `values` is not null, elements will be copied as raw data, disregarding any Copy/Clone restrictions,
  /// this is only safe if the values in the output buffer are manually dropped or are POD
  pub unsafe fn pop_n (&mut self, values: *mut T, n: usize) -> bool {
    let len = self.len();

    if len >= n {
      self.remove_n(len - n, values, n)
    } else {
      false
    }
  }

  /// Remove an element from the end of a DynBuf
  pub fn pop (&mut self) -> Maybe<T> {
    let mut out = std::mem::MaybeUninit::uninit();

    unsafe { 
      if self.pop_n(out.as_mut_ptr(), 1) {
        Just(out.assume_init())
      } else {
        Nothing
      }
    }
  }


  /// Remove an element from the beginning of a DynBuf
  /// Returns true if there were at least `n` elements in the DynBuf, and the removal took place
  /// # Safety
  /// Values will be copied as raw data, disregarding any Copy/Clone restrictions,
  /// this is only safe if the values in the output buffer are manually dropped or are POD
  pub unsafe fn unshift_n (&mut self, values: *mut T, n: usize) -> bool {
    self.remove_n(0, values, n)
  }

  /// Remove an element from the beginning of a DynBuf
  pub fn unshift (&mut self) -> Maybe<T> {
    self.remove(0)
  }


  /// Remove multiple elements from the end of a DynBuf.
  /// Returns true if there were at least `n` elements at the given index in the DynBuf, and the removal took place.
  /// This moves values from the end of the DynBuf to close the gap.
  /// See `remove_n` for retaining the order of items, at a decreased efficiency
  /// # Safety
  /// If `values` is not null, elements will be copied as raw data, disregarding any Copy/Clone restrictions,
  /// this is only safe if the values in the output buffer are manually dropped or are POD
  pub unsafe fn swap_remove_n (&mut self, idx: usize, values: *mut T, n: usize) -> bool {
    if n == 0 { return true }

    if let Just(mut i) = self.read_info() {
      if idx + n <= i.length {
        let p = self.get_ptr_unchecked(idx);

        if !values.is_null() { ptr::copy(p, values, n) }
        else { self.drop_range(idx, n) }

        if idx + n < i.length {
          ptr::copy_nonoverlapping(self.end_ptr_unchecked().offset(-(n as isize)), p, n);
        }

        i.length -= n;

        self.write_info(i);

        return true;
      }
    }
    
    false
  }

  /// Remove an element at the given index in a DynBuf, if it exists.
  /// This moves a value from the end of the DynBuf to close the gap.
  /// See `remove` for retaining the order of items, at a decreased efficiency
  pub fn swap_remove (&mut self, idx: usize) -> Maybe<T> {
    let mut out = std::mem::MaybeUninit::uninit();

    unsafe { 
      if self.swap_remove_n(idx, out.as_mut_ptr(), 1) {
        Just(out.assume_init())
      } else {
        Nothing
      }
    } 
  }


  /// Append another DynBuf to the end of a DynBuf
  pub fn append (&mut self, other: Self) {
    if let Just(mut i) = other.read_info() {
      if i.length != 0 { 
        unsafe {
          self.push_n(other.0, i.length);
          i.length = 0;
          other.write_info(i)
        }
      }
    }
  }


  /// Split a DynBuf at the given index, yielding a new DynBuf containing all elements after and including the index
  pub fn split (&mut self, idx: usize) -> Self {
    if let Just(mut i) = self.read_info() {
      if idx < i.length {
        let mov_len = i.length - idx;
        let mut out = Self::with_capacity(mov_len);

        unsafe {
          out.push_n(self.get_ptr_unchecked(idx), mov_len);
          i.length -= mov_len;
          self.write_info(i);
        }

        return out
      }
    }

    Self::new()
  }


  /// Reallocate a DynBuf's internal memory, discarding the DynBufInfo segment and any additional capacity, leaving a plain buf in memory.
  /// 
  /// Returns the buf and the length. The pointer will be null if the DynBuf's internal pointer was null
  /// # Safety
  /// The ptr returned by this function must be freed with `std::alloc::dealloc`, and the caller is responsible for dropping the elements where required
  pub unsafe fn release (self) -> (*mut T, usize) {
    if let Just(i) = self.read_info() {
      let length = i.length;

      if length != 0 {
        let layout = Layout::from_size_align(size_of::<T>() * i.capacity + size_of::<DynBufInfo>(), align_of::<T>()).unwrap();
        let mut new_base = self.get_info_ptr_unchecked() as _;
        
        ptr::copy(self.0, new_base, length);

        new_base = realloc(new_base as _, layout, length) as _;

        mem::forget(self);

        return (new_base, length)
      }
    }

    (ptr::null_mut(), 0)
  }
  

  /// Drop any items in the DynBuf if they implement Drop, and set the length to 0
  pub fn clear (&mut self) {
    if let Just(mut i) = self.read_info() {
      unsafe {
        self.drop_range(0, i.length);

        i.length = 0;

        self.write_info(i);
      };
    }
  }

  /// Set the length of a DynBuf to any value
  /// # Safety
  /// + The DynBuf must be initialized
  /// + The new length must be <= the capacity
  /// + If the new length is less than the old length, it is up to the caller to Drop the discarded values.
  pub unsafe fn set_len (&mut self, length: usize) {
    let mut info = self.read_info_unchecked();
    info.length = length;
    self.write_info(info)
  }

  /// Increase the length of a DynBuf {
  /// # Safety
  /// + The DynBuf must be initialized
  /// + The new length must be <= the capacity
  pub unsafe fn increment_len (&mut self, add: usize) {
    let mut info = self.read_info_unchecked();
    info.length += add;
    self.write_info(info);
  }


  /// Clean up the heap allocation of a DynBuf if it has one.
  /// Drops any items in the DynBuf if they implement Drop
  pub fn destroy (&mut self) {
    if let Just(i) = self.read_info() {
      unsafe { self.drop_range(0, i.length) };

      let layout = Layout::from_size_align(size_of::<T>() * i.capacity + size_of::<DynBufInfo>(), align_of::<T>()).unwrap();

      unsafe { dealloc(self.get_info_ptr_unchecked() as *mut _, layout) };

      self.0 = ptr::null_mut();
    }
  }


  /// Get an immutable slice over the items in a DynBuf
  /// # Safety
  /// This is only safe if it is known that the DynBuf has already been allocated and is no longer null
  pub unsafe fn as_slice_unchecked (&self) -> &[T] {
    slice::from_raw_parts(self.0, self.read_info_unchecked().length)
  }

  /// Get an immutable slice over the items in a DynBuf.
  /// Note that this requires that the DynBuf has already been allocated, though it does not require it to have any elements.
  /// This requirement is due to the fact that slice requires its base pointer to be non-null
  pub fn as_slice_maybe (&self) -> Maybe<&[T]> {
    self.read_info().map(|i| unsafe { slice::from_raw_parts(self.0, i.length) })
  }

  /// Get an immutable slice over the items in a DynBuf.
  /// Note that due to the fact that slice requires its base pointer to be non-null,
  /// this will create an allocation for the DynBuf if one does not already exist
  pub fn as_slice (&mut self) -> &[T] {
    self.allocate_total(1);
    unsafe { self.as_slice_unchecked() }
  }


  /// Get a mutable slice over the items in a DynBuf
  /// # Safety
  /// This is only safe if it is known that the DynBuf has already been allocated and is no longer null
  pub unsafe fn as_slice_unchecked_mut (&mut self) -> &mut [T] {
    slice::from_raw_parts_mut(self.0, self.read_info_unchecked().length)
  }

  /// Get a mutable slice over the items in a DynBuf.
  /// Note that this requires that the DynBuf has already been allocated, though it does not require it to have any elements.
  /// This requirement is due to the fact that slice requires its base pointer to be non-null
  pub fn as_slice_maybe_mut (&mut self) -> Maybe<&mut [T]> {
    self.read_info().map(|i| unsafe { slice::from_raw_parts_mut(self.0, i.length) })
  }

  /// Get a mutable slice over the items in a DynBuf.
  /// Note that due to the fact that slice requires its base pointer to be non-null,
  /// this will create an allocation for the DynBuf if one does not already exist
  pub fn as_slice_mut (&mut self) -> &mut [T] {
    self.allocate_total(1);
    unsafe { self.as_slice_unchecked_mut() }
  }


  /// Get an immutable Iterator over the items in a DynBuf
  pub fn iter (&self) -> Iter<T> {
    Iter {
      ptr: unsafe { self.first_ptr_unchecked() },
      end: unsafe { self.end_ptr_unchecked() },
      phantom: PhantomData
    }
  }


  /// Get a mutable Iterator over the items in a DynBuf
  pub fn iter_mut (&mut self) -> IterMut<T> {
    IterMut {
      ptr: unsafe { self.first_ptr_unchecked() },
      end: unsafe { self.end_ptr_unchecked() },
      phantom: PhantomData
    }
  }


  /// Unwrap the pointer backing a DynBuf
  /// # Safety
  /// The user is responsible for memory management from this point,
  /// unless the pointer is re-wrapped via `from_ptr`
  pub unsafe fn into_ptr (self) -> *mut T {
    self.0
  }

  /// Wrap a backing pointer for a DynBuf
  /// # Safety
  /// This is only safe to do if `p - size_of::<DynBufInfo>()` contains a valid `DynBufInfo` with information about the allocation,
  /// and the allocation was created with `std::alloc::alloc`, or if `p` is null
  pub unsafe fn from_ptr (p: *mut T) -> Self {
    Self(p)
  }


  /// Fill a given number of bytes in a DynStr with the result of calling a closure for each
  pub fn fill_with_indexed_escapable<F: FnMut (usize) -> Maybe<T>> (&mut self, count: usize, mut f: F) {
    self.allocate_additional(count);

    let mut values_filled = 0;
    while values_filled < count { 
      if let Just(ch) = f(values_filled) { self.push(ch); values_filled += 1 }
      else { break }
    }
  }

  /// Fill a given number of values in a DynBuf with the result of calling a closure for each
  pub fn fill_with_indexed<F: FnMut (usize) -> T> (&mut self, count: usize, mut f: F) {
    self.fill_with_indexed_escapable(count, |i| Just(f(i)))
  }

  /// Fill a given number of values in a DynBuf with the result of calling a closure for each
  pub fn fill_with_escapable<F: FnMut () -> Maybe<T>> (&mut self, count: usize, mut f: F) {
    self.fill_with_indexed_escapable(count, |_| f())
  }

  /// Fill a given number of values in a DynBuf with the result of calling a closure for each
  pub fn fill_with<F: FnMut () -> T> (&mut self, count: usize, mut f: F) {
    self.fill_with_indexed(count, |_| f())
  }

  /// Fill a given number of values in a DynBuf with clones of a given value
  pub fn fill (&mut self, count: usize, value: &T)
  where T: Clone
  {
    self.fill_with(count, || value.clone())
  }

  /// Fill a given number of values in a DynBuf with Default::default()
  pub fn fill_default (&mut self, count: usize)
  where T: Default
  {
    self.fill_with(count, Default::default)
  }


  /// Create a new DynBuf and fill a given number of values in it with the result of calling a closure for each
  pub fn filled_with_indexed_escapable<F: FnMut (usize) -> Maybe<T>> (count: usize, f: F) -> Self {
    let mut out = Self::default();
    out.fill_with_indexed_escapable(count, f);
    out
  }

  /// Create a new DynBuf and fill a given number of values in it with the result of calling a closure for each
  pub fn filled_with_indexed<F: FnMut (usize) -> T> (count: usize, f: F) -> Self {
    let mut out = Self::default();
    out.fill_with_indexed(count, f);
    out
  }

  /// Create a new DynBuf and fill a given number of values in it with the result of calling a closure for each
  pub fn filled_with_escapable<F: FnMut () -> Maybe<T>> (count: usize, f: F) -> Self {
    let mut out = Self::default();
    out.fill_with_escapable(count, f);
    out
  }

  /// Create a new DynBuf and fill a given number of values in it with the result of calling a closure for each
  pub fn filled_with<F: FnMut () -> T> (count: usize, f: F) -> Self {
    let mut out = Self::default();
    out.fill_with(count, f);
    out
  }

  /// Fill a given number of values in a DynBuf with clones of a given value
  pub fn filled (count: usize, value: &T) -> Self
  where T: Clone
  {
    let mut out = Self::default();
    out.fill_with(count, || value.clone());
    out
  }

  /// Fill a given number of values in a DynBuf with Default::default()
  pub fn filled_default (count: usize) -> Self
  where T: Default
  {
    let mut out = Self::default();
    out.fill_with(count, Default::default);
    out
  }
}

impl DynBuf<u8> {
  /// Read all of the bytes a Reader will provide into an existing DynBuf<u8>
  pub fn read_all_into (&mut self, segment_size: usize, r: &mut dyn io::Read) -> Outcome<usize, io::Error> {
    let start_len = self.len();

    loop {
      self.allocate_additional(segment_size);
      
      match r.read(unsafe { slice::from_raw_parts_mut(self.end_ptr_unchecked(), segment_size) }) {
        Ok(n) => if n == 0 {
          return Succ(self.len() - start_len)
        } else {
          unsafe { self.increment_len(n) }
        },

        Err(e) => if e.kind() == io::ErrorKind::Interrupted {
          continue
        } else {
          return Fail(e)
        }
      }
    }
  }

  /// Read all of the bytes a Reader will provide into a new DynBuf<u8>
  pub fn read_all (initial_capacity: usize, segment_size: usize, r: &mut dyn io::Read) -> Outcome<Self, io::Error> {
    let mut out = DynBuf::with_capacity(initial_capacity);

    out.read_all_into(segment_size, r).map(|_| out)
  }
}


impl<T> Default for DynBuf<T> { fn default () -> Self { Self::new() } }


// TODO it should be possible to avoid creating a new allocation when converting to/from Vec but I've no idea how to do it

impl<T> From<Vec<T>> for DynBuf<T> {
  fn from (mut v: Vec<T>) -> DynBuf<T> {
    let len = v.len();

    let mut b = DynBuf::with_capacity(len);

    unsafe {
      ptr::copy_nonoverlapping(v.as_ptr(), b.first_ptr_unchecked(), len);
      
      v.set_len(0);
      b.set_len(len);
    }

    b
  }
}

impl<T> From<DynBuf<T>> for Vec<T> {
  fn from (mut b: DynBuf<T>) -> Vec<T> {
    if !b.is_init() { return Vec::default() }

    let len = b.len();

    let mut v = Vec::with_capacity(len);

    unsafe {
      ptr::copy_nonoverlapping(b.first_ptr_unchecked(), v.as_mut_ptr(), len);

      b.set_len(0);
      v.set_len(len);
    }

    v
  }
}


impl<T> From<&[T]> for DynBuf<T>
where T: Clone
{
  fn from (slice: &[T]) -> Self {
    let mut buf = Self::with_capacity(slice.len());

    for el in slice { buf.push(el.clone()) }

    buf
  }
}


impl<T> fmt::Debug for DynBuf<T>
where T: fmt::Debug
{
  fn fmt (&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "DynBuf({}/{}) ", self.len(), self.capacity())?;
    if self.is_init() {
      f.debug_list().entries(self.iter()).finish()
    } else {
      write!(f, "<null>")
    }
  }
}


impl<T> Clone for DynBuf<T>
where T: Clone
{
  fn clone (&self) -> Self {
    if let Just(i) = self.read_info() {
      let mut new = Self::with_capacity(i.capacity);

      for idx in 0..i.length {
        new.push(unsafe { self.get_unchecked(idx) }.clone());
      }

      new
    } else {
      Self::new()
    }
  }
}


impl<T> Drop for DynBuf<T> {
  #[inline] fn drop (&mut self) { self.destroy() }
}


impl<T> AsRef<[T]> for DynBuf<T> {
  #[track_caller]
  fn as_ref (&self) -> &[T] {
    self.as_slice_maybe().expect("DynBuf must be initialized before using implicit conversion to slice")
  }
}

impl<T> AsMut<[T]> for DynBuf<T> {
  #[track_caller]
  fn as_mut (&mut self) -> &mut [T] {
    self.as_slice_maybe_mut().expect("DynBuf must be initialized before using implicit conversion to slice")
  }
}


impl<T> Borrow<[T]> for DynBuf<T> {
  #[track_caller]
  fn borrow (&self) -> &[T] {
    self.as_slice_maybe().expect("DynBuf must be initialized before using implicit conversion to slice")
  }
}

impl<T> BorrowMut<[T]> for DynBuf<T> {
  #[track_caller]
  fn borrow_mut (&mut self) -> &mut [T] {
    self.as_slice_maybe_mut().expect("DynBuf must be initialized before using implicit conversion to slice")
  }
}


impl<T> ops::Deref for DynBuf<T> {
  type Target = [T];

  #[track_caller]
  fn deref (&self) -> &Self::Target {
    self.as_slice_maybe().expect("DynBuf must be initialized before using implicit conversion to slice")
  }
}

impl<T> ops::DerefMut for DynBuf<T> {
  #[track_caller]
  fn deref_mut (&mut self) -> &mut Self::Target {
    self.as_slice_maybe_mut().expect("DynBuf must be initialized before using implicit conversion to slice")
  }
}


impl<T> PartialEq<DynBuf<T>> for DynBuf<T>
where T: PartialEq<T>
{
  fn eq (&self, other: &Self) -> bool {
    self.0 == other.0 || self.as_slice_maybe() == other.as_slice_maybe()
  }
}

impl<T> PartialEq<[T]> for DynBuf<T>
where T: PartialEq<T>
{
  fn eq (&self, other: &[T]) -> bool {
    self.as_slice_maybe() == Just(other)
  }
}

impl<T> PartialEq<DynBuf<T>> for [T]
where T: PartialEq<T>
{
  #[inline] fn eq (&self, other: &DynBuf<T>) -> bool { other == self }
}

impl<T> Eq for DynBuf<T>
where T: Eq { }


impl<T> hash::Hash for DynBuf<T>
where T: hash::Hash
{
  fn hash<H: hash::Hasher> (&self, hasher: &mut H) {
    let len = self.len();

    len.hash(hasher);

    if len > 0 {
      for i in 0..len {
        unsafe { self.get_unchecked(i) }.hash(hasher)
      }
    }
  }
}

impl<T> PartialOrd<DynBuf<T>> for DynBuf<T>
where T: PartialOrd<T>
{
  fn partial_cmp (&self, other: &Self) -> Option<std::cmp::Ordering> {
    self.as_slice_maybe().partial_cmp(&other.as_slice_maybe())
  }
}

impl<T> PartialOrd<[T]> for DynBuf<T>
where T: PartialOrd<T>
{
  fn partial_cmp (&self, other: &[T]) -> Option<std::cmp::Ordering> {
    self.as_slice_maybe().partial_cmp(&Just(other))
  }
}

impl<T> PartialOrd<DynBuf<T>> for [T]
where T: PartialOrd<T>
{
  fn partial_cmp (&self, other: &DynBuf<T>) -> Option<std::cmp::Ordering> {
    Just(self).partial_cmp(&other.as_slice_maybe())
  }
}


impl<T> Ord for DynBuf<T>
where T: Ord
{
  fn cmp (&self, other: &Self) -> std::cmp::Ordering {
    self.as_slice_maybe().cmp(&other.as_slice_maybe())
  }
}




/// The iterator created by DynBuf for immutably iterating its values
pub struct Iter<'a, T: 'a> {
  ptr: *const T,
  end: *const T,
  phantom: PhantomData<&'a T>
}

impl<'a, T: 'a> Iterator for Iter<'a, T> {
  type Item = &'a T;

  fn next (&mut self) -> Option<&'a T> {
    if self.ptr < self.end {
      unsafe {
        let out = Some(&*self.ptr);
        self.ptr = self.ptr.add(1);
        out
      }
    } else {
      None
    }
  }
}

/// The iterator created by DynBuf for mutably iterating its values
pub struct IterMut<'a, T: 'a> {
  ptr: *mut T,
  end: *mut T,
  phantom: PhantomData<&'a mut T>
}

impl<'a, T: 'a> Iterator for IterMut<'a, T> {
  type Item = &'a mut T;

  fn next (&mut self) -> Option<&'a mut T> {
    if self.ptr < self.end {
      unsafe {
        let out = Some(&mut *self.ptr);
        self.ptr = self.ptr.add(1);
        out
      }
    } else {
      None
    }
  }
}


/// The Iterator used when DynBuf is turned into a consuming Iterator via IntoIterator
pub struct IntoIter<T> (DynBuf<T>);

impl<T> Iterator for IntoIter<T> {
  type Item = T;

  fn next (&mut self) -> Option<T> {
    self.0.unshift().into()
  }
}

impl<T> IntoIterator for DynBuf<T> {
  type Item = T;

  type IntoIter = IntoIter<T>;

  fn into_iter (self) -> Self::IntoIter {
    IntoIter::<T> (self)
  }
}

impl<'i, T> IntoIterator for &'i DynBuf<T> {
  type Item = &'i T;

  type IntoIter = Iter<'i, T>;

  fn into_iter (self) -> Self::IntoIter {
    self.iter()
  }
}

impl<'i, T> IntoIterator for &'i mut DynBuf<T> {
  type Item = &'i mut T;

  type IntoIter = IterMut<'i, T>;

  fn into_iter (self) -> Self::IntoIter {
    self.iter_mut()
  }
}


impl<T> std::iter::FromIterator<T> for DynBuf<T> {
  fn from_iter<I: IntoIterator<Item = T>> (iter: I) -> Self {
    let mut out = Self::new();

    for it in iter {
      out.push(it);
    }

    out
  }
}

impl<'a, T: 'a> std::iter::FromIterator<&'a T> for DynBuf<T>
where T: Clone
{
  fn from_iter<I: IntoIterator<Item = &'a T>> (iter: I) -> Self {
    let mut out = Self::new();

    for it in iter {
      out.push(it.clone());
    }

    out
  }
}


impl fmt::Write for DynBuf<char> {
  #[inline]
  fn write_str (&mut self, s: &str) -> fmt::Result {
    for ch in s.chars() {
      self.push(ch);
    }
    Ok(())
  }

  #[inline]
  fn write_char (&mut self, ch: char) -> fmt::Result {
    self.push(ch);
    Ok(())
  }
}

impl fmt::Write for DynBuf<u8> {
  #[inline]
  fn write_str (&mut self, s: &str) -> fmt::Result {
    unsafe { self.push_n(s.as_ptr(), s.len()) };
    Ok(())
  }

  #[inline]
  fn write_char (&mut self, ch: char) -> fmt::Result {
    self.write_str(ch.encode_utf8(&mut [0; 4]))
  }
}


impl io::Write for DynBuf<u8> {
  #[inline]
  fn write (&mut self, bytes: &[u8]) -> io::Result<usize> {
    let len = bytes.len();
    unsafe { self.push_n(bytes.as_ptr(), len) };
    Ok(len)
  }

  #[inline] fn flush (&mut self) -> io::Result<()> { Ok(()) }
}


impl io::Read for DynBuf<u8> {
  #[inline]
  fn read (&mut self, bytes: &mut [u8]) -> io::Result<usize> {
    let len = self.len().min(bytes.len());
    unsafe { self.unshift_n(bytes.as_mut_ptr(), len) };
    Ok(len)
  }
}

/// A macro like `vec!` for creating DynBuf's initialized with values
#[macro_export]
macro_rules! dynbuf {
  ($($values:expr),+ $(,)?) => {
    {
      let mut dbuf = $crate::core::DynBuf::default();

      {
        $(dbuf.push($values));+
      }

      dbuf
    }
  };

  () => { $crate::core::DynBuf::default() }
}