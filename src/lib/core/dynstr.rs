//! Contains an implementation of a heap-allocated, dynamically resized buffer like String,
//! that enables easy C FFI by being representable as a raw pointer.

use std::{
  path::Path,
  borrow::{ Borrow, BorrowMut },
  iter::FromIterator,
  ops,
  fmt,
  str,
  mem,
  ptr,
  io,
  hash
};


pub use super::cstr::Utf8Error;

use super::{
  dynbuf::{ self, DynBuf },
  maybe::*,
  outcome::*,
};


/// The immutable reference iterator type used by DynStr (alias for dynbuf::Iter)
pub type Iter<'a> = dynbuf::Iter<'a, u8>;

/// The mutable reference iterator type used by DynStr (alias for dynbuf::IterMut)
pub type IterMut<'a> = dynbuf::IterMut<'a, u8>;

/// The character iterator used by DynStr (alias for str::Chars)
pub type Chars<'a> = str::Chars<'a>;


/// A heap-allocated, dynamically resized buffer like String,
/// but enabling easy C FFI by being representable by a raw pointer
#[repr(transparent)]
pub struct DynStr (DynBuf<u8>);


impl DynStr {
  /// Null ptr initialized DynStr
  pub const NULL: Self = Self(DynBuf::NULL);

  /// Create a new null-initialized DynStr with no capacity
  #[inline] pub const fn new () -> Self { Self(DynBuf::new()) }

  /// Create a new heap-allocated DynStr with a given capacity.
  /// Note that if `initial_capacity` is `0` no heap allocation will be created, and instead the internal pointer will be initialized to null.
  /// To force a heap allocation of at least `MINIMUM_ALLOCATION`, use `new`/`default` followed by `initialize`
  #[inline] pub fn with_capacity (initial_capacity: usize) -> Self { Self(DynBuf::with_capacity(initial_capacity)) }

  /// Ensure a DynStr has a heap allocation with room for at least `DynBuf::MINIMUM_ALLOCATION` or `initial_capacity` bytes, whichever is larger
  #[inline] pub fn initialize (&mut self, initial_capacity: usize) { self.0.initialize(initial_capacity) }

  /// Determine if a DynStr is initialized and has a heap allocation
  #[inline] pub fn is_init (&self) -> bool { self.0.is_init() }


  /// Ensure enough space to store at least `desired_capacity` bytes has been allocated in a DynStr.
  /// Note that if `desired_capacity` is `0` no heap allocation will be created
  #[inline] pub fn allocate_total (&mut self, desired_capacity: usize) { self.0.allocate_total(desired_capacity) }

  /// Ensure enough space to store at least `additional_capacity` *more* bytes has been allocated in a DynStr.
  /// Note that if `additional_capacity` is `0` this does nothing, including not creating a minimum-sized heap allocation for an uninitialized DynStr
  #[inline] pub fn allocate_additional (&mut self, additional_capacity: usize) { self.0.allocate_additional(additional_capacity) }


  /// Get the number of bytes currently contained in a DynStr
  #[inline] pub fn len (&self) -> usize { self.0.len() }

  /// Determine if a DynBuf has any content
  #[inline] pub fn is_empty (&self) -> bool { self.0.is_empty() }

  /// Get the number of bytes a DynStr has currently allocated space for
  #[inline] pub fn capacity (&self) -> usize { self.0.capacity() }


  /// Get a sentinal pointer one element past the last element in a DynStr
  /// # Safety
  /// This is only safe if it is known that the DynStr has already been allocated and is no longer null
  #[inline] pub unsafe fn end_ptr_unchecked (&self) -> *mut u8 { self.0.end_ptr_unchecked() }

  /// Get a sentinal pointer one element past the last element in a DynStr
  #[inline] pub fn end_ptr (&self) -> Maybe<*mut u8> { self.0.end_ptr() }
  


  /// Insert a str at the given index in a DynStr
  pub fn insert_str (&mut self, byte_idx: usize, s: &str) {
    unsafe { self.0.insert_n(byte_idx, s.as_ptr(), s.len()); }
  }

  /// Insert a char at the given index in a DynStr
  pub fn insert_char (&mut self, byte_idx: usize, ch: char) {
    self.insert_str(byte_idx, ch.encode_utf8(&mut [0; 4]))
  }
  
  /// Add a str to the beginning of a DynStr
  pub fn shift_str (&mut self, s: &str) {
    unsafe { self.0.shift_n(s.as_ptr(), s.len()); }
  }

  /// Add a single char to the beginning of a DynStr
  pub fn shift_char (&mut self, ch: char) {
    self.shift_str(ch.encode_utf8(&mut [0; 4]))
  }

  /// Add a str to the end of a DynStr
  pub fn push_str (&mut self, s: &str) {
    unsafe { self.0.push_n(s.as_ptr(), s.len()); }
  }

  /// Add a single char to the end of a DynStr
  pub fn push_char (&mut self, ch: char) {
    self.push_str(ch.encode_utf8(&mut [0; 4]))
  }


  /// Remove a subsection from the given index in a DynStr.
  /// Returns true if the range was valid and the removal took place
  pub fn remove_bytes (&mut self, byte_idx: usize, byte_count: usize) -> bool {
    if self.is_init() {
      unsafe {
        let s = self.as_str_unchecked();

        if s.is_char_boundary(byte_idx)
        && s.is_char_boundary(byte_idx + byte_count) {
          return self.0.remove_n(byte_idx, ptr::null_mut(), byte_count);
        }
      }
    }

    false
  }

  /// Remove and return a single char from the given index in a DynStr
  pub fn remove_char (&mut self, byte_idx: usize) -> Maybe<char> {
    if self.is_init() {
      unsafe { 
        let s = self.as_str_unchecked();

        if s.is_char_boundary(byte_idx) {
          let ch = s[byte_idx..].chars().next()?;
          self.0.remove_n(byte_idx, ptr::null_mut(), ch.len_utf8());
          return Just(ch)
        }
      }
    }

    Nothing
  }

  /// Remove the given range at the end of a DynStr.
  /// Returns true if the range was valid and the removal took place
  pub fn pop_bytes (&mut self, byte_count: usize) -> bool {
    let len = self.len();

    if len >= byte_count {
      return self.remove_bytes(len - byte_count, byte_count);
    }

    false
  }

  /// Remove and return a single char from the end of a DynStr
  pub fn pop_char (&mut self) -> Maybe<char> {
    if self.is_init() {
      unsafe { 
        let ch = self.as_str_unchecked().chars().rev().next()?;
        self.0.pop_n(ptr::null_mut(), ch.len_utf8());
        return Just(ch)
      }
    }

    Nothing
  }

  /// Remove the given range at the beginning of a DynStr.
  /// Returns true if the range was valid and the removal took place
  pub fn unshift_bytes (&mut self, byte_count: usize) -> bool {
    let len = self.len();

    if len >= byte_count {
      return self.remove_bytes(0, byte_count);
    }

    false
  }

  /// Remove and return a single char from the beginning of a DynStr
  pub fn unshift_char (&mut self) -> Maybe<char> {
    if self.is_init() {
      unsafe { 
        let ch = self.as_str_unchecked().chars().next()?;
        self.0.unshift_n(ptr::null_mut(), ch.len_utf8());
        return Just(ch)
      }
    }

    Nothing
  }


  /// Get the number of invidual codepoints currently contained in a DynStr.
  /// Note this is an expensive operation and requires iterating the entire DynStr
  pub fn cp_count (&self) -> usize {
    self.as_str_maybe().map(|s| s.chars().count()).unwrap_or(0)
  }

  /// Get a byte index from a codepoint index
  /// Note this is an expensive operation and requires iterating the entire DynStr up to `cp_idx`
  pub fn get_byte_idx_from_cp (&self, cp_idx: usize) -> Maybe<usize> {
    let mut byte_idx = 0;

    for (i, ch) in self.as_str_maybe()?.chars().enumerate() {
      byte_idx += ch.len_utf8();
      if i == cp_idx { return Just(byte_idx) }
    }

    Nothing
  }

  /// Read a char from the given codepoint index in a DynStr.
  /// Note this is an expensive operation and requires iterating the entire DynStr up to `cp_idx`
  pub fn get_cp (&self, cp_idx: usize) -> Maybe<char> {
    self.as_str_maybe().and_then(|s| s.chars().nth(cp_idx).into())
  }

  /// Write a char into the given codepoint index in a DynStr.
  /// Returns whether or not the given index was in range and the write took place.
  /// Note this is an expensive operation and requires iterating the entire DynStr up to `cp_idx`
  pub fn set_cp (&mut self, cp_idx: usize, ch: char) -> bool {
    if let Just(byte_idx) = self.get_byte_idx_from_cp(cp_idx) {
      self.insert_char(byte_idx, ch);
      return true
    }

    false
  }


  /// Unwrap a DynStr and yield its inner DynBuf<u8>
  pub fn into_inner (mut self) -> DynBuf<u8> {
    let mut out = DynBuf::default();
    mem::swap(&mut self.0, &mut out);
    out
  }

  /// Create a DynStr by wrapping a DynBuf<u8>.
  /// Returns Just(Self) if the DynBuf contains valid utf8
  pub fn from_inner (buf: DynBuf<u8>) -> Outcome<Self, Utf8Error> {
    if let Just(slice) = buf.as_slice_maybe() {
      match str::from_utf8(slice) {
        Ok(_) => Succ(Self(buf)),
        Err(e) => Fail(e.into())
      }
    } else { // buf is null, which is fine
      Succ(Self(buf))
    }
  }


  /// Reallocate a DynStr's internal memory, discarding the DynBufInfo segment and any additional capacity, leaving a plain buf in memory.
  /// 
  /// Returns the buf and the length. The pointer will be null if the DynStr's internal pointer was null
  /// # Safety
  /// The ptr returned by this function must be freed with `std::alloc::dealloc`
  #[inline] pub unsafe fn release (self) -> (*mut u8, usize) { self.into_inner().release() }


  /// Sets the length of a DynStr to 0
  #[inline] pub fn clear (&mut self) { self.0.clear() }

  /// Clean up the heap allocation of a DynStr if it has one.
  #[inline] pub fn destroy (&mut self) { self.0.destroy() }


  /// Get an immutable slice over the utf bytes in a DynStr
  /// # Safety
  /// This is only safe if it is known that the DynStr has already been allocated and is no longer null
  #[inline] pub unsafe fn as_slice_unchecked (&self) -> &[u8] { self.0.as_slice_unchecked() }

  /// Get an immutable slice over the utf bytes in a DynStr.
  /// Note that this requires that the DynStr has already been allocated, though it does not require it to have any content.
  /// This requirement is due to the fact that slice requires its base pointer to be non-null
  #[inline] pub fn as_slice_maybe (&self) -> Maybe<&[u8]> { self.0.as_slice_maybe() }

  /// Get an immutable slice over the utf bytes in a DynStr.
  /// Note that due to the fact that slice requires its base pointer to be non-null,
  /// this will create an allocation for the DynStr if one does not already exist
  #[inline] pub fn as_slice (&mut self) -> &[u8] { self.0.as_slice() }


  /// Get an immutable str over the utf bytes in a DynStr
  /// # Safety
  /// This is only safe if it is known that the DynStr has already been allocated and is no longer null
  pub unsafe fn as_str_unchecked (&self) -> &str {
    str::from_utf8_unchecked(self.as_slice_unchecked())
  }

  /// Get an immutable str over the utf bytes in a DynStr.
  /// Note that this requires that the DynStr has already been allocated, though it does not require it to have any content.
  /// This requirement is due to the fact that str requires its base pointer to be non-null
  pub fn as_str_maybe (&self) -> Maybe<&str> {
    self.as_slice_maybe().map(|s| unsafe { str::from_utf8_unchecked(s) })
  }

  /// Get an immutable str over the utf bytes in a DynStr.
  /// Note that due to the fact that str requires its base pointer to be non-null,
  /// this will create an allocation for the DynStr if one does not already exist
  pub fn as_str (&mut self) -> &str {
    unsafe { str::from_utf8_unchecked(self.as_slice()) }
  }

  /// Get a mutable slice over the utf bytes in a DynStr
  /// # Safety
  /// This is only safe if it is known that the DynStr has already been allocated and is no longer null
  #[inline] pub unsafe fn as_slice_unchecked_mut (&mut self) -> &mut [u8] { self.0.as_slice_unchecked_mut() }

  /// Get a mutable slice over the utf bytes in a DynStr.
  /// Note that this requires that the DynStr has already been allocated, though it does not require it to have any content.
  /// This requirement is due to the fact that slice requires its base pointer to be non-null
  #[inline] pub fn as_slice_maybe_mut (&mut self) -> Maybe<&mut [u8]> { self.0.as_slice_maybe_mut() }

  /// Get a mutable slice over the utf bytes in a DynStr.
  /// Note that due to the fact that slice requires its base pointer to be non-null,
  /// this will create an allocation for the DynStr if one does not already exist
  #[inline] pub fn as_slice_mut (&mut self) -> &mut [u8] { self.0.as_mut() }


  /// Get an immutable str over the utf bytes in a DynStr
  /// # Safety
  /// This is only safe if it is known that the DynStr has already been allocated and is no longer null
  pub unsafe fn as_str_unchecked_mut (&mut self) -> &mut str {
    str::from_utf8_unchecked_mut(self.as_slice_unchecked_mut())
  }

  /// Get an immutable str over the utf bytes in a DynStr.
  /// Note that this requires that the DynStr has already been allocated, though it does not require it to have any content.
  /// This requirement is due to the fact that str requires its base pointer to be non-null
  pub fn as_str_maybe_mut (&mut self) -> Maybe<&mut str> {
    self.as_slice_maybe_mut().map(|s| unsafe { str::from_utf8_unchecked_mut(s) })
  }

  /// Get an immutable str over the utf bytes in a DynStr.
  /// Note that due to the fact that str requires its base pointer to be non-null,
  /// this will create an allocation for the DynStr if one does not already exist
  pub fn as_str_mut (&mut self) -> &mut str {
    unsafe { str::from_utf8_unchecked_mut(self.as_slice_mut()) }
  }

  
  /// Get an immutable Iterator over the utf bytes in a DynStr
  #[inline] pub fn iter (&mut self) -> Iter { self.0.iter() }

  /// Get a mutable Iterator over the utf bytes in a DynStr
  #[inline] pub fn iter_mut (&mut self) -> IterMut { self.0.iter_mut() }


  /// Unwrap the pointer backing a DynStr
  /// # Safety
  /// The user is responsible for memory management from this point,
  /// unless the pointer is re-wrapped via `from_ptr`
  pub unsafe fn into_ptr (self) -> *mut u8 {
    self.into_inner().into_ptr()
  }

  /// Wrap a backing pointer for a DynStr
  /// # Safety
  /// Either `p` must be null ptr, or
  /// `p - size_of::<DynBufInfo>()` must contain a valid `DynBufInfo` with information about the allocation,
  /// and the allocation must have been created with `std::alloc::alloc`
  pub unsafe fn from_ptr (p: *mut u8) -> Outcome<Self, Utf8Error> {
    Self::from_inner(DynBuf::from_ptr(p))
  }


  /// Fill a given number of bytes in a DynStr with the result of calling a closure for each char.
  /// 
  /// Because chars have variable width encoding when converted to utf8, this may not fill the exact byte count, or it may overfill by up to 3 bytes
  pub fn fill_with_indexed_escapable<F: FnMut (usize) -> Maybe<char>> (&mut self, byte_count: usize, mut f: F) {
    self.allocate_additional(byte_count);

    let mut bytes_filled = 0;
    while bytes_filled < byte_count { 
      if let Just(ch) = f(bytes_filled) { self.push_char(ch); bytes_filled += ch.len_utf8() }
      else { break }
    }
  }

  /// Fill a given number of bytes in a DynStr with the result of calling a closure for each char.
  /// 
  /// Because chars have variable width encoding when converted to utf8, this may not fill the exact byte count, or it may overfill by up to 3 bytes
  pub fn fill_with_indexed<F: FnMut (usize) -> char> (&mut self, byte_count: usize, mut f: F) {
    self.fill_with_indexed_escapable(byte_count, |i| Just(f(i)))
  }

  /// Fill a given number of bytes in a DynStr with the result of calling a closure for each char.
  /// 
  /// Because chars have variable width encoding when converted to utf8, this may not fill the exact byte count, or it may overfill by up to 3 bytes
  pub fn fill_with_escapable<F: FnMut () -> Maybe<char>> (&mut self, byte_count: usize, mut f: F) {
    self.fill_with_indexed_escapable(byte_count, |_| f())
  }

  /// Fill a given number of bytes in a DynStr with the result of calling a closure for each char.
  /// 
  /// Because chars have variable width encoding when converted to utf8, this may not fill the exact byte count, or it may overfill by up to 3 bytes
  pub fn fill_with<F: FnMut () -> char> (&mut self, byte_count: usize, mut f: F) {
    self.fill_with_indexed(byte_count, |_| f())
  }

  /// Fill a given number of bytes in a DynStr with copies of a char.
  /// 
  /// Because chars have variable width encoding when converted to utf8, this may not fill the exact byte count, or it may overfill by up to 3 bytes
  pub fn fill (&mut self, byte_count: usize, ch: char) {
    self.fill_with_indexed(byte_count, |_| ch)
  }

  /// Fill a given number of bytes in a DynStr with the null terminator.
  pub fn fill_null (&mut self, byte_count: usize) {
    self.fill(byte_count, '\0')
  }


  /// Create a new DynStr and fill a given number of bytes in it with the result of calling a closure for each char.
  /// 
  /// Because chars have variable width encoding when converted to utf8, this may not fill the exact byte count, or it may overfill by up to 3 bytes
  pub fn filled_with_indexed_escapable<F: FnMut (usize) -> Maybe<char>> (byte_count: usize, f: F) -> Self {
    let mut out = Self::default();
    out.fill_with_indexed_escapable(byte_count, f);
    out
  }

  /// Create a new DynStr and fill a given number of bytes in it with the result of calling a closure for each char.
  /// 
  /// Because chars have variable width encoding when converted to utf8, this may not fill the exact byte count, or it may overfill by up to 3 bytes
  pub fn filled_with_indexed<F: FnMut (usize) -> char> (byte_count: usize, f: F) -> Self {
    let mut out = Self::default();
    out.fill_with_indexed(byte_count, f);
    out
  }

  /// Create a new DynStr and fill a given number of bytes in it with the result of calling a closure for each char.
  /// 
  /// Because chars have variable width encoding when converted to utf8, this may not fill the exact byte count, or it may overfill by up to 3 bytes
  pub fn filled_with_escapable<F: FnMut () -> Maybe<char>> (byte_count: usize, f: F) -> Self {
    let mut out = Self::default();
    out.fill_with_escapable(byte_count, f);
    out
  }

  /// Create a new DynStr and fill a given number of bytes in it with the result of calling a closure for each char.
  /// 
  /// Because chars have variable width encoding when converted to utf8, this may not fill the exact byte count, or it may overfill by up to 3 bytes
  pub fn filled_with<F: FnMut () -> char> (byte_count: usize, f: F) -> Self {
    let mut out = Self::default();
    out.fill_with(byte_count, f);
    out
  }

  /// Create a new DynStr and fill a given number of bytes in it with copies of a char.
  /// 
  /// Because chars have variable width encoding when converted to utf8, this may not fill the exact byte count, or it may overfill by up to 3 bytes
  pub fn filled (byte_count: usize, ch: char) -> Self {
    let mut out = Self::default();
    out.fill(byte_count, ch);
    out
  }

  /// Create a new DynStr and fill a given number of bytes in it with the null terminator.
  pub fn filled_null (byte_count: usize) -> Self {
    let mut out = Self::default();
    out.fill_null(byte_count);
    out
  }
}


impl Default for DynStr { fn default () -> Self { Self::new() } }


impl From<&str> for DynStr {
  fn from (s: &str) -> Self {
    let mut out = Self::with_capacity(s.len());

    out.push_str(s);

    out
  }
}

impl str::FromStr for DynStr {
  type Err = ();

  fn from_str (s: &str) -> Result<Self, Self::Err> {
    Ok(Self::from(s))
  }
}

impl std::convert::TryFrom<&[u8]> for DynStr {
  type Error = Utf8Error;

  fn try_from (bytes: &[u8]) -> Result<Self, Self::Error> {
    match str::from_utf8(bytes) {
      Ok(s) => Ok(s.into()),
      Err(e) => Err(e.into())
    }
  }
}

impl From<String> for DynStr {
  fn from (s: String) -> DynStr {
    DynStr(s.into_bytes().into())
  }
}

impl From<DynStr> for String {
  fn from (d: DynStr) -> String {
    unsafe { String::from_utf8_unchecked(d.into_inner().into()) }
  }
}


impl fmt::Debug for DynStr {
  fn fmt (&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    fmt::Debug::fmt(self.as_str_maybe().unwrap_or(""), f)
  }
}

impl fmt::Display for DynStr {
  fn fmt (&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    fmt::Display::fmt(self.as_str_maybe().unwrap_or(""), f)
  }
}


impl Clone for DynStr {
  #[inline] fn clone (&self) -> Self { Self(self.0.clone()) }
}


impl Drop for DynStr {
  #[inline] fn drop (&mut self) { self.destroy() }
}


impl AsRef<[u8]> for DynStr {
  #[track_caller]
  fn as_ref (&self) -> &[u8] {
    self.as_slice_maybe().expect("DynStr must be initialized before using implicit conversion to slice")
  }
}

impl AsMut<[u8]> for DynStr {
  #[track_caller]
  fn as_mut (&mut self) -> &mut [u8] {
    self.as_slice_maybe_mut().expect("DynStr must be initialized before using implicit conversion to slice")
  }
}


impl Borrow<[u8]> for DynStr {
  #[track_caller]
  fn borrow (&self) -> &[u8] {
    self.as_slice_maybe().expect("DynStr must be initialized before using implicit conversion to slice")
  }
}

impl BorrowMut<[u8]> for DynStr {
  #[track_caller]
  fn borrow_mut (&mut self) -> &mut [u8] {
    self.as_slice_maybe_mut().expect("DynStr must be initialized before using implicit conversion to slice")
  }
}


impl AsRef<str> for DynStr {
  #[track_caller]
  fn as_ref (&self) -> &str {
    self.as_str_maybe().expect("DynStr must be initialized before using implicit conversion to str")
  }
}

impl AsRef<Path> for DynStr {
  #[track_caller]
  fn as_ref (&self) -> &Path {
    AsRef::<str>::as_ref(self).as_ref()
  }
}

impl AsMut<str> for DynStr {
  #[track_caller]
  fn as_mut (&mut self) -> &mut str {
    self.as_str_maybe_mut().expect("DynStr must be initialized before using implicit conversion to str")
  }
}


impl Borrow<str> for DynStr {
  #[track_caller]
  fn borrow (&self) -> &str {
    self.as_str_maybe().expect("DynStr must be initialized before using implicit conversion to str")
  }
}

impl BorrowMut<str> for DynStr {
  #[track_caller]
  fn borrow_mut (&mut self) -> &mut str {
    self.as_str_maybe_mut().expect("DynStr must be initialized before using implicit conversion to str")
  }
}


impl ops::Deref for DynStr {
  type Target = str;

  #[track_caller]
  fn deref (&self) -> &Self::Target {
    self.as_str_maybe().expect("DynStr must be initialized before using implicit conversion to str")
  }
}

impl ops::DerefMut for DynStr {
  #[track_caller]
  fn deref_mut (&mut self) -> &mut Self::Target {
    self.as_str_maybe_mut().expect("DynStr must be initialized before using implicit conversion to str")
  }
}


impl PartialEq<DynStr> for DynStr {
  #[inline] fn eq (&self, other: &Self) -> bool {
    self.as_str_maybe() == other.as_str_maybe()
  }
}

impl PartialEq<str> for DynStr {
  fn eq (&self, other: &str) -> bool {
    self.as_str_maybe() == Just(other)
  }
}

impl PartialEq<DynStr> for str {
  #[inline] fn eq (&self, other: &DynStr) -> bool { other == self }
}

impl Eq for DynStr { }


impl PartialOrd<DynStr> for DynStr {
  fn partial_cmp (&self, other: &Self) -> Option<std::cmp::Ordering> {
    self.as_str_maybe().partial_cmp(&other.as_str_maybe())
  }
}

impl PartialOrd<str> for DynStr {
  fn partial_cmp (&self, other: &str) -> Option<std::cmp::Ordering> {
    self.as_str_maybe().partial_cmp(&Just(other))
  }
}

impl Ord for DynStr {
  fn cmp (&self, other: &Self) -> std::cmp::Ordering {
    self.as_str_maybe().cmp(&other.as_str_maybe())
  }
}


impl hash::Hash for DynStr {
  fn hash<H: hash::Hasher> (&self, hasher: &mut H) {
    self.as_str_maybe().unwrap_or("").hash(hasher)
  }
}


/// The Iterator used when DynStr is turned into a consuming Iterator via IntoIterator
pub struct IntoIter (pub DynStr);

impl Iterator for IntoIter {
  type Item = char;

  fn next (&mut self) -> Option<char> {
    self.0.unshift_char().into()
  }
}

impl IntoIterator for DynStr {
  type Item = char;

  type IntoIter = IntoIter;

  fn into_iter (self) -> Self::IntoIter {
    IntoIter(self)
  }
}


impl FromIterator<char> for DynStr {
  fn from_iter<I: IntoIterator<Item = char>> (iter: I) -> Self {
    let mut out = Self::new();

    for ch in iter {
      out.push_char(ch);
    }

    out
  }
}


impl fmt::Write for DynStr {
  #[inline]
  fn write_str (&mut self, s: &str) -> fmt::Result {
    self.push_str(s);
    Ok(())
  }

  #[inline]
  fn write_char (&mut self, ch: char) -> fmt::Result {
    self.push_char(ch);
    Ok(())
  }
}


impl io::Write for DynStr {
  #[inline]
  fn write (&mut self, bytes: &[u8]) -> io::Result<usize> {
    match str::from_utf8(bytes) {
      Ok(s) => {
        self.push_str(s);
        Ok(s.len())
      },
      Err(e) => Err(io::Error::new(io::ErrorKind::InvalidData, e))
    }
  }

  #[inline] fn flush (&mut self) -> io::Result<()> { Ok(()) }
}


impl io::Read for DynStr {
  fn read (&mut self, bytes: &mut [u8]) -> io::Result<usize> {
    if let Just(ch) = self.unshift_char() {
      let len = ch.len_utf8();
      
      if len <= bytes.len() {
        ch.encode_utf8(bytes);
        Ok(len)
      } else {
        Err(io::ErrorKind::InvalidInput.into())
      }
    } else {
      Ok(0)
    }
  }
}

/// Works like ToString but produces a DynStr instead of a String
pub trait ToDynStr {
  /// Create a DynStr from a value (Same as to_string, but produces a DynStr)
  fn to_dynstr (&self) -> DynStr;
}

impl<T> ToDynStr for T
where T: fmt::Display + ?Sized
{
  #[inline]
  default fn to_dynstr(&self) -> DynStr {
    use fmt::Write;
    let mut buf = DynStr::new();
    buf.write_fmt(format_args!("{}", self))
       .expect("a Display implementation returned an error unexpectedly");
    buf
  }
}


/// `format!` equivalent that generates a `DynStr` instead of a `String`
#[macro_export]
macro_rules! dynformat {
  () => { $crate::core::DynStr::default() };
  
  ($($args:expr),+) => { {
    let mut dstr = $crate::core::DynStr::default();
    
    ::std::fmt::Write::write_fmt(&mut dstr, ::std::format_args!($($args),+)).expect("dynformat failure");

    dstr
  } };
}