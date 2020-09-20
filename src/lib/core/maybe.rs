//! Contains a c-ffi friendly version of Option

use core::hint::unreachable_unchecked;

use std::{
  ops,
  fmt,
  mem,
  option::NoneError
};


use super::outcome::*;


/// A monadic wrapper enum like Option, that enables easy c-ffi via repr(C, u8)
/// 
/// The equivalent C code would be
/// ```c
/// #include <stdbool.h>
/// 
/// typedef struct {
///   bool has_value;
///   T value;
/// } Maybe;
/// ```
#[repr(C, u8)]
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Maybe<T> {
  /// A Maybe with no value
  Nothing,
  /// A Maybe with a value
  Just(T)
}

/// Constructs a Maybe with no value
pub use Maybe::Nothing;

/// Constructs a Maybe with a value
pub use Maybe::Just;


impl<T> Maybe<T> {
  /// Determine if a Maybe has a value
  pub fn is_just (&self) -> bool {
    matches!(self, Just(_))
  }

  /// Determine if a Maybe has no value
  pub fn is_nothing (&self) -> bool {
    matches!(self, Nothing)
  }


  /// Determine if a Maybe has a value and that value is equal to some expected value
  pub fn contains<E> (&self, expected_value: &E) -> bool
  where T: PartialEq<E>
  {
    if let Just(value) = self { value == expected_value } else { false }
  }


  /// Convert `&Maybe<T>` to `Maybe<&T>`
  pub fn as_ref (&self) -> Maybe<&T> {
    if let Just(v) = self { Just(v) } else { Nothing }
  }

  /// Convert `&Maybe<T>` to `Maybe<&<T as std::ops::Deref>::Target>`
  pub fn as_deref (&self) -> Maybe<&T::Target>
  where T: ops::Deref
  {
    self.as_ref().map(|r| r.deref())
  }
  
  /// Convert `&mut Maybe<T>` to `Maybe<&mut T>`
  pub fn as_mut (&mut self) -> Maybe<&mut T> {
    if let Just(v) = self { Just(v) } else { Nothing }
  }

  /// Convert `&mut Maybe<T>` to `Maybe<&mut <T as std::ops::DerefMut>::Target>`
  pub fn as_deref_mut (&mut self) -> Maybe<&mut T::Target>
  where T: ops::DerefMut
  {
    self.as_mut().map(|r| r.deref_mut())
  }


  /// Extract the value of a Maybe or panic with a provided error message if it is Nothing
  #[track_caller]
  pub fn expect<D: fmt::Display> (self, msg: D) -> T {
    if let Just(v) = self { v } else { panic!("Expected Maybe to have a value ({})", msg) }
  }

  /// Panic with a provided error message if a Maybe is not Nothing
  #[track_caller]
  pub fn expect_nothing<D: fmt::Display> (self, msg: D)
  where T: fmt::Debug
  {
    if let Just(v) = self { panic!("Did not expect Maybe to have a value ({}), found: {:?}", msg, v) }
  }


  /// Extract the value of a Maybe or panic with a generic error message if it is Nothing
  #[track_caller]
  pub fn unwrap (self) -> T {
    self.expect("Could not unwrap")
  }

  /// Panic with a generic error message if a Maybe is not Nothing
  #[track_caller]
  pub fn unwrap_nothing (self)
  where T: fmt::Debug
  {
    self.expect_nothing("Could not unwrap")
  }

  /// Extract the value of a Maybe or use a provided default if it is Nothing
  pub fn unwrap_or (self, default: T) -> T {
    if let Just(v) = self { v } else { default }
  }

  /// Extract the value of a Maybe or use the result of a call to a provided closure if it is Nothing
  pub fn unwrap_or_else<F: FnOnce() -> T> (self, f: F) -> T {
    if let Just(v) = self { v } else { f() }
  }

  /// Extract the value of a Maybe or use the result of `<T as Default>::default()` if it is Nothing
  pub fn unwrap_or_default (self) -> T
  where T: Default
  {
    if let Just(v) = self { v } else { T::default() }
  }


  /// Extract the value of a Maybe without checking if it is Nothing
  /// # Safety
  /// This is only safe if the caller has already checked that Maybe is not Nothing
  pub unsafe fn unwrap_unchecked (self) -> T {
    if let Just(v) = self { v } else { unreachable_unchecked() }
  }

  /// Get an immutable reference to the value of a Maybe without checking if it is Nothing
  /// # Safety
  /// This is only safe if the caller has already checked that Maybe is not Nothing
  pub unsafe fn inner_unchecked (&self) -> &T {
    if let Just(v) = self { v } else { unreachable_unchecked() }
  }

  /// Get a mutable reference to the value of a Maybe without checking if it is Nothing
  /// # Safety
  /// This is only safe if the caller has already checked that Maybe is not Nothing
  pub unsafe fn inner_unchecked_mut (&mut self) -> &mut T {
    if let Just(v) = self { v } else { unreachable_unchecked() }
  }


  /// Logical AND operation on the Just variant of two Maybe values.
  /// Returns `next` if `self` is not Nothing, otherwise returns Nothing
  pub fn and<N> (self, next: Maybe<N>) -> Maybe<N> {
    if self.is_just() { next } else { Nothing }
  }

  /// Returns the result of calling `f(v)` if `self` is `Just(v)`
  pub fn and_then<R, F: FnOnce(T) -> Maybe<R>> (self, f: F) -> Maybe<R> {
    if let Just(v) = self { f(v) } else { Nothing }
  }

  /// Logical OR operation on the Just variant of two Maybe values.
  /// Returns `next` if `self` is Nothing, otherwise returns `self`
  pub fn or (self, next: Self) -> Self {
    if self.is_just() { self } else { next }
  }

  /// Returns the result of calling `f()` if `self` is Nothing
  pub fn or_else<F: FnOnce() -> Self> (self, f: F) -> Self {
    if self.is_just() { self } else { f() }
  }

  /// Logical XOR operation on the variants of two Maybe values:
  /// 
  /// + If `self` is `Just(a)` and `other` is Nothing, returns `Just(a)`
  /// + If `other` is `Just(b)` and `self` is Nothing, returns `Just(b)`
  /// + Otherwise, returns Nothing
  pub fn xor (self, other: Self) -> Self {
    match (self, other) {
      (Just(a), Nothing) => Just(a),
      (Nothing, Just(b)) => Just(b),
      _ => Nothing,
    }
  }


  /// Returns `self` if it is `Just(v)` and `pred(&v)` returns true, otherwise return Nothing
  pub fn filter<P: FnOnce(&T) -> bool> (self, pred: P) -> Maybe<T> {
    if let Just(v) = &self {
      if pred(v) {
        return self
      }
    }

    Nothing
  }
  
  /// Returns true if it is `Just(v)` and `pred(&v)` returns true, otherwise returns false
  pub fn matches<P: FnOnce(&T) -> bool> (self, pred: P) -> bool {
    if let Just(v) = &self {
      if pred(v) {
        return true
      }
    }

    false
  }


  /// If `self` is `Just(v)` return `Just(m(v))`, otherwise return Nothing
  pub fn map<R, M: FnOnce(T) -> R> (self, m: M) -> Maybe<R> {
    if let Just(v) = self { Just(m(v)) } else { Nothing }
  }

  /// If `self` is `Just(v)` return `m(v)`, otherwise return `default`
  pub fn map_or<R, M: FnOnce(T) -> R> (self, m: M, default: R) -> R {
    if let Just(v) = self { m(v) } else { default }
  }

  /// If `self` is `Just(v)` return `m(v)`, otherwise return `d()`
  pub fn map_or_else<R, D: FnOnce() -> R, M: FnOnce(T) -> R> (self, m: M, d: D) -> R {
    if let Just(v) = self { m(v) } else { d() }
  }


  /// Map `Just(v)` to `Ok(v)` and `Nothing` to `Err(e)`
  pub fn ok_or<E> (self, e: E) -> Result<T, E> {
    if let Just(v) = self { Ok(v) } else { Err(e) }
  }

  /// Map `Just(v)` to `Ok(v)` and `Nothing` to `Err(m())`
  pub fn ok_or_else<E, M: FnOnce() -> E> (self, m: M) -> Result<T, E> {
    if let Just(v) = self { Ok(v) } else { Err(m()) }
  }


  /// Map `Just(v)` to `Succ(v)` and `Nothing` to `Fail(f)`
  pub fn succ_or<F> (self, f: F) -> Outcome<T, F> {
    if let Just(v) = self { Succ(v) } else { Fail(f) }
  }

  /// Map `Just(v)` to `Succ(v)` and `Nothing` to `Fail(m())`
  pub fn succ_or_else<F, M: FnOnce() -> F> (self, m: M) -> Outcome<T, F> {
    if let Just(v) = self { Succ(v) } else { Fail(m()) }
  }


  /// If a Maybe is Nothing, convert it to `Just(default)`,
  /// then return a mutable reference to the contained value
  pub fn get_or_insert (&mut self, default: T) -> &mut T {
    if self.is_nothing() { *self = Just(default) }

    unsafe { self.inner_unchecked_mut() }
  }

  /// If a Maybe is Nothing, convert it to `Just(f())`,
  /// then return a mutable reference to the contained value
  pub fn get_or_insert_with<F: FnOnce() -> T> (&mut self, f: F) -> &mut T {
    if self.is_nothing() { *self = Just(f()) }

    unsafe { self.inner_unchecked_mut() }
  }


  /// Take the value of a Maybe, leaving it as Nothing
  pub fn take (&mut self) -> Self {
    mem::take(self)
  }

  /// Replace a Maybe with `Just(value)` and return the old value
  pub fn replace (&mut self, value: T) -> Self {
    mem::replace(self, Just(value))
  }

  
  /// Combine `Maybe<T>` and `Maybe<U>` into `Maybe<(T, U)>`:
  /// If either value is Nothing, returns Nothing.
  /// Otherwise, returns the pair of both Justs
  pub fn zip<U> (self, other: Maybe<U>) -> Maybe<(T, U)> {
    if let (Just(a), Just(b)) = (self, other) { Just((a, b)) } else { Nothing }
  }

  /// Combine `Maybe<T>` and the result of a callback, if `self` is `Just`
  pub fn and_zip<U, F: FnOnce() -> Maybe<U>> (self, f: F) -> Maybe<(T, U)> {
    if let Just(a) = self { if let Just(b) = f() { return Just((a, b)) } }

    Nothing
  }

  /// Combine `Maybe<T>` with `Maybe<U>` via a callback
  /// If either value is Nothing, returns Nothing.
  /// Otherwise, calls `f` with the value of both Justs
  pub fn zip_with<U, R, F: FnOnce(T, U) -> R> (self, other: Maybe<U>, f: F) -> Maybe<R> {
    if let (Just(a), Just(b)) = (self, other) { Just(f(a, b)) } else { Nothing }
  }
}


impl<T> fmt::Display for Maybe<T>
where T: fmt::Display
{
  fn fmt (&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if let Just(v) = self { write!(f, "{}", v) } else { write!(f, "<Nothing>") }
  }
}


impl<T> Maybe<&T>
where T: Copy
{
  /// Map `Maybe<&T>` to `Maybe<T>` via Copy
  pub fn copied (self) -> Maybe<T> {
    if let Just(&v) = self { Just(v) } else { Nothing }
  }
}

impl<T> Maybe<&mut T>
where T: Copy
{
  /// Map `Maybe<&mut T>` to `Maybe<T>` via Copy
  pub fn copied (self) -> Maybe<T> {
    if let Just(&mut v) = self { Just(v) } else { Nothing }
  }
}

impl<T> Maybe<&T>
where T: Clone
{
  /// Map `Maybe<&T>` to `Maybe<T>` via Clone
  pub fn cloned (self) -> Maybe<T> {
    if let Just(v) = self { Just(v.clone()) } else { Nothing }
  }
}

impl<T> Maybe<&mut T>
where T: Clone
{
  /// Map `Maybe<&mut T>` to `Maybe<T>` via Clone
  pub fn cloned (self) -> Maybe<T> {
    if let Just(v) = self { Just(v.clone()) } else { Nothing }
  }
}


impl<T> Maybe<Maybe<T>> {
  /// Map `Maybe<Maybe<T>>` to `Maybe<T>`.
  /// If the value is `Just(Just(v))` returns `Just(v)`, otherwise returns Nothing
  pub fn flatten (self) -> Maybe<T> {
    if let Just(Just(v)) = self { Just(v) } else { Nothing }
  }
}


impl<T, E> Maybe<Result<T, E>> {
  /// Map `Maybe<Result<T, E>>` to `Result<Maybe<T>, E>`
  pub fn transpose (self) -> Result<Maybe<T>, E> {
    match self {
      Just(Ok(v)) => Ok(Just(v)),
      Just(Err(e)) => Err(e),
      Nothing => Ok(Nothing)
    }
  }
}

impl<S, F> Maybe<Outcome<S, F>> {
  /// Map `Maybe<Outcome<S, F>>` to `Outcome<Maybe<S>, F>`
  pub fn transpose (self) -> Outcome<Maybe<S>, F> {
    match self {
      Just(Succ(s)) => Succ(Just(s)),
      Just(Fail(f)) => Fail(f),
      Nothing => Succ(Nothing)
    }
  }
}


impl<T> Default for Maybe<T> {
  #[inline] fn default () -> Self { Nothing }
}


impl<T> Clone for Maybe<T>
where T: Clone
{
  fn clone (&self) -> Self {
    if let Just(v) = self { Just(v.clone()) } else { Nothing }
  }

  fn clone_from (&mut self, from: &Self) {
    match (self, from) {
      (Just(to), Just(from)) => to.clone_from(from),
      (to, from) => *to = from.clone()
    }
  }
}

impl<T> Copy for Maybe<T>
where T: Copy { }


impl<T> From<T> for Maybe<T> {
  #[inline] fn from (v: T) -> Maybe<T> { Just(v) }
}

impl<'a, T> From<&'a Maybe<T>> for Maybe<&'a T> {
  #[inline] fn from (m: &'a Maybe<T>) -> Maybe<&'a T> { m.as_ref() }
}

impl<'a, T> From<&'a mut Maybe<T>> for Maybe<&'a mut T> {
  #[inline] fn from (m: &'a mut Maybe<T>) -> Maybe<&'a mut T> { m.as_mut() }
}


impl<T> From<Option<T>> for Maybe<T> {
  fn from (o: Option<T>) -> Maybe<T> {
    match o {
      Some(v) => Just(v),
      None => Nothing
    }
  }
}

impl<T> From<Maybe<T>> for Option<T> {
  fn from (m: Maybe<T>) -> Option<T> {
    match m {
      Just(v) => Some(v),
      Nothing => None
    }
  }
}



/// The type used when converting a Maybe into a Result via `std::ops::Try`
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default, Hash)]
pub struct NothingError;

impl From<NoneError> for NothingError {
  #[inline] fn from (_: NoneError) -> NothingError { NothingError }
}

impl From<NothingError> for NoneError {
  #[inline] fn from (_: NothingError) -> NoneError { NoneError }
}

impl<T> From<Maybe<T>> for Result<T, NothingError> {
  #[inline] fn from (m: Maybe<T>) -> Result<T, NothingError> { m.ok_or(NothingError) }
}

impl<T> From<Result<T, NothingError>> for Maybe<T> {
  #[inline] fn from (r: Result<T, NothingError>) -> Maybe<T> { r.ok().into() }
}

impl<T> From<Maybe<T>> for Outcome<T, NothingError> {
  #[inline] fn from (m: Maybe<T>) -> Outcome<T, NothingError> { m.succ_or(NothingError) }
}

impl<T> From<Outcome<T, NothingError>> for Maybe<T> {
  #[inline] fn from (o: Outcome<T, NothingError>) -> Maybe<T> { o.succ() }
}



impl<T> ops::Try for Maybe<T> {
  type Ok = T;
  type Error = NothingError;

  #[inline]
  fn into_result (self) -> Result<Self::Ok, Self::Error> { self.into() }

  #[inline] fn from_ok (v: Self::Ok) -> Self { Just(v) }

  #[inline] fn from_error (_: Self::Error) -> Self { Nothing }
}