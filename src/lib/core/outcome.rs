//! Contains a c-ffi friendly version of Result

use core::hint::unreachable_unchecked;

use std::{
  fmt,
  ops
};


use super::maybe::*;


/// A wrapper enum like Result, that enables easy c-ffi via repr(C, u8)
/// 
/// The equivalent C code would be
/// ```c
/// #include <stdbool.h>
/// 
/// typedef struct {
///   bool is_succ;
///   union {
///     T succ;
///     E fail;
///   };
/// } Outcome;
/// ```
#[repr(C, u8)]
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Outcome<S, F> {
  /// Represents an invalid or unexpected value or event
  Fail(F),
  /// Represents a valid, expected value or event
  Succ(S)
}

/// Constructs a successful Outcome
pub use Outcome::Succ;

/// Constructs a failure Outcome
pub use Outcome::Fail;


impl<S, F> Outcome<S, F> {
  /// Determine if an Outcome is Succ
  pub fn is_succ (&self) -> bool {
    matches!(self, Succ(_))
  }

  /// Determine if an Outcome is Fail
  pub fn is_fail (&self) -> bool {
    matches!(self, Fail(_))
  }

  
  /// Determine if an Outcome is Succ and it's value is equal to some expected value
  pub fn contains<E> (&self, expected_value: &E) -> bool
  where S: PartialEq<E>
  {
    if let Succ(value) = self { value == expected_value } else { false }
  }

  /// Determine if an Outcome is Fail and it's value is equal to some expected value
  pub fn contains_fail<E> (&self, expected_value: &E) -> bool
  where F: PartialEq<E>
  {
    if let Fail(value) = self { value == expected_value } else { false }
  }


  /// Convert `Outcome<S, F>` to `Maybe<S>`:
  /// + If Outcome is `Succ(v)`, returns `Just(v)`
  /// + If Outcome is `Fail(_)`, returns `Nothing`
  pub fn succ (self) -> Maybe<S> {
    if let Succ(value) = self { Just(value) } else { Nothing }
  }

  /// Convert `Outcome<S, F>` to `Option<S>`:
  /// + If Outcome is `Succ(v)`, returns `Some(v)`
  /// + If Outcome is `Fail(_)`, returns `None`
  pub fn succ_opt (self) -> Option<S> {
    if let Succ(value) = self { Some(value) } else { None }
  }

  /// Get an immutable reference to the Succ value of an Outcome without checking if it is Fail
  /// # Safety
  /// This is only safe if the caller has already checked that Outcome is not Fail
  pub unsafe fn succ_unchecked (&self) -> &S {
    if let Succ(v) = self { v } else { unreachable_unchecked() }
  }

  /// Get a mutable reference to the Succ value of an Outcome without checking if it is Fail
  /// # Safety
  /// This is only safe if the caller has already checked that Outcome is not Fail
  pub unsafe fn succ_unchecked_mut (&mut self) -> &mut S {
    if let Succ(v) = self { v } else { unreachable_unchecked() }
  }
  

  /// Convert `Outcome<S, F>` to `Maybe<F>`:
  /// + If Outcome is `Fail(v)`, returns `Just(v)`
  /// + If Outcome is `Succ(_)`, returns `Nothing`
  pub fn fail (self) -> Maybe<F> {
    if let Fail(value) = self { Just(value) } else { Nothing }
  }

  /// Convert `Outcome<S, F>` to `Option<F>`:
  /// + If Outcome is `Fail(v)`, returns `Some(v)`
  /// + If Outcome is `Succ(_)`, returns `None`
  pub fn fail_opt (self) -> Option<F> {
    if let Fail(value) = self { Some(value) } else { None }
  }

  /// Get an immutable reference to the Fail value of an Outcome without checking if it is Succ
  /// # Safety
  /// This is only safe if the caller has already checked that Outcome is not Succ
  pub unsafe fn fail_unchecked (&self) -> &F {
    if let Fail(v) = self { v } else { unreachable_unchecked() }
  }

  /// Get a mutable reference to the Fail value of an Outcome without checking if it is Succ
  /// # Safety
  /// This is only safe if the caller has already checked that Outcome is not Succ
  pub unsafe fn fail_unchecked_mut (&mut self) -> &mut F {
    if let Fail(v) = self { v } else { unreachable_unchecked() }
  }


  /// Convert `&Outcome<S, F>` to `Outcome<&S, &F>`
  pub fn as_ref (&self) -> Outcome<&S, &F> {
    match self {
      Succ(s) => Succ(s),
      Fail(f) => Fail(f)
    }
  }

  /// Convert `&Outcome<S, F>` to `Outcome<&<S as std::ops::Deref>::Target, &F>`
  pub fn as_deref (&self) -> Outcome<&S::Target, &F>
  where S: ops::Deref
  {
    self.as_ref().map(|s| s.deref())
  }

  /// Convert `&mut Outcome<S, F>` to `Outcome<&mut S, &mut F>`
  pub fn as_mut (&mut self) -> Outcome<&mut S, &mut F> {
    match self {
      Succ(s) => Succ(s),
      Fail(f) => Fail(f)
    }
  }
  
  /// Convert `&mut Outcome<S, F>` to `Outcome<&mut <S as std::ops::DerefMut>::Target, &mut F>`
  pub fn as_deref_mut (&mut self) -> Outcome<&mut S::Target, &mut F>
  where S: ops::DerefMut
  {
    self.as_mut().map(|s| s.deref_mut())
  }

  
  /// Convert `&Outcome<S, F>` to `Outcome<&S, &<F as std::ops::Deref>::Target>`
  pub fn as_fail_deref (&self) -> Outcome<&S, &F::Target>
  where F: ops::Deref
  {
    self.as_ref().map_fail(|f| f.deref())
  }
  
  /// Convert `&mut Outcome<S, F>` to `Outcome<&mut S, &mut <F as std::ops::DerefMut>::Target>`
  pub fn as_fail_deref_mut (&mut self) -> Outcome<&mut S, &mut F::Target>
  where F: ops::DerefMut
  {
    self.as_mut().map_fail(|f| f.deref_mut())
  }


  /// Extract the Succ value of an Outcome or panic with a provided error message if it is Fail
  #[track_caller]
  pub fn expect<D: fmt::Display> (self, msg: D) -> S
  where F: fmt::Debug
  {
    match self {
      Succ(s) => s,
      Fail(f) => panic!("Expected Outcome::Succ ({}), found: Fail({:?})", msg, f)
    }
  }

  /// Panic with a provided error message if an Outcome is not Fail
  #[track_caller]
  pub fn expect_fail<D: fmt::Display> (self, msg: D) -> F
  where S: fmt::Debug
  {
    match self {
      Succ(s) => panic!("Expected Outcome::Fail ({}), found: Succ({:?})", msg, s),
      Fail(f) => f
    }
  }


  /// Extract the Succ value of an Outcome or panic with a generic error message if it is Fail
  #[track_caller]
  pub fn unwrap (self) -> S
  where F: fmt::Debug
  {
    self.expect("Could not unwrap")
  }

  /// Extract the Succ value of an Outcome without checking if it is Fail
  /// # Safety
  /// This is only safe if the caller has already checked that Outcome is not Fail
  pub unsafe fn unwrap_unchecked (self) -> S {
    if let Succ(s) = self { s } else { unreachable_unchecked() }
  }

  /// Panic with a generic error message if an Outcome is not Fail
  #[track_caller]
  pub fn unwrap_fail (self) -> F
  where S: fmt::Debug
  {
    self.expect_fail("Could not unwrap")
  }

  /// Extract the Fail value of an Outcome without checking if it is Succ
  /// # Safety
  /// This is only safe if the caller has already checked that Outcome is not Succ
  pub unsafe fn unwrap_fail_unchecked (self) -> S {
    if let Succ(s) = self { s } else { unreachable_unchecked() }
  }

  /// Extract the value of an Outcome or use a provided default if it is Fail
  pub fn unwrap_or (self, default: S) -> S {
    if let Succ(v) = self { v } else { default }
  }

  /// Extract the value of an Outcome or use the result of a call to a provided closure if it is Fail
  pub fn unwrap_or_else<E: FnOnce(F) -> S> (self, e: E) -> S {
    match self {
      Succ(s) => s,
      Fail(f) => e(f)
    }
  }

  /// Extract the value of an Outcome or use the result of `<S as Default>::default()` if it is Fail
  pub fn unwrap_or_default (self) -> S
  where S: Default
  {
    if let Succ(v) = self { v } else { S::default() }
  }


  /// Logical AND operation on the Just variant of two Outcome values.
  /// Returns `next` if `self` is Succ, otherwise returns `self`'s Fail
  pub fn and<N> (self, next: Outcome<N, F>) -> Outcome<N, F> {
    match self {
      Succ(_) => next,
      Fail(f) => Fail(f)
    }
  }

  /// Map the Succ value of an Outcome:
  /// + `Succ(s)` => `m(s)`
  /// + `Fail(f)` => `Fail(f)`
  pub fn and_then<N, M: FnOnce(S) -> Outcome<N, F>> (self, m: M) -> Outcome<N, F> {
    match self {
      Succ(s) => m(s),
      Fail(f) => Fail(f)
    }
  }

  /// Logical OR operation on the Just variant of two Outcome values.
  /// Returns `next` if `self` is Fail, otherwise returns the existing Succ
  pub fn or<N> (self, next: Outcome<S, N>) -> Outcome<S, N> {
    match self {
      Succ(s) => Succ(s),
      Fail(_) => next
    }
  }

  
  /// Map the Fail value of an Outcome:
  /// + `Succ(s)` => `Succ(s)`
  /// + `Fail(f)` => `m(f)`
  pub fn or_else<N, M: FnOnce(F) -> Outcome<S, N>> (self, m: M) -> Outcome<S, N> {
    match self {
      Succ(s) => Succ(s),
      Fail(f) => m(f)
    }
  }


  /// If `self` is `Succ(v)` return `Succ(m(v))`, otherwise return the existing Fail
  pub fn map<N, M: FnOnce(S) -> N> (self, m: M) -> Outcome<N, F> {
    match self {
      Succ(s) => Succ(m(s)),
      Fail(f) => Fail(f),
    }
  }

  /// If `self` is `Succ(v)` return `m(v)`, otherwise return `default`
  pub fn map_or<N, M: FnOnce(S) -> N> (self, default: N, m: M) -> N {
    match self {
      Succ(s) => m(s),
      Fail(_) => default
    }
  }
  
  /// Map either of an Outcome's variants to a single value type via callbacks:
  /// + `Succ(x)` => `s(x)`
  /// + `Fail(y)` => `f(y)`
  pub fn map_or_else<N, X: FnOnce(S) -> N, Y: FnOnce(F) -> N> (self, s: X, f: Y) -> N {
    match self {
      Succ(x) => s(x),
      Fail(y) => f(y)
    }
  }


  /// If `self` is `Fail(v)` return `Fail(m(v))`, otherwise return the existing Succ
  pub fn map_fail<N, M: FnOnce(F) -> N> (self, m: M) -> Outcome<S, N> {
    match self {
      Succ(s) => Succ(s),
      Fail(f) => Fail(m(f)),
    }
  }
}


impl<S, F> fmt::Display for Outcome<S, F>
where S: fmt::Display,
      F: fmt::Debug
{
  fn fmt (&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Succ(s) => write!(f, "{}", s),
      Fail(e) => write!(f, "Fail({:?})", e)
    }
  }
}


impl<S, F> Outcome<&S, F>
where S: Copy
{
  /// Map `Outcome<&S, F>` to `Outcome<S, F>` via Copy
  pub fn copied (self) -> Outcome<S, F> {
    match self {
      Succ(&v) => Succ(v),
      Fail(f) => Fail(f)
    }
  }
}

impl<S, F> Outcome<&mut S, F>
where S: Copy
{
  /// Map `Outcome<&mut S, F>` to `Outcome<S, F>` via Copy
  pub fn copied (self) -> Outcome<S, F> {
    match self {
      Succ(&mut v) => Succ(v),
      Fail(f) => Fail(f)
    }
  }
}

impl<S, F> Outcome<&S, F>
where S: Clone
{
  /// Map `Outcome<&S, F>` to `Outcome<S, F>` via Clone
  pub fn cloned (self) -> Outcome<S, F> {
    match self {
      Succ(v) => Succ(v.clone()),
      Fail(f) => Fail(f)
    }
  }
}

impl<S, F> Outcome<&mut S, F>
where S: Clone
{
  /// Map `Outcome<&mut S, F>` to `Outcome<S, F>` via Clone
  pub fn cloned (self) -> Outcome<S, F> {
    match self {
      Succ(v) => Succ(v.clone()),
      Fail(f) => Fail(f)
    }
  }
}


impl<'a, S, F> From<&'a Outcome<S, F>> for Outcome<&'a S, &'a F> {
  #[inline] fn from (m: &'a Outcome<S, F>) -> Outcome<&'a S, &'a F> { m.as_ref() }
}

impl<'a, S, F> From<&'a mut Outcome<S, F>> for Outcome<&'a mut S, &'a mut F> {
  #[inline] fn from (m: &'a mut Outcome<S, F>) -> Outcome<&'a mut S, &'a mut F> { m.as_mut() }
}


impl<S, F> From<Result<S, F>> for Outcome<S, F> {
  fn from (r: Result<S, F>) -> Outcome<S, F> {
    match r {
      Ok(x) => Succ(x),
      Err(y) => Fail(y)
    }
  }
}

impl<S, F> From<Outcome<S, F>> for Result<S, F> {
  fn from (o: Outcome<S, F>) -> Result<S, F> {
    match o {
      Succ(x) => Ok(x),
      Fail(y) => Err(y)
    }
  }
}



impl<S, F> Outcome<Outcome<S, F>, F> {
  /// Map `Outcome<Outcome<T, F>, F>` to `Outcome<T, F>`.
  /// If the value is `Succ(Succ(s))` returns `Succ(s)`, otherwise returns the first Fail
  pub fn flatten (self) -> Outcome<S, F> {
    match self {
      Succ(Succ(s)) => Succ(s),
      Succ(Fail(f)) | Fail(f) => Fail(f)
    }
  }
}


impl<S, F> Outcome<Maybe<S>, F> {
  /// Convert `Outcome<Maybe<S>, F>` to `Maybe<Outcome<S, F>>`
  pub fn transpose (self) -> Maybe<Outcome<S, F>> {
    match self {
      Succ(Just(s)) => Just(Succ(s)),
      Succ(Nothing) => Nothing,
      Fail(f) => Just(Fail(f))
    }
  }
}


impl<S, F> Outcome<Option<S>, F> {
  /// Convert `Outcome<Option<S>, F>` to `Option<Outcome<S, F>>`
  pub fn transpose (self) -> Option<Outcome<S, F>> {
    match self {
      Succ(Some(s)) => Some(Succ(s)),
      Succ(None) => None,
      Fail(f) => Some(Fail(f))
    }
  }
}


impl<S, F> Clone for Outcome<S, F>
where S: Clone,
      F: Clone
{
  fn clone (&self) -> Self {
    match self {
      Succ(s) => Succ(s.clone()),
      Fail(x) => Fail(x.clone()),
    }
  }

  fn clone_from (&mut self, from: &Self) {
    match (self, from) {
      (Succ(to), Succ(from)) => to.clone_from(from),
      (Fail(to), Fail(from)) => to.clone_from(from),
      (to, from) => *to = from.clone(),
    }
  }
}

impl<S, F> Copy for Outcome<S, F>
where S: Copy,
      F: Copy
{ }


impl<S, F> ops::Try for Outcome<S, F> {
  type Ok = S;
  type Error = F;

  #[inline] fn into_result (self) -> Result<S, F> { self.into() }

  #[inline] fn from_ok (s: S) -> Self { Succ(s) }

  #[inline] fn from_error (f: F) -> Self { Fail(f) }
}