//! Contains a utility for converting *const c_char to CStr with simplified C-FFI returns

use std::{
  str,
  ffi::CStr
};

pub use std::os::raw::c_char;


use super::{
  maybe::*,
  outcome::*,
};


/// C-compatible version of std::str::Utf8Error
#[repr(C)]
#[derive(Debug)]
pub struct Utf8Error {
  /// The byte offset at which the encoding error occurred
  pub valid_up_to: u64,
  /// The number of bytes that were found to be invalid
  pub error_len: Maybe<u8>
}

impl From<str::Utf8Error> for Utf8Error {
  fn from (e: str::Utf8Error) -> Utf8Error {
    Utf8Error {
      valid_up_to: e.valid_up_to() as _,
      error_len: if let Some(len) = e.error_len() { Just(len as _ ) } else { Nothing }
    }
  }
}

/// Create a str from a *const c_char
/// # Safety
/// Same safety considerations of `Cstr::from_ptr` apply here
/// since we cannot guarantee the given pointer is valid
pub unsafe fn wrap_cstr (p: *const c_char) -> Outcome<&'static str, Utf8Error> {
  match CStr::from_ptr(p).to_str() {
    Ok(cs) => Succ(cs),
    Err(e) => Fail(e.into())
  }
}