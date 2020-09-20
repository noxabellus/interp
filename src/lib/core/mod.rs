//! Contains core data structures used through out Zeta

pub mod ptr;
pub mod cstr;
pub mod num;
pub mod dynbuf;
pub mod dynstr;
pub mod fnv1a;
pub mod dynmap;
pub mod maybe;
pub mod outcome;
mod pat;

pub use crate::dynbuf;
pub use crate::dynformat;
pub use crate::pat;



mod c_test {
  use crate::prelude::*;
  use cstr::*;

  
  #[no_mangle]
  unsafe extern "C" fn zDynStr_from_cstr (p: *const c_char) -> Outcome<DynStr, Utf8Error> {
    wrap_cstr(p).map(|s| s.into())
  }
}