//! Unwrapper module for easy wildcard importing common items

pub use crate::{
  core::{
    ptr,
    cstr,
    num,
    dynbuf::DynBuf,
    dynmap::DynMap,
    dynstr::{ DynStr, ToDynStr },
    maybe::{ Maybe, Just, Nothing },
    outcome::{ Outcome, Succ, Fail }
  },
  dynbuf, dynformat, pat
};
