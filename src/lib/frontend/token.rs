//! Contains lexical token implementation

use std::fmt;

use super::common::{ Operator, Loc };

/// Wraps an erroneous byte when creating an error token
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UnrecognizedByte(pub u8);

impl fmt::Debug for UnrecognizedByte {
  fn fmt (&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if self.0.is_ascii() { write!(f, "{:?}", self.0 as char) }
    else { write!(f, "byte({:x?})", self.0) }
  }
}

/// Variant-specific data associated with a Token
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TokenData<'src> {
  Identifier(&'src str),
  Number(&'src str),
  Operator(Operator),

  Error(UnrecognizedByte)
}

/// An atom of source
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Token<'src> {
  pub data: TokenData<'src>,
  pub loc: Loc
}