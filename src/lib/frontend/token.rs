//! Contains lexical token implementation

use std::fmt;

use super::common::{ Operator, Keyword, Loc };


/// Represents a lexical error
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TokenErr {
  /// A byte that was not expected in whatever syntactic context it was found
  UnrecognizedByte(u8),
  /// An unterminated Token, usually a string literal
  Unterminated
}


impl fmt::Display for TokenErr {
  fn fmt (&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::UnrecognizedByte(byte) => {
        if byte.is_ascii() { write!(f, "unexpected char {:?}", *byte as char) }
        else { write!(f, "unexpected byte {:x?}", byte) }
      }

      Self::Unterminated => {
        write!(f, "unterminated literal")
      }
    }
  }
}

/// Variant-specific data associated with a Token
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TokenData<'src> {
  Identifier(&'src str),
  Number(&'src str),
  String(&'src str),
  Character(&'src str),

  Operator(Operator),
  Keyword(Keyword),

  Error(TokenErr)
}

/// An atom of source
#[allow(missing_docs)]
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Token<'src> {
  pub data: TokenData<'src>,
  pub loc: Loc
}

impl<'src> fmt::Debug for Token<'src> {
  fn fmt (&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{:?} {:?}", self.data, self.loc)
  }
}