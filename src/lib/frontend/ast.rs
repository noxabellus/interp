//! Contains the abstract syntax tree implementation

use std::{ fmt, str };

use super::common::{ Operator, Loc };


/// Either a real (`f64`) or an integer (`i32`)
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum Number {
  Real(f64),
  Integer(i32),
}

impl fmt::Display for Number {
  fn fmt (&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Number::Real(real) => writeln!(f, "{}", real),
      Number::Integer(int) => writeln!(f, "{}", int)
    }
  }
}

impl Default for Number {
  fn default () -> Self { Self::Integer(0) }
}

impl str::FromStr for Number {
  type Err = ();

  fn from_str (s: &str) -> Result<Self, Self::Err> {
    if s.contains('.') { Ok(Number::Real(s.parse().map_err(|_| ())?)) }
    else { Ok(Number::Integer(s.parse().map_err(|_| ())?)) }
  }
}

/// Variant-specific for an ast node
#[derive(Debug)]
#[allow(missing_docs)]
pub enum ExprData<'src> {
  Nil,
  Number(Number),
  Boolean(bool),
  Identifier(&'src str),
  Array(Vec<Expr<'src>>),
  Unary(Operator, Box<Expr<'src>>),
  Binary(Operator, Box<Expr<'src>>, Box<Expr<'src>>),
  Call(Box<Expr<'src>>, Vec<Expr<'src>>),
  Member(Box<Expr<'src>>, &'src str),
  Subscript(Box<Expr<'src>>, Box<Expr<'src>>)
}

/// A grammar node
#[allow(missing_docs)]
pub struct Expr<'src> {
  pub data: ExprData<'src>,
  pub loc: Loc
}

impl<'src> fmt::Debug for Expr<'src> {
  fn fmt (&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if f.alternate() {
      write!(f, "{:#?}", self.data)
    } else{
      write!(f, "{:?}", self.data)
    }?;

    write!(f, " {:?}", self.loc)
  }
}