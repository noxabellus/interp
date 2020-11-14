//! Contains the abstract syntax tree implementation

use std::{ fmt, str };

use crate::utils::DisplayInDebug;
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
      // using debug fmt because display for reals doesnt
      // print `.0` on nums with no fractional part
      Number::Real(real) => write!(f, "{:?}", real),
      Number::Integer(int) => write!(f, "{:?}", int)
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
#[allow(missing_docs)]
pub enum ExprData<'src> {
  Nil,
  Number(Number),
  Boolean(bool),
  Character(char),
  String(&'src str),
  Identifier(&'src str),
  Array(Vec<Expr<'src>>),
  Unary(Operator, Box<Expr<'src>>),
  Binary(Operator, Box<Expr<'src>>, Box<Expr<'src>>),
  Call(Box<Expr<'src>>, Vec<Expr<'src>>),
  Member(Box<Expr<'src>>, &'src str),
  Subscript(Box<Expr<'src>>, Box<Expr<'src>>)
}


impl<'src> fmt::Debug for ExprData<'src> {
  fn fmt (&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    use ExprData::*;
    match self {
      Nil => write!(f, "nil"),
      Number(number) => write!(f, "{}", number),
      Boolean(boolean) => write!(f, "{}", boolean),
      Character(ch) => write!(f, "'{}'", ch.escape_default()),
      String(string) => write!(f, "\"{}\"", string.escape_default()),
      Identifier(identifier) => write!(f, "{}", identifier),
      Array(array) => f.debug_list().entries(array).finish(),
      Unary(operator, operand) => f.debug_tuple("Unary").field(operator).field(operand).finish(),
      Binary(operator, left, right) => f.debug_tuple("Binary").field(operator).field(left).field(right).finish(),
      Call(callee, arguments) => f.debug_tuple("Call").field(callee).field(arguments).finish(),
      Member(target, field) => f.debug_tuple("Member").field(target).field(&DisplayInDebug(field)).finish(),
      Subscript(target, accessor) => f.debug_tuple("Subscript").field(target).field(accessor).finish(),
    }
  }
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
      if f.sign_minus() { write!(f, "{:-#?}", self.data) }
      else { write!(f, "{:#?}", self.data) }
    } else if f.sign_minus() { write!(f, "{:-?}", self.data) }
    else { write!(f, "{:?}", self.data) }
    ?;

    if f.sign_minus() { write!(f, " {:?}", self.loc)?; }

    Ok(())
  }
}