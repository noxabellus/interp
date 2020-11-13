//! Parser implementation

#![allow(dead_code)]

use std::{
  str, fmt, ops,
  iter::Peekable,
};
use macros::{ option_matcher, expand_or_else, c_enum };
use super::{ common::*, lexer::* };

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
#[derive(Debug)]
#[allow(missing_docs)]
pub struct Expr<'src> {
  pub data: ExprData<'src>,
  pub loc: Loc
}


/// Wraps a TokenIter to allow syntax analysis
pub struct ParserIter<'src> {
  base: Peekable<TokenIter<'src>>
}

impl<'src> ops::Deref for ParserIter<'src> {
  type Target = Peekable<TokenIter<'src>>;

  fn deref (&self) -> &Self::Target { &self.base }
}

impl<'src> ops::DerefMut for ParserIter<'src> {
  fn deref_mut (&mut self) -> &mut Self::Target { &mut self.base }
}

impl<'src> ParserIter<'src> {
  /// Create a new ParserIter from a Lexical collection
  pub fn new<L: Lexical<'src>> (src: L) -> Self {
    Self { base: src.lex().peekable() }
  }

  fn consume<'res, T: 'res> (&mut self, f: impl FnOnce (TokenData<'res>) -> Option<T>) -> Option<(T, Loc)>
  where 'src: 'res
  {
    if let Some(&Token { data, loc }) = self.peek() {
      if let Some(value) = f(data) {
        self.next();
        return Some((value, loc))
      }
    }

    None
  }

  fn consume_parsed<'int, T> (&mut self, on_fail: &'static str, f: impl FnOnce (TokenData<'int>) -> Option<&'int str>) -> ParseResult<(T, Loc)>
  where 'src: 'int,
        T: str::FromStr
  {
    match self.consume(f) {
      Some((int, loc)) => match int.parse() {
        Ok(out) => Value((out, loc)),
        Err(_) => Problem(on_fail)
      }
      None => Nothing
    }
  }

  /// Wrap an error in a formatting struct for display
  pub fn display_error<'f> (&mut self, file_name: &'f str, err: &'static str) -> ParseErrDisplay<'f> {
    ParseErrDisplay(file_name, self.peek().map(|tok| tok.loc), err)
  }
}

/// Display wrapper for formatting parser errors, produced by `ParserIter::display_error`
pub struct ParseErrDisplay<'f>(&'f str, Option<Loc>, &'static str);

impl<'f> fmt::Display for ParseErrDisplay<'f> {
  fn fmt (&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self(file_name, Some(Loc { line, column, .. }), err)
      => write!(f, "Error at [{}:{}:{}]: {}", file_name, line + 1, column + 1, err),

      Self(file_name, None, err)
      => write!(f, "Error in [{} (Location not provided, possibly at EOF)]: {}", file_name, err)
    }
  }
}


/// Allows quick conversion of a Lexical value into parsed ast nodes
pub trait Syntactic<'src>: Lexical<'src> {
  /// Wrap a Lexical item in a ParserIter
  fn syn (self) -> ParserIter<'src>;

  /// Try to parse a Syntactic item as an Expr ast
  fn expr (self) -> (ParseResult<Expr<'src>>, ParserIter<'src>);
}

impl<'src, T> Syntactic<'src> for T where T: Lexical<'src> {
  fn syn (self) -> ParserIter<'src> { ParserIter::new(self) }
  fn expr (self) -> (ParseResult<Expr<'src>>, ParserIter<'src>) {
    let mut parser = self.syn();
    (expr(&mut parser), parser)
  }
}


/// The outcome of attempted parsing of a portion of a token stream
pub enum ParseResult<T> {
  /// Parsing succeeded
  Value(T),
  /// Parsing failed due to a syntax error
  Problem(&'static str),
  /// Parsing failed due to no matching value, but was not an error
  Nothing
}

pub use ParseResult::*;

impl<T> ops::Try for ParseResult<T> {
  type Ok = Option<T>;
  type Error = &'static str;

  fn into_result (self) -> Result<Self::Ok, Self::Error> {
    match self {
      Value(v) => Ok(Some(v)),
      Problem(e) => Err(e),
      Nothing => Ok(None)
    }
  }

  fn from_ok (v: Self::Ok) -> Self {
    match v {
      Some(v) => Value(v),
      None => Nothing
    }
  }

  fn from_error (e: Self::Error) -> Self {
    Problem(e)
  }
}

impl<T> From<Option<T>> for ParseResult<T> {
  fn from (opt: Option<T>) -> Self {
    match opt {
      Some(v) => Value(v),
      None => Nothing
    }
  }
}

impl<T> From<Result<T, &'static str>> for ParseResult<T> {
  fn from (opt: Result<T, &'static str>) -> Self {
    match opt {
      Ok(v) => Value(v),
      Err(e) => Problem(e)
    }
  }
}

impl<T> From<Result<Option<T>, &'static str>> for ParseResult<T> {
  fn from (res: Result<Option<T>, &'static str>) -> Self {
    match res {
      Ok(Some(v)) => Value(v),
      Err(e) => Problem(e),
      Ok(None) => Nothing,
    }
  }
}

impl<T> From<Option<Result<T, &'static str>>> for ParseResult<T> {
  fn from (res: Option<Result<T, &'static str>>) -> Self {
    match res {
      Some(Ok(v)) => Value(v),
      Some(Err(e)) => Problem(e),
      None => Nothing,
    }
  }
}

impl<T> ParseResult<T> {
  /// Result/Option map equivalent
  pub fn map<U> (self, f: impl FnOnce (T) -> U) -> ParseResult<U> {
    match self {
      Value(v) => Value(f(v)),
      Problem(e) => Problem(e),
      Nothing => Nothing
    }
  }

  /// Option or_else equivalent, calls closure if there is not a Value or Problem
  pub fn or_else (self, f: impl FnOnce () -> Self) -> Self {
    match self {
      Value(v) => Value(v),
      Problem(e) => Problem(e),
      Nothing => f()
    }
  }

  /// Determine if a ParseResult is the Value variant
  pub fn is_value (&self) -> bool { matches!(self, Value(_)) }

  /// Determine if a ParseResult is the Problem variant
  pub fn is_problem (&self) -> bool { matches!(self, Problem(_)) }

  /// Determine if a ParseResult is the Nothing variant
  pub fn is_nothing (&self) -> bool { matches!(self, Nothing) }
}




const fn mk_expr<'src, T> (x: impl FnOnce (T) -> ExprData<'src>) -> impl FnOnce ((T, Loc)) -> Expr<'src> {
  move |(v, loc)| Expr { data: x(v), loc }
}

macro_rules! any_of {
  ($it:expr, $first:expr, $($rest:expr),*) => { $first($it) $(.or_else(|| $rest($it)))* }
}


fn identifier_raw<'src> (it: &mut ParserIter<'src>) -> ParseResult<(&'src str, Loc)> {
  it.consume(option_matcher!(TokenData::Identifier(identifier) => identifier)).into()
}

fn number_raw (it: &mut ParserIter) -> ParseResult<(Number, Loc)> {
  it.consume_parsed(
    "Invalid number literal",
    option_matcher!(TokenData::Number(number) => number)
  )
}

fn boolean_raw (it: &mut ParserIter) -> ParseResult<(bool, Loc)> {
  it.consume_parsed(
    "Invalid boolean literal",
    option_matcher!(TokenData::Identifier(boolean @ ("true" | "false")) => boolean)
  )
}

fn nil_raw (it: &mut ParserIter) -> ParseResult<((), Loc)> {
  it.consume(option_matcher!(TokenData::Identifier("nil") => ())).into()
}

fn identifier<'src> (it: &mut ParserIter<'src>) -> ParseResult<Expr<'src>> {
  identifier_raw(it)
    .map(mk_expr(ExprData::Identifier))
}

fn number<'src> (it: &mut ParserIter<'src>) -> ParseResult<Expr<'src>> {
  number_raw(it)
    .map(mk_expr(ExprData::Number))
}

fn boolean<'src> (it: &mut ParserIter<'src>) -> ParseResult<Expr<'src>> {
  boolean_raw(it)
    .map(mk_expr(ExprData::Boolean))
}

fn nil<'src> (it: &mut ParserIter<'src>) -> ParseResult<Expr<'src>> {
  nil_raw(it)
    .map(mk_expr(|_| ExprData::Nil))
}

fn atom<'src> (it: &mut ParserIter<'src>) -> ParseResult<Expr<'src>> {
  any_of!(it, identifier, number, boolean, nil)
}

fn any_operator (it: &mut ParserIter) -> ParseResult<(Operator, Loc)> {
  it.consume(option_matcher!(TokenData::Operator(op) => op)).into()
}

fn any_operator_of (it: &mut ParserIter, allowed: &[Operator]) -> ParseResult<(Operator, Loc)> {
  it.consume(option_matcher!(TokenData::Operator(op) if allowed.contains(&op) => op)).into()
}

fn operator (it: &mut ParserIter, op: Operator) -> ParseResult<(Operator, Loc)> {
  it.consume(option_matcher!(TokenData::Operator(found) if found == op => op)).into()
}

fn keyword<'src> (it: &mut ParserIter<'src>, kw: &str) -> ParseResult<(&'src str, Loc)> {
  it.consume(option_matcher!(TokenData::Identifier(id) if id == kw => id)).into()
}




c_enum! {
  enum Precedence: u8 {
    FullExpr = 0,
    Access = 10,
    Exponent = 20,
    Unary = 30,
    Mul = 40,
    Add = 50,
    Bitshift = 60,
    BAnd = 70,
    BXor = 80,
    BOr = 90,
    Comp = 100,
    LAnd = 110,
    LOr = 120,
  }

  enum Associativity: u8 {
    Left = 0,
    Right = 1,
  }
}

const PREFIX: &[Operator] = { use Operator::*; &[
  Add, Sub,
  LNot, BNot
] };

macro_rules! soft_unwrap {
  ($expr:expr) => {
    match $expr {
      Value(v) => v,
      Problem(e) => return Problem(e),
      Nothing => return Nothing
    }
  };
}

macro_rules! unwrap {
  ($expr:expr $(, $problem:expr)?) => {
    match $expr {
      Value(v) => v,
      Problem(e) => return Problem(e),
      Nothing => return Problem(expand_or_else!($($problem)?, "Expected a value"))
    }
  };
}

impl<'src> ParserIter<'src> {
  fn problem_msg (&mut self, msg_if_not_at_end: &'static str) -> &'static str {
    if self.peek().is_some() { msg_if_not_at_end }
    else { "Unexpected end of input" }
  }

  fn unexpected (&mut self) -> &'static str {
    self.problem_msg("Unexpected symbol")
  }
}

fn unary<'src> (it: &mut ParserIter<'src>) -> ParseResult<Expr<'src>> {
  let (operator, loc) = soft_unwrap!(any_operator_of(it, PREFIX));

  let operand = unwrap!(pratt(it, Precedence::Unary), it.unexpected());

  Value(Expr { data: ExprData::Unary(operator, box operand), loc })
}


fn array<'src> (it: &mut ParserIter<'src>) -> ParseResult<Expr<'src>> {
  let (_, loc) = soft_unwrap!(operator(it, Operator::LBrace));
  let elements = unwrap!(list_body(it, expr, |it| operator(it, Operator::Comma)));

  unwrap!(operator(it, Operator::RBrace), "Expected ] to close array literal or , to separate array elements");

  Value(Expr { data: ExprData::Array(elements), loc })
}

fn sem_group<'src> (it: &mut ParserIter<'src>) -> ParseResult<Expr<'src>> {
  let (_, loc) = soft_unwrap!(operator(it, Operator::LParen));
  let mut inner = unwrap!(expr(it), "Expected an expression inside semantic grouping ()");
  inner.loc = loc;

  unwrap!(operator(it, Operator::RParen), "Expected ) to close semantic grouping expression");

  Value(inner)
}

fn prefix<'src> (it: &mut ParserIter<'src>) -> ParseResult<Expr<'src>> {
  any_of!(it, atom, sem_group, array, unary)
}




type InfixFn = for<'src> fn (it: &mut ParserIter<'src>, left: Expr<'src>, info: (Precedence::Repr, Operator)) -> ParseResult<Expr<'src>>;
type InfixEntry = (Operator, Precedence::Repr, InfixFn);

const INFIX_TABLE: &[InfixEntry] = {
  macro_rules! table {
    ( $( $op:ident @ $prec:ident $($assoc:ident)? => $func:expr ),* $(,)? ) => {
      &[$((Operator::$op, Precedence::$prec $(+ Associativity::$assoc)?, $func)),*]
    }
  }

  table! [
    Add @ Add => binary,
    Sub @ Add => binary,
    Mul @ Mul => binary,
    Div @ Mul => binary,
    Rem @ Mul => binary,
    Pow @ Exponent Right => binary,

    Concat @ Bitshift => binary,

    LShift @ Bitshift => binary,
    RShift @ Bitshift => binary,

    Eq @ Comp => binary,
    Ne @ Comp => binary,
    Lt @ Comp => binary,
    Gt @ Comp => binary,
    Le @ Comp => binary,
    Ge @ Comp => binary,
    
    LAnd @ LAnd => binary,
    LOr @ LOr Right => binary,
    BAnd @ BAnd => binary,
    BOr @ BOr => binary,
    BXOr @ BXor => binary,

    LParen @ Access => call,
    LBrace @ Access => subscript,
    Dot @ Access => member,
  ]
};


fn operator_in_table (it: &mut ParserIter, prec: Precedence::Repr, table: &[InfixEntry]) -> Option<InfixEntry> {
  if let Some(&Token { data: TokenData::Operator(op), .. }) = it.peek() {
    for &entry @ (table_op, op_prec, _) in table {
      if op == table_op
      && op_prec >= prec {
        it.next();

        return Some(entry)
      }
    }
  }

  None
}

fn list_body<'src, T, U> (it: &mut ParserIter<'src>, mut element: impl FnMut (&mut ParserIter<'src>) -> ParseResult<T>, mut separator: impl FnMut (&mut ParserIter<'src>) -> ParseResult<U>) -> ParseResult<Vec<T>> {
  let mut items = vec![];

  loop {
    match element(it) {
      Value(v) => items.push(v),
      Problem(e) => return Problem(e),
      Nothing => break
    }

    match separator(it) {
      Value(_) => continue,
      Problem(e) => return Problem(e),
      Nothing => break
    }
  }

  Value(items)
}

fn binary<'src> (it: &mut ParserIter<'src>, left: Expr<'src>, (prec, op): (Precedence::Repr, Operator)) -> ParseResult<Expr<'src>> {
  let loc = left.loc;
  let right = unwrap!(pratt(it, prec), it.unexpected());

  Value(Expr {
    data: ExprData::Binary(op, box left, box right),
    loc
  })
}

fn call<'src> (it: &mut ParserIter<'src>, left: Expr<'src>, _: (Precedence::Repr, Operator)) -> ParseResult<Expr<'src>> {
  let loc = left.loc;
  let args = unwrap!(list_body(it, expr, |it| operator(it, Operator::Comma)));

  unwrap!(operator(it, Operator::RParen), "Expected ) to close argument list or , to separate arguments");

  Value(Expr { data: ExprData::Call(box left, args), loc })
}

fn subscript<'src> (it: &mut ParserIter<'src>, left: Expr<'src>, _: (Precedence::Repr, Operator)) -> ParseResult<Expr<'src>> {
  let loc = left.loc;
  let accessor = unwrap!(expr(it), "Expected a subscript accessor expression");

  unwrap!(operator(it, Operator::RBrace), "Expected ] to close subscript accessor expression");

  Value(Expr { data: ExprData::Subscript(box left, box accessor), loc })
}

fn member<'src> (it: &mut ParserIter<'src>, left: Expr<'src>, _: (Precedence::Repr, Operator)) -> ParseResult<Expr<'src>> {
  let loc = left.loc;
  let (field, _) = unwrap!(identifier_raw(it), "Expected an identifier");

  Value(Expr { data: ExprData::Member(box left, field), loc })
}



fn pratt<'src> (it: &mut ParserIter<'src>, prec: Precedence::Repr) -> ParseResult<Expr<'src>> {
  let mut left = soft_unwrap!(prefix(it));

  while let Some((op, op_prec, op_fn)) = operator_in_table(it, prec, INFIX_TABLE) {
    left = unwrap!(op_fn(it, left, (op_prec, op)));
  }

  Value(left)
}

/// Attempt to parse a complete expression out of a stream
pub fn expr<'src> (it: &mut ParserIter<'src>) -> ParseResult<Expr<'src>> {
  pratt(it, Precedence::FullExpr)
}