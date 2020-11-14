//! Parser implementation

#![allow(dead_code)]

use std::{
  str, fmt,
  iter::Peekable,
};

use macros::{ option_matcher, expand_or_else, c_enum };

use crate::utils::{ UnescapedChar, /* UnescapedString */ };

use super::{
  common::{ Operator, Loc },
  lexer::{ TokenIter, Lexical },
  token::{ Token, TokenData, TokenErr, },
  ast::{ Number, Expr, ExprData },
};




/// Wraps a TokenIter to allow syntax analysis
pub struct Parser<'src> {
  base: Peekable<TokenIter<'src>>
}

impl<'src> Parser<'src> {
  /// Create a new Parser from a Lexical collection
  pub fn new<L: Lexical<'src>> (src: L) -> Self {
    Self { base: src.lex().peekable() }
  }

  /// Wrap an error in a formatting struct for display
  pub fn display_error<'f> (&'f mut self, file_name: &'f str, err: ParseErr) -> ParseErrDisplay<'f> {
    ParseErrDisplay(file_name, self.base.peek().map(|tok| tok.loc), err)
  }

  /// Attempt to parse a complete Expr ast node
  pub fn expr (&mut self) -> ParseResult<Expr<'src>> {
    self::expr(self)
  }

  /// Determine if there are any remaining Tokens left to parse
  pub fn is_empty (&mut self) -> bool {
    self.base.peek().is_none()
  }

  fn problem_msg (&mut self, msg_if_not_at_end: &'static str) -> &'static str {
    if self.base.peek().is_some() { msg_if_not_at_end }
    else { "Unexpected end of input" }
  }

  fn unexpected (&mut self) -> &'static str {
    self.problem_msg("Unexpected symbol")
  }
}

/// Display wrapper for formatting parser errors, produced by `Parser::display_error`
pub struct ParseErrDisplay<'f>(&'f str, Option<Loc>, ParseErr);

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
pub trait Syntactic<'src> {
  /// Wrap an item yielding Tokens in a Parser to yield ast nodes
  fn syn (self) -> Parser<'src>;
}

impl<'src, T> Syntactic<'src> for T where T: Lexical<'src> {
  fn syn (self) -> Parser<'src> { Parser::new(self) }
}

/// Represents a syntactic error
pub enum ParseErr {
  /// An error with the formation of Tokens
  Lexical(TokenErr),
  /// An error with the arrangement of Tokens
  Syntactic(&'static str)
}

impl fmt::Display for ParseErr {
  fn fmt (&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Lexical(e) => write!(f, "{}", e),
      Self::Syntactic(s) => write!(f, "{}", s)
    }
  }
}


/// The outcome of attempted parsing of a portion of a token stream
pub enum ParseResult<T> {
  /// Parsing succeeded
  Value(T),
  /// Parsing failed due to a syntax error
  Problem(ParseErr),
  /// Parsing failed due to no matching value, but was not an error
  Nothing
}

pub use ParseResult::*;


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


macro_rules! soft_unwrap {
  ($expr:expr) => {
    match $expr {
      Value(v) => v,
      Problem(e) => return Problem(e),
      Nothing => return Nothing
    }
  };
}

fn mk_problem<T> (err: ParseErr) -> ParseResult<T> {
  #[cfg(debug_assertions)] {
    println!("Making ParseResult::Problem({})", err)
  }
  Problem(err)
}

macro_rules! unwrap {
  ($expr:expr $(, $problem:expr)?) => {
    match $expr {
      Value(v) => v,
      Problem(e) => return Problem(e),
      Nothing => {
        return mk_problem(ParseErr::Syntactic(expand_or_else!({ $($problem)? }, { "Expected a value" })))
      }
    }
  };
}


fn consume<'src, 'res, T: 'res> (it: &mut Parser<'src>, f: impl FnOnce (TokenData<'res>) -> Option<T>) -> ParseResult<(T, Loc)>
where 'src: 'res
{
  if let Some(&Token { data, loc }) = it.base.peek() {
    match data {
      TokenData::Error(e) => return Problem(ParseErr::Lexical(e)),
      
      _ => if let Some(value) = f(data) {
        it.base.next();
        return Value((value, loc))
      }
    }
  }

  Nothing
}

fn consume_parsed<'int, 'src: 'int, T: str::FromStr> (it: &mut Parser<'src>, on_fail: &'static str, f: impl FnOnce (TokenData<'int>) -> Option<&'int str>) -> ParseResult<(T, Loc)> {
  if let Some(&Token { data, loc }) = it.base.peek() {
    match data {
      TokenData::Error(e) => return Problem(ParseErr::Lexical(e)),
      
      _ => if let Some(int) = f(data) {
        return match int.parse() {
          Ok(out) => {
            it.base.next();
            Value((out, loc))
          },
          Err(_) => Problem(ParseErr::Syntactic(on_fail))
        }
      }
    }
  }
  
  Nothing
}

const fn mk_expr<'src, T> (x: impl FnOnce (T) -> ExprData<'src>) -> impl FnOnce ((T, Loc)) -> Expr<'src> {
  move |(v, loc)| Expr { data: x(v), loc }
}


fn identifier_raw<'src> (it: &mut Parser<'src>) -> ParseResult<(&'src str, Loc)> {
  consume(it, option_matcher!(TokenData::Identifier(identifier) => identifier))
}

fn string_raw<'src> (it: &mut Parser<'src>) -> ParseResult<(&'src str, Loc)> {
  consume(it, option_matcher!(TokenData::String(string) => string))
  // consume_parsed(it,
  //   "Invalid sequence in string literal",
  //   option_matcher!(TokenData::String(string) => string)
  // ).map(|(us, loc): (UnescapedString, Loc)| (us.inner(), loc))
}

fn number_raw (it: &mut Parser) -> ParseResult<(Number, Loc)> {
  consume_parsed(it, 
    "Invalid number literal",
    option_matcher!(TokenData::Number(number) => number)
  )
}

fn boolean_raw (it: &mut Parser) -> ParseResult<(bool, Loc)> {
  consume_parsed(it, 
    "Invalid boolean literal",
    option_matcher!(TokenData::Identifier(boolean @ ("true" | "false")) => boolean)
  )
}

fn character_raw (it: &mut Parser) -> ParseResult<(char, Loc)> {
  consume_parsed(it,
    "Invalid character literal",
    option_matcher!(TokenData::Character(ch) => ch)
  ).map(|(uch, loc): (UnescapedChar, Loc)| (uch.inner(), loc))
}


fn nil_raw (it: &mut Parser) -> ParseResult<((), Loc)> {
  consume(it, option_matcher!(TokenData::Identifier("nil") => ()))
}

fn any_operator (it: &mut Parser) -> ParseResult<(Operator, Loc)> {
  consume(it, option_matcher!(TokenData::Operator(op) => op))
}

fn any_operator_of<'src> (it: &mut Parser<'src>, allowed: &[Operator]) -> ParseResult<(Operator, Loc)> {
  consume(it, option_matcher!(TokenData::Operator(op) if allowed.contains(&op) => op))
}

fn operator (it: &mut Parser, op: Operator) -> ParseResult<(Operator, Loc)> {
  consume(it, option_matcher!(TokenData::Operator(found) if found == op => op))
}

fn keyword<'src> (it: &mut Parser<'src>, kw: &str) -> ParseResult<(&'src str, Loc)> {
  consume(it, option_matcher!(TokenData::Identifier(id) if id == kw => id))
}


macro_rules! any_of {
  ($it:expr, $first:expr, $($rest:expr),*) => { $first($it) $(.or_else(|| $rest($it)))* }
}


fn list_body<'src, T, U> (
  it: &mut Parser<'src>,
  mut element: impl FnMut (&mut Parser<'src>) -> ParseResult<T>,
  mut separator: impl FnMut (&mut Parser<'src>) -> ParseResult<U>
) -> ParseResult<Vec<T>> {
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


c_enum! {
  Precedence: u8 {
    FullExpr = 0,
    Access = 120,
    Exponent = 110,
    Unary = 100,
    Mul = 90,
    Add = 80,
    Bitshift = 70,
    BAnd = 60,
    BXor = 50,
    BOr = 40,
    Comp = 30,
    LAnd = 20,
    LOr = 10,
  }

  Associativity: u8 {
    Left = 0,
    Right = 1,
  }
}

const PREFIX: &[Operator] = { use Operator::*; &[
  Add, Sub,
  LNot, BNot
] };


type InfixFn = for<'src> fn (it: &mut Parser<'src>, left: Expr<'src>, info: (Precedence::Repr, Operator)) -> ParseResult<Expr<'src>>;
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

fn operator_in_table (it: &mut Parser, prec: Precedence::Repr, table: &[InfixEntry]) -> Option<InfixEntry> {
  if let Some(&Token { data: TokenData::Operator(op), .. }) = it.base.peek() {
    for &entry @ (table_op, op_prec, _) in table {
      if op == table_op
      && op_prec > prec {
        it.base.next();

        return Some(entry)
      }
    }
  }

  None
}



fn identifier<'src> (it: &mut Parser<'src>) -> ParseResult<Expr<'src>> {
  identifier_raw(it)
    .map(mk_expr(ExprData::Identifier))
}

fn string<'src> (it: &mut Parser<'src>) -> ParseResult<Expr<'src>> {
  string_raw(it)
    .map(mk_expr(ExprData::String))
}

fn number<'src> (it: &mut Parser<'src>) -> ParseResult<Expr<'src>> {
  number_raw(it)
    .map(mk_expr(ExprData::Number))
}

fn boolean<'src> (it: &mut Parser<'src>) -> ParseResult<Expr<'src>> {
  boolean_raw(it)
    .map(mk_expr(ExprData::Boolean))
}

fn character<'src> (it: &mut Parser<'src>) -> ParseResult<Expr<'src>> {
  character_raw(it)
    .map(mk_expr(ExprData::Character))
}

fn nil<'src> (it: &mut Parser<'src>) -> ParseResult<Expr<'src>> {
  nil_raw(it)
    .map(mk_expr(|_| ExprData::Nil))
}

fn atom<'src> (it: &mut Parser<'src>) -> ParseResult<Expr<'src>> {
  any_of!(it, identifier, string, number, boolean, character, nil)
}

fn array<'src> (it: &mut Parser<'src>) -> ParseResult<Expr<'src>> {
  let (_, loc) = soft_unwrap!(operator(it, Operator::LBrace));
  let elements = unwrap!(list_body(it, expr, |it| operator(it, Operator::Comma)));

  unwrap!(operator(it, Operator::RBrace), "Expected ] to close array literal or , to separate array elements");

  Value(Expr { data: ExprData::Array(elements), loc })
}

fn sem_group<'src> (it: &mut Parser<'src>) -> ParseResult<Expr<'src>> {
  let (_, loc) = soft_unwrap!(operator(it, Operator::LParen));
  let mut inner = unwrap!(expr(it), "Expected an expression inside semantic grouping ()");
  inner.loc = loc;

  unwrap!(operator(it, Operator::RParen), "Expected ) to close semantic grouping expression");

  Value(inner)
}

fn unary<'src> (it: &mut Parser<'src>) -> ParseResult<Expr<'src>> {
  let (operator, loc) = soft_unwrap!(any_operator_of(it, PREFIX));

  let operand = unwrap!(pratt(it, Precedence::Unary), it.unexpected());

  Value(Expr { data: ExprData::Unary(operator, box operand), loc })
}

fn prefix<'src> (it: &mut Parser<'src>) -> ParseResult<Expr<'src>> {
  any_of!(it, atom, sem_group, array, unary)
}



fn binary<'src> (it: &mut Parser<'src>, left: Expr<'src>, (prec, op): (Precedence::Repr, Operator)) -> ParseResult<Expr<'src>> {
  let loc = left.loc;
  let right = unwrap!(pratt(it, prec), it.unexpected());

  Value(Expr {
    data: ExprData::Binary(op, box left, box right),
    loc
  })
}

fn call<'src> (it: &mut Parser<'src>, left: Expr<'src>, _: (Precedence::Repr, Operator)) -> ParseResult<Expr<'src>> {
  let loc = left.loc;
  let args = unwrap!(list_body(it, expr, |it| operator(it, Operator::Comma)));
  
  unwrap!(operator(it, Operator::RParen), "Expected ) to close argument list or , to separate arguments");

  Value(Expr { data: ExprData::Call(box left, args), loc })
}

fn subscript<'src> (it: &mut Parser<'src>, left: Expr<'src>, _: (Precedence::Repr, Operator)) -> ParseResult<Expr<'src>> {
  let loc = left.loc;
  let accessor = unwrap!(expr(it), "Expected a subscript accessor expression");

  unwrap!(operator(it, Operator::RBrace), "Expected ] to close subscript accessor expression");

  Value(Expr { data: ExprData::Subscript(box left, box accessor), loc })
}

fn member<'src> (it: &mut Parser<'src>, left: Expr<'src>, _: (Precedence::Repr, Operator)) -> ParseResult<Expr<'src>> {
  let loc = left.loc;
  let (field, _) = unwrap!(identifier_raw(it), "Expected an identifier");

  Value(Expr { data: ExprData::Member(box left, field), loc })
}

fn pratt<'src> (it: &mut Parser<'src>, prec: Precedence::Repr) -> ParseResult<Expr<'src>> {
  let mut left = soft_unwrap!(prefix(it));

  while let Some((op, op_prec, op_fn)) = operator_in_table(it, prec, INFIX_TABLE) {
    left = unwrap!(op_fn(it, left, (op_prec, op)));
  }

  Value(left)
}

/// Attempt to parse a complete expression out of a stream
fn expr<'src> (it: &mut Parser<'src>) -> ParseResult<Expr<'src>> {
  pratt(it, Precedence::FullExpr)
}