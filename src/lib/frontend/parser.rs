//! Parser implementation

use std::{
  str, fmt,
  iter::Peekable,
};

use macros::{ matcher, option_matcher, c_enum };

use crate::utils::{ UnescapedChar, /* UnescapedString */ };

use super::{
  common::{ Operator, Keyword, Constant, Loc },
  lexer::{ TokenIter, Lexical },
  token::{ Token, TokenData, TokenErr, },
  ast::{ Number, Function, * },
};


use TokenData::*;
use Operator::*;
use Keyword::*;
use Constant::*;
use Number::*;


/// Wraps a TokenIter to allow syntax analysis
pub struct Parser<'src> {
  base: Peekable<TokenIter<'src>>
}

impl<'src> Parser<'src> {
  /// Create a new Parser from a Lexical collection
  pub fn new<L: Lexical<'src>> (src: L) -> Self {
    Self { base: src.lex().peekable() }
  }

  /// Attempt to parse a complete TyExpr ast node
  pub fn ty_expr (&mut self) -> ParseResult<TyExpr<'src>> {
    self::ty_expr(self)
  }

  /// Attempt to parse a complete Expr ast node
  pub fn expr (&mut self) -> ParseResult<Expr<'src>> {
    self::expr(self)
  }

  /// Attempt to parse a complete Stmt ast node
  pub fn stmt (&mut self) -> ParseResult<Stmt<'src>> {
    self::stmt(self)
  }

  /// Attempt to parse a complete Item ast node
  pub fn item (&mut self) -> ParseResult<Item<'src>> {
    self::item(self)
  }

  /// Attempt to parse all Item ast nodes available
  pub fn items (&mut self) -> ParseResult<Vec<Item<'src>>> {
    let mut out = vec![];

    loop {
      match self.item() {
        Value(v) => out.push(v),
        Problem(e) => return Problem(e),
        Nothing => break
      }
    }

    Value(out)
  }


  /// Determine if there are any remaining Tokens left to parse
  pub fn is_finished (&mut self) -> bool {
    self.base.peek().is_none()
  }


  fn problem (&mut self, msg_if_not_at_end: &'static str) -> ParseErr {
    let msg = if self.base.peek().is_some() { msg_if_not_at_end }
    else {
      "Unexpected end of input"
    };

    ParseErr { data: ParseErrData::Syntactic(msg), loc: self.base.peek().map(|&Token { loc, .. }| loc) }
  }

  fn unexpected (&mut self) -> ParseErr {
    self.problem("Unexpected symbol")
  }
}

/// Display wrapper for formatting parser errors, produced by `Parser::display_error`
pub struct ParseErrDisplay<'f>(&'f str, ParseErr);

impl<'f> fmt::Display for ParseErrDisplay<'f> {
  fn fmt (&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {

    match self {
      Self(file_name, ParseErr { data, loc: Some(Loc { line, column, .. }) }) 
      => write!(f, "Error at [{}:{}:{}]: {}", file_name, line + 1, column + 1, data),

      Self(file_name, ParseErr { data, loc: None }) 
      => write!(f, "Error in [{} (Location not provided, possibly at EOF)]: {}", file_name, data)
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

/// Variant specific data for a syntactic error
#[derive(Debug)]
pub enum ParseErrData {
  /// An error with the formation of Tokens
  Lexical(TokenErr),
  /// An error with the arrangement of Tokens
  Syntactic(&'static str)
}

impl fmt::Display for ParseErrData {
  fn fmt (&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Lexical(e) => write!(f, "{}", e),
      Self::Syntactic(s) => write!(f, "{}", s)
    }
  }
}

/// Represents a syntactic error
#[derive(Debug)]
pub struct ParseErr {
  data: ParseErrData,
  loc: Option<Loc>
}

impl ParseErr {
  /// Wrap an error in a formatting struct for display
  pub fn display (self, file_name: &str) -> ParseErrDisplay {
    ParseErrDisplay(file_name, self)
  }
}


/// The outcome of attempted parsing of a portion of a token stream
#[derive(Debug)]
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


macro_rules! into_option {
  ($expr:expr) => {
    match $expr {
      Value(v) => Some(v),
      Problem(e) => return Problem(e),
      Nothing => None
    }
  };

  ($expr:expr, |$value:ident| $convert:expr) => {
    match $expr {
      Value($value) => Some($convert),
      Problem(e) => return Problem(e),
      Nothing => None
    }
  };

  ($expr:expr, $convert:expr) => {
    match $expr {
      Value(_) => Some($convert),
      Problem(e) => return Problem(e),
      Nothing => None
    }
  };
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

impl Into<ParseErrData> for &'static str {
  fn into (self) -> ParseErrData {
    ParseErrData::Syntactic(self)
  }
}

impl Into<ParseErrData> for TokenErr {
  fn into (self) -> ParseErrData {
    ParseErrData::Lexical(self)
  }
}

fn mk_problem<T> (data: impl Into<ParseErrData>, loc: Loc) -> ParseResult<T> {
  let data = data.into();
  #[cfg(debug_assertions)] {
    println!("Making ParseResult::Problem({}) @ {}:{}", data, loc.line, loc.column)
  }
  Problem(ParseErr { data, loc: Some(loc) })
}

macro_rules! unwrap {
  ($expr:expr, $problem:expr, $loc:expr) => {
    match $expr {
      Value(v) => v,
      Problem(e) => return Problem(e),
      Nothing => {
        return mk_problem(ParseErr::Syntactic($problem), loc)
      }
    }
  };

  ($expr:expr, $problem:expr) => {
    match $expr {
      Value(v) => v,
      Problem(e) => return Problem(e),
      Nothing => {
        return Problem($problem)
      }
    }
  }
}



fn peek<'src, 'res, T: 'res> (it: &mut Parser<'src>, f: impl FnOnce (TokenData<'res>) -> Option<T>) -> ParseResult<T>
where 'src: 'res
{
  if let Some(&Token { data, loc }) = it.base.peek() {
    match data {
      Error(e) => return mk_problem(e, loc),
      
      _ => if let Some(value) = f(data) {
        return Value(value)
      }
    }
  }

  Nothing
}

fn check (it: &mut Parser, f: impl FnOnce (&TokenData) -> bool) -> bool {
  if let Some(Token { data, .. }) = it.base.peek() {
    f(data)
  } else { false }
}

fn consume<'src, 'res, T: 'res> (it: &mut Parser<'src>, f: impl FnOnce (TokenData<'res>) -> Option<T>) -> ParseResult<(T, Loc)>
where 'src: 'res
{
  if let Some(&Token { data, loc }) = it.base.peek() {
    match data {
      Error(e) => return mk_problem(e, loc),
      
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
      Error(e) => return mk_problem(e, loc),
      
      _ => if let Some(int) = f(data) {
        return match int.parse() {
          Ok(out) => {
            it.base.next();
            Value((out, loc))
          },
          Err(_) => mk_problem(ParseErrData::Syntactic(on_fail), loc)
        }
      }
    }
  }
  
  Nothing
}


const fn node_builder<U, T: Node> (x: impl FnOnce (U) -> <T as Node>::Data) -> impl FnOnce ((U, Loc)) -> T {
  move |(data, loc)| T::create(x(data), loc)
}

fn build_node<T: Node> (data: <T as Node>::Data, loc: Loc) -> T {
  T::create(data, loc)
}

fn wrap_node<T: Node, U: Node> (data: T, f: impl FnOnce(T) -> <U as Node>::Data) -> U {
  let loc = data.get_loc();
  U::create(f(data), loc)
}

fn wrap_box_node<T: Node, U: Node> (data: T, f: impl FnOnce(Box<T>) -> <U as Node>::Data) -> U {
  let loc = data.get_loc();
  U::create(f(box data), loc)
}





fn identifier_raw<'src> (it: &mut Parser<'src>) -> ParseResult<(&'src str, Loc)> {
  consume(it, option_matcher!(Identifier(identifier) => identifier))
}

fn string_raw<'src> (it: &mut Parser<'src>) -> ParseResult<(&'src str, Loc)> {
  consume(it, option_matcher!(String(string) => string))
  // consume_parsed(it,
  //   "Invalid sequence in string literal",
  //   option_matcher!(String(string) => string)
  // ).map(|(us, loc): (UnescapedString, Loc)| (us.inner(), loc))
}

fn number_raw (it: &mut Parser) -> ParseResult<(Number, Loc)> {
  consume_parsed(it, 
    "Invalid number literal",
    option_matcher!(Number(number) => number)
  )
}

fn boolean_raw (it: &mut Parser) -> ParseResult<(bool, Loc)> {
  consume_parsed(it, 
    "Invalid boolean literal",
    option_matcher!(Identifier(boolean @ ("true" | "false")) => boolean)
  )
}

fn character_raw (it: &mut Parser) -> ParseResult<(char, Loc)> {
  consume_parsed(it,
    "Invalid character literal",
    option_matcher!(Character(ch) => ch)
  ).map(|(uch, loc): (UnescapedChar, Loc)| (uch.inner(), loc))
}


const fn from_raw<'src, U, T: Node>(
  raw: impl FnOnce (&mut Parser<'src>) -> ParseResult<(U, Loc)>,
  x: impl FnOnce (U) -> <T as Node>::Data
) -> impl FnOnce (&mut Parser<'src>) -> ParseResult<T> {
  move |it|
    raw(it).map(node_builder(x))
}

fn any_operator_of<'src> (it: &mut Parser<'src>, allowed: &[Operator]) -> ParseResult<(Operator, Loc)> {
  consume(it, option_matcher!(Operator(op) if allowed.contains(&op) => op))
}

fn operator (it: &mut Parser, op: Operator) -> ParseResult<(Operator, Loc)> {
  consume(it, option_matcher!(Operator(found) if found == op => op))
}

fn keyword (it: &mut Parser, kw: Keyword) -> ParseResult<(Keyword, Loc)> {
  consume(it, option_matcher!(Keyword(found) if found == kw => kw))
}

fn constant (it: &mut Parser, cons: Constant) -> ParseResult<(Constant, Loc)> {
  consume(it, option_matcher!(Constant(found) if found == cons => cons))
}

fn any_constant (it: &mut Parser) -> ParseResult<(Constant, Loc)> {
  consume(it, option_matcher!(Constant(cons) => cons))
}


macro_rules! any_of {
  ($it:expr, $first:expr, $($rest:expr),* $(,)?) => { $first($it) $(.or_else(|| $rest($it)))* }
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


mod expr {
  use super::*;

  c_enum! {
    Precedence: u8 {
      FullExpr = 0,
      Cast = 130,
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

  const PREFIX: &[Operator] = &[
    Add, Sub,
    LNot, BNot
  ];


  type InfixInfo = (Operator, Precedence::Repr);
  type InfixFn = for<'src> fn (it: &mut Parser<'src>, left: Expr<'src>, info: InfixInfo) -> ParseResult<Expr<'src>>;
  type InfixEntry = (InfixInfo, InfixFn);

  const INFIX_TABLE: &[InfixEntry] = {
    macro_rules! table {
      ( $( $op:ident @ $prec:ident $($assoc:ident)? => $func:expr ),* $(,)? ) => {
        &[$(((Operator::$op, Precedence::$prec $(+ Associativity::$assoc)?), $func)),*]
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
      
      Colon @ Access => methodize,
      LParen @ Access => call,
      LBrace @ Access => subscript,
      Dot @ Access => member,
      Cast @ Cast => cast,
    ]
  };

  fn expr_in_table (it: &mut Parser, prec: Precedence::Repr) -> Option<InfixEntry> {
    if let Some(&Token { data: Operator(op), .. }) = it.base.peek() {
      for &entry @ ((table_op, op_prec), _) in INFIX_TABLE {
        if op == table_op
        && op_prec > prec {
          it.base.next();

          return Some(entry)
        }
      }
    }

    None
  }

  
  fn string<'src> (it: &mut Parser<'src>) -> ParseResult<Expr<'src>> {
    string_raw(it)
      .map(node_builder(ExprData::String))
  }

  fn number<'src> (it: &mut Parser<'src>) -> ParseResult<Expr<'src>> {
    number_raw(it)
      .map(node_builder(ExprData::Number))
  }

  fn boolean<'src> (it: &mut Parser<'src>) -> ParseResult<Expr<'src>> {
    boolean_raw(it)
      .map(node_builder(ExprData::Boolean))
  }

  fn character<'src> (it: &mut Parser<'src>) -> ParseResult<Expr<'src>> {
    character_raw(it)
      .map(node_builder(ExprData::Character))
  }

  fn identifier_or_path<'src> (it: &mut Parser<'src>) -> ParseResult<Expr<'src>> {
    let (name, loc) = soft_unwrap!(identifier_raw(it));

    if into_option!(operator(it, Path)).is_some() {
      let (sub, _) = unwrap!(identifier_raw(it), it.problem("Expected identifier to follow `::` in path"));

      Value(build_node(ExprData::Path(name, sub), loc))
    } else {
      Value(build_node(ExprData::Identifier(name), loc))
    }
  }

  fn atom<'src> (it: &mut Parser<'src>) -> ParseResult<Expr<'src>> {
    any_of!(it,
      from_raw(any_constant, |cons| match cons {
        Nil => ExprData::Nil,
        Nan => ExprData::Number(Real(f64::NAN)),
        Inf => ExprData::Number(Real(f64::INFINITY)),
      }),
      identifier_or_path,
      number,
      boolean,
      string,
      character,
    )
  }

  fn array<'src> (it: &mut Parser<'src>) -> ParseResult<Expr<'src>> {
    let (_, loc) = soft_unwrap!(operator(it, LBrace));
    let elements = unwrap!(list_body(it, expr, |it| operator(it, Comma)), it.unexpected());
    unwrap!(operator(it, RBrace), it.problem("Expected `]` to close array literal or `,` to separate array elements"));

    Value(build_node(ExprData::Array(elements), loc))
  }

  fn record_expr_element<'src> (it: &mut Parser<'src>) -> ParseResult<RecordElement<'src>> {
    let (key, loc) = soft_unwrap!(identifier_raw(it));
    unwrap!(operator(it, Assign), it.problem("Expected `=` to separate record field name from value"));

    let val = unwrap!(expr(it), it.problem("Expected value expression to follow `=` in record field"));

    Value(build_node((key, val), loc))
  }

  fn record<'src> (it: &mut Parser<'src>) -> ParseResult<Expr<'src>> {
    let (_, loc) = soft_unwrap!(keyword(it, Record));
    
    unwrap!(operator(it, LBracket), it.problem("Expected `{` to begin record literal body"));
    
    let elements = unwrap!(list_body(it, record_expr_element, |it| operator(it, Comma)), it.problem("Expected record field initializer list"));
    
    let (_, err_loc) = unwrap!(operator(it, RBracket), it.problem("Expected `}` to close record literal or `,` to separate record fields"));
    if elements.is_empty() {
      return mk_problem("Expected at least one field for record literal", err_loc);
    }
    
    Value(build_node(ExprData::Record(elements), loc))
  }

  fn map_element<'src> (it: &mut Parser<'src>) -> ParseResult<MapElement<'src>> {
    let key = soft_unwrap!(expr(it));
    let loc = key.loc;
    unwrap!(operator(it, Assign), it.problem("Expected `=` to separate map element key from value"));

    let val = unwrap!(expr(it), it.problem("Expected value expression to follow `=` in map element"));

    Value(build_node((key, val), loc))
  }

  fn map<'src> (it: &mut Parser<'src>) -> ParseResult<Expr<'src>> {
    let (_, loc) = soft_unwrap!(keyword(it, Map));
    unwrap!(operator(it, LBracket), it.problem("Expected `{` to begin map literal body"));

    let elements = unwrap!(list_body(it, map_element, |it| operator(it, Comma)), it.problem("Expected map element initializer list"));
    unwrap!(operator(it, RBracket), it.problem("Expected `}` to close map literal or `,` to separate map elements"));

    Value(build_node(ExprData::Map(elements), loc))
  }

  fn function<'src> (it: &mut Parser<'src>) -> ParseResult<Expr<'src>> {
    let (_, loc) = soft_unwrap!(keyword(it, Function));

    if check(it, matcher!(Identifier(_))) {
      return Problem(it.problem("Anonymous function expression should not have a name"))
    }

    let content = unwrap!(function_content(it, loc), it.problem("Expected a function body for anonymous function expression"));

    Value(build_node(ExprData::Function(content), loc))
  }

  fn conditional<'src> (it: &mut Parser<'src>) -> ParseResult<Expr<'src>> {
    let cond = soft_unwrap!(super::conditional(it));

    if !cond.is_expr() {
      return mk_problem("Expected an expression", cond.loc)
    }

    Value(wrap_box_node(cond, ExprData::Conditional))
  }

  fn block<'src> (it: &mut Parser<'src>) -> ParseResult<Expr<'src>> {
    let blk = soft_unwrap!(super::block(it));

    if !blk.is_expr() {
      return mk_problem("Expected an expression", blk.loc)
    }

    Value(wrap_box_node(blk, ExprData::Block))
  }

  fn sem_group<'src> (it: &mut Parser<'src>) -> ParseResult<Expr<'src>> {
    let (_, loc) = soft_unwrap!(operator(it, LParen));
    let mut inner = unwrap!(expr(it), it.problem("Expected an expression inside semantic grouping `()`"));
    inner.loc = loc;

    unwrap!(operator(it, RParen), it.problem("Expected `)` to close semantic grouping expression"));

    Value(inner)
  }

  fn unary<'src> (it: &mut Parser<'src>) -> ParseResult<Expr<'src>> {
    let (operator, loc) = soft_unwrap!(any_operator_of(it, PREFIX));

    let operand = unwrap!(pratt(it, Precedence::Unary), it.unexpected());

    Value(build_node(ExprData::Unary(operator, box operand), loc))
  }

  fn prefix<'src> (it: &mut Parser<'src>) -> ParseResult<Expr<'src>> {
    any_of!(it,
      atom,
      sem_group,
      unary,
      conditional,
      block,
      array,
      record,
      map,
      function,
    )
  }



  fn binary<'src> (it: &mut Parser<'src>, left: Expr<'src>, (op, prec): InfixInfo) -> ParseResult<Expr<'src>> {
    let loc = left.loc;
    let right = unwrap!(pratt(it, prec), it.unexpected());

    Value(build_node(ExprData::Binary(op, box left, box right), loc))
  }

  fn methodize<'src> (it: &mut Parser<'src>, left: Expr<'src>, (_, prec): InfixInfo) -> ParseResult<Expr<'src>> {
    let loc = left.loc;
    let right = unwrap!(pratt(it, prec), it.unexpected());

    Value(build_node(ExprData::Methodize(box left, box right), loc))
  }

  fn call<'src> (it: &mut Parser<'src>, left: Expr<'src>, _: InfixInfo) -> ParseResult<Expr<'src>> {
    let loc = left.loc;
    let args = unwrap!(list_body(it, expr, |it| operator(it, Comma)), it.unexpected());
    unwrap!(operator(it, RParen), it.problem("Expected `)` to close argument list or `,` to separate arguments"));

    Value(build_node(ExprData::Call(box left, args), loc))
  }

  fn subscript<'src> (it: &mut Parser<'src>, left: Expr<'src>, _: InfixInfo) -> ParseResult<Expr<'src>> {
    let loc = left.loc;
    let accessor = unwrap!(expr(it), it.problem("Expected a subscript accessor expression"));
    unwrap!(operator(it, RBrace), it.problem("Expected `]` to close subscript accessor expression"));

    Value(build_node(ExprData::Subscript(box left, box accessor), loc))
  }

  fn member<'src> (it: &mut Parser<'src>, left: Expr<'src>, _: InfixInfo) -> ParseResult<Expr<'src>> {
    let loc = left.loc;
    let (field, _) = unwrap!(identifier_raw(it), it.problem("Expected an identifier"));

    Value(build_node(ExprData::Member(box left, field), loc))
  }

  fn cast<'src> (it: &mut Parser<'src>, left: Expr<'src>, _: InfixInfo) -> ParseResult<Expr<'src>> {
    let loc = left.loc;
    let ty = unwrap!(ty_expr(it), it.problem("Expected type expression to follow `as` in cast"));

    Value(build_node(ExprData::Cast(box left, ty), loc))
  }

  fn pratt<'src> (it: &mut Parser<'src>, prec: Precedence::Repr) -> ParseResult<Expr<'src>> {
    let mut left = soft_unwrap!(prefix(it));

    while let Some((info, expr_fn)) = expr_in_table(it, prec) {
      left = unwrap!(expr_fn(it, left, info), it.problem("Expected right hand side for operator"));
    }

    Value(left)
  }

  pub fn expr<'src> (it: &mut Parser<'src>) -> ParseResult<Expr<'src>> {
    pratt(it, Precedence::FullExpr)
  }
}

use expr::expr;



mod ty_expr {
  use super::*;

  fn record<'src> (it: &mut Parser<'src>) -> ParseResult<TyExpr<'src>> {
    let (_, loc) = soft_unwrap!(keyword(it, Record));

    unwrap!(operator(it, LBracket), it.problem("Expected `{` to begin record type body"));

    let fields = unwrap!(list_body(it, element_decl, |it| operator(it, Comma)), it.problem("Expected fields for record type"));
    
    let (_, err_loc) = unwrap!(operator(it, RBracket), it.problem("Expected `}` to end record field list or `,` to separate fields"));
    if fields.is_empty() {
      return mk_problem("Expected at least one field for record type", err_loc)
    }


    Value(build_node(TyExprData::Record(fields), loc))
  }

  fn map<'src> (it: &mut Parser<'src>) -> ParseResult<TyExpr<'src>> {
    let (_, loc) = soft_unwrap!(operator(it, LBracket));

    let key = unwrap!(ty_expr(it), it.problem("Expected map key type expression to follow `{`"));
    unwrap!(operator(it, Colon), it.problem("Expected `:` to separate key and value types in map type expression"));

    let val = unwrap!(ty_expr(it), it.problem("Expected map value type expression to follow `:`"));
    unwrap!(operator(it, RBracket), it.problem("Expected `}` to close map type expression"));

    Value(build_node(TyExprData::Map(box key, box val), loc))
  }

  fn array<'src> (it: &mut Parser<'src>) -> ParseResult<TyExpr<'src>> {
    let (_, loc) = soft_unwrap!(operator(it, LBrace));

    let elem = unwrap!(ty_expr(it), it.problem("Expected array element type expression to follow `[`"));
    unwrap!(operator(it, RBrace), it.problem("Expected `]` to close array type expression"));

    Value(build_node(TyExprData::Array(box elem), loc))
  }

  fn function<'src> (it: &mut Parser<'src>) -> ParseResult<TyExpr<'src>> {
    let (_, loc) = soft_unwrap!(keyword(it, Fn));

    let params = if into_option!(operator(it, LParen)).is_some() {
      let elems = unwrap!(list_body(it, ty_expr, |it| operator(it, Comma)), it.problem("Expected parameter type list for function type expression"));
      unwrap!(operator(it, RParen), it.problem("Expected `)` to close parameter list for function type expression"));

      elems
    } else {
      vec![]
    };

    let ret_ty = if into_option!(operator(it, Arrow)).is_some() {
      Some(box unwrap!(ty_expr(it), it.problem("Expected return type to follow `->` in function type expression")))
    } else {
      None
    };

    Value(build_node(TyExprData::Function(params, ret_ty), loc))
  }


  fn identifier_or_path<'src> (it: &mut Parser<'src>) -> ParseResult<TyExpr<'src>> {
    let (name, loc) = soft_unwrap!(identifier_raw(it));

    if into_option!(operator(it, Path)).is_some() {
      let (sub, _) = unwrap!(identifier_raw(it), it.problem("Expected identifier to follow `::` in path"));

      Value(build_node(TyExprData::Path(name, sub), loc))
    } else {
      Value(build_node(TyExprData::Identifier(name), loc))
    }
  }


  pub fn ty_expr<'src> (it: &mut Parser<'src>) -> ParseResult<TyExpr<'src>> {
    any_of!(it,
      from_raw(|it| constant(it, Nil), |_| TyExprData::Nil),
      identifier_or_path,
      record,
      map,
      array,
      function,
    )
  }
}

use ty_expr::ty_expr;



mod stmt {
  use super::*;

  type StmtFn = for<'src> fn (it: &mut Parser<'src>, loc: Loc) -> ParseResult<Stmt<'src>>;
  const STMT_TABLE: &[(Keyword, StmtFn)] = {
    &[
      (Local, local),
      (Type, r#type),
      (Loop, r#loop),
      (Return, r#return),
      (Break, r#break),
      (Continue, r#continue),
    ]
  };

  fn stmt_in_table (it: &mut Parser) -> Option<(StmtFn, Loc)> {
    if let Some(&Token { data: Keyword(kw), loc }) = it.base.peek() {
      for &(table_kw, func) in STMT_TABLE {
        if kw == table_kw {
          it.base.next();

          return Some((func, loc))
        }
      }
    }

    None
  }

  const ASSIGN_OPS: &[Operator] = &[
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    RemAssign,
    PowAssign,
  ];


  fn local<'src> (it: &mut Parser<'src>, loc: Loc) -> ParseResult<Stmt<'src>> {
    let (name, _) = unwrap!(identifier_raw(it), it.problem("Expected name for variable declaration"));

    let ty = if into_option!(operator(it, Colon)).is_some() {
      Some(unwrap!(ty_expr(it), it.problem("Expected explicit type expression to follow `:` in variable declaration")))
    } else {
      None
    };

    let init = if into_option!(operator(it, Assign)).is_some() {
      Some(unwrap!(expr(it), it.problem("Expected initializer value to follow `=` in variable declaration")))
    } else {
      None
    };

    if ty.is_none() && init.is_none() {
      return mk_problem("Variable declaration requires an explicit type, an initial value, or both; found neither", loc)
    }

    Value(build_node(StmtData::Local(name, ty, init), loc))
  }
  
  fn r#type<'src> (it: &mut Parser<'src>, loc: Loc) -> ParseResult<Stmt<'src>> {
    let (name, _) = unwrap!(identifier_raw(it), it.problem("Expected name for type alias"));
    unwrap!(operator(it, Colon), it.problem("Expected `:` to separate name from type expression in alias"));

    let ty = unwrap!(ty_expr(it), it.problem("Expected type expression to follow `:` in alias"));

    Value(build_node(StmtData::Type(name, ty), loc))
  }
  
  fn r#loop<'src> (it: &mut Parser<'src>, loc: Loc) -> ParseResult<Stmt<'src>> {
    Value(build_node(StmtData::Loop(unwrap!(block(it), it.problem("Expected body block for loop"))), loc))
  }
  
  fn r#return<'src> (it: &mut Parser<'src>, loc: Loc) -> ParseResult<Stmt<'src>> {
    Value(build_node(StmtData::Return(into_option!(expr(it))), loc))
  }
  
  fn r#break<'src> (_: &mut Parser<'src>, loc: Loc) -> ParseResult<Stmt<'src>> {
    Value(build_node(StmtData::Break, loc))
  }
  
  fn r#continue<'src> (_: &mut Parser<'src>, loc: Loc) -> ParseResult<Stmt<'src>> {
    Value(build_node(StmtData::Continue, loc))
  }
  


  pub fn stmt<'src> (it: &mut Parser<'src>) -> ParseResult<Stmt<'src>> {
    let left = if let Some((stmt_fn, loc)) = stmt_in_table(it) { // handle unambiguous stmts
      return stmt_fn(it, loc);
    } else if let Some((_, loc)) = into_option!(keyword(it, Function)) { // handle functions
      let name = into_option!(identifier_raw(it));
      let content = unwrap!(function_content(it, loc), it.unexpected());

      if let Some((name, _)) = name { // discern whether function was anonymous (making it an expr)
        return Value(build_node(StmtData::Function(name, content), loc))
      } else {
        build_node(ExprData::Function(content), loc)
      }
    } else if let Some(cond) = into_option!(conditional(it)) { // handle conditionals
      // discern whether any block in conditional had trailing expr
      if cond.is_expr() {
        wrap_box_node(cond, ExprData::Conditional)
      } else {
        return Value(wrap_node(cond, StmtData::Conditional))
      }
    } else if let Some(block) = into_option!(block(it)) { // handle blocks
      if block.is_expr() { // discern whether block had trailing expr, making it an expr
        wrap_box_node(block, ExprData::Block)
      } else {
        return Value(wrap_node(block, StmtData::Block))
      }
    }  else { // handle unambiguous exprs
      soft_unwrap!(expr(it))
    };

    let loc = left.loc;

    if let Some((op,_)) = into_option!(any_operator_of(it, ASSIGN_OPS)) {
      let right = unwrap!(expr(it), it.problem("Expected right hand side for assignment statement"));

      Value(build_node(StmtData::Assign(op, left, right), loc))
    } else {
      Value(wrap_node(left, StmtData::Expr))
    }
  }
}

use stmt::stmt;



fn element_decl<'src> (it: &mut Parser<'src>) -> ParseResult<ElementDecl<'src>> {
  let (name, loc) = soft_unwrap!(identifier_raw(it));
  unwrap!(operator(it, Colon), it.problem("Expected `:` to separate declaration identifier from type"));
  let ty = unwrap!(ty_expr(it), it.unexpected());

  Value(build_node((name, ty), loc))
}


fn function_content<'src> (it: &mut Parser<'src>, loc: Loc) -> ParseResult<Function<'src>> {
  let params = if into_option!(operator(it, LParen)).is_some() {
    let elems = unwrap!(list_body(it, element_decl, |it| operator(it, Comma)), it.problem("Expected a list of function parameters"));
    unwrap!(operator(it, RParen), it.problem("Expected `)` to close function parameter list or `,` to separate parameters"));
    elems
  } else {
    vec![]
  };

  let ret = into_option!(operator(it, Arrow), box unwrap!(ty_expr(it), it.unexpected()));

  let body = box unwrap!(block(it), it.problem("Expected body block for function"));

  Value(build_node((params, ret, body), loc))
}


fn block<'src> (it: &mut Parser<'src>) -> ParseResult<Block<'src>> {
  let (_, loc) = soft_unwrap!(operator(it, LBracket));

  let mut stmts = vec! [];
  let mut trail = None;

  while into_option!(operator(it, RBracket)).is_none()  {
    let stmt_or_expr = unwrap!(stmt(it), it.unexpected());

    let next = into_option!(peek(it, option_matcher!(
      Operator(op @ (Semi | RBracket)) => op
    )));

    match (next, stmt_or_expr) {
      (Some(RBracket), Stmt { data: StmtData::Expr(expr), .. }) => {
        trail = Some(expr);
      },

      (op, stmt) => {
        if matches!(op, Some(Semi)) { it.base.next(); }
        stmts.push(stmt);
      }
    }
  }

  Value(build_node((stmts, trail), loc))
}


fn branch<'src> (it: &mut Parser<'src>) -> ParseResult<Branch<'src>> {
  let (_, loc) = soft_unwrap!(keyword(it, If));
  let predicate = unwrap!(expr(it), it.unexpected());
  let body = unwrap!(block(it), it.unexpected());

  Value(build_node((predicate, body), loc))
}


fn conditional<'src> (it: &mut Parser<'src>) -> ParseResult<Conditional<'src>> {
  let (loc, mut branches) = {
    let first_branch = soft_unwrap!(branch(it));
    (first_branch.loc, vec! [ first_branch ])
  };

  let mut else_block = None;

  loop {
    match keyword(it, Else) {
      Value(_) => match branch(it) {
        Value(branch) => branches.push(branch),
        Problem(e) => return Problem(e),
        Nothing => {
          else_block = Some(unwrap!(block(it), it.problem("Expected `if expr { .. }` or `{ .. }` to follow `else`")));
          break
        }
      },
      Problem(e) => return Problem(e),
      Nothing => break
    }
  }

  Value(build_node((branches, else_block), loc))
}




mod item {
  use super::*;

  pub fn global<'src> (it: &mut Parser<'src>) -> ParseResult<Item<'src>> {
    let (_, loc) = soft_unwrap!(keyword(it, Global));
    let (name, _) = unwrap!(identifier_raw(it), it.problem("Expected name for global declaration"));

    unwrap!(operator(it, Colon), it.problem("Expected `:` to separate name from type expression in global declaration"));

    let ty = unwrap!(ty_expr(it), it.problem("Expected type expression to follow `:` in global declaration"));

    let init = if into_option!(operator(it, Assign)).is_some() {
      Some(unwrap!(expr(it), it.problem("Expected initializer expression to follow `=` in global declaration")))
    } else {
      None
    };

    Value(build_node(ItemData::Global(name, ty, init), loc))
  }
  
  pub fn function<'src> (it: &mut Parser<'src>) -> ParseResult<Item<'src>> {
    let (_, loc) = soft_unwrap!(keyword(it, Function));
    let (name, _) = unwrap!(identifier_raw(it), it.problem("Expected name for function declaration"));
    let content = unwrap!(function_content(it, loc), it.problem("Expected body for function declaration"));

    Value(build_node(ItemData::Function(name, content), loc))
  }
  
  pub fn r#type<'src> (it: &mut Parser<'src>) -> ParseResult<Item<'src>> {
    let (_, loc) = soft_unwrap!(keyword(it, Type));
    let (name, _) = unwrap!(identifier_raw(it), it.problem("Expected name for type alias"));

    unwrap!(operator(it, Colon), it.problem("Expected `:` to separate name from type expression in alias"));

    let ty = unwrap!(ty_expr(it), it.problem("Expected type expression to follow `:` in alias"));
    
    Value(build_node(ItemData::Type(name, ty), loc))
  }
  
  pub fn import<'src> (it: &mut Parser<'src>) -> ParseResult<Item<'src>> {
    let (_, loc) = soft_unwrap!(keyword(it, Import));
    let (name, _) = unwrap!(identifier_raw(it), it.problem("Expected name of module to import"));

    let sub = if into_option!(operator(it, Path)).is_some() {
      let (s, _) = unwrap!(identifier_raw(it), it.problem("Expected identifier to follow `::` in import path"));
      Some(s)
    } else {
      None
    };

    Value(build_node(ItemData::Import(name, sub), loc))
  }
  
  pub fn export<'src> (it: &mut Parser<'src>) -> ParseResult<Item<'src>> {
    let (_, loc) = soft_unwrap!(keyword(it, Export));
    let exp = unwrap!(item(it), it.problem("Expected item for export"));

    match &exp.data {
      | ItemData::Import { .. }
      | ItemData::Export { .. }
      => mk_problem("Export item cannot contain export or import", exp.loc),
      _ => Value(build_node(ItemData::Export(box exp), loc))
    }
  }
  
  pub fn item<'src> (it: &mut Parser<'src>) -> ParseResult<Item<'src>> {
    any_of!(it,
      global,
      function,
      r#type,
      import,
      export
    )
  }
}

use item::item;