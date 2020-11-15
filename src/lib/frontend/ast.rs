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

/// Variant-specific data for an ast expression node
#[allow(missing_docs)]
pub enum ExprData<'src> {
  Nil,
  Number(Number),
  Boolean(bool),
  Character(char),
  String(&'src str),
  Identifier(&'src str),
  Record(Vec<RecordElement<'src>>),
  Map(Vec<MapElement<'src>>),
  Array(Vec<Expr<'src>>),
  Unary(Operator, Box<Expr<'src>>),
  Binary(Operator, Box<Expr<'src>>, Box<Expr<'src>>),
  Call(Box<Expr<'src>>, Vec<Expr<'src>>),
  Member(Box<Expr<'src>>, &'src str),
  Subscript(Box<Expr<'src>>, Box<Expr<'src>>),
  Function(Function<'src>),
  Conditional(Box<Conditional<'src>>),
  Block(Box<Block<'src>>),
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
      Record(r) => { write!(f,"Record")?; r.fmt(f) },
      Map(m) => { write!(f,"Map")?; m.fmt(f) },
      Array(array) => { write!(f, "Array ")?; f.debug_list().entries(array).finish() },
      Unary(operator, operand) => f.debug_tuple("Unary").field(operator).field(operand).finish(),
      Binary(operator, left, right) => f.debug_tuple("Binary").field(operator).field(left).field(right).finish(),
      Call(callee, arguments) => f.debug_tuple("Call").field(callee).field(arguments).finish(),
      Member(target, field) => f.debug_tuple("Member").field(target).field(&DisplayInDebug(field)).finish(),
      Subscript(target, accessor) => f.debug_tuple("Subscript").field(target).field(accessor).finish(),
      Function(d) => { write!(f, "Function")?; d.fmt(f) },
      Conditional(c) => { write!(f, "Conditional")?; c.fmt(f) },
      Block(b) => { write!(f, "Block")?; b.fmt(f) },
    }
  }
}


/// Variant-specific data for an ast type expression node
#[allow(missing_docs)]
pub enum TyExprData<'src> {
  Nil,
  Identifier(&'src str),
  Record(Vec<ElementDecl<'src>>),
  Map(Box<TyExpr<'src>>, Box<TyExpr<'src>>),
  Array(Box<TyExpr<'src>>),
  Function(Vec<TyExpr<'src>>, Option<Box<TyExpr<'src>>>),
}

impl<'src> fmt::Debug for TyExprData<'src> {
  fn fmt (&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    use TyExprData::*;
    match self {
      Nil => write!(f, "nil"),
      Identifier(identifier) => write!(f, "{}", identifier),
      Record(fields) => { write!(f,"Record")?; f.debug_list().entries(fields).finish() },
      Map(key, val) => f.debug_tuple("Map").field(key).field(val).finish(),
      Array(elem) => f.debug_tuple("Array").field(elem).finish(),
      Function(params, ret) => f.debug_tuple("Function").field(params).field(ret).finish(),
    }
  }
}




/// Variant-specific data for an ast statement node
#[allow(missing_docs)]
pub enum StmtData<'src> {
  Local(&'src str, Option<TyExpr<'src>>, Option<Expr<'src>>),
  Function(&'src str, Function<'src>),
  Type(&'src str, TyExpr<'src>),

  Assign(Expr<'src>, Expr<'src>),

  Expr(Expr<'src>),

  Block(Block<'src>),
  Conditional(Conditional<'src>),
  Loop(Block<'src>),
  
  Return(Option<Expr<'src>>),
  Break,
  Continue,
}

impl<'src> fmt::Debug for StmtData<'src> {
  fn fmt (&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    use StmtData::*;
    match self {
      Local(name, ty, init) => f.debug_tuple("Local").field(name).field(ty).field(init).finish(),
      Function(name, data) => f.debug_tuple("Function").field(name).field(data).finish(),
      Type(name, ty) => f.debug_tuple("Type").field(name).field(ty).finish(),
      
      Assign(target, value) => f.debug_tuple("Assign").field(target).field(value).finish(),
      
      Expr(e) => { write!(f,"Expr")?; e.fmt(f) },

      Block(b) => { write!(f,"Block")?; b.fmt(f) },
      Conditional(c) => { write!(f,"Conditional")?; c.fmt(f) },
      Loop(l) => { write!(f,"Loop")?; l.fmt(f) },

      Return(r) => { write!(f,"Expr")?; r.fmt(f) },
      Break => write!(f, "Break"),
      Continue => write!(f, "Continue")
    }
  }
}


/// Variant-specific data for an ast item node
#[allow(missing_docs)]
pub enum ItemData<'src> {
  Global(&'src str, TyExpr<'src>, Option<Expr<'src>>),
  Function(&'src str, Function<'src>),
  Type(&'src str, TyExpr<'src>),
  Import(&'src str),
  Export(Box<Item<'src>>)
}

impl<'src> fmt::Debug for ItemData<'src> {
  fn fmt (&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    use ItemData::*;
    match self {
      Global(name, ty, init) => f.debug_tuple("Local").field(name).field(ty).field(init).finish(),
      Function(name, data) => f.debug_tuple("Function").field(name).field(data).finish(),
      Type(name, ty) => f.debug_tuple("Type").field(name).field(ty).finish(),
      Import(name) => write!(f, "Import {}", name),
      Export(item) => { write!(f, "Export ")?; item.fmt(f) }
    }
  }
}



/// Allows genericly constructing ast nodes from their data and location
pub trait Node {
  /// The type of the data component used by a node type
  type Data;

  /// Create a node from its components
  fn create (data: Self::Data, loc: Loc) -> Self;

  /// Get the data of a node
  fn get_data (&self) -> &Self::Data;

  /// Get the loc of a node
  fn get_loc (&self) -> Loc;

  /// Convert a Node into a generic tuple of (Data, Loc)
  fn into_tuple (self) -> (Self::Data, Loc);
}

macro_rules! mk_node {
  ($(
    $(#[$meta:meta])*
    $name:ident<$life:lifetime> => $data:ty
  ),* $(,)?) => { $(
    $(#[$meta])*
    pub struct $name<$life> {
      /// Variant specific data for this ast node
      pub data: $data,
      /// The location in source this ast node originated from
      pub loc: Loc
    }

    impl<$life> fmt::Debug for $name<$life> {
      fn fmt (&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.data.fmt(f)?;
    
        if f.sign_minus() { self.loc.fmt(f)?; }
    
        Ok(())
      }
    }

    impl<$life> Node for $name<$life> {
      type Data = $data;

      fn create (data: Self::Data, loc: Loc) -> Self { Self { data, loc } }

      fn get_data (&self) -> &Self::Data { &self.data }
      fn get_loc (&self) -> Loc { self.loc }

      fn into_tuple (self) -> (Self::Data, Loc) { (self.data, self.loc) }
    }
  )* };
}

mk_node! {
  /// A grammar node representing an item in a module
  Item<'src> => ItemData<'src>,

  /// A grammar node representing an action
  Stmt<'src> => StmtData<'src>,

  /// A grammar node representing a value or actions yielding a value
  Expr<'src> => ExprData<'src>,

  /// A grammar node representing a type
  TyExpr<'src> => TyExprData<'src>,


  /// A list of statements followed by an optional trailing expression
  Block<'src> => (Vec<Stmt<'src>>, Option<Expr<'src>>),

  /// A conditional branch
  Branch<'src> => (Expr<'src>, Block<'src>),

  /// A collection of conditional branches followed by an optional else block
  Conditional<'src> => (Vec<Branch<'src>>, Option<Block<'src>>),

  /// Represents an elemental declaration, e.g. a function parameter or record field
  ElementDecl<'src> => (&'src str, TyExpr<'src>),

  /// Represents an element in a map literal
  MapElement<'src> => (Expr<'src>, Expr<'src>),

  /// Represents an element in a record literal
  RecordElement<'src> => (&'src str, Expr<'src>),

  /// Parameters, return type, and body of a function
  Function<'src> => (Vec<ElementDecl<'src>>, Option<Box<TyExpr<'src>>>, Box<Block<'src>>),
}


impl<'src> Block<'src> {
  /// Determine if a Block contains a trailing expression
  pub fn is_expr (&self) -> bool {
    self.data.1.is_some()
  }
}

impl<'src> Branch<'src> {
  /// Determine if a Branch's Block contains a trailing expression
  pub fn is_expr (&self) -> bool {
    self.data.1.is_expr()
  }
}

impl<'src> Conditional<'src> {
  /// Determine if any of a Conditional's descendant blocks contain a trailing expression
  pub fn is_expr (&self) -> bool {
    let mut is_expr = false;
    for branch in self.data.0.iter() {
      is_expr |= branch.is_expr();
      if is_expr { break }
    }

    is_expr || self.data.1.as_ref().map(|else_block| else_block.is_expr()).unwrap_or(false)
  }
}
