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
#[derive(Debug)]
#[allow(missing_docs)]
pub enum ExprData<'src> {
  Nil,
  Number(Number),
  Boolean(bool),
  Character(char),
  String(&'src str),
  Identifier(&'src str),
  Path(&'src str, &'src str),
  Record(Vec<RecordElement<'src>>),
  Map(Vec<MapElement<'src>>),
  Array(Vec<Expr<'src>>),
  Unary(Operator, Box<Expr<'src>>),
  Binary(Operator, Box<Expr<'src>>, Box<Expr<'src>>),
  Methodize(Box<Expr<'src>>, Box<Expr<'src>>),
  Call(Box<Expr<'src>>, Vec<Expr<'src>>),
  Member(Box<Expr<'src>>, &'src str),
  Subscript(Box<Expr<'src>>, Box<Expr<'src>>),
  Cast(Box<Expr<'src>>, TyExpr<'src>),
  Function(Function<'src>),
  Conditional(Box<Conditional<'src>>),
  Block(Box<Block<'src>>),
}


/// Variant-specific data for an ast type expression node
#[derive(Debug)]
#[allow(missing_docs)]
pub enum TyExprData<'src> {
  Nil,
  Identifier(&'src str),
  Path(&'src str, &'src str),
  Record(Vec<ElementDecl<'src>>),
  Map(Box<TyExpr<'src>>, Box<TyExpr<'src>>),
  Array(Box<TyExpr<'src>>),
  Function(Vec<TyExpr<'src>>, Option<Box<TyExpr<'src>>>),
}


/// Variant-specific data for an ast statement node
#[derive(Debug)]
#[allow(missing_docs)]
pub enum StmtData<'src> {
  Local(&'src str, Option<TyExpr<'src>>, Option<Expr<'src>>),
  Function(&'src str, Function<'src>),
  Type(&'src str, TyExpr<'src>),

  Assign(Operator, Expr<'src>, Expr<'src>),

  Expr(Expr<'src>),

  Block(Block<'src>),
  Conditional(Conditional<'src>),
  Loop(Block<'src>),
  
  Return(Option<Expr<'src>>),
  Break,
  Continue,
}


/// Variant-specific data for an ast item node
#[derive(Debug)]
#[allow(missing_docs)]
pub enum ItemData<'src> {
  Global(&'src str, TyExpr<'src>, Option<Expr<'src>>),
  Function(&'src str, Function<'src>),
  Type(&'src str, TyExpr<'src>),
  Import(&'src str, Option<&'src str>),
  Export(Box<Item<'src>>)
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
