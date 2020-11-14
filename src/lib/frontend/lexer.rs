//! Lexer implementation

use std::str;

use macros::{ matcher, expand_or_else, unchecked_destructure, discard };

use super::{
  common::{ Loc, Operator },
  token::{ UnrecognizedByte, Token, TokenData },
};


/// Iterator providing lexical analysis of a source
pub struct TokenIter<'src> {
  src: &'src [u8],
  mark: Loc,
  loc: Loc
}

impl<'src> TokenIter<'src> {
  /// Create a new TokenIter from a byte slice
  pub fn new (src: &'src [u8]) -> Self {
    Self {
      src,
      mark: Loc::default(),
      loc: Loc::default()
    }
  }

  fn begin_tok (&mut self) -> Option<u8> {
    if let Some(&ch) = self.src.get(self.loc.index) {
      self.mark = self.loc;
      Some(ch)
    } else {
      None
    }
  }

  fn curr (&self) -> Option<u8> {
    self.src.get(self.loc.index).copied()
  }

  fn peek (&self) -> Option<u8> {
    self.src.get(self.loc.index + 1).copied()
  }

  fn span (&self, start: usize) -> &'src [u8] {
    &self.src[start..self.loc.index]
  }

  fn str (&self, start: usize) -> &'src str {
    unsafe { str::from_utf8_unchecked(self.span(start)) }
  }

  fn advance (&mut self) {
    let &ch = unsafe { self.src.get_unchecked(self.loc.index) };
    if ch == b'\n' { self.loc.line += 1; self.loc.column = 0; }
    else { self.loc.column += 1; }
    self.loc.index += 1;
  }

  fn scan<F: FnMut (u8) -> bool> (&mut self, mut f: F) {
    while let Some(ch) = self.curr() {
      if f(ch) { self.advance() }
      else { break }
    }
  }

  fn end_tok (&self, data: TokenData<'src>) -> Token<'src> {
    Token {
      data,
      loc: self.mark
    }
  }

  fn end_tok_with (&self, data_builder: fn (&'src str) -> TokenData<'src>) -> Token<'src> {
    self.end_tok(data_builder(self.str(self.mark.index)))
  }
}



impl<'src> Iterator for TokenIter<'src> {
  type Item = Token<'src>;

  fn next (&mut self) -> Option<Self::Item> {
    self.scan(|ch| ch.is_ascii_whitespace());

    let first_ch = self.begin_tok()?;
    let second_ch = self.peek();

    self.advance();

    macro_rules! lexer_cases {
      (match ($expr:expr) {
        $($(#$op_marker:ident)? ($($pat:tt)*) => $body:tt),*
      }) => {
        #[allow(unused_parens)]
        match $expr {
          $(lexer_cases!(:PAT: $(#$op_marker)? $($pat)*) => lexer_cases!(:BODY: [$(#$op_marker)? $($pat)*] $body)),*
        }
      };
      
      (:PAT: #op $first_ch:literal $($second_ch:literal)?) => { (($first_ch, expand_or_else!({ $(Some($second_ch))? }, { _ }))) };
      (:PAT: $($pat:pat)|+) => { ($($pat)|+) };
      
      (:BODY: [#op $first_ch:literal $($second_ch:literal)?] $op:ident) => { {
        $( discard!($second_ch); self.advance(); )?
        self.end_tok(TokenData::Operator(Operator::$op))
      } };
      (:BODY: [$($tt:tt)+] $body:tt) => { $body };
    }

    Some(lexer_cases! {
      match ((first_ch, second_ch)) {
        ((b'_' | b'a'..=b'z' | b'A'..=b'Z', _)) => {
          self.scan(matcher!(b'_' | b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9'));
          
          let mut tok = self.end_tok_with(TokenData::Identifier);

          unsafe { unchecked_destructure! {
            tok,
            Token { data: TokenData::Identifier(ident), .. } => {
              tok.data = match ident {
                "not" => TokenData::Operator(Operator::LNot),
                "and" => TokenData::Operator(Operator::LAnd),
                "or"  => TokenData::Operator(Operator::LOr),
                _     => tok.data
              }
            }
          } }

          tok
        },

        ((b'.', Some(b'0'..=b'9')) | (b'0'..=b'9', _)) => {
          let mut dec = first_ch == b'.';

          self.scan(|ch| {
            if ch == b'.' && !dec {
              dec = true;
              true
            } else {
              ch.is_ascii_digit()
            }
          });

          self.end_tok_with(TokenData::Number)
        },

        #op (b'+') => Add,
        #op (b'-') => Sub,
        #op (b'*') => Mul,
        #op (b'/') => Div,
        #op (b'%') => Rem,
        #op (b'^') => Pow,

        #op (b'.' b'.') => Concat,

        #op (b'<' b'<') => LShift,
        #op (b'>' b'>') => RShift,

        #op (b'=' b'=') => Eq,
        #op (b'!' b'=') => Ne,
        #op (b'<' b'=') => Le,
        #op (b'>' b'=') => Ge,
        #op (b'<') => Lt,
        #op (b'>') => Gt,

        #op (b'!') => BNot,

        #op (b'&') => BAnd,
        #op (b'|') => BOr,
        #op (b'~') => BXOr,

        #op (b'=') => Assign,
        #op (b',') => Comma,
        #op (b'.') => Dot,
        #op (b';') => Semi,
        #op (b':') => Colon,

        #op (b'(') => LParen,
        #op (b')') => RParen,
        #op (b'[') => LBrace,
        #op (b']') => RBrace,

        #op (b'{') => LBracket,
        #op (b'}') => RBracket,

        (_) => {
          self.end_tok(TokenData::Error(UnrecognizedByte(first_ch)))
        }
      }
    })
  }
}


/// Allows lexical analysis
pub trait Lexical<'src> {
  /// Lexically analyze a source and provide a stream of Token
  fn lex (self) -> TokenIter<'src>;
}


impl<'src> Lexical<'src> for &'src str {
  fn lex (self) -> TokenIter<'src> {
    TokenIter::new(self.as_bytes())
  }
}

impl<'src> Lexical<'src> for &'src [u8] {
  fn lex (self) -> TokenIter<'src> {
    TokenIter::new(self)
  }
}

impl<'src> Lexical<'src> for TokenIter<'src> {
  fn lex (self) -> Self { self }
}


impl<'src> Lexical<'src> for &'src String {
  fn lex (self) -> TokenIter<'src> { self.as_str().lex() }
}