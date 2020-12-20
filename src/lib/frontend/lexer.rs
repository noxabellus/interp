//! Lexer implementation

use std::str;

use macros::{ matcher, unchecked_destructure };

use super::{
	common::{ Loc, Operator, Keyword, Constant },
	token::{ Token, TokenData, TokenErr },
};

use std::string::String;
use TokenData::*;
use Operator::*;
use Keyword::*;
use Constant::*;


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

	fn prev (&self) -> Option<u8> {
		self.src.get(self.loc.index.checked_sub(1)?).copied()
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

	fn scan<F: FnMut (u8) -> bool> (&mut self, mut f: F) -> bool {
		loop {
			if let Some(ch) = self.curr() {
				if f(ch) { self.advance() }
				else { return true }
			} else {
				return false
			}
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

		macro_rules! string_body {
			($kind:ident, $delim:literal) => {
				{
					loop {
						if self.scan(matcher!(! $delim)) {
							let prev = self.prev();
							self.advance();
							if prev != Some(b'\\') {
								break
							}
						} else {
							return Some(self.end_tok(Error(TokenErr::Unterminated)))
						}
					}
					
					let mut tok = self.end_tok_with($kind);

					unsafe { unchecked_destructure! {
						&mut tok,
						Token { data: $kind(x), .. } => {
							*x = &(*x)[1..x.len()-1]
						}
					} }

					tok
				}
			}
		}


		fn convert_op (pair: (u8, Option<u8>)) -> Option<(Operator, bool)> {
			Some(match pair {
				(b'-', Some(b'>')) => (Arrow, true),

				(b'+', Some(b'=')) => (AddAssign, true),
				(b'-', Some(b'=')) => (SubAssign, true),
				(b'*', Some(b'=')) => (MulAssign, true),
				(b'/', Some(b'=')) => (DivAssign, true),
				(b'%', Some(b'=')) => (RemAssign, true),
				(b'^', Some(b'=')) => (PowAssign, true),

				(b':', Some(b':')) => (Path, true),

				(b'+', _) => (Add, false),
				(b'-', _) => (Sub, false),
				(b'*', _) => (Mul, false),
				(b'/', _) => (Div, false),
				(b'%', _) => (Rem, false),
				(b'^', _) => (Pow, false),

				(b'.', Some(b'.')) => (Concat, true),

				(b'<', Some(b'<')) => (LShift, true),
				(b'>', Some(b'>')) => (RShift, true),

				(b'=', Some(b'=')) => (Eq, true),
				(b'!', Some(b'=')) => (Ne, true),
				(b'<', Some(b'=')) => (Le, true),
				(b'>', Some(b'=')) => (Ge, true),
				(b'<', _) => (Lt, false),
				(b'>', _) => (Gt, false),

				(b'!', _) => (BNot, false),

				(b'&', _) => (BAnd, false),
				(b'|', _) => (BOr, false),
				(b'~', _) => (BXOr, false),

				(b'=', _) => (Assign, false),

				(b',', _) => (Comma, false),
				(b'.', _) => (Dot, false),
				(b';', _) => (Semi, false),
				(b':', _) => (Colon, false),

				(b'(', _) => (LParen, false),
				(b')', _) => (RParen, false),
				(b'[', _) => (LBrace, false),
				(b']', _) => (RBrace, false),

				(b'{', _) => (LBracket, false),
				(b'}', _) => (RBracket, false),

				_ => return None
			})
		}

		Some(match (first_ch, second_ch) {
			| (b'_', _)
			| (b'a'..=b'z', _)
			| (b'A'..=b'Z', _)
			=> {
				self.scan(matcher!(b'_' | b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9'));
				
				let mut tok = self.end_tok_with(Identifier);

				unsafe { unchecked_destructure! {
					tok,
					Token { data: Identifier(ident), .. } => {
						tok.data = match ident {
							"not" => Operator(LNot),
							"and" => Operator(LAnd),
							"or"  => Operator(LOr),
							"as"  => Operator(Cast),

							"global" => Keyword(Global),
							"export" => Keyword(Export),
							"import" => Keyword(Import),

							"type" => Keyword(Type),
							"local" => Keyword(Local),
							
							"map" => Keyword(Map),
							"record" => Keyword(Record),

							"function" => Keyword(Function),
							"fn" => Keyword(Fn),

							"loop" => Keyword(Loop),
							"if" => Keyword(If),
							"else" => Keyword(Else),

							"return" => Keyword(Return),
							"break" => Keyword(Break),
							"continue" => Keyword(Continue),

							"nil" => Constant(Nil),
							"nan" => Constant(Nan),
							"inf" => Constant(Inf),

							_     => tok.data
						}
					}
				} }

				tok
			},


			| (b'.', Some(b'0'..=b'9'))
			| (b'0'..=b'9', _) => {
				let mut dec = first_ch == b'.';

				self.scan(|ch| {
					if ch == b'.' && !dec {
						dec = true;
						true
					} else {
						ch.is_ascii_digit()
					}
				});

				self.end_tok_with(Number)
			},


			(b'"', _) => { string_body!(String, b'"') },

			(b'\'', _) => { string_body!(Character, b'\'') },

			pair => {
				if let Some((op, adv)) = convert_op(pair) {
					if adv { self.advance() }
					self.end_tok(TokenData::Operator(op))
				} else {
					self.end_tok(Error(TokenErr::UnrecognizedByte(first_ch)))
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