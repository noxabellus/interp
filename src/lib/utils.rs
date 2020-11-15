//! Various utilities

#![allow(dead_code)]

use std::{
  str, fmt,
  borrow::{ Borrow, ToOwned },
  hash::{ Hash, BuildHasher },
  collections::hash_map::{
    HashMap,
    RawEntryMut
  }
};


/// Give an "a" or an "an" depending on whether a string starts with a vowel or not
///
/// Note this will not always give the correct answer, for words like "hour" it will still provide "a"
pub fn a_or_an (word: &str) -> &'static str {
  const VOWELS: &[char] = &['a', 'e', 'i', 'o', 'u'];

  if word.starts_with(VOWELS) { "an" } else { "a" }
}


/// Align an `address` to a multiple of `alignment`
pub fn align_addr (address: usize, alignment: usize) -> usize {
  debug_assert!(alignment.is_power_of_two());
  (address + alignment - 1) & !(alignment - 1)
}

/// Calculate the offset required to align an `address` to a multiple of `alignment`
pub fn get_align_offset (address: usize, alignment: usize) -> usize {
  align_addr(address, alignment) - address
}


/// Allows printing a value in a Debug formatter using its Display formatter
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct DisplayInDebug<T: fmt::Display> (pub T);

impl<T: fmt::Display> fmt::Debug for DisplayInDebug<T> {
  fn fmt (&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.0)
  }
}

impl<T: fmt::Display> fmt::Display for DisplayInDebug<T> {
  fn fmt (&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    fmt::Debug::fmt(self, f)
  }
}


/// An error that occurred while converting an escape code str to a char
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum UnescapeErr {
  /// The str provided didnt contain any data
  Empty,
  /// The escape code did not have enough data
  Incomplete,
  /// The escape code contained too many characters
  TooManyCharacters,
  /// The escape code contained invalid characters
  InvalidSequence,
  /// The escape code parsed, but the str contained more data that was unused
  UnusedData,
}

/// Convert an escape sequence into a char
pub fn unescape_char<I: Iterator<Item = char>> (chars: &mut I) -> Result<char, UnescapeErr> {
  match chars.next() {
    Some('\\') =>  match chars.next() {
      Some('x') => {
        if let Some((x, y)) = chars.next().and_then(|a| chars.next().map(|b| (a,b))) {
          let temp_buf = &[x as u8, y as u8];

          match u8::from_str_radix(unsafe { str::from_utf8_unchecked(temp_buf) }, 16) {
            Ok(ch) if ch.is_ascii() => Ok(ch as char),
            _ => Err(UnescapeErr::InvalidSequence)
          }
        } else {
          Err(UnescapeErr::Incomplete)
        }
      }

      Some('u') => {
        if let Some('{') = chars.next() {
          let mut temp_buf = [0u8; 6];
          let mut len = 0;

          loop {
            match chars.next() {
              Some(ch @ ('a'..='f' | 'A'..='F' | '0'..='9')) => {
                if len < 6 {
                  temp_buf[len] = ch as u8;
                } else {
                  return Err(UnescapeErr::TooManyCharacters)
                }

                len += 1;
              }

              Some('}') => break,

              Some(_) => return Err(UnescapeErr::InvalidSequence),

              None => return Err(UnescapeErr::Incomplete)
            }
          }

          u32::from_str_radix(unsafe { str::from_utf8_unchecked(&temp_buf[..len]) }, 16)
            .ok()
            .and_then(char::from_u32)
            .ok_or(UnescapeErr::InvalidSequence)
        } else {
          Err(UnescapeErr::InvalidSequence)
        }
      }

      Some(ch) => {
        Ok(match ch {
          '0' => '\x00',  // Null terminator
          'a' => '\x07',	// Alert (Beep, Bell) 
          'b' => '\x08',	// Backspace
          'e' => '\x1B',	// Escape character
          'f' => '\x0C',	// Formfeed Page Break
          'n' => '\x0A',	// Newline
          'r' => '\x0D',	// Carriage Return
          't' => '\x09',	// Horizontal Tab
          'v' => '\x0B',	// Vertical Tab

          x @ ('\\' | '\'' | '"') => x,  // literal values

          _ => return Err(UnescapeErr::InvalidSequence)
        })
      },

      None => Err(UnescapeErr::InvalidSequence)
    }

    Some(ch) => Ok(ch),

    None => Err(UnescapeErr::Empty)
  }
}


/// Allows creating values by unescaping characters in a char iterator;
/// implementations are provided for `char` and `String`
pub trait FromUnescape: Sized {
  fn from_unescape<I: IntoIterator<Item = char>> (i: I) -> Result<Self, UnescapeErr>;
}

impl FromUnescape for char {
  fn from_unescape<I: IntoIterator<Item = char>> (i: I) -> Result<Self, UnescapeErr> {
    let mut it = i.into_iter();

    unescape_char(&mut it)
      .and_then(|ch|
        if it.next().is_none() { Ok(ch) }
        else { Err(UnescapeErr::TooManyCharacters) }
      )
  }
}

impl FromUnescape for String {
  fn from_unescape<I: IntoIterator<Item = char>> (i: I) -> Result<Self, UnescapeErr> {
    let mut it = i.into_iter();
    let mut out = String::new();
    
    loop {
      match unescape_char(&mut it) {
        Ok(ch) => out.push(ch),
        Err(UnescapeErr::Empty) => break,
        Err(e) => return Err(e)
      }
    }

    Ok(out)
  }
}


/// Uses the FromUnescape trait to provided FromStr capability utilizing `unescape_char` to generate `char`
pub struct UnescapedChar(char);

impl str::FromStr for UnescapedChar {
  type Err = UnescapeErr;

  fn from_str (s: &str) -> Result<Self, Self::Err> {
    Ok(Self(char::from_unescape(s.chars())?))
  }
}

impl UnescapedChar {
  /// Extract the `char` from an `UnescapedChar`
  pub fn inner (self) -> char { self.0 }
}


/// Uses the FromUnescape trait to provided FromStr capability utilizing `unescape_char` to generate `String`
pub struct UnescapedString(pub String);

impl str::FromStr for UnescapedString {
  type Err = UnescapeErr;

  fn from_str (s: &str) -> Result<Self, Self::Err> {
    Ok(Self(String::from_unescape(s.chars())?))
  }
}

impl UnescapedString {
  /// Extract the `String` from an `UnescapedString`
  pub fn inner (self) -> String { self.0 }
}




/// Allows inserting values into HashMaps if the key provided is not already present
pub trait InsertUnique
{
  /// The type of Key used by this HashMap
  type Key;

  /// The type of Value used by this HashMap
  type Value;

  /// Insert a value into this HashMap if the provided key is not already present;
  /// Returns false if the key was already bound to a value
  fn insert_unique<Q> (&mut self, key: &Q, value: Self::Value) -> bool
  where Self::Key: Borrow<Q>,
        Q: Eq + Hash + ToOwned<Owned = Self::Key> + ?Sized;
}

impl<K, V, H> InsertUnique for HashMap<K, V, H>
where K: Hash,
      H: BuildHasher
{
  type Key = K;
  type Value = V;

  fn insert_unique<Q> (&mut self, key: &Q, value: V) -> bool
  where Self::Key: Borrow<Q>,
        Q: Eq + Hash + ToOwned<Owned = Self::Key> + ?Sized
  {
    match self.raw_entry_mut().from_key(key) {
      RawEntryMut::Occupied(_) => false,
      RawEntryMut::Vacant(slot) => {
        let key = key.to_owned();
        slot.insert(key, value);
        true
      }
    }
  }
}