//! Various utilities

#![allow(dead_code)]

use std::fmt;


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