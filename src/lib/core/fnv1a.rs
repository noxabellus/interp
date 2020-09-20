//! Contains an implementation of the FNV-1a hashing algorithm

use core::hash::{ Hasher, BuildHasher };

/// A Hasher implementation for the FNV-1a hashing algorithm
pub struct Fnv1a { hash: u64 }

impl Fnv1a {
  const INIT: u64 = 14695981039346656037;
  const PRIME: u64 = 1099511628211;

  fn new () -> Self {
    Self { hash: Self::INIT }
  }

  fn write_impl (&mut self, bytes: &[u8]) {
    for &byte in bytes {
      self.hash = (self.hash ^ (byte as u64)).wrapping_mul(Self::PRIME)
    }
  }
}

impl Default for Fnv1a { fn default () -> Self { Self::new() } }

impl Hasher for Fnv1a {
  fn finish (&self) -> u64 { self.hash }

  fn write (&mut self, bytes: &[u8]) { self.write_impl(bytes) }
}


/// A zst used to generically generate Fnv1a Hashers
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Fnv1aBuilder;


impl BuildHasher for Fnv1aBuilder {
  type Hasher = Fnv1a;

  fn build_hasher (&self) -> Self::Hasher { Fnv1a::new() }
}