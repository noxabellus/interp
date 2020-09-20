#![allow(clippy::type_complexity)]

//! Contains an implementation of a hashmap, backed by DynBuf
use std::{
  ops,
  mem::replace,
  iter::Flatten,
  hash::{ Hash, Hasher, BuildHasher }
};

use super::{
  fnv1a::Fnv1aBuilder,
  dynbuf::{ self, DynBuf },
  maybe::*,
};


/// Makes a type compatible with DynMap comparisons.
/// In general, this is the same as `PartialEq<T> + Eq`.
///
/// The purpose of this is allowing floating point numbers and other values
/// that have mathematic implementations of `PartialEq`, that do not meet the needs of hash maps
pub trait MapEq<T: ?Sized = Self> {
  /// Determines whether two values are equivalent for the purposes of use as a hashmap key
  fn map_eq (&self, other: &T) -> bool;
}

impl<T, U> MapEq<U> for T where T: PartialEq<U> + Eq {
  default fn map_eq (&self, other: &U) -> bool { self.eq(other) }
}


/// Classification trait for types that are compatible with DynMap as a key in a key/value pair.
/// This is an alias trait for `Hash + MapEq`
pub trait Key = Hash + MapEq;

/// Classification trait for types that are compatible with DynMap for comparison to keys
/// This is an alias trait for `Hash + MapEq<K>`
pub trait KeyComp<K> = Hash + MapEq<K>;





/// A hashmap implementation backed by DynBuf
#[derive(Debug, Clone)]
pub struct DynMap<K: Key, V, B: BuildHasher = Fnv1aBuilder> {
  buckets: DynBuf<DynBuf<(u64, (K, V))>>,
  length: usize,
  builder: B
}


impl<K: Key, V> DynMap<K, V, Fnv1aBuilder> {
  /// Create a new DynMap without allocating
  pub fn new () -> Self {
    Self::with_hasher(Fnv1aBuilder)
  }

  /// Create a new DynMap with a power-of-two *bucket* capacity of at least `desired_capacity`
  pub fn with_capacity (desired_capacity: usize) -> Self {
    Self::with_capacity_and_hasher(desired_capacity, Fnv1aBuilder)
  }
}

impl<K: Key, V, B: BuildHasher> DynMap<K, V, B> {
  const MINIMUM_CAPACITY: usize = 256;
  const MAXIMUM_SATURATION: f64 = 0.75;

   /// Create a new DynMap without allocating
   pub fn with_hasher (builder: B) -> Self {
    Self {
      buckets: DynBuf::default(),
      length: 0,
      builder
    }
  }

  /// Create a new DynMap with a power-of-two *bucket* capacity of at least `desired_capacity`
  pub fn with_capacity_and_hasher (desired_capacity: usize, builder: B) -> Self {
    let mut out = Self::with_hasher(builder);

    out.initialize(desired_capacity);

    out
  }

  fn initialize (&mut self, capacity: usize) {
    if self.buckets.is_empty() {
      let mut cap = Self::MINIMUM_CAPACITY;
      while cap < capacity { cap *= 2 }
      self.buckets.fill_default(cap);
    }
  }


  fn hash<Q> (&self, k: &Q) -> u64
  where Q: Hash
  {
    let mut h = self.builder.build_hasher();

    k.hash(&mut h);

    h.finish()
  }


  /// Get the number of pairs in a DynMap
  pub fn len (&self) -> usize { self.length }

  /// Determine if there are any pairs in a DynMap
  pub fn is_empty (&self) -> bool { self.len() == 0 }



  fn bucket_idx (&self, hash: u64) -> usize {
    hash as usize % self.buckets.len()
  }

  fn bucket (&self, idx: usize) -> &DynBuf<(u64, (K, V))> {
    unsafe { self.buckets.get_unchecked(idx) }
  }

  fn bucket_mut (&mut self, idx: usize) -> &mut DynBuf<(u64, (K, V))> {
    unsafe { self.buckets.get_unchecked_mut(idx) }
  }


  fn find_in_bucket_impl<Q> (&self, bucket_idx: usize, k: &Q) -> Maybe<usize>
  where Q: KeyComp<K>
  {
    let bucket = self.bucket(bucket_idx);

    for (i, (_, (key, _))) in bucket.iter().enumerate() {
      if k.map_eq(key) {
        return Just(i)
      }
    }

    Nothing
  }

  fn find_impl<Q> (&self, k: &Q) -> Maybe<(usize, usize)>
  where Q: KeyComp<K>
  {
    let hash = self.hash(k);
    let bucket_idx = self.bucket_idx(hash);
    self.find_in_bucket_impl(bucket_idx, k).map(|elem_idx| (bucket_idx, elem_idx))
  }


  fn growth_heuristic (&self) -> bool {
    let lf = self.length as f64;
    let cf = self.buckets.len() as f64;

    lf / cf >= Self::MAXIMUM_SATURATION
  }

  
  fn grow_buckets (&mut self) {
    let curr_cap = self.buckets.len();
    let new_cap = if curr_cap > 0 { curr_cap * 2 } else { Self::MINIMUM_CAPACITY };

    let new_buckets = DynBuf::filled_default(new_cap);

    let pairs = replace(&mut self.buckets, new_buckets).into_iter().flatten();

    for (hash, (k, v)) in pairs {
      self.insert_preapproved(hash, k, v);
    }
  }


  fn insert_final (&mut self, bucket_idx: usize, hash: u64, k: K, v: V) {
    let bucket = self.bucket_mut(bucket_idx);

    bucket.push((hash, (k, v)));

    self.length += 1;
  }

  fn insert_preapproved (&mut self, hash: u64, k: K, v: V) {
    let bucket_idx = self.bucket_idx(hash);
    self.insert_final(bucket_idx, hash, k, v)
  }


  /// Add or set a key/value pair in a DynMap
  pub fn insert (&mut self, k: K, v: V) -> Maybe<V> {
    self.initialize(Self::MINIMUM_CAPACITY);

    let hash = self.hash(&k);
    let bucket_idx = self.bucket_idx(hash);

    if let Just(elem_idx) = self.find_in_bucket_impl(bucket_idx, &k) {
      return Just(replace(unsafe { &mut self.bucket_mut(bucket_idx).get_unchecked_mut(elem_idx).1.1 }, v))
    }

    if self.growth_heuristic() {
      self.grow_buckets();
      self.insert_preapproved(hash, k, v);
    } else {
      self.insert_final(bucket_idx, hash, k, v);
    }

    Nothing
  }


  /// Get a reference pair associated with a key in a DynMap
  pub fn get_pair<Q> (&self, k: &Q) -> Maybe<(&K, &V)>
  where Q: KeyComp<K>
  {
    if let Just((i, j)) = self.find_impl(k) {
      let bucket = self.bucket(i);

      unsafe {
        let (_, (key, val)) = bucket.get_unchecked(j);

        Just((key, val))
      }
    } else {
      Nothing
    }
  }

  /// Get a mutable reference pair associated with a key in a DynMap
  pub fn get_pair_mut<Q> (&mut self, k: &Q) -> Maybe<(&K, &mut V)>
  where Q: KeyComp<K>
  {
    if let Just((i, j)) = self.find_impl(k) {
      let bucket = self.bucket_mut(i);

      unsafe {
        let (_, (key, val)) = bucket.get_unchecked_mut(j);

        Just((key, val))
      }
    } else {
      Nothing
    }
  }

  /// Get a reference to the value associated with a key in a DynMap
  pub fn get<Q> (&self, k: &Q) -> Maybe<&V>
  where Q: KeyComp<K>
  {
    self.get_pair(k).map(|(_k, v)| v)
  }

  /// Get a mutable reference to the value associated with a key in a DynMap
  pub fn get_mut<Q> (&mut self, k: &Q) -> Maybe<&mut V>
  where Q: KeyComp<K>
  {
    self.get_pair_mut(k).map(|(_k, v)| v)
  }


  /// Remove a key/value pair in a DynMap and return it
  pub fn remove_pair<Q> (&mut self, k: &Q) -> Maybe<(K, V)>
  where Q: KeyComp<K>
  {
    let (i, j) = self.find_impl(k)?;

    self.bucket_mut(i).remove(j).map(|(_, p)| p)
  }

  /// Remove a key/value pair in a DynMap and return the value
  pub fn remove<Q> (&mut self, k: &Q) -> Maybe<V>
  where Q: KeyComp<K>
  {
    self.remove_pair(k).map(|(_k, v)| v)
  }

  /// Get an iterator over all the keys in a DynMap
  pub fn keys (&self) -> KeyIter<K, V> {
    KeyIter(self.buckets.iter().flatten())
  }

  /// Get an iterator over all the values in a DynMap
  pub fn vals (&self) -> ValIter<K, V> {
    ValIter(self.buckets.iter().flatten())
  }

  /// Get a mutable iterator over all the values in a DynMap
  pub fn vals_mut (&mut self) -> ValIterMut<K, V> {
    ValIterMut(self.buckets.iter_mut().flatten())
  }

  /// Get an iterator over all the pairs in a DynMap
  pub fn iter (&self) -> PairIter<K, V> {
    PairIter(self.buckets.iter().flatten())
  }

  /// Get a mutable iterator over all the pairs in a DynMap
  pub fn iter_mut (&mut self) -> PairIterMut<K, V> {
    PairIterMut(self.buckets.iter_mut().flatten())
  }
}

impl<K: Key, V> Default for DynMap<K, V, Fnv1aBuilder> { fn default () -> Self { Self::new() } }


/// Iterator over the key side of the pairs in a DynMap
pub struct KeyIter<'i, K: Key, V>(Flatten<dynbuf::Iter<'i, DynBuf<(u64, (K, V))>>>);

impl<'i, K: Key, V> Iterator for KeyIter<'i, K, V> {
  type Item = &'i K;
  fn next (&mut self) -> Option<&'i K> { self.0.next().map(|(_h, (k, _v))| k) }
}

/// Iterator over the value side of the pairs in a DynMap
pub struct ValIter<'i, K: Key, V>(Flatten<dynbuf::Iter<'i, DynBuf<(u64, (K, V))>>>);

impl<'i, K: Key, V> Iterator for ValIter<'i, K, V> {
  type Item = &'i V;
  fn next (&mut self) -> Option<&'i V> { self.0.next().map(|(_h, (_k, v))| v) }
}

/// Mutable iterator over the value side of the pairs in a DynMap
pub struct ValIterMut<'i, K: Key, V>(Flatten<dynbuf::IterMut<'i, DynBuf<(u64, (K, V))>>>);

impl<'i, K: Key, V> Iterator for ValIterMut<'i, K, V> {
  type Item = &'i mut V;
  fn next (&mut self) -> Option<&'i mut V> { self.0.next().map(|(_h, (_k, v))| v) }
}

/// Iterator over the pairs in a DynMap
pub struct PairIter<'i, K: Key, V>(Flatten<dynbuf::Iter<'i, DynBuf<(u64, (K, V))>>>);

impl<'i, K: Key, V> Iterator for PairIter<'i, K, V> {
  type Item = (&'i K, &'i V);
  fn next (&mut self) -> Option<(&'i K, &'i V)> { self.0.next().map(|(_h, (k, v))| (k, v)) }
}

/// Value-mutable iterator over the pairs in a DynMap
pub struct PairIterMut<'i, K: Key, V>(Flatten<dynbuf::IterMut<'i, DynBuf<(u64, (K, V))>>>);

impl<'i, K: Key, V> Iterator for PairIterMut<'i, K, V> {
  type Item = (&'i K, &'i mut V);
  fn next (&mut self) -> Option<(&'i K, &'i mut V)> { self.0.next().map(|(_h, (k, v))| (&*k, v)) }
}


/// The iterator type yielded when DynMap is converted into an owning by-val iterator with IntoIterator
pub struct IntoIter<K: Key, V>(Flatten<dynbuf::IntoIter<DynBuf<(u64, (K, V))>>>);

impl<K: Key, V> Iterator for IntoIter<K, V> {
  type Item = (K, V);
  fn next (&mut self) -> Option<(K, V)> { self.0.next().map(|(_h, p)| p) }
}

impl<K: Key, V, B: BuildHasher> IntoIterator for DynMap<K, V, B> {
  type Item = (K, V);
  type IntoIter = IntoIter<K, V>;

  fn into_iter (self) -> Self::IntoIter {
    IntoIter(self.buckets.into_iter().flatten())
  }
}


impl<K: Key, V, B0: BuildHasher, B1: BuildHasher> PartialEq<DynMap<K, V, B1>> for DynMap<K, V, B0>
where V: PartialEq
{
  fn eq (&self, other: &DynMap<K, V, B1>) -> bool {
    for (k, v) in self.iter() {
      if !other.get(k).map_or(|ov| v == ov, false) { return false }
    }

    true
  }
}

impl<K: Key, V, B: BuildHasher, Q> ops::Index<&Q> for DynMap<K, V, B>
where Q: KeyComp<K>
{
  type Output = V;

  #[track_caller]
  fn index (&self, k: &Q) -> &V {
    self.get(k).expect("Implicit unwrap of DynMap::get failed")
  }
}


impl<K: Key, V, B: BuildHasher, Q> ops::IndexMut<&Q> for DynMap<K, V, B>
where Q: KeyComp<K>
{
  #[track_caller]
  fn index_mut (&mut self, k: &Q) -> &mut V {
    self.get_mut(k).expect("Implicit unwrap of DynMap::get failed")
  }
}


#[cfg(test)]
mod test {
  use super::*;
  use crate::prelude::*;

  #[test]
  fn test_dynmap () {
    let mut dmap: DynMap<DynStr, u64> = DynMap::default();

    dmap.insert("test_0".into(), 0);
    dmap.insert("test_1".into(), 1);
    dmap.insert("test_2".into(), 2);
    dmap.insert("test_3".into(), 3);
    dmap.insert("test_4".into(), 4);
    dmap.insert("test_5".into(), 5);
    dmap.insert("test_6".into(), 6);
    dmap.insert("test_7".into(), 7);
    dmap.insert("test_8".into(), 8);
    dmap.insert("test_9".into(), 9);
    dmap.insert("test_10".into(), 10);

    dbg!(dmap);
  }
}