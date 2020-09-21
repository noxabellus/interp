//! A hashmap implementation utilizing VM type info

use std::{
  iter::Flatten,
  mem::replace,
  hash::Hasher,
};


use super::{
  fnv1a::Fnv1a,
  type_info::TypeID,
  value::ValueData,
};

#[derive(Clone, Copy)]
struct MapEntry {
  hash: u64,
  key: ValueData,
  value: ValueData,
}

// TODO this needs to be implemented without Vec,
// because some functions are performing duplicate validation

/// A hashmap utilizing type info
#[derive(Clone)]
pub struct Map {
  key_type: TypeID,
  key_hasher: fn (&ValueData, &mut Fnv1a),
  key_comparator: fn (&ValueData, &ValueData) -> bool,
  value_type: TypeID,
  buckets: Vec<Vec<MapEntry>>,
  length: usize,
}

impl Map {
  const MINIMUM_CAPACITY: usize = 16;
  const MAXIMUM_SATURATION: f64 = 0.75;

  /// Create a new Map without allocating
  pub fn new (key_type: TypeID, value_type: TypeID) -> Self {
    Self {
      key_type,
      key_hasher: key_type.get_hasher_func(),
      key_comparator: key_type.get_eq_func(),
      value_type,
      buckets: Vec::default(),
      length: 0,
    }
  }

  /// Create a new Map with a power-of-two *bucket* capacity of at least `desired_capacity`
  pub fn with_capacity_and_hasher (key_type: TypeID, value_type: TypeID, desired_capacity: usize) -> Self {
    let mut out = Self::new(key_type, value_type);

    out.initialize(desired_capacity);

    out
  }

  fn initialize (&mut self, capacity: usize) {
    if self.buckets.is_empty() {
      let mut cap = Self::MINIMUM_CAPACITY;
      while cap < capacity { cap *= 2 }
      self.buckets.reserve(cap);
      for _ in 0..cap { self.buckets.push(Vec::default()) }
    }
  }


  fn hash (&self, k: &ValueData) -> u64 {
    let mut h = Fnv1a::default();

    (self.key_hasher)(k, &mut h);

    h.finish()
  }


  /// Get the number of pairs in a Map
  pub fn len (&self) -> usize { self.length }

  /// Determine if there are any pairs in a Map
  pub fn is_empty (&self) -> bool { self.len() == 0 }



  fn bucket_idx (&self, hash: u64) -> usize {
    hash as usize % self.buckets.len()
  }

  fn bucket (&self, idx: usize) -> &Vec<MapEntry> {
    unsafe { self.buckets.get_unchecked(idx) }
  }

  fn bucket_mut (&mut self, idx: usize) -> &mut Vec<MapEntry> {
    unsafe { self.buckets.get_unchecked_mut(idx) }
  }


  fn find_in_bucket_impl (&self, bucket_idx: usize, k: &ValueData) -> Option<usize> {
    let bucket = self.bucket(bucket_idx);

    for (i, MapEntry { key, .. }) in bucket.iter().enumerate() {
      if (self.key_comparator)(k, key) {
        return Some(i)
      }
    }

    None
  }

  fn find_impl (&self, k: &ValueData) -> Option<(usize, usize)> {
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

    let mut new_buckets = Vec::with_capacity(new_cap);
    for _ in 0..new_cap { new_buckets.push(Vec::default()) }

    let pairs = replace(&mut self.buckets, new_buckets).into_iter().flatten();

    for MapEntry { hash, key, value } in pairs {
      self.insert_preapproved(hash, key, value);
    }
  }


  fn insert_final (&mut self, bucket_idx: usize, hash: u64, key: ValueData, value: ValueData) {
    let bucket = self.bucket_mut(bucket_idx);

    bucket.push(MapEntry { hash, key, value });

    self.length += 1;
  }

  fn insert_preapproved (&mut self, hash: u64, k: ValueData, v: ValueData) {
    let bucket_idx = self.bucket_idx(hash);
    self.insert_final(bucket_idx, hash, k, v)
  }


  /// Add or set a key/value pair in a Map
  pub fn insert (&mut self, k: ValueData, v: ValueData) -> Option<ValueData> {
    self.initialize(Self::MINIMUM_CAPACITY);

    let hash = self.hash(&k);
    let bucket_idx = self.bucket_idx(hash);

    if let Some(elem_idx) = self.find_in_bucket_impl(bucket_idx, &k) {
      return Some(replace(unsafe { &mut self.bucket_mut(bucket_idx).get_unchecked_mut(elem_idx).value }, v))
    }

    if self.growth_heuristic() {
      self.grow_buckets();
      self.insert_preapproved(hash, k, v);
    } else {
      self.insert_final(bucket_idx, hash, k, v);
    }

    None
  }


  /// Get a reference pair associated with a key in a Map
  pub fn get_pair (&self, k: &ValueData) -> Option<(&ValueData, &ValueData)> {
    if let Some((i, j)) = self.find_impl(k) {
      let bucket = self.bucket(i);

      unsafe {
        let MapEntry { key, value, .. } = bucket.get_unchecked(j);

        Some((key, value))
      }
    } else {
      None
    }
  }

  /// Get a mutable reference pair associated with a key in a Map
  pub fn get_pair_mut (&mut self, k: &ValueData) -> Option<(&ValueData, &mut ValueData)> {
    if let Some((i, j)) = self.find_impl(k) {
      let bucket = self.bucket_mut(i);

      unsafe {
        let MapEntry { key, value, .. } = bucket.get_unchecked_mut(j);

        Some((key, value))
      }
    } else {
      None
    }
  }

  /// Get a reference to the value associated with a key in a Map
  pub fn get (&self, k: &ValueData) -> Option<&ValueData> {
    self.get_pair(k).map(|(_k, v)| v)
  }

  /// Get a mutable reference to the value associated with a key in a Map
  pub fn get_mut (&mut self, k: &ValueData) -> Option<&mut ValueData> {
    self.get_pair_mut(k).map(|(_k, v)| v)
  }


  /// Remove a key/value pair in a Map and return it
  pub fn remove_pair (&mut self, k: &ValueData) -> Option<(ValueData, ValueData)> {
    let (i, j) = self.find_impl(k)?;

    let entry = self.bucket_mut(i).remove(j);
    
    Some((entry.key, entry.value))
  }

  /// Remove a key/value pair in a Map and return the value
  pub fn remove (&mut self, k: &ValueData) -> Option<ValueData> {
    self.remove_pair(k).map(|(_k, v)| v)
  }

  /// Get an iterator over all the keys in a Map
  pub fn keys (&self) -> KeyIter {
    KeyIter(self.buckets.iter().flatten())
  }

  /// Get an iterator over all the values in a Map
  pub fn vals (&self) -> ValIter {
    ValIter(self.buckets.iter().flatten())
  }

  /// Get a mutable iterator over all the values in a Map
  pub fn vals_mut (&mut self) -> ValIterMut {
    ValIterMut(self.buckets.iter_mut().flatten())
  }

  /// Get an iterator over all the pairs in a Map
  pub fn iter (&self) -> PairIter {
    PairIter(self.buckets.iter().flatten())
  }

  /// Get a mutable iterator over all the pairs in a Map
  pub fn iter_mut (&mut self) -> PairIterMut {
    PairIterMut(self.buckets.iter_mut().flatten())
  }
}


/// Iterator over the key side of the pairs in a Map
pub struct KeyIter<'i>(Flatten<std::slice::Iter<'i, Vec<MapEntry>>>);

impl<'i> Iterator for KeyIter<'i> {
  type Item = &'i ValueData;
  fn next (&mut self) -> Option<&'i ValueData> { self.0.next().map(|MapEntry { key, .. }| key) }
}

/// Iterator over the value side of the pairs in a Map
pub struct ValIter<'i>(Flatten<std::slice::Iter<'i, Vec<MapEntry>>>);

impl<'i> Iterator for ValIter<'i> {
  type Item = &'i ValueData;
  fn next (&mut self) -> Option<&'i ValueData> { self.0.next().map(|MapEntry { value, .. }| value) }
}

/// Mutable iterator over the value side of the pairs in a Map
pub struct ValIterMut<'i>(Flatten<std::slice::IterMut<'i, Vec<MapEntry>>>);

impl<'i> Iterator for ValIterMut<'i> {
  type Item = &'i mut ValueData;
  fn next (&mut self) -> Option<&'i mut ValueData> { self.0.next().map(|MapEntry { value, .. }| value) }
}

/// Iterator over the pairs in a Map
pub struct PairIter<'i>(Flatten<std::slice::Iter<'i, Vec<MapEntry>>>);

impl<'i> Iterator for PairIter<'i> {
  type Item = (&'i ValueData, &'i ValueData);
  fn next (&mut self) -> Option<(&'i ValueData, &'i ValueData)> { self.0.next().map(|MapEntry { key, value, .. }| (key, value)) }
}

/// Value-mutable iterator over the pairs in a Map
pub struct PairIterMut<'i>(Flatten<std::slice::IterMut<'i, Vec<MapEntry>>>);

impl<'i> Iterator for PairIterMut<'i> {
  type Item = (&'i ValueData, &'i mut ValueData);
  fn next (&mut self) -> Option<(&'i ValueData, &'i mut ValueData)> { self.0.next().map(|MapEntry { key, value, .. }| (&*key, value)) }
}


/// The iterator type yielded when Map is converted into an owning by-val iterator with IntoIterator
pub struct IntoIter(Flatten<std::vec::IntoIter<Vec<MapEntry>>>);

impl Iterator for IntoIter {
  type Item = (ValueData, ValueData);
  fn next (&mut self) -> Option<(ValueData, ValueData)> { self.0.next().map(|MapEntry { key, value, .. }| (key, value)) }
}

impl IntoIterator for Map {
  type Item = (ValueData, ValueData);
  type IntoIter = IntoIter;

  fn into_iter (self) -> Self::IntoIter {
    IntoIter(self.buckets.into_iter().flatten())
  }
}


impl PartialEq<Map> for Map {
  fn eq (&self, other: &Map) -> bool {
    if self.key_type != other.key_type || self.value_type != other.value_type { return false }
    
    let value_comparator = self.value_type.get_eq_func();

    for (k, v) in self.iter() {
      if !other.get(k).map_or(false, |ov| value_comparator(v, ov)) { return false }
    }

    true
  }
}