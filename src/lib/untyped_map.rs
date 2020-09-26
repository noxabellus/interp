//! A hashmap implementation with untyped data


use std::{
  mem::transmute,
  ptr,
  intrinsics::{ likely, unlikely }
};


use std::{
  iter::{ Zip, Flatten },
  mem::replace,
  hash::Hasher,
};


use super::{
  unsafe_vec::{ self, UnsafeVec },
  untyped_vec::UntypedVec,
  fnv1a::Fnv1a,
  type_info::TypeID
};

type Raw = u8;

/// A hashmap utilizing type info
#[derive(Clone)]
pub struct Map {
  key_type: TypeID,
  key_hasher: fn (*const Raw, &mut Fnv1a),
  key_comparator: fn (*const Raw, *const Raw) -> bool,
  key_size: usize,
  key_align: usize,

  value_type: TypeID,
  value_size: usize,
  value_align: usize,

  hash_buckets: UnsafeVec<UnsafeVec<u64>>,
  key_buckets: UnsafeVec<UntypedVec>,
  value_buckets: UnsafeVec<UntypedVec>,

  length: usize
}


impl Map {
  const MINIMUM_CAPACITY: usize = 16;
  const MAXIMUM_SATURATION: f64 = 0.75;

  /// Create a new Map without allocating
  pub fn new (key_type: TypeID, key_size: usize, key_align: usize, value_type: TypeID, value_size: usize, value_align: usize) -> Self {
    unsafe { 
      Self {
        key_type,
        key_hasher: key_type.get_untyped_hasher_func(),
        key_comparator: key_type.get_untyped_eq_func(),
        key_size,
        key_align,

        value_type,
        value_size,
        value_align,

        hash_buckets: UnsafeVec::new(),
        key_buckets: UnsafeVec::new(),
        value_buckets: UnsafeVec::new(),
        
        length: 0,
      }
    }
  }

  /// Create a new Map with a power-of-two *bucket* capacity of at least `desired_capacity`
  pub fn with_capacity_and_hasher (key_type: TypeID, key_size: usize, key_align: usize, value_type: TypeID, value_size: usize, value_align: usize, desired_capacity: usize) -> Self {
    let mut out = Self::new(key_type, key_size, key_align, value_type, value_size, value_align);

    out.initialize(desired_capacity);

    out
  }

  fn initialize (&mut self, capacity: usize) {
    unsafe {
      if unlikely(self.hash_buckets.is_empty()) {
        let mut cap = Self::MINIMUM_CAPACITY;
        while cap < capacity { cap *= 2 }

        self.hash_buckets.allocate_total(cap);
        self.key_buckets.allocate_total(cap);
        self.value_buckets.allocate_total(cap);

        for _ in 0..cap {
          self.hash_buckets.push(UnsafeVec::new());
          self.key_buckets.push(UntypedVec::new(self.key_size, self.key_align));
          self.value_buckets.push(UntypedVec::new(self.value_size, self.value_align));
        }
      }
    }
  }


  fn hash (&self, k: *const Raw) -> u64 {
    let mut h = Fnv1a::default();

    (self.key_hasher)(k, &mut h);

    h.finish()
  }


  /// Get the number of pairs in a Map
  pub fn len (&self) -> usize { self.length }

  /// Determine if there are any pairs in a Map
  pub fn is_empty (&self) -> bool { self.len() == 0 }



  fn bucket_idx (&self, hash: u64) -> usize {
    hash as usize % self.hash_buckets.len()
  }

  fn bucket (&self, idx: usize) -> (&UnsafeVec<u64>, &UntypedVec, &UntypedVec) {
    unsafe { (
      self.hash_buckets.get(idx),
      self.key_buckets.get(idx),
      self.value_buckets.get(idx)
    ) }
  }

  fn bucket_mut (&mut self, idx: usize) -> (&mut UnsafeVec<u64>, &mut UntypedVec, &mut UntypedVec) {
    unsafe { (
      self.hash_buckets.get_mut(idx),
      self.key_buckets.get_mut(idx),
      self.value_buckets.get_mut(idx)
    ) }
  }


  fn find_in_bucket_impl (&self, bucket_idx: usize, k: *const Raw) -> Option<usize> {
    let (_, key_bucket, _) = self.bucket(bucket_idx);

    for (i, key) in key_bucket.iter().enumerate() {
      if (self.key_comparator)(k, key) {
        return Some(i)
      }
    }

    None
  }

  fn find_impl (&self, k: *const Raw) -> Option<(usize, usize)> {
    let hash = self.hash(k);
    let bucket_idx = self.bucket_idx(hash);
    self.find_in_bucket_impl(bucket_idx, k).map(|elem_idx| (bucket_idx, elem_idx))
  }


  fn growth_heuristic (&self) -> bool {
    let lf = self.length as f64;
    let cf = self.hash_buckets.len() as f64;

    lf / cf >= Self::MAXIMUM_SATURATION
  }

  
  unsafe fn grow_buckets (&mut self) {
    let curr_cap = self.hash_buckets.len();
    let new_cap = if likely(curr_cap > 0) { curr_cap * 2 } else { Self::MINIMUM_CAPACITY };

    let mut new_hash_buckets = { UnsafeVec::with_capacity(new_cap) };
    for _ in 0..new_cap { new_hash_buckets.push(UnsafeVec::new()); }

    let mut new_key_buckets = { UnsafeVec::with_capacity(new_cap) };
    for _ in 0..new_cap { new_key_buckets.push(UntypedVec::new(self.key_size, self.key_align)); }

    let mut new_value_buckets = { UnsafeVec::with_capacity(new_cap) };
    for _ in 0..new_cap { new_value_buckets.push(UntypedVec::new(self.value_size, self.value_align)); }

    let (old_hashes, old_keys, old_values) = (replace(&mut self.hash_buckets, new_hash_buckets), replace(&mut self.key_buckets, new_key_buckets), replace(&mut self.value_buckets, new_value_buckets));
    let tris = old_hashes.into_iter().flatten().zip(old_keys.into_iter().flatten().zip(old_values.into_iter().flatten()));

    for (hash, (key, value)) in tris {
      self.insert_preapproved(hash, key, value);
    }
  }


  fn insert_final (&mut self, bucket_idx: usize, hash: u64, key: *const Raw, value: *const Raw) {
    let (hash_bucket, key_bucket, value_bucket) = self.bucket_mut(bucket_idx);

    unsafe {
      hash_bucket.allocate_additional(1);
      hash_bucket.push(hash);

      key_bucket.push(key);
      value_bucket.push(value);
    }

    self.length += 1;
  }

  fn insert_preapproved (&mut self, hash: u64, k: *const Raw, v: *const Raw) {
    let bucket_idx = self.bucket_idx(hash);
    self.insert_final(bucket_idx, hash, k, v)
  }


  /// Add or set a key/value pair in a Map
  /// # Safety
  /// The caller must ensure that `k` and `v` are valid for reads of `key_size` and `value_size` bytes respectively
  pub unsafe fn insert (&mut self, k: *const Raw, v: *const Raw) {
    self.initialize(Self::MINIMUM_CAPACITY);

    let hash = self.hash(k);
    let bucket_idx = self.bucket_idx(hash);

    if let Some(elem_idx) = self.find_in_bucket_impl(bucket_idx, k) {
      self.value_buckets.get_mut(bucket_idx).replace(elem_idx, v);
      return
    }

    if self.growth_heuristic() {
      self.grow_buckets();
      self.insert_preapproved(hash, k, v);
    } else {
      self.insert_final(bucket_idx, hash, k, v);
    }
  }


  /// Get a reference pair associated with a key in a Map
  pub fn get_pair (&self, k: *const Raw) -> Option<(*const Raw, *const Raw)> {
    if let Some((i, j)) = self.find_impl(k) {
      let (_, key_bucket, value_bucket) = self.bucket(i);

      unsafe {
        let key = key_bucket.get_unchecked(j);
        let value = value_bucket.get_unchecked(j);

        Some((key, value))
      }
    } else {
      None
    }
  }

  /// Get a mutable reference pair associated with a key in a Map
  pub fn get_pair_mut (&mut self, k: *const Raw) -> Option<(*const Raw, *mut Raw)> {
    if let Some((i, j)) = self.find_impl(k) {
      let (_, key_bucket, value_bucket) = self.bucket_mut(i);

      unsafe {
        let key = key_bucket.get_unchecked(j);
        let value = value_bucket.get_unchecked(j);

        Some((key, value))
      }
    } else {
      None
    }
  }

  /// Get a reference to the value associated with a key in a Map
  pub fn get (&self, k: *const Raw) -> Option<*const Raw> {
    self.get_pair(k).map(|(_k, v)| v)
  }

  /// Get a mutable reference to the value associated with a key in a Map
  pub fn get_mut (&mut self, k: *const Raw) -> Option<*mut Raw> {
    self.get_pair_mut(k).map(|(_k, v)| v)
  }



  /// Remove a key/value pair in a Map and return the value
  /// # Safety
  /// The caller must ensure that `out_v` is valid for writes of `value_size` bytes
  pub unsafe fn remove (&mut self, k: *const Raw, out_v: *mut Raw) -> bool {
    if let Some((i, j)) = self.find_impl(k) {
      let (hash_bucket, key_bucket, value_bucket) = self.bucket_mut(i);

      hash_bucket.remove_n(j, ptr::null_mut(), 1);
      key_bucket.remove_unchecked(j, ptr::null_mut(), 1);
      value_bucket.remove_unchecked(j, out_v, 1);
      
      true
    } else {
      false
    }
  }

  /// Get an iterator over all the keys in a Map
  pub fn keys (&self) -> KeyIter {
    KeyIter(self.key_buckets.iter().flatten())
  }

  /// Get an iterator over all the values in a Map
  pub fn values (&self) -> ValIter {
    ValIter(self.value_buckets.iter().flatten())
  }

  /// Get a mutable iterator over all the values in a Map
  pub fn values_mut (&mut self) -> ValIterMut {
    ValIterMut(self.value_buckets.iter_mut().flatten())
  }

  /// Get an iterator over all the pairs in a Map
  pub fn iter (&self) -> PairIter {
    PairIter(self.keys().zip(self.values()))
  }

  /// Get a mutable iterator over all the pairs in a Map
  pub fn iter_mut (&mut self) -> PairIterMut {
    // HACK: This is filthy but theres really nothing unsafe here
    let values: ValIterMut<'static> = unsafe { transmute(self.values_mut()) };
    PairIterMut(self.keys().zip(unsafe { transmute::<ValIterMut<'static>, ValIterMut<'_>>(values) }))
  }
}


/// Iterator over the key side of the pairs in a Map
pub struct KeyIter<'i>(Flatten<unsafe_vec::Iter<'i, UntypedVec>>);

impl<'i> Iterator for KeyIter<'i> {
  type Item = *const Raw;
  fn next (&mut self) -> Option<*const Raw> { self.0.next() }
}

/// Iterator over the value side of the pairs in a Map
pub struct ValIter<'i>(Flatten<unsafe_vec::Iter<'i, UntypedVec>>);

impl<'i> Iterator for ValIter<'i> {
  type Item = *const Raw;
  fn next (&mut self) -> Option<*const Raw> { self.0.next() }
}

/// Mutable iterator over the value side of the pairs in a Map
pub struct ValIterMut<'i>(Flatten<unsafe_vec::IterMut<'i, UntypedVec>>);

impl<'i> Iterator for ValIterMut<'i> {
  type Item = *mut Raw;
  fn next (&mut self) -> Option<*mut Raw> { self.0.next() }
}

/// Iterator over the pairs in a Map
pub struct PairIter<'i>(Zip<KeyIter<'i>, ValIter<'i>>);

impl<'i> Iterator for PairIter<'i> {
  type Item = (*const Raw, *const Raw);
  fn next (&mut self) -> Option<(*const Raw, *const Raw)> { self.0.next() }
}

/// Value-mutable iterator over the pairs in a Map
pub struct PairIterMut<'i>(Zip<KeyIter<'i>, ValIterMut<'i>>);

impl<'i> Iterator for PairIterMut<'i> {
  type Item = (*const Raw, *mut Raw);
  fn next (&mut self) -> Option<(*const Raw, *mut Raw)> { self.0.next() }
}


/// The iterator type yielded when Map is converted into an owning by-val iterator with IntoIterator
pub struct IntoIter(Zip<Flatten<unsafe_vec::IntoIter<UntypedVec>>, Flatten<unsafe_vec::IntoIter<UntypedVec>>>);

impl Iterator for IntoIter {
  type Item = (*mut Raw, *mut Raw);
  fn next (&mut self) -> Option<(*mut Raw, *mut Raw)> { self.0.next() }
}

impl IntoIterator for Map {
  type Item = (*mut Raw, *mut Raw);
  type IntoIter = IntoIter;

  fn into_iter (self) -> Self::IntoIter {
    IntoIter(self.key_buckets.into_iter().flatten().zip(self.value_buckets.into_iter().flatten()))
  }
}


impl PartialEq<Map> for Map {
  fn eq (&self, other: &Map) -> bool {
    if self.key_type != other.key_type || self.value_type != other.value_type { return false }
    
    let value_comparator = self.value_type.get_untyped_eq_func();

    for (k, v) in self.iter() {
      if !other.get(k).map_or(false, |ov| value_comparator(v, ov)) { return false }
    }

    true
  }
}