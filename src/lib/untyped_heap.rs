//! Garbage collected heap allocator wrapper

use std::{
  mem::swap,
  hint::unreachable_unchecked,
  intrinsics::unlikely,
  alloc::{ alloc, dealloc, Layout, handle_alloc_error },
  collections::HashMap,
};


type Raw = u8;

/// The type of function used as a callback to traverse heap allocations during mark phase of garbage collection
pub type MarkTraversalFn = *const fn (data_ptr: *const Raw, layout: Layout, heap: &mut Heap);

/// The type of function used as a callback when heap allocations are being deallocated by the garbage collector
pub type DropFn = *const fn (data_ptr: *mut Raw, layout: Layout);

struct AllocData {
  layout: Layout,
  mark_fn: MarkTraversalFn,
  drop_fn: DropFn,
  mark: bool
}



/// A garbage collected heap allocator wrapper
pub struct Heap {
  map: HashMap<*mut Raw, AllocData>,
  map2: HashMap<*mut Raw, AllocData>
}

impl Heap {
  /// Create a new Heap
  pub fn new () -> Self {
    Self { map: HashMap::default(), map2: HashMap::default() }
  }

  /// Create a garbage collected allocation with a given layout.
  //
  /// This method can be used to allocate data that contains internal pointers,
  /// which will be marked via the provided callback function during mark and sweep.
  /// If the allocated data will never contain internal pointers, `ptr::null()` can be passed instead.
  ///
  /// If the allocated data needs to run some logic upon deallocation, the drop_fn may be provided;
  /// Otherwise, `ptr::null()` can be passed instead.
  ///
  /// # Safety
  /// Caller must determine the validity of the `mark_fn` passed.
  /// Layout is assumed to be valid.
  /// If the allocation fails, `handle_alloc_error` is called
  pub unsafe fn alloc (&mut self, layout: Layout, mark_fn: MarkTraversalFn, drop_fn: DropFn) -> *mut Raw {
    let p = alloc(layout);

    if unlikely(p.is_null()) { handle_alloc_error(layout) }

    self.map.insert(p, AllocData { layout, mark_fn, drop_fn, mark: false });

    p
  }

  /// Manually free a garbage collected allocation.
  /// This is not necessary to call under normal circumstances, as `sweep` will free any unmarked data
  /// # Safety
  /// Caller must determine that no other pointers to the data still live.
  /// It is UB to call this with a pointer not allocated with this Heap
  pub unsafe fn dealloc (&mut self, addr: *mut Raw) {
    if let Some(AllocData { layout, .. }) = self.map.remove(&addr) {
      dealloc(addr, layout)
    } else {
      unreachable_unchecked()
    }
  }


  /// Try to mark a pointer within this Heap during a round of mark and sweep.
  /// If the pointer is not allocated by this Heap, this does nothing
  /// # Safety
  /// This is safe if all other invariants of the Heap have been maintained
  pub unsafe fn mark (&mut self, addr: *mut Raw) {
    if let Some(&mut AllocData { ref mut mark, mark_fn, layout, .. }) = self.map.get_mut(&addr) {
      if !*mark {
        *mark = true;
        
        if let Some(mark_fn) = mark_fn.as_ref() {
          (mark_fn)(addr, layout, self)
        }
      }
    }
  }

  /// Traverse all allocations and free those which have not been marked
  /// # Safety
  /// This must be called after all gc marking functions have finished
  pub unsafe fn sweep (&mut self) {
    swap(&mut self.map, &mut self.map2);

    for (addr, mut data) in self.map2.drain() {
      if data.mark {
        data.mark = false;
        self.map.insert(addr, data);
      } else {
        if let Some(drop_fn) = data.drop_fn.as_ref() {
          drop_fn(addr, data.layout);
        }

        dealloc(addr, data.layout);
      }
    }
  }
}

impl Default for Heap { fn default () -> Self { Self::new() } }