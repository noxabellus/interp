//! Garbage collected heap allocator wrapper

use std::{
  ops,
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


/// A VTable used for garbage collected Heap interactions
pub struct VTable {
  /// This function must be provided, it gives the Heap the size and alignment required by the type represented
  pub get_layout: *const fn () -> Layout,
  /// This function is optional, but it must be implemented if the type represented has internal pointers to garbage collected Heap allocations
  pub mark_traverse: *const fn (*const Raw, &mut Heap),
  /// This function is optional, but it must be implemented if the type represented has code to execute upon deallocation
  pub handle_drop: *const fn (*mut Raw),
}

impl VTable {
  fn get_layout (&self) -> Layout {
    (unsafe { &*self.get_layout })()
  }

  fn mark_traverse (&self, data: *const Raw, heap: &mut Heap) {
    if let Some(mark_traverse) = unsafe { self.mark_traverse.as_ref() } {
      (mark_traverse)(data, heap)
    }
  }

  fn handle_drop (&self, data: *mut Raw) {
    if let Some(handle_drop) = unsafe { self.handle_drop.as_ref() } {
      (handle_drop)(data)
    }
  }
}


#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct AllocData(u64);

impl AllocData {
  const PTR_MASK:  u64 = 0x00_00_FF_FF_FF_FF_FF_FF;
  const FLAG_MASK: u64 = 0xFF_FF_00_00_00_00_00_00;
  
  fn new (vtable: *const VTable) -> Self {
    debug_assert!(!vtable.is_null());
    debug_assert!(!unsafe {&*vtable}.get_layout.is_null());
    Self((vtable as u64) & Self::PTR_MASK)
  }

  fn is_marked (self) -> bool {
    (self.0 & Self::FLAG_MASK) != 0
  }

  // fn unmark (&mut self) {
  //   self.0 &= Self::PTR_MASK;
  // }

  fn mark (&mut self) {
    self.0 &= ((true as u64) << 48) | Self::PTR_MASK;
  }

  fn unmarked (self) -> Self {
    Self(self.0 & Self::PTR_MASK)
  }

  // fn marked (self) -> Self {
  //   Self(self.0 & (((true as u64) << 48) | Self::PTR_MASK))
  // }

  fn as_ptr (self) -> *const VTable {
    (self.0 & Self::PTR_MASK) as _
  }

  fn as_ref (self) -> &'static VTable {
    let p = self.as_ptr();
    unsafe { &*p }
  }
}


impl From<*const VTable> for AllocData {
  fn from (p: *const VTable) -> Self { Self::new(p) }
}


impl From<AllocData> for *const VTable {
  fn from (d: AllocData) -> Self { d.as_ptr() }
}


impl From<AllocData> for &'static VTable {
  fn from (d: AllocData) -> Self { d.as_ref() }
}


impl ops::Deref for AllocData {
  type Target = VTable;
  fn deref (&self) -> &VTable { self.as_ref() }
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

  /// Create a garbage collected allocation with a given vtable.
  ///
  /// The provided vtable must contain at least a layout method.
  ///
  /// # Safety
  /// Caller must determine the validity of the `vtable` passed.
  /// Layout provided by the vtable is assumed to be valid.
  /// If the allocation fails, `handle_alloc_error` is called
  pub unsafe fn alloc (&mut self, vtable: *const VTable) -> *mut Raw {
    let adata: AllocData = vtable.into();

    let layout = adata.get_layout();

    let p = alloc(layout);

    if unlikely(p.is_null()) { handle_alloc_error(layout) }

    self.map.insert(p, adata);

    p
  }

  /// Manually free a garbage collected allocation.
  /// This is not necessary to call under normal circumstances, as `sweep` will free any unmarked data
  /// # Safety
  /// Caller must determine that no other pointers to the data still live.
  /// It is UB to call this with a pointer not allocated with this Heap
  pub unsafe fn dealloc (&mut self, addr: *mut Raw) {
    if let Some(adata) = self.map.remove(&addr) {
      dealloc(addr, adata.get_layout())
    } else {
      unreachable_unchecked()
    }
  }


  /// Try to mark a pointer within this Heap during a round of mark and sweep.
  /// If the pointer is not allocated by this Heap, this does nothing
  /// # Safety
  /// This is safe if all other invariants of the Heap have been maintained
  pub unsafe fn mark (&mut self, addr: *mut Raw) {
    if let Some(adata) = self.map.get_mut(&addr) {
      if !adata.is_marked() {
        adata.mark();
        
        adata.clone().mark_traverse(addr, self)
      }
    }
  }

  /// Traverse all allocations and free those which have not been marked
  /// # Safety
  /// This must be called after all gc marking functions have finished
  pub unsafe fn sweep (&mut self) {
    swap(&mut self.map, &mut self.map2);

    for (addr, data) in self.map2.drain() {
      if data.is_marked() {
        self.map.insert(addr, data.unmarked());
      } else {
        data.handle_drop(addr);

        dealloc(addr, data.get_layout());
      }
    }
  }
}

impl Default for Heap { fn default () -> Self { Self::new() } }