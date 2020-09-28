//! Garbage collected heap allocator

#![allow(dead_code, unused_imports)]

use std::{
  ptr,
  mem::size_of,
  hint::unreachable_unchecked,  
  intrinsics::{ likely, unlikely },
  alloc::{ handle_alloc_error, Layout }
};

use crate::{
  static_assert,
  ptr::align_addr,
  valloc::{ alloc, commit, dealloc, page_size },
  type_info::TypeID,
};


struct FreeNode(u64, u64);

impl FreeNode {
  const PTR_MASK: u64 = 0x00_00_FF_FF_FF_FF_FF_FF;
  const LEN_MASK: u64 = 0xFF_FF_00_00_00_00_00_00;
  const LEN0_MASK: u32 = 0xFF_FF_00_00;
  const LEN1_MASK: u32 = 0x00_00_FF_FF;

  fn new (length: usize, prev: *mut FreeNode, next: *mut FreeNode) -> Self {
    Self(
      (prev as u64 & Self::PTR_MASK) & (((length as u32 & Self::LEN0_MASK) as u64) << 32),
      (next as u64 & Self::PTR_MASK) & (((length as u32 & Self::LEN1_MASK) as u64) << 48)
    )
  }

  fn clear_length (&mut self) {
    self.0 &= Self::PTR_MASK;
    self.1 &= Self::PTR_MASK;
  }

  fn set_length (&mut self, length: usize) {
    self.clear_length();
    self.0 |= ((length as u32 & Self::LEN0_MASK) as u64) << 32;
    self.1 |= ((length as u32 & Self::LEN1_MASK) as u64) << 48
  }

  fn clear_prev (&mut self) {
    self.0 &= Self::LEN_MASK;
  }

  fn clear_next (&mut self) {
    self.1 &= Self::LEN_MASK;
  }

  fn set_prev (&mut self, prev: *mut FreeNode) {
    self.clear_prev();
    self.0 |= prev as u64;
  }

  fn set_next (&mut self, next: *mut FreeNode) {
    self.clear_next();
    self.1 |= next as u64;
  }


  fn get_length (&self) -> usize {
      (((self.0 & Self::LEN_MASK) >> 32) as usize)
    | (((self.1 & Self::LEN_MASK) >> 48) as usize)
  }

  fn get_prev (&self) -> *mut FreeNode {
    (self.0 & Self::PTR_MASK) as *mut FreeNode
  }

  fn get_next (&self) -> *mut FreeNode {
    (self.1 & Self::PTR_MASK) as *mut FreeNode
  }


  fn as_addr (&mut self) -> *mut u8 {
    self as *mut FreeNode as *mut u8
  }


  fn merge (mut self: &mut Self) { unsafe {
    let prev_addr = self.get_prev();

    if let Some(prev_node) = prev_addr.as_mut() {
      let prev_length = prev_node.get_length();

      if prev_node.as_addr().add(prev_length * Heap::BLOCK_SIZE) == self.as_addr() {
        prev_node.set_length(prev_length + self.get_length());
        prev_node.set_next(self.get_next());

        self = prev_node;
      }
    }

    let next_addr = self.get_next();

    if let Some(next_node) = next_addr.as_mut() {
      let cur_length = self.get_length();

      if self.as_addr().add(cur_length * Heap::BLOCK_SIZE) == next_node.as_addr() {
        self.set_length(cur_length + next_node.get_length());
        self.set_next(next_node.get_next());
      }
    }
  } }
}

/// A VTable used for garbage collected Heap interactions
pub struct VTable {
  /// This function must be provided, it gives the Heap the size and alignment required by the type's base representation
  pub get_initial_layout: *const fn () -> Layout,
  /// This function must be provided, it gives the Heap the size used by the given instance of the type
  pub get_value_size: *const fn (*const u8) -> usize,
  /// This function is optional, but it must be implemented if the type represented has internal pointers to garbage collected Heap allocations
  pub mark_traverse: *const fn (*const u8, &mut Heap),
  /// This function is optional, but it must be implemented if the type represented has code to execute upon deallocation
  pub handle_drop: *const fn (*mut u8),
}

impl VTable {
  fn get_initial_layout (&self) -> Layout {
    (unsafe { &*self.get_initial_layout })()
  }

  fn get_value_size (&self, data: *const u8) -> usize {
    (unsafe { &*self.get_value_size })(data)
  }

  fn mark_traverse (&self, data: *const u8, heap: &mut Heap) {
    if let Some(mark_traverse) = unsafe { self.mark_traverse.as_ref() } {
      (mark_traverse)(data, heap)
    }
  }

  fn handle_drop (&self, data: *mut u8) {
    if let Some(handle_drop) = unsafe { self.handle_drop.as_ref() } {
      (handle_drop)(data)
    }
  }
}


struct Header(u64, u64);

impl Header {
  const PTR_MASK: u64 = 0x00_00_FF_FF_FF_FF_FF_FF;
  const VAL_MASK: u64 = 0xFF_FF_00_00_00_00_00_00;

  fn new (vtable: u16, prev: *mut Header, next: *mut Header) -> Self {
    Self(
      (prev as u64 & Self::PTR_MASK) | ((vtable as u64) << 48),
       next as u64 & Self::PTR_MASK // mark is implicitly initialized to false
    )
  }

  fn clear_vtable (&mut self) {
    self.0 &= Self::PTR_MASK;
  }

  fn set_vtable (&mut self, vtable: u16) {
    self.clear_vtable();
    self.0 |= (vtable as u64) << 48;
  }

  fn clear_mark (&mut self) {
    self.1 &= Self::PTR_MASK;
  }

  fn set_mark (&mut self, mark: bool) {
    self.clear_mark();
    self.1 |= (mark as u64) << 48;
  }

  fn clear_prev (&mut self) {
    self.0 &= Self::VAL_MASK;
  }

  fn clear_next (&mut self) {
    self.1 &= Self::VAL_MASK;
  }

  fn set_prev (&mut self, prev: *mut Header) {
    self.clear_prev();
    self.0 |= prev as u64;
  }

  fn set_next (&mut self, next: *mut Header) {
    self.clear_next();
    self.1 |= next as u64;
  }


  fn get_vtable (&self) -> u16 {
    ((self.0 & Self::VAL_MASK) >> 48) as u16
  }

  fn get_mark (&self) -> bool {
    (self.1 & Self::VAL_MASK) != 0
  }

  fn get_prev (&self) -> *mut Header {
    (self.0 & Self::PTR_MASK) as *mut Header
  }

  fn get_next (&self) -> *mut Header {
    (self.1 & Self::PTR_MASK) as *mut Header
  }


  fn as_addr (&self) -> *mut u8 {
    self as *const Header as *mut u8
  }

  fn val_addr (&self) -> *mut u8 {
    unsafe { self.as_addr().add(Heap::BLOCK_SIZE) }
  }

  fn from_val_addr<'s> (val: *mut u8) -> &'s mut Self {
    unsafe { &mut *(val.sub(Heap::BLOCK_SIZE) as *mut Self) }
  }


  fn is_marked (&self) -> bool {
    self.get_mark()
  }

  fn mark (&mut self) {
    self.1 |= (true as u64) << 48;
  }
}


/// Garbage collected heap allocator
pub struct Heap {
  free_list_head: *mut FreeNode,
  header_list_head: *mut Header,
  vtable_list: *mut VTable,
  page_size: usize,
  vmem: *mut u8,
  page_count: usize,
  max_pages: usize,
}

impl Heap {
  /// The minimum size of allocation for a Heap
  pub const BLOCK_SIZE: usize = 16;

  /// Controls which FreeNode search algorithm is used, `first_fit` or `best_fit`
  const NODE_SEARCH: unsafe fn (&mut Self, usize) -> *mut u8 = Self::best_fit;

  /// Controls whether or not newly allocated blocks are zeroed before being returned to the caller
  const CLEAR_BLOCKS: bool = true;


  /// Create a new Heap and define its maximum memory usage in pages
  ///
  /// Note that this maximum memory usage is for *allocations*, and additional memory is required for bookkeeping.
  /// Specifically, 32 bytes of memory are needed per unique type registered with the Heap,
  /// in order to store vtables, up to a maximum of 65536 unique types. (A maximum of 2mb of overhead)
  pub fn new (max_pages: usize) -> Self {
    debug_assert!(max_pages != 0);

    unsafe {
      let page_size = page_size();
      let byte_len = max_pages * page_size;

      let vmem = alloc(byte_len);
      if unlikely(vmem.is_null()) {
        eprintln!("Initial virtual memory allocation of {} pages of size {} ({} bytes total) for Heap allocation storage failed", max_pages, page_size, byte_len);
        handle_alloc_error(Layout::from_size_align_unchecked(byte_len, page_size))
      }
      
      let vtable_list = alloc(size_of::<VTable>() * u16::MAX as usize) as *mut VTable;
      if unlikely(vtable_list.is_null()) {
        eprintln!("Initial virtual memory allocation of 2mb for Heap vtable storage failed");
        handle_alloc_error(Layout::from_size_align_unchecked(byte_len, page_size))
      }

      Self {
        free_list_head: ptr::null_mut(),
        header_list_head: ptr::null_mut(),
        vtable_list,
        page_size,
        vmem,
        page_count: 0,
        max_pages
      }
    }
  }


  /// Create a VTable for a given TypeID in a Heap
  /// # Safety
  /// This will overwrite any existing VTable
  pub unsafe fn access_vtable_memory (&mut self, type_id: TypeID, vtable: VTable) {
    let addr = self.vtable_list.add(type_id.0 as usize);
    commit(addr as _, size_of::<VTable>());
    addr.write(vtable)
  }

  
  /// Get the number of allocation blocks required to store a value of a given size
  pub fn calc_num_blocks (size: usize) -> usize {
    if size == 0 { 1 }
    else { align_addr(size, Self::BLOCK_SIZE) / Self::BLOCK_SIZE }
  }


  unsafe fn get_vtable (&self, vtable: u16) -> &VTable {
    &*self.vtable_list.add(vtable as usize)
  }

  unsafe fn get_header_vtable (&self, header: &Header) -> &VTable {
    self.get_vtable(header.get_vtable())
  }


  unsafe fn pop_node (&mut self, node: &mut FreeNode) -> *mut u8 {
    let prev = node.get_prev();
    let next = node.get_next();

    if let Some(prev_ref) = prev.as_mut() {
      prev_ref.set_next(next);
      
      if let Some(next_ref) = next.as_mut() {
        next_ref.set_prev(prev);
      }
    } else {
      if let Some(new_head) = next.as_mut() {
        new_head.clear_prev()
      }

      self.free_list_head = next;
    }

    node.as_addr()
  }

  unsafe fn partition_node_unchecked (&mut self, node: &mut FreeNode, num_blocks: usize) -> *mut u8 {
    let new_node = (node as *mut FreeNode).add(num_blocks);

    new_node.write(FreeNode::new(node.get_length() - num_blocks, node.get_prev(), node.get_next()));

    if let Some(prev) = node.get_prev().as_mut() {
      prev.set_next(new_node)
    }

    if let Some(next) = node.get_next().as_mut() {
      next.set_prev(new_node)
    }

    node.as_addr()
  }

  unsafe fn try_partition_node (&mut self, node: &mut FreeNode, num_blocks: usize) -> Option<*mut u8> {
    let node_len = node.get_length();
    
    use std::cmp::Ordering::*;
    match node_len.cmp(&num_blocks) {
      Equal => Some(self.pop_node(node)),

      Greater => {
        Some(self.partition_node_unchecked(node, num_blocks))
      },

      Less => {
        None
      }
    }
  }


  unsafe fn insert_header (&mut self, addr: *mut Header, vtable: u16) {
    if unlikely(self.header_list_head.is_null()) {
      self.header_list_head = addr;
      
      addr.write(Header::new(vtable, ptr::null_mut(), ptr::null_mut()));
    } else if unlikely(self.header_list_head > addr) {
      self.header_list_head = addr;
      (&mut *self.header_list_head).set_prev(addr);
      
      addr.write(Header::new(vtable, ptr::null_mut(), self.header_list_head));
    } else {
      let mut existing_header_addr = self.header_list_head;

      while let Some(existing_header) = existing_header_addr.as_mut() {
        if existing_header_addr < addr {
          let next_header_addr = existing_header.get_next();

          if next_header_addr.is_null() {
            existing_header.set_next(addr);

            addr.write(Header::new(vtable, existing_header_addr, ptr::null_mut()));
            return
          } else if next_header_addr > addr {
            let next_header = &mut *next_header_addr;
            
            existing_header.set_next(addr);
            next_header.set_prev(addr);

            addr.write(Header::new(vtable, existing_header_addr, next_header_addr));
            return
          } else {
            existing_header_addr = next_header_addr;
          }
        }
      }
      
      if cfg!(debug_assertions) {
        unreachable!()
      } else {
        unreachable_unchecked()
      }
    }
  }

  
  unsafe fn insert_free_node (&mut self, addr: *mut FreeNode, num_blocks: usize) {
    if unlikely(self.free_list_head.is_null()) {
      self.free_list_head = addr;
      
      addr.write(FreeNode::new(num_blocks, ptr::null_mut(), ptr::null_mut()));
    } else if unlikely(self.free_list_head > addr) {
      self.free_list_head = addr;
      (&mut *self.free_list_head).set_prev(addr);
      
      addr.write(FreeNode::new(num_blocks, ptr::null_mut(), self.free_list_head));
    } else {
      let search = #[inline(always)] || {
        let mut existing_node_addr = self.free_list_head;

        while let Some(existing_node) = existing_node_addr.as_mut() {
          if existing_node_addr < addr {
            let next_node_addr = existing_node.get_next();

            if next_node_addr.is_null() {
              existing_node.set_next(addr);

              addr.write(FreeNode::new(num_blocks, existing_node_addr, ptr::null_mut()));
              return
            } else if next_node_addr > addr {
              let next_node = &mut *next_node_addr;
              
              existing_node.set_next(addr);
              next_node.set_prev(addr);

              addr.write(FreeNode::new(num_blocks, existing_node_addr, next_node_addr));
              return
            } else {
              existing_node_addr = next_node_addr;
            }
          }
        }
        
        if cfg!(debug_assertions) {
          unreachable!()
        } else {
          unreachable_unchecked()
        }
      };

      search()
    }

    (&mut *addr).merge()
  }


  unsafe fn free_header (&mut self, addr: *mut Header) {
    let header = &*addr;
    let vtable = self.get_header_vtable(header);

    let val = header.val_addr();
    let alloc_size = vtable.get_value_size(val);

    vtable.handle_drop(val);

    let blocks_freed = Self::calc_num_blocks(alloc_size) + 1;

    self.insert_free_node(addr as _, blocks_freed)
  }


  /// Try to allocate without committing more memory or garbage collecting.
  /// Returns null if there is not a large enough sequence of blocks available
  /// # Safety
  /// Provided Layout alignment must be less than or equal to `Heap::BLOCK_SIZE`
  pub unsafe fn try_alloc (&mut self, type_id: TypeID) -> *mut u8 {
    let layout = self.get_vtable(type_id.0).get_initial_layout();

    // NOTE: Current freelist logic does not handle larger alignments than block size; 
    // this is currently considered an optimization, as block addresses are valid
    // for all alignments smaller than block size and it simplifies the logic.
    // However, if we should ever have to handle types with large alignments,
    // this will have to be overhauled, or block size will have to be increased
    if unlikely(layout.align() > Self::BLOCK_SIZE) {
      eprintln!("Tried to allocate value with an alignment {} larger than the maximum {}", layout.align(), Self::BLOCK_SIZE);
      handle_alloc_error(layout)
    }

    let blocks_needed = Self::calc_num_blocks(layout.size()) + 1;
    
    let mut addr = Self::NODE_SEARCH(self, blocks_needed);

    if !addr.is_null() {
      self.insert_header(addr as _, type_id.0);

      addr = addr.add(Self::BLOCK_SIZE);

      if Self::CLEAR_BLOCKS { addr.write_bytes(0, blocks_needed * Self::BLOCK_SIZE) }
    }

    addr
  }

  /// Manually deallocate an allocation.
  /// This normally does not need to be called directly.
  ///
  /// # Safety
  /// Caller must ensure that no other locations hold the given pointer, or they will be left dangling.
  /// Calling this with a pointer not allocated by this Heap is immediate UB
  pub unsafe fn dealloc (&mut self, addr: *mut u8) {
    self.free_header(Header::from_val_addr(addr));
  }



  #[allow(dead_code)]
  unsafe fn first_fit (&mut self, blocks_needed: usize) -> *mut u8 {
    let mut free_list_node = self.free_list_head;

    while let Some(node) = free_list_node.as_ref() {
      if let Some(addr) = self.try_partition_node(&mut *free_list_node, blocks_needed) {

        return addr
      } else {
        free_list_node = node.get_next()
      }
    }

    ptr::null_mut()
  }

  #[allow(dead_code)]
  unsafe fn best_fit (&mut self, blocks_needed: usize) -> *mut u8 {
    let mut free_list_node = self.free_list_head;

    let mut best_length = 0;
    let mut best_node: *mut FreeNode = ptr::null_mut();

    while let Some(node) = free_list_node.as_mut() {
      let node_length = node.get_length();

      use std::cmp::Ordering::*;
      match node_length.cmp(&blocks_needed) {
        Equal => {
          return self.pop_node(node)
        },

        Greater if (best_length == 0) | (node_length < best_length) => {
          best_length = node_length;
          best_node = node;
        },

        _ => { }
      }

      free_list_node = node.get_next()
    }

    if best_length != 0 {
      self.partition_node_unchecked(&mut *best_node, blocks_needed)
    } else {
      ptr::null_mut()
    }
  }
  
}

impl Drop for Heap {
  fn drop (&mut self) {
    unsafe { debug_assert!(dealloc(self.vmem, self.max_pages * self.page_size)) }
  }
}



static_assert!(Heap::BLOCK_SIZE >= size_of::<FreeNode>() && Heap::BLOCK_SIZE.is_power_of_two());

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_freenode_rw () {
    let node = FreeNode::new(u32::MAX as _, 0x00_00_FF_FF_FF_FF_FF_FFu64 as _, 512u64 as _);
    
    assert_eq!(node.get_length(), u32::MAX as _);
    assert_eq!(node.get_prev(), 0x00_00_FF_FF_FF_FF_FF_FFu64 as _);
    assert_eq!(node.get_next(), 512u64 as _);
  }

  #[test]
  fn test_allocator_handles_zst () {
    assert_eq!(Heap::calc_num_blocks(0), 1)
  }
}