//! Garbage collected heap allocator


use std::{
  ptr,
  mem::{ size_of, align_of },
  hint::unreachable_unchecked,  
  intrinsics::unlikely,
  alloc::{ handle_alloc_error, Layout }
};

use crate::{
  static_assert,
  ptr::align_addr,
  valloc::{ alloc, commit, dealloc, page_size },
  type_info::TypeID,
};


macro_rules! alloc_assert {
  ($cond:expr, $layout:expr $(, $fmt:literal $(, $($fmt_args:expr),+)?)?) => {
    if unlikely(!$cond) {
      $(eprintln!($fmt $(, $($fmt_args),+)? );)?
      handle_alloc_error($layout)
    }
  };
}


struct FreeNode(u64, u64);

impl FreeNode {
  const PTR_MASK: u64 = 0x00_00_FF_FF_FF_FF_FF_FF;
  const LEN_MASK: u64 = 0xFF_FF_00_00_00_00_00_00;
  const LEN0_MASK: u32 = 0xFF_FF_00_00;
  const LEN1_MASK: u32 = 0x00_00_FF_FF;

  fn new (length: usize) -> Self {
    debug_assert!(length <= u32::MAX as _);

    Self(
      ((length as u32 & Self::LEN0_MASK) as u64) << 32,
      ((length as u32 & Self::LEN1_MASK) as u64) << 48
    )
  }

  fn with_neighbors (length: usize, prev: *mut FreeNode, next: *mut FreeNode) -> Self {
    let mut out = Self::new(length);
    out.set_prev(prev);
    out.set_next(next);
    out
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


  unsafe fn merge (mut self: &mut Self) {
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
  }
}

/// A VTable used for garbage collected Heap interactions
pub struct VTable {
  /// This function must be provided, it gives the Heap the size and alignment required by the type
  pub get_layout: *const fn () -> Layout,
  /// This function is optional, but it must be implemented if the type represented has internal pointers to garbage collected Heap allocations
  pub mark_traverse: *const fn (*const u8, &mut Heap),
  /// This function is optional, but it must be implemented if the type represented has code to execute upon deallocation
  pub handle_drop: *const fn (*mut u8),
}

impl VTable {
  fn get_layout (&self) -> Layout {
    (unsafe { &*self.get_layout })()
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

#[allow(clippy::inconsistent_digit_grouping)]
impl Header {
  const PTR_MASK: u64            = 0b0_0_00000000000000_111111111111111111111111111111111111111111111111;
  const VALUE_MASK: u64          = 0b1_1_11111111111111_000000000000000000000000000000000000000000000000;

  const MARKER_MASK: u64         = 0b0_1_00000000000000_000000000000000000000000000000000000000000000000;
  const MANAGED_FLAG_MASK: u64   = 0b1_0_00000000000000_000000000000000000000000000000000000000000000000;

  const MANUAL_LENGTH_MASK0: u64 = 0b0_1_11111111111111_000000000000000000000000000000000000000000000000;
  const MANUAL_LENGTH_MASK1: u64 = Self::VALUE_MASK;

  const INPUT_LENGTH_MASK0: u32  = 0xFF_FF_00_00;
  const INPUT_LENGTH_MASK1: u32  = 0x00_00_FF_FF;
  
  const MAX_LENGTH: usize = 0b11111111111111111111111111111111;

  fn new_manual (length: usize) -> Self {
    debug_assert!(length <= Self::MAX_LENGTH);
    
    Self(
      ((length as u32 & Self::INPUT_LENGTH_MASK0) as u64) << 32,
      ((length as u32 & Self::INPUT_LENGTH_MASK1) as u64) << 48
    )
  }

  fn new_managed (vtable: u16) -> Self {
    Self(
      Self::MANAGED_FLAG_MASK,
      (vtable as u64) << 48
    )
  }

  fn is_managed (&self) -> bool {
    (self.0 & Self::MANAGED_FLAG_MASK) != 0
  }

  // fn clear_vtable (&mut self) {
  //   debug_assert!(self.is_managed());
  //   self.1 &= Self::PTR_MASK;
  // }

  // fn set_vtable (&mut self, vtable: u16) {
  //   self.clear_vtable();
  //   self.1 |= (vtable as u64) << 48;
  // }

  // fn clear_mark (&mut self) {
  //   self.1 &= Self::PTR_MASK;
  // }

  // fn set_mark (&mut self, mark: bool) {
  //   self.clear_mark();
  //   if mark {
  //     self.0 |= Self::MARKER_MASK
  //   } else {
  //     self.0 &= !Self::MARKER_MASK
  //   }
  // }

  fn clear_length (&mut self) {
    debug_assert!(!self.is_managed());
    self.0 &= !Self::MANUAL_LENGTH_MASK0;
    self.1 &= !Self::MANUAL_LENGTH_MASK1;
  }

  fn set_length (&mut self, length: usize) {
    self.clear_length();

    debug_assert!(length <= Self::MAX_LENGTH);

    self.0 |= ((length as u32 & Self::INPUT_LENGTH_MASK0) as u64) << 32;
    self.1 |= ((length as u32 & Self::INPUT_LENGTH_MASK1) as u64) << 48;
  }

  fn get_length (&self) -> usize {
    debug_assert!(!self.is_managed());

      (((self.0 & Self::MANUAL_LENGTH_MASK0) >> 32) as usize)
    | (((self.1 & Self::MANUAL_LENGTH_MASK1) >> 48) as usize)
  }

  fn clear_prev (&mut self) {
    self.0 &= Self::VALUE_MASK;
  }

  fn clear_next (&mut self) {
    self.1 &= Self::VALUE_MASK;
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
    ((self.1 & Self::VALUE_MASK) >> 48) as u16
  }

  fn get_mark (&self) -> bool {
    debug_assert!(self.is_managed());
    (self.0 & Self::MARKER_MASK) != 0
  }

  // fn get_prev (&self) -> *mut Header {
  //   (self.0 & Self::PTR_MASK) as *mut Header
  // }

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
    debug_assert!(self.is_marked());
    self.0 |= Self::MARKER_MASK;
  }
}


/// Represents a root data item in a Heap, where mark traversal begins
pub struct Root {
  addr: *const u8,
  length: usize,
  mark_traverse: *const fn (addr: *const u8, length: usize, heap: &mut Heap),
}

impl Root {
  fn mark_traverse (&self, heap: &mut Heap) {
    (unsafe { &*self.mark_traverse })(self.addr, self.length, heap)
  }
}



fn untyped_mark_traverse (addr: *const u8, length: usize, heap: &mut Heap) {
  let addr = align_addr(addr as _, align_of::<*const u8>());
  
  for maybe_addr in (addr..addr + length).step_by(size_of::<*const u8>()) {
    unsafe { heap.mark(maybe_addr as _) }
  }
}

/// Garbage collected heap allocator
pub struct Heap {
  free_list_head: *mut FreeNode,
  header_list_head: *mut Header,
  vtable_list: *mut VTable,
  root_list: *mut Root,
  root_count: u16,
  page_size: usize,
  vmem: *mut u8,
  page_count: usize,
  max_pages: usize,
}

static_assert!(Heap::BLOCK_SIZE >= size_of::<FreeNode>() && Heap::BLOCK_SIZE >= size_of::<Header>() && Heap::BLOCK_SIZE.is_power_of_two());

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
  /// Specifically:
  /// + 32 bytes of memory are needed per unique type registered with the Heap, in order to store vtables,
  /// up to a maximum of 65536 unique types. (A maximum of 2mb of overhead)
  /// + 16 bytes of memory are needed per unique root node, in order to store registrations,
  /// up to a maximum of 65536 unique roots. (A maximum of 1mb of overhead)
  pub fn new (max_pages: usize) -> Self {
    debug_assert!(max_pages != 0);

    unsafe {
      let page_size = page_size();
      let byte_len = max_pages * page_size;

      let vmem = alloc(byte_len);
      alloc_assert!(
        !vmem.is_null(),
        Layout::from_size_align_unchecked(byte_len, page_size),
        "Initial virtual memory allocation of {} pages of size {} ({} bytes total) for Heap allocation storage failed",
        max_pages, page_size, byte_len
      );
      
      let vtable_bytes = size_of::<VTable>() * u16::MAX as usize;
      let vtable_list = alloc(vtable_bytes) as *mut VTable;

      alloc_assert!(
        !vtable_list.is_null(),
        Layout::from_size_align_unchecked(vtable_bytes, page_size),
        "Initial virtual memory allocation of 2mb for Heap vtable storage failed"
      );

      let root_bytes = size_of::<Root>() * u16::MAX as usize;
      let root_list = alloc(root_bytes) as *mut Root;

      alloc_assert!(
        !root_list.is_null(),
        Layout::from_size_align_unchecked(root_bytes, page_size),
        "Initial virtual memory allocation of 1mb for Heap root storage failed"
      );

      Self {
        free_list_head: ptr::null_mut(),
        header_list_head: ptr::null_mut(),
        vtable_list,
        root_list,
        root_count: 0,
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
  pub unsafe fn set_vtable (&mut self, type_id: TypeID, vtable: VTable) {
    let addr = self.vtable_list.add(type_id.0 as usize);
    commit(addr as _, size_of::<VTable>());
    addr.write(vtable)
  }


  /// Register a root node in a Heap, for a given variable.
  /// Returns a unique ID for the root registered
  /// # Safety
  /// The caller must ensure only one Root is added for a given address.
  /// If `u16::MAX` roots are exceeded, this function will panic
  pub unsafe fn add_root (&mut self, addr: *mut u8, length: usize, mut mark_traverse: *const fn (*const u8, usize, &mut Heap)) -> u16 {
    assert!(self.root_count < u16::MAX, "Max root (global) variable count exceeded");
    let id = self.root_count;
    self.root_count += 1;

    if mark_traverse.is_null() {
      mark_traverse = &untyped_mark_traverse as *const _ as _;
    }
    
    self.root_list.add(id as usize).write(Root { addr, length, mark_traverse });

    id
  }

  /// Change the length associated with a root node in a Heap, for a given variable
  /// # Safety
  /// The caller must ensure the Root referenced has been registered
  pub unsafe fn set_root_length (&mut self, root_id: u16, new_length: usize) {
    debug_assert!(self.root_count > root_id);
    let root = &mut *self.root_list.add(root_id as usize);
    root.length = new_length;
  }

  
  /// Get the number of blocks a page can contain
  pub fn blocks_per_page (&self) -> usize {
    self.page_size / Self::BLOCK_SIZE
  }

  /// Get the number of pages required for a value of a given size
  pub fn calc_num_pages (&self, size: usize) -> usize {
    align_addr(size, self.page_size) / self.page_size
  }


  /// Get the number of allocation blocks required to store a value of a given size
  pub fn calc_num_blocks (size: usize) -> usize {
    align_addr(size, Self::BLOCK_SIZE) / Self::BLOCK_SIZE
  }


  unsafe fn get_vtable (&self, vtable: u16) -> &'static VTable {
    &*self.vtable_list.add(vtable as usize)
  }

  unsafe fn get_header_vtable (&self, header: &Header) -> &'static VTable {
    self.get_vtable(header.get_vtable())
  }

  // unsafe fn get_header_size (&self, header: &Header) -> usize {
  //   if header.is_managed() {
  //     self.get_header_vtable(header).get_layout().size()
  //   } else {
  //     header.get_length()
  //   }
  // }

  // unsafe fn get_header_block_size (&self, header: &Header) -> usize {
  //   Self::calc_num_blocks(self.get_header_size(header))
  // }


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

    new_node.write(FreeNode::with_neighbors(node.get_length() - num_blocks, node.get_prev(), node.get_next()));

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


  unsafe fn insert_header (&mut self, addr: *mut Header) {
    let new_header = &mut *addr;
    if unlikely(self.header_list_head.is_null()) {
      self.header_list_head = addr;
    } else if unlikely(self.header_list_head > addr) {
      new_header.set_next(self.header_list_head);
      (&mut *self.header_list_head).set_prev(addr);
      self.header_list_head = addr;
    } else {
      let mut existing_header_addr = self.header_list_head;

      while let Some(existing_header) = existing_header_addr.as_mut() {
        if existing_header_addr < addr {
          let next_header_addr = existing_header.get_next();

          if next_header_addr.is_null() {
            existing_header.set_next(addr);

            new_header.set_prev(existing_header);
            return
          } else if next_header_addr > addr {
            let next_header = &mut *next_header_addr;
            
            existing_header.set_next(addr);
            next_header.set_prev(addr);

            new_header.set_prev(existing_header);
            new_header.set_next(next_header);
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

  
  unsafe fn insert_free_node (&mut self, addr: *mut FreeNode) {
    let new_node = &mut *addr;

    if unlikely(self.free_list_head.is_null()) {
      self.free_list_head = addr;
    } else if unlikely(self.free_list_head > addr) {
      (&mut *self.free_list_head).set_prev(addr);
      new_node.set_next(self.free_list_head);
      self.free_list_head = addr;
    } else {
      let mut search = #[inline(always)] || {
        let mut existing_node_addr = self.free_list_head;

        while let Some(existing_node) = existing_node_addr.as_mut() {
          if existing_node_addr < addr {
            let next_node_addr = existing_node.get_next();

            if next_node_addr.is_null() {
              existing_node.set_next(addr);

              new_node.set_prev(existing_node);
              return
            } else if next_node_addr > addr {
              let next_node = &mut *next_node_addr;
              
              existing_node.set_next(addr);
              next_node.set_prev(addr);

              new_node.set_prev(existing_node);
              new_node.set_next(next_node);
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

    let blocks_freed = Self::calc_num_blocks(if header.is_managed() {
      let vtable = self.get_header_vtable(header);

      let val = header.val_addr();
      let alloc_size = vtable.get_layout().size();

      vtable.handle_drop(val);

      alloc_size
    } else {
      header.get_length()
    }) + 1;

    let node_addr = addr as *mut FreeNode;
    node_addr.write(FreeNode::new(blocks_freed));

    self.insert_free_node(node_addr)
  }


  unsafe fn try_alloc_impl (&mut self, layout: Layout, type_id: Option<TypeID>) -> *mut u8 {
    // NOTE: Current freelist logic does not handle larger alignments than block size; 
    // this is currently considered an optimization, as block addresses are valid
    // for all alignments smaller than block size and it simplifies the logic.
    // However, if we should ever have to handle types with large alignments,
    // this will have to be overhauled, or block size will have to be increased
    alloc_assert!(
      layout.align() <= Self::BLOCK_SIZE,
      layout,
      "Tried to allocate value with an alignment {} larger than the maximum {}",
      layout.align(), Self::BLOCK_SIZE
    );

    let blocks_needed = Self::calc_num_blocks(layout.size());
    
    let mut addr = Self::NODE_SEARCH(self, blocks_needed + 1);

    if !addr.is_null() {
      let header_addr = addr as *mut Header;
      
      header_addr.write(if let Some(type_id) = type_id {
        Header::new_managed(type_id.0)
      } else {
        Header::new_manual(layout.size())
      });

      self.insert_header(header_addr);

      addr = addr.add(Self::BLOCK_SIZE);

      if Self::CLEAR_BLOCKS { addr.write_bytes(0, blocks_needed * Self::BLOCK_SIZE) }
    }

    addr
  }

  /// Try to allocate garbage collected memory, without committing more memory or garbage collecting.
  /// Returns null if there is not a large enough sequence of blocks available
  /// # Safety
  /// Provided Layout alignment must be less than or equal to `Heap::BLOCK_SIZE`.
  /// Calls `handle_alloc_error` if this invariant does not hold
  pub unsafe fn try_alloc_managed (&mut self, type_id: TypeID) -> *mut u8 {
    self.try_alloc_impl(self.get_vtable(type_id.0).get_layout(), Some(type_id))
  }

  /// Try to allocate manually managed memory, without committing more memory or garbage collecting.
  /// Returns null if there is not a large enough sequence of blocks available
  /// # Safety
  /// Provided Layout alignment must be less than or equal to `Heap::BLOCK_SIZE`.
  /// Calls `handle_alloc_error` if this invariant does not hold
  pub unsafe fn try_alloc_manual (&mut self, layout: Layout) -> *mut u8 {
    self.try_alloc_impl(layout, None)
  }


  unsafe fn collected_alloc_impl (&mut self, layout: Layout, type_id: Option<TypeID>) -> *mut u8 {
    let mut addr = self.try_alloc_impl(layout, type_id);

    if addr.is_null() {
      self.mark_and_sweep();
      addr = self.try_alloc_impl(layout, type_id);
    }
    
    addr
  }

  /// Try to allocate garbage collected memory, performing garbage collection if necessary, but not allocating new memory.
  /// Returns null if there is not a large enough sequence of blocks available
  /// # Safety
  /// Provided Layout alignment must be less than or equal to `Heap::BLOCK_SIZE`.
  /// Calls `handle_alloc_error` if this invariant does not hold
  pub unsafe fn collected_alloc_managed (&mut self, type_id: TypeID) -> *mut u8 {
    self.collected_alloc_impl(self.get_vtable(type_id.0).get_layout(), Some(type_id))
  }

  /// Try to allocate manually managed memory, performing garbage collection if necessary, but not allocating new memory.
  /// Returns null if there is not a large enough sequence of blocks available
  /// # Safety
  /// Provided Layout alignment must be less than or equal to `Heap::BLOCK_SIZE`.
  /// Calls `handle_alloc_error` if this invariant does not hold
  pub unsafe fn collected_alloc_manual (&mut self, layout: Layout) -> *mut u8 {
    self.collected_alloc_impl(layout, None)
  }


  unsafe fn alloc_impl (&mut self, layout: Layout, type_id: Option<TypeID>) -> *mut u8 {
    let mut addr = self.collected_alloc_impl(layout, type_id);

    if addr.is_null() {
      let rem_pages = self.max_pages - self.page_count;

      let req_pages = self.calc_num_pages(layout.size());
      let req_blocks = Self::calc_num_blocks(layout.size());
      
      if req_pages <= rem_pages {
        addr = self.vmem.add(self.page_count * self.page_size);

        let num_blocks = self.blocks_per_page() * req_pages;

        let header_addr = addr as *mut Header;
        
        header_addr.write(if let Some(type_id) = type_id {
          Header::new_managed(type_id.0)
        } else {
          Header::new_manual(layout.size())
        });

        self.insert_header(header_addr);

        addr = addr.add(Self::BLOCK_SIZE);

        if Self::CLEAR_BLOCKS { addr.write_bytes(0, req_blocks * Self::BLOCK_SIZE) }


        let extra_blocks = num_blocks - req_blocks;

        if extra_blocks > 0 {
          let node_addr = addr.add(req_blocks * Self::BLOCK_SIZE) as *mut FreeNode;

          node_addr.write(FreeNode::new(extra_blocks));

          self.insert_free_node(node_addr);
        }
      }
    }

    addr
  }


  /// Try to allocate garbage collected memory, performing garbage collection if necessary, and allocating new memory if that fails.
  /// Returns null if there is not a large enough sequence of blocks available
  /// # Safety
  /// Provided Layout alignment must be less than or equal to `Heap::BLOCK_SIZE`.
  /// Calls `handle_alloc_error` if this invariant does not hold
  pub unsafe fn alloc_managed (&mut self, type_id: TypeID) -> *mut u8 {
    self.alloc_impl(self.get_vtable(type_id.0).get_layout(), Some(type_id))
  }

  /// Try to allocate manually managed memory, performing garbage collection if necessary, and allocating new memory if that fails.
  /// Returns null if there is not a large enough sequence of blocks available
  /// # Safety
  /// Provided Layout alignment must be less than or equal to `Heap::BLOCK_SIZE`.
  /// Calls `handle_alloc_error` if this invariant does not hold
  pub unsafe fn alloc_manual (&mut self, layout: Layout) -> *mut u8 {
    self.alloc_impl(layout, None)
  }


  /// Reallocate a manuall managed allocation, performing garbage collection if necessary, or allocating new memory of that fails.
  /// Returns null if there is not enough room to fit the new size
  /// # Safety
  /// Calling this with a pointer not allocated by this Heap is immediate UB
  pub unsafe fn realloc_manual (&mut self, addr: *mut u8, new_length: usize) -> *mut u8 {
    let header = Header::from_val_addr(addr);

    let old_length = header.get_length();

    if unlikely(old_length == new_length) { return addr }

    let old_block_count = Self::calc_num_blocks(old_length);
    let new_block_count = Self::calc_num_blocks(new_length);

    if new_length < old_length {
      let free_block_count = old_block_count - new_block_count;
      header.set_length(new_length);
      let node_addr = addr.add(new_block_count * Self::BLOCK_SIZE) as *mut FreeNode;
      node_addr.write(FreeNode::new(free_block_count));
      self.insert_free_node(node_addr);

      addr
    } else {
      let new_addr = self.alloc_manual(Layout::from_size_align_unchecked(new_length, Self::BLOCK_SIZE));
      ptr::copy_nonoverlapping(addr, new_addr, new_length);
      self.dealloc(addr);
      new_addr
    }
  }



  /// Manually deallocate an allocation.
  ///
  /// # Safety
  /// Caller must ensure that no other locations hold the given pointer, or they will be left dangling.
  /// Calling this with a pointer not allocated by this Heap is immediate UB
  pub unsafe fn dealloc (&mut self, addr: *mut u8) {
    self.free_header(Header::from_val_addr(addr));
  }


  /// Perform a mark traversal on the roots of a Heap
  /// # Safety
  /// This call must be followed by a call to sweep
  pub unsafe fn traverse (&mut self) {
    for root_idx in 0..self.root_count {
      let root = &mut *self.root_list.add(root_idx as usize);
      root.mark_traverse(self)
    }
  }
  
  /// Mark an address in a Heap during a mark traversal, preventing its garbage collection
  /// Note that this does nothing if the address does not point within a managed allocation in the Heap
  /// # Safety
  /// This should only be done as part of a traversal, and must be followed by a sweep
  pub unsafe fn mark (&mut self, addr: *const u8) {
    let mut header_addr = self.header_list_head;

    while let Some(header) = header_addr.as_mut() {
      if header.is_managed() {
        let vtable = self.get_header_vtable(header);
        let length = vtable.get_layout().size();

        if (addr >= header.as_addr())
         & (addr <= header.as_addr().add(length))
         & !header.is_marked() {
          header.mark();

          vtable.mark_traverse(header.as_addr(), self)
        }
      }

      header_addr = header.get_next()
    }
  }


  /// Sweep the heap, garbage collecting any managed allocations that are not marked
  /// # Safety
  /// This must be proceeded by a call to traverse, or all managed allocations will be collected
  pub unsafe fn sweep (&mut self) {
    let mut header_addr = self.header_list_head;

    while let Some(header) = header_addr.as_mut() {
      if header.is_managed() & !header.is_marked() {
        self.dealloc(header.as_addr())
      }

      header_addr = header.get_next()
    }
  }


  /// Mark and sweep the heap, performing a full round of garbage collection
  /// # Safety
  /// This is safe to do if all invariants of the Heap have been maintained
  pub unsafe fn mark_and_sweep (&mut self) {
    self.traverse();
    self.sweep()
  }



  #[allow(dead_code)]
  unsafe fn first_fit (&mut self, blocks_needed: usize) -> *mut u8 {
    let mut free_list_node = self.free_list_head;

    while let Some(node) = free_list_node.as_mut() {
      if let Some(addr) = self.try_partition_node(node, blocks_needed) {
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
  fn drop (&mut self) { unsafe { 
    let mut addr = self.header_list_head;

    while let Some(header) = addr.as_mut() {
      addr = header.get_next(); // must be done first because freeing the header creates a FreeNode overwriting the data
      
      self.free_header(header);
    }

    let res = dealloc(self.vmem, self.max_pages * self.page_size);

    debug_assert!(res)
  } }
}