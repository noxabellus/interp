//! Contains a virtual memory allocator abstraction over platform implementations

#![allow(dead_code)]

pub use std::alloc::handle_alloc_error;


#[cfg(not(target_os = "windows"))]
mod os {
  #[allow(non_camel_case_types)]
  mod raw {
    use std::os::raw::c_void;

    #[repr(i32)]
    pub(super) enum PROT {
      NONE = 0,
      // READ = 1,
      // WRITE = 2,
      // EXEC = 4,
      READ_WRITE = 1|2,
      // READ_EXEC = 1|4,
      // WRITE_EXEC = 2|4,
      // READ_WRITE_EXEC = 1|2|4,
    }

    #[repr(i32)]
    pub(super) enum MAP {
      // FILE = 0x0000,
      // SHARED = 0x0001,
      // PRIVATE = 0x0002,
      // FIXED = 0x0010,
      // HUGETLB = 0x040000,
      // LOCKED = 0x02000,
      // NORESERVE = 0x04000,
      // THIRTY_TWO_BIT = 0x0040,
      // ANONYMOUS = 0x0020,
      // DENYWRITE = 0x0800,
      // EXECUTABLE = 0x01000,
      // POPULATE = 0x08000,
      // NONBLOCK = 0x010000,
      // STACK = 0x020000,
      // SYNC = 0x080000,
      // SHARED_VALIDATE = 0x3,
      // FIXED_NOREPLACE = 0x100000,
      // GROWSDOWN = 0x0100,
      PRIVATE_ANONYMOUS = 0x0002|0x0020,
    }

    pub(super) const MAP_FAILED: *mut c_void = -1 as _;
    pub(super) const _SC_PAGESIZE: i32 = 30;

    extern "C" {
      pub(super) fn mmap (
          addr: *mut c_void, 
          len: usize, 
          prot: PROT, 
          flags: MAP, 
          fd: i32, 
          offset: i64
      ) -> *mut c_void;

      pub(super) fn munmap (addr: *mut c_void, len: usize) -> i32;

      pub(super) fn mprotect (addr: *mut c_void, len: usize, prot: PROT) -> i32;

      pub(super) fn sysconf (name: i32) -> i64;


      #[cfg(debug_assertions)]
      pub(super) fn __errno_location () -> *mut i32;
    }
  }



  #[cfg(debug_assertions)]
  unsafe fn clear_errno () { raw::__errno_location().write(0) }

  #[cfg(debug_assertions)]
  unsafe fn read_errno () -> i32 { raw::__errno_location().read() }



  #[inline]
  pub(super) unsafe fn virtual_alloc (max_size: usize) -> *mut u8 {
    #[cfg(debug_assertions)]
    clear_errno();

    let data = raw::mmap(std::ptr::null_mut(), max_size, raw::PROT::NONE, raw::MAP::PRIVATE_ANONYMOUS, -1, 0);

    if data != raw::MAP_FAILED {
      data as _
    } else {
      if cfg!(debug_assertions) {
        let e = read_errno();

        eprintln!("mmap error for size {}: {}", max_size, e);
      }

      std::ptr::null_mut()
    }
  }

  #[inline]
  pub(super) unsafe fn virtual_commit (addr: *mut u8, size: usize) -> bool {
    #[cfg(debug_assertions)]
    clear_errno();

    let ret = raw::mprotect(addr as _, size, raw::PROT::READ_WRITE);

    if cfg!(debug_assertions) && ret != 0 {
      let e = read_errno();

      eprintln!("mprotect(commit) error for addr {:?} and size {}: {}", addr, size, e);
    }
    
    ret == 0
  }

  #[inline]
  pub(super) unsafe fn virtual_uncommit (addr: *mut u8, size: usize) -> bool {
    #[cfg(debug_assertions)]
    clear_errno();

    let ret = raw::mprotect(addr as _, size, raw::PROT::NONE);

    if cfg!(debug_assertions) && ret != 0 {
      let e = read_errno();

      eprintln!("mprotect(uncommit) error for addr {:?} and size {}: {}", addr, size, e);
    }
    
    ret == 0
  }

  #[inline]
  pub(super) unsafe fn virtual_free (addr: *mut u8, size: usize) -> bool {
    #[cfg(debug_assertions)]
    clear_errno();

    let ret = raw::munmap(addr as _, size);

    if cfg!(debug_assertions) && ret != 0 {
      let e = read_errno();

      eprintln!("munmap error for addr {:?} and size {}: {:?}", addr, size, e);
    }

    ret == 0
  }


  #[inline]
  pub(super) unsafe fn page_size () -> usize {
    let ret = raw::sysconf(raw::_SC_PAGESIZE);

    assert!(ret != -1, "Non-standard unix system? Could not query page size");

    ret as usize
  }
}


#[cfg(target_os = "windows")]
mod os {
  #[allow(non_snake_case, non_camel_case_types)]
  mod raw {
    use std::os::raw::c_void;

    #[repr(u32)]
    pub(super) enum MEM {
      COMMIT = 0x1000,
      RESERVE = 0x2000,
      DECOMMIT = 0x4000,
      RELEASE = 0x8000,
      // FREE = 0x10000,
      // PRIVATE = 0x20000,
      // MAPPED = 0x40000,
      // RESET = 0x80000,
      // TOP_DOWN = 0x100000,
      // WRITE_WATCH = 0x200000,
      // PHYSICAL = 0x400000,
      // ROTATE_OR_DIFFERENT_IMAGE_BASE_OK = 0x800000,
      // RESET_UNDO = 0x1000000,
      // LARGE_PAGES = 0x20000000,
      // FOUR_MB_PAGES = 0x80000000,
      // SIXTY_FOUR_K_PAGES = 0x20000000 | 0x400000,
    }

    #[repr(u32)]
    pub(super) enum PAGE {
      // NOACCESS = 0x01,
      // READONLY = 0x02,
      READWRITE = 0x04,
      // WRITECOPY = 0x08,
      // EXECUTE = 0x10,
      // EXECUTE_READ = 0x20,
      // EXECUTE_READWRITE = 0x40,
      // EXECUTE_WRITECOPY = 0x80,
      // GUARD = 0x100,
      // NOCACHE = 0x200,
      // WRITECOMBINE = 0x400,
      // ENCLAVE_THREAD_CONTROL_OR_REVERT_TO_FILE_MAP = 0x80000000,
      // TARGETS_NO_UPDATE_OR_TARGETS_INVALID = 0x40000000,
      // ENCLAVE_UNVALIDATED = 0x20000000,
      // ENCLAVE_DECOMMIT = 0x10000000,
    }

    #[repr(C)]
    #[derive(Clone, Copy)]
    struct DUMMYSTRUCTNAME {
      _wProcessorArchitecture: u16,
      _wReserved: u16,
    }

    #[repr(C)]
    #[derive(Clone, Copy)]
    union DUMMYUNIONNAME {
      _dwOemId: u32,
      _s: DUMMYSTRUCTNAME
    }

    #[repr(C)]
    #[derive(Clone, Copy)]
    pub(super) struct SYSTEM_INFO {
      _u: DUMMYUNIONNAME,
      pub dwPageSize: u32,
      _lpMinimumApplicationAddress: *mut c_void,
      _lpMaximumApplicationAddress: *mut c_void,
      _dwActiveProcessorMask: *mut u32,
      _dwNumberOfProcessors: u32,
      _dwProcessorType: u32,
      _dwAllocationGranularity: u32,
      _wProcessorLevel: u16,
      _wProcessorRevision: u16,
    }

    

    extern "system" {
      pub(super) fn VirtualAlloc (
        lpAddress: *mut c_void,
        dwSize: usize,
        flAllocationType: MEM,
        flProtect: PAGE,
      ) -> *mut c_void;

      pub(super) fn VirtualFree (
        lpAddress: *mut c_void,
        dwSize: usize,
        dwFreeType: MEM,
      ) -> i32;

      pub(super) fn GetSystemInfo (lpSystemInfo: *mut SYSTEM_INFO);


      #[cfg(debug_assertions)]
      pub(super) fn GetLastError () -> u32;
    }
  }


  #[inline]
  pub(super) unsafe fn virtual_alloc (max_size: usize) -> *mut u8 {
    let data = raw::VirtualAlloc(std::ptr::null_mut(), max_size, raw::MEM::RESERVE, raw::PAGE::READWRITE);

    if cfg!(debug_assertions) && data.is_null() {
      eprintln!("VirtualAlloc(new) for size {} failed: {}", max_size, raw::GetLastError())
    }

    data as _
  }

  #[inline]
  pub(super) unsafe fn virtual_commit (addr: *mut u8, size: usize) -> bool {
    let ret = raw::VirtualAlloc(addr as _, size, raw::MEM::COMMIT, raw::PAGE::READWRITE);

    if cfg!(debug_assertions) && ret.is_null() {
      eprintln!("VirtualAlloc(commit) for addr {:?} and size {} failed: {}", addr, size, raw::GetLastError())
    }

    !ret.is_null()
  }

  #[inline]
  pub(super) unsafe fn virtual_uncommit (addr: *mut u8, size: usize) -> bool {
    let ret = raw::VirtualFree(addr as _, size, raw::MEM::DECOMMIT);

    if cfg!(debug_assertions) && ret == 0 {
      eprintln!("VirtualFree(decommit) for addr {:?} and size {} failed: {}", addr, size, raw::GetLastError())
    }

    ret != 0
  }

  #[inline]
  pub(super) unsafe fn virtual_free (addr: *mut u8, _size: usize) -> bool {
    let ret = raw::VirtualFree(addr as _, 0, raw::MEM::RELEASE);

    if cfg!(debug_assertions) && ret == 0 {
      eprintln!("VirtualFree(release) for addr {:?} and (unused) size {} failed: {}", addr, _size, raw::GetLastError())
    }

    ret != 0
  }


  #[inline]
  pub(super) unsafe fn page_size () -> usize {
    let mut info = std::mem::MaybeUninit::uninit();
    raw::GetSystemInfo(info.as_mut_ptr());
    info.assume_init().dwPageSize as usize
  }
}


/// Create a virtual memory allocation of the designated size.
///
/// Allocated vmem must be committed with `commit` before it may be used.
///
/// The memory allocated is purely virtual until committed,
/// so feel free to allocate the maximum amount you will ever realistically use (e.g. gb's).
///
/// Committing a larger area of memory will never cause the base pointer of the allocation to change,
/// allowing growth with address stability.
///
/// Unused areas may be uncommitted with `uncommit` to give physical memory back to the system,
/// but this is not required; one can simply call `dealloc` when done with the total allocation
///
/// # Safety
/// Generally, the same safety concerns as with all allocators apply:
/// + Allocation failure results in null pointer, which must be handled by the caller.
/// + You must manually initialize and drop any values stored here.
///
/// In addition to the usual, there is the aforementioned fact that allocated memory must be committed before use.
///
/// The alignment of values you are allocating must be smaller than a page size, but this is unlikely to be a concern as pages are usually ~4kb
pub unsafe fn alloc (max_size: usize) -> *mut u8 {
  os::virtual_alloc(max_size)
}


/// Commit a region of virtual memory to physical memory, preparing it for use.
///
/// Returns `true` if the memory was successfully committed
/// (e.g. the provided range was a valid virtual memory region)
///
/// Multiple calls for the same region are accepted
///
/// # Safety
/// This function must be called before the given area of virtual memory may be used,
/// failure to do so will cause segfault.
///
/// Please note that un/commits take place at the page level,
/// so this may affect data outside the range provided if it is not page-aligned
pub unsafe fn commit (addr: *mut u8, size: usize) -> bool {
  os::virtual_commit(addr, size)
}


/// Uncommit a region of virtual memory from physical memory, freeing it for reuse by the operating system.
///
/// Returns `true` if the memory was successfully uncommitted
/// (e.g. the provided range was a valid virtual memory region)
///
/// Multiple calls for the same region are accepted
///
/// # Safety
/// Further access attempts of the designated region with out first calling `commit` will result in segfault after this function is called.
///
/// Please note that un/commits take place at the page level,
/// so this may affect data outside the range provided if it is not page-aligned
pub unsafe fn uncommit (addr: *mut u8, size: usize) -> bool {
  os::virtual_uncommit(addr, size)
}


/// Free a virtual memory allocation, returning all pages, committed or otherwise for reuse by the operating system.
///
/// Returns `true` if the memory was successfully freed
/// (e.g. the provided range was a valid virtual memory region)
///
/// # Safety
/// Further access attempts of the designated region will result in segfault after this function is called.
/// 
/// Unlike with `uncommit`, regions passed to `dealloc` are not valid for further calls to `commit`.
///
/// Please note that frees take place at the page level,
/// so this may affect data outside the range provided if it is not page-aligned
pub unsafe fn dealloc (addr: *mut u8, size: usize) -> bool {
  os::virtual_free(addr, size)
}


/// Get the page size used by the virtual memory allocator
///
/// # Safety
/// While this should always be safe to do on standards-compliant systems,
/// it is potentially an expensive operation; so it should probably only be done
/// when initializing systems that utilizes virtual allocators, and should have its result cached
pub unsafe fn page_size () -> usize {
  os::page_size()
}



#[cfg(test)]
mod tests {
  use super::*;
  
  #[test]
  fn test_valloc () { unsafe {
    let page_size = dbg!(page_size());

    let mem_size = 1024 * 1024 * 1024; 

    let vmem = alloc(mem_size);
    assert!(!vmem.is_null());

    assert!(commit(vmem.add(page_size), 1024 * 1024 * 2));

    for i in 0..1024 * 1024 * 2 {
      std::ptr::write(vmem.add(i + page_size), i as u8)
    }

    assert!(uncommit(vmem, page_size + 1024 * 1024));

    assert!(dealloc(vmem, mem_size));
  } }

  #[test]
  fn test_page_align () { unsafe {
    let page_size = page_size();

    let mem_size = 1024 * 1024;

    let vmem = alloc(mem_size) as usize;


    for &align in &[1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024] {
      if align <= page_size {
        assert_eq!(crate::utils::align_addr(vmem, align), vmem)
      }
    }
  } }

  // This test should segfault, but I dont feel like handling segfaults to make the test workable
  // #[test]
  // fn test_uncommit () { unsafe {
  //   let page_size = dbg!(page_size());

  //   let mem_size = 1024 * 1024 * 128; 

  //   let vmem = alloc(mem_size);
  //   assert!(!vmem.is_null());

  //   assert!(commit(vmem, 1024 * 1024 * 2));

  //   for i in 0..1024 * 1024 * 2 {
  //     std::ptr::write(vmem.add(i), i as u8)
  //   }

  //   assert!(uncommit(vmem.add(1024 * 1024), 1024 * 1024 * 2));

  //   std::ptr::write(vmem, 0); // okay
  //   std::ptr::write(vmem.add(1024 * 1024), 0); // segfault

  //   assert!(dealloc(vmem, mem_size));
  // } }
}