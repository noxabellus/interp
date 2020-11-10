//! Pointer manipulation utilities

/// Align an `address` to a multiple of `alignment`
pub fn align_addr (address: usize, alignment: usize) -> usize {
  debug_assert!(alignment.is_power_of_two());
  (address + alignment - 1) & !(alignment - 1)
}

/// Calculate the offset required to align an `address` to a multiple of `alignment`
pub fn get_align_offset (address: usize, alignment: usize) -> usize {
  align_addr(address, alignment) - address
}