/// Calculate the offset needed to align a pointer.
///
/// Substitute for the code in the [rust core ptr impl](https://github.com/rust-lang/rust/blob/master/library/core/src/ptr/mod.rs#L1118-L1255);
///
/// Note that the number given here is an amount of *bytes*, not a count of values of T, and is not necessarily a multiple of `size_of::<T>()`
pub fn align_offset<T> (ptr: *const T, align: usize) -> usize {
  align - (ptr as usize & (align - 1))
}