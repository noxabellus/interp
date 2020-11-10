//! wip interpreter

#![allow(
  incomplete_features,
  clippy::inconsistent_digit_grouping,
  clippy::unusual_byte_groupings
)]

#![warn(missing_docs)]

#![feature(
  test,
  specialization,
  try_trait,
  trait_alias,
  assoc_char_funcs,
  associated_type_defaults,
  associated_type_bounds,
  core_intrinsics,
  const_fn, const_panic, const_fn_union, const_discriminant, const_fn_transmute,
  variant_count,
  stmt_expr_attributes,
  llvm_asm
)]

extern crate self as interp;

mod static_assert;
mod unchecked_destructure;
pub mod ptr;
pub mod fnv1a;
pub mod valloc;
pub mod typeinfo;
pub mod value;
pub mod object;
pub mod stack;
pub mod instruction;
pub mod global;
pub mod vm;