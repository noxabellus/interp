//! wip interpreter

#![allow(
  incomplete_features,
  clippy::inconsistent_digit_grouping,
  clippy::unusual_byte_groupings,
  clippy::match_ref_pats,
)]

#![warn(
  missing_docs,
  clippy::if_same_then_else,
)]

#![feature(
  test,
  specialization,
  try_trait,
  trait_alias,
  array_methods,
  assoc_char_funcs,
  associated_type_defaults,
  associated_type_bounds,
  core_intrinsics,
  const_fn, const_panic, const_fn_union, const_discriminant, const_fn_transmute, const_impl_trait,
  min_const_generics,
  impl_trait_in_bindings,
  variant_count,
  box_syntax, box_patterns,
  or_patterns, bindings_after_at,
  stmt_expr_attributes,
  llvm_asm,
)]

extern crate self as interp;

extern crate interp_macros as macros;

mod utils;
mod valloc;

pub mod typeinfo;
pub mod value;
pub mod object;
pub mod stack;
pub mod instruction;
pub mod global;
pub mod vm;

pub mod frontend;