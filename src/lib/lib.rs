//! wip interpreter

#![allow(incomplete_features)]

#![warn(missing_docs)]

#![feature(
  test,
  specialization,
  try_trait,
  trait_alias,
  associated_type_defaults,
  associated_type_bounds,
  core_intrinsics
)]

extern crate self as interp;

pub mod type_info;
pub mod value;
pub mod fnv1a;
pub mod map;
pub mod stack;
pub mod unsafe_vec;
pub mod untyped_vec;
pub mod untyped_stack;
pub mod untyped_map;