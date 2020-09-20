//! wip interpreter

#![allow(incomplete_features)]

#![warn(missing_docs)]

#![feature(
  test,
  specialization,
  try_trait,
  trait_alias,
  associated_type_defaults,
  associated_type_bounds
)]

extern crate self as interp;

pub mod core;
pub mod prelude;