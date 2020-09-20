//! wip interpreter

#![allow(incomplete_features)]

#![warn(missing_docs)]

#![feature(
  specialization,
  try_trait,
  trait_alias,
  associated_type_defaults,
  associated_type_bounds
)]

extern crate self as interp;

pub mod core;
pub mod prelude;
pub mod wip;