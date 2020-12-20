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



extern crate self as interp;

extern crate interp_macros as macros;

mod utils;
mod valloc;


pub mod vm;

pub mod frontend;