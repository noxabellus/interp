//! wip interpreter

#![allow(
	incomplete_features,
	clippy::inconsistent_digit_grouping,
	// clippy::unusual_byte_groupings,
	clippy::match_ref_pats,
	clippy::many_single_char_names,
)]

#![warn(
	missing_docs,
	clippy::if_same_then_else,
)]



extern crate self as zeta;

extern crate zeta_macros as macros;

mod utils;
mod valloc;

pub mod source;

pub mod vm;

pub mod frontend;