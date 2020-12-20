/// Constructs a closure that returns the result of a matches! expression
#[macro_export]
macro_rules! matcher {
	($($pats:pat)|+) => { |x| matches!(x, $($pats)|+) };
	(! $($pats:pat)|+) => { |x| !matches!(x, $($pats)|+) };
}

/// Constructs a closure that returns Some(match) or None depending on the result of an if let pattern
#[macro_export]
macro_rules! option_matcher {
	($($pats:pat)|+ $(if $cond:expr)? => $some:expr) => { |x| match x { $($pats)|+ $(if $cond)? => Some($some), _ => None } }
}

/// Constructs a closure that returns Some(match) or None depending on the result of an if let pattern
#[macro_export]
macro_rules! result_matcher {
	($($pats:pat)|+ $(if $cond:expr)? => Ok($ok:expr), Err($err:expr)) => { |x| match x { $($pats)|+ $(if $cond)? => Ok($ok), _ => Err($err) } }
}



/// Used to expand an optional macro segment or substitute a default
#[macro_export]
macro_rules! expand_or_else {
	({ }, { $($default:tt)* }) => { $($default)* };
	({ $($value:tt)* }, { $($default:tt)* }) => { $($value)* };
}

/// Used to expand some pattern in a macro segment without actually using its value
#[macro_export]
macro_rules! discard {
	($($content:tt)*) => { };
}

/// Used to expand some pattern in a macro segment but use different content instead of its value
#[macro_export]
macro_rules! discard_replace {
	({ $($replace:tt)* }, { $($content:tt)* }) => { $($replace)* };
}


/// Unsafely destructures a refutable pattern
///
/// In debug mode, this will panic if the pattern is not matched, but in release it will produce UB;
/// Only use this if you know for certain that a given pattern is the only possible state for a value
///
/// The internal `unreachable_unchecked` is left unwrapped so that use of this macro must be enclosed in an `unsafe` block
#[macro_export]
macro_rules! unchecked_destructure {
	($val:expr, $pat:pat => $body:expr) => {
		if let $pat = $val {
			$body
		} else if cfg!(debug_assertions) {
			panic!("unchecked_destructure failed");
		} else {
			::std::hint::unreachable_unchecked()
		}
	}
}



/// Compile time assert!
#[macro_export]
macro_rules! static_assert {
	($cond:expr) => {
		const _: [();0] = [(); 0 - (!($cond) as usize)];
	}
}


/// Sort of a c-like enum builder for when you need to do arithmetic on enums and have unnamed variants
#[macro_export]
macro_rules! c_enum {
	( $(
		$(#[$meta:meta])*
		$vis:vis $name:ident : $repr:ty {
			$($vars:ident = $vals:expr),* $(,)?
		}
	)* ) => { $(
		$(#[$meta])*
		#[allow(non_snake_case)]
		$vis mod $name {
			#![allow(non_upper_case_globals)]
			/// The type representing a c_enum
			pub type Repr = $repr;
			$(pub const $vars: $repr = $vals;)*
		}
	)* };
}
