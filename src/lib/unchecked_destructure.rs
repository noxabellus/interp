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