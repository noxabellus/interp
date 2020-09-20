/// Allows quick generation of a subset of common patterns as closures
#[macro_export]
macro_rules! pat {
  ($($pat:pat)|+) => { |val| matches!(val, $($pat)|+) };
  ($($pat:pat)|+ => $expr:expr) => { |val| if let $($pat)|+ = val { $crate::core::Just($expr) } else { $crate::core::Nothing } };
}