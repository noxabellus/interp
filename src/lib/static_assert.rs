/// Compile time assert!
#[macro_export]
macro_rules! static_assert {
  ($cond:expr) => {
    const _: () = assert!($cond);
  }
}