//! Contains a stack data structure

/// Stack data structure for use by the VM for temporary operands, and for the call frame stack
pub struct Stack<T>(Vec<T>);


impl<T> Stack<T> {
  /// Create a new Stack
  pub fn new () -> Self { Self(Vec::default()) }

  /// Add a value on the top of the Stack
  pub fn push (&mut self, v: T) { self.0.push(v) }

  /// Get the number of elements on the Stack
  pub fn len (&self) -> usize { self.0.len() }

  /// Determine if a Stack is empty
  pub fn is_empty (&self) -> bool { self.0.is_empty() }

  /// Remove and return a value from the top of the Stack without validation
  /// # Safety
  /// This does not check that there is at least 1 element in the Stack
  pub unsafe fn pop_unchecked (&mut self) -> T {
    let idx = self.0.len() - 1;
    let out = std::ptr::read(self.0.get_unchecked(idx));
    self.0.set_len(idx);
    out
  }

  /// Remove and return a value from the top of the Stack
  pub fn pop (&mut self) -> Option<T> { self.0.pop() }

  /// Get a reference to a value at an offset downward from the top of the Stack
  /// # Safety
  /// This does not bounds check the index provided
  pub unsafe fn peek_unchecked (&self, idx: usize) -> &T {
    self.0.get_unchecked(self.0.len() - idx)
  }

  /// Get a reference to a value at an offset downward from the top of the Stack
  pub fn peek (&self, idx: usize) -> Option<&T> {
    if self.0.len() >= idx { Some(unsafe { self.peek_unchecked(idx) }) } else { None }
  }
}

impl<T> Default for Stack<T> { fn default () -> Self { Self::new() } }