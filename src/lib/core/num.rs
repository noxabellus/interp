use std::{ ops, hash::{ Hash, Hasher } };

pub use std::num::FpCategory;


/// Indicates a type that represents a form of number
pub trait Numeric: Copy
                 + PartialEq<Self>
                 + PartialOrd<Self>
                 + ops::Add<Self, Output = Self>
                 + ops::Sub<Self, Output = Self>
                 + ops::Mul<Self, Output = Self>
                 + ops::Div<Self, Output = Self>
                 + ops::Rem<Self, Output = Self>
                 + ops::AddAssign<Self>
                 + ops::SubAssign<Self>
                 + ops::MulAssign<Self>
                 + ops::DivAssign<Self>
                 + ops::RemAssign<Self>
                 { }

impl<T> Numeric for T
where T: Copy
       + PartialEq<Self>
       + PartialOrd<Self>
       + ops::Add<Self, Output = Self>
       + ops::Sub<Self, Output = Self>
       + ops::Mul<Self, Output = Self>
       + ops::Div<Self, Output = Self>
       + ops::Rem<Self, Output = Self>
       + ops::AddAssign<Self>
       + ops::SubAssign<Self>
       + ops::MulAssign<Self>
       + ops::DivAssign<Self>
       + ops::RemAssign<Self>
       { }


pub trait Bitwise: Copy
                 + ops::BitAnd<Self, Output = Self>
                 + ops::BitOr<Self, Output = Self>
                 + ops::BitXor<Self, Output = Self>
                 + ops::BitAndAssign<Self>
                 + ops::BitOrAssign<Self>
                 + ops::BitXorAssign<Self>
                 + ops::Not<Output = Self>
                 { }

impl<T> Bitwise for T
where T: Copy
       + ops::BitAnd<Self, Output = Self>
       + ops::BitOr<Self, Output = Self>
       + ops::BitXor<Self, Output = Self>
       + ops::BitAndAssign<Self>
       + ops::BitOrAssign<Self>
       + ops::BitXorAssign<Self>
       + ops::Not<Output = Self>
       { }         


/// Indicates a Numeric value that does not have a sign (u8..u128, etc)
pub trait Unsigned: Numeric { }
impl Unsigned for u8 { }
impl Unsigned for u16 { }
impl Unsigned for u32 { }
impl Unsigned for u64 { }
impl Unsigned for u128 { }
impl Unsigned for usize { }

/// Indicates a Numeric value that does have a sign (i8..i128, f32, f64, etc)
pub trait Signed: Numeric + ops::Neg<Output = Self> {
  /// Determine if a Signed Numeric value is >= 0
  fn is_pos (&self) -> bool;

  /// Determine if a Signed Numeric value is < 0
  fn is_neg (&self) -> bool;
}

impl Signed for i8 {
  fn is_pos (&self) -> bool { *self >= 0 }
  fn is_neg (&self) -> bool { *self < 0 }
}

impl Signed for i16 {
  fn is_pos (&self) -> bool { *self >= 0 }
  fn is_neg (&self) -> bool { *self < 0 }
}

impl Signed for i32 {
  fn is_pos (&self) -> bool { *self >= 0 }
  fn is_neg (&self) -> bool { *self < 0 }
}

impl Signed for i64 {
  fn is_pos (&self) -> bool { *self >= 0 }
  fn is_neg (&self) -> bool { *self < 0 }
}

impl Signed for i128 {
  fn is_pos (&self) -> bool { *self >= 0 }
  fn is_neg (&self) -> bool { *self < 0 }
}

impl Signed for isize {
  fn is_pos (&self) -> bool { *self >= 0 }
  fn is_neg (&self) -> bool { *self < 0 }
}

impl Signed for f32 {
  fn is_pos (&self) -> bool { self.is_sign_positive() }
  fn is_neg (&self) -> bool { self.is_sign_negative() }
}

impl Signed for f64 {
  fn is_pos (&self) -> bool { self.is_sign_positive() }
  fn is_neg (&self) -> bool { self.is_sign_negative() }
}


/// Allows getting the absolute representation of a value
pub trait Abs: Sized {
  /// The result type given by calls to `Abs::abs` for a type
  type Output = Self;

  /// Get the absolute (unsigned) representation of a value
  fn abs (&self) -> Self::Output;
}

impl<T> Abs for T where T: Signed + Sized {
  default fn abs (&self) -> Self {
    if self.is_neg() { -*self }
    else { *self }
  }
}

impl Abs for u8    { fn abs (&self) -> Self { *self } }
impl Abs for u16   { fn abs (&self) -> Self { *self } }
impl Abs for u32   { fn abs (&self) -> Self { *self } }
impl Abs for u64   { fn abs (&self) -> Self { *self } }
impl Abs for u128  { fn abs (&self) -> Self { *self } }
impl Abs for usize { fn abs (&self) -> Self { *self } }



/// Allows approximate comparison
pub trait ApproxEq<'l, Rhs = Self, Diff = Self>
where Self: 'l, Rhs: 'l, Diff: 'l,
  &'l Self: ops::Sub<&'l Rhs, Output: Abs<Output: PartialOrd<&'l Diff>>>
{
  /// Compare two values with some margin of error
  fn approx_eq (&'l self, rhs: &'l Rhs, max_diff: &'l Diff) -> bool {
    (self - rhs).abs() < max_diff
  }
}


/// Classification trait for floating point number types (`f32`, `f64`)
pub trait FloatingPoint: Signed {
  /// The `NaN` value for a FloatingPoint number
  const NAN: Self;

  /// The positive infinity value for a FloatingPoint number
  const INF: Self;

  /// The negative infinity value for a FloatingPoint number
  const NEG_INF: Self;

  /// The smallest deviation from zero for a FloatingPoint number
  const EPS: Self;


  /// Get the `FpCategory` of a FloatingPoint number
  fn classify (self) -> FpCategory;


  /// Determine if a FloatingPoint number is `NaN`
  fn is_nan (self) -> bool;

  /// Determine if a FloatingPoint number is zero
  fn is_zero (self) -> bool;

  /// Determine if a FloatingPoint number is infinity, disregarding sign
  fn is_inf (self) -> bool;

  /// Determine if a FloatingPoint number is positive infinity
  fn is_pos_inf (self) -> bool { self.is_inf() && self.is_pos() }

  /// Determine if a FloatingPoint number is negative infinity
  fn is_neg_inf (self) -> bool { self.is_inf() && self.is_neg() }


  /// The type of integer returned by FloatingPoint::to_bits, and accepted by FloatingPoint::from_bits
  type Bits: Bitwise + Hash;

  /// Bitcast a FloatingPoint number to an integer
  fn to_bits (self) -> Self::Bits;
  /// Bitcast a FloatingPoint number from an integer
  fn from_bits (bits: Self::Bits) -> Self;
}

impl FloatingPoint for f32 {
  const NAN: Self = std::f32::NAN;
  const INF: Self = std::f32::INFINITY;
  const NEG_INF: Self = std::f32::NEG_INFINITY;
  const EPS: Self = std::f32::EPSILON;

  fn classify (self) -> FpCategory { self.classify() }
  fn is_nan (self) -> bool { self.is_nan() }
  fn is_zero (self) -> bool { self == 0.0 }
  fn is_inf (self) -> bool { self.is_infinite() }

  type Bits = u32;
  fn to_bits (self) -> u32 { f32::to_bits(self) }
  fn from_bits (bits: u32) -> f32 { f32::from_bits(bits) }
}

impl FloatingPoint for f64 {
  const NAN: Self = std::f64::NAN;
  const INF: Self = std::f64::INFINITY;
  const NEG_INF: Self = std::f64::NEG_INFINITY;
  const EPS: Self = std::f64::EPSILON;

  fn classify (self) -> FpCategory { self.classify() }
  fn is_nan (self) -> bool { self.is_nan() }
  fn is_zero (self) -> bool { self == 0.0 }
  fn is_inf (self) -> bool { self.is_infinite() }

  type Bits = u64;
  fn to_bits (self) -> u64 { self.to_bits() }
  fn from_bits (bits: u64) -> f64 { f64::from_bits(bits) }
}


#[derive(Clone, Copy)]
pub struct MapFloat<T: FloatingPoint = f64>(pub T);

macro_rules! impl_maths {
  ($tn:ident, $a_tn:ident = $f:ident ($op:tt), $a_f:ident ($a_op:tt)  $(; $($rest:tt)*)?) => {
    impl<T: FloatingPoint> ops::$tn<MapFloat<T>> for MapFloat<T> {
      type Output = MapFloat<T>;
      fn $f (self, other: MapFloat<T>) -> MapFloat<T> { MapFloat(self.0 $op other.0) }
    }

    impl<T: FloatingPoint> ops::$tn<&T> for &MapFloat<T> {
      type Output = MapFloat<T>;
      fn $f (self, other: &T) -> MapFloat<T> { MapFloat(self.0 $op *other) }
    }

    impl<T: FloatingPoint> ops::$tn<&MapFloat<T>> for &MapFloat<T> {
      type Output = MapFloat<T>;
      fn $f (self, other: &MapFloat<T>) -> MapFloat<T> { MapFloat(self.0 $op other.0) }
    }

    impl<T: FloatingPoint> ops::$a_tn<MapFloat<T>> for MapFloat<T> {
      fn $a_f (&mut self, other: MapFloat<T>) { self.0 $a_op other.0 }
    }

    impl<T: FloatingPoint> ops::$a_tn<T> for MapFloat<T> {
      fn $a_f (&mut self, other: T) { self.0 $a_op other }
    }

    impl_maths! { $($($rest)*)? }
  };

  () => { };
}

impl_maths! {
  Add, AddAssign = add (+), add_assign (+=);
  Sub, SubAssign = sub (-), sub_assign (-=);
  Mul, MulAssign = mul (*), mul_assign (*=);
  Div, DivAssign = div (/), div_assign (/=);
  Rem, RemAssign = rem (%), rem_assign (%=);
}


impl<T: FloatingPoint> ops::Neg for MapFloat<T> {
  type Output = MapFloat<T>;
  fn neg (self) -> MapFloat<T> { MapFloat(-self.0) }
}

impl<T: FloatingPoint> PartialEq for MapFloat<T> {
  fn eq (&self, other: &Self) -> bool {
    (self.0.is_nan() && other.0.is_nan()) || self.0 == other.0
  }
}

impl<T: FloatingPoint> PartialEq<T> for MapFloat<T> {
  fn eq (&self, other: &T) -> bool {
    (self.0.is_nan() && other.is_nan()) || self.0 == *other
  }
}

impl<T: FloatingPoint> Eq for MapFloat<T> { }

impl<T: FloatingPoint> PartialOrd for MapFloat<T> {
  fn partial_cmp (&self, other: &Self) -> Option<std::cmp::Ordering> { self.0.partial_cmp(&other.0) }
}

impl<T: FloatingPoint> PartialOrd<T> for MapFloat<T> {
  fn partial_cmp (&self, other: &T) -> Option<std::cmp::Ordering> { self.0.partial_cmp(other) }
}

impl<T: FloatingPoint> Hash for MapFloat<T> {
  fn hash<H: Hasher> (&self, h: &mut H) {
    self.0.to_bits().hash(h)
  }
}

impl<T: FloatingPoint> ops::Deref for MapFloat<T> {
  type Target = T;
  fn deref (&self) -> &T { &self.0 }
}

impl<T: FloatingPoint> ops::DerefMut for MapFloat<T> {
  fn deref_mut (&mut self) -> &mut T { &mut self.0 }
}

impl<T: FloatingPoint> Signed for MapFloat<T> {
  fn is_pos (&self) -> bool { self.0.is_pos() }
  fn is_neg (&self) -> bool { self.0.is_neg() }
}

impl<T: FloatingPoint> FloatingPoint for MapFloat<T> {
  const NAN: Self = MapFloat(T::NAN);
  const INF: Self = MapFloat(T::INF);
  const NEG_INF: Self = MapFloat(T::NEG_INF);
  const EPS: Self = MapFloat(T::EPS);

  fn classify (self) -> FpCategory { self.0.classify() }
  fn is_nan (self) -> bool { self.0.is_nan() }
  fn is_zero (self) -> bool { self.0.is_zero() }
  fn is_inf (self) -> bool { self.0.is_inf() }

  type Bits = T::Bits;
  fn to_bits (self) -> T::Bits { self.0.to_bits() }
  fn from_bits (bits: T::Bits) -> Self { Self(T::from_bits(bits)) }
}