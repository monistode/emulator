use num_traits::ops::overflowing::{OverflowingAdd, OverflowingMul, OverflowingSub};
use num_traits::{Signed, Unsigned, Zero};

pub trait ToSigned: Unsigned {
    type SignedVariant: Signed
        + OverflowingAdd
        + OverflowingSub
        + OverflowingMul
        + Zero
        + PartialEq
        + PartialOrd;

    fn as_signed(&self) -> Self::SignedVariant;
}

macro_rules! impl_to_signed {
    ($($t:ty,$s:ty)*) => ($(
        impl ToSigned for $t {
            type SignedVariant = $s;

            #[inline]
            fn as_signed(&self) -> Self::SignedVariant {
                *self as Self::SignedVariant
            }
        }
    )*)
}

impl_to_signed! {
    u8,i8
    u16,i16
    u32,i32
    u64,i64
    u128,i128
}
