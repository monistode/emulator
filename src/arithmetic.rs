use super::flag_register::{FlagRegister, ProcessorFlags};
use super::num_type_conversions::ToSigned;
use num_traits::{
    ops::overflowing::{OverflowingAdd, OverflowingMul, OverflowingSub},
    Zero,
};
use std::ops::{BitAnd, BitOr, BitXor, Not, Shl, Shr};

pub trait ArithmeticOperand:
    Zero
    + PartialEq
    + ToSigned
    + OverflowingAdd
    + OverflowingSub
    + OverflowingMul
    + BitAnd<Self, Output = Self>
    + BitOr<Self, Output = Self>
    + BitXor<Self, Output = Self>
    + Not<Output = Self>
    + Shl<Self, Output = Self>
    + Shr<Self, Output = Self>
{
}
impl<
        T: Zero
            + PartialEq
            + ToSigned
            + OverflowingAdd
            + OverflowingSub
            + OverflowingMul
            + BitAnd<T, Output = T>
            + BitOr<T, Output = T>
            + BitXor<T, Output = T>
            + Not<Output = T>
            + Shl<T, Output = T>
            + Shr<T, Output = T>,
    > ArithmeticOperand for T
{
}

trait FlagRegisterExt: FlagRegister {
    fn set_zf_if_zero<T: Zero>(&mut self, value: &T)
    where
        Self: Sized;

    fn set_sf_if_negative<T: ArithmeticOperand>(&mut self, value: &T)
    where
        Self: Sized;
}

impl<T: FlagRegister> FlagRegisterExt for T {
    #[inline]
    fn set_zf_if_zero<U: Zero>(&mut self, value: &U) {
        self.set_if(value.is_zero(), ProcessorFlags::ZF);
    }

    #[inline]
    fn set_sf_if_negative<U: ArithmeticOperand>(&mut self, value: &U) {
        self.set_if(
            value.as_signed() < U::SignedVariant::zero(),
            ProcessorFlags::SF,
        );
    }
}

#[inline]
pub fn add<T, U>(flags: &mut U, a: T, b: T) -> T
where
    T: ArithmeticOperand,
    U: FlagRegister,
{
    let result = a.overflowing_add(&b);
    flags.reset();
    flags.set_if(result.1, ProcessorFlags::CF);
    flags.set_if(
        a.as_signed().overflowing_add(&b.as_signed()).1,
        ProcessorFlags::OF,
    );
    flags.set_zf_if_zero(&result.0);
    flags.set_sf_if_negative(&result.0);

    result.0
}

#[inline]
pub fn addc<T, U>(flags: &mut U, a: T, b: T) -> T
where
    T: ArithmeticOperand,
    U: FlagRegister,
{
    let raw_sum = a.overflowing_add(&b);
    let result = raw_sum
        .0
        .overflowing_add(&if flags.get(ProcessorFlags::CF) {
            T::one()
        } else {
            T::zero()
        });
    flags.reset();
    flags.set_if(result.1 || raw_sum.1, ProcessorFlags::CF);
    flags.set_if(
        a.as_signed().overflowing_add(&b.as_signed()).1
            || a.as_signed()
                .overflowing_add(&b.as_signed())
                .0
                .overflowing_add(&if flags.get(ProcessorFlags::CF) {
                    T::one().as_signed()
                } else {
                    T::zero().as_signed()
                })
                .1,
        ProcessorFlags::OF,
    );
    flags.set_zf_if_zero(&result.0);
    flags.set_sf_if_negative(&result.0);

    result.0
}

#[inline]
pub fn inc<T, U>(flags: &mut U, a: T) -> T
where
    T: ArithmeticOperand,
    U: FlagRegister,
{
    let result = a.overflowing_add(&T::one());
    flags.reset();
    flags.set_if(result.1, ProcessorFlags::CF);
    flags.set_if(
        a.as_signed().overflowing_add(&T::one().as_signed()).1,
        ProcessorFlags::OF,
    );
    flags.set_zf_if_zero(&result.0);
    flags.set_sf_if_negative(&result.0);

    result.0
}

#[inline]
pub fn test<T, U>(flags: &mut U, a: T, b: T) -> T
where
    T: ArithmeticOperand,
    U: FlagRegister,
{
    let result = a.overflowing_add(&b);
    flags.reset();
    flags.set_if(result.1, ProcessorFlags::CF);
    flags.clear(ProcessorFlags::OF);
    flags.set_zf_if_zero(&result.0);
    flags.set_sf_if_negative(&result.0);

    a
}

#[inline]
pub fn dec<T, U>(flags: &mut U, a: T) -> T
where
    T: ArithmeticOperand,
    U: FlagRegister,
{
    let result = a.overflowing_sub(&T::one());
    flags.reset();
    flags.set_if(!result.1, ProcessorFlags::CF);
    flags.set_if(
        a.as_signed().overflowing_sub(&T::one().as_signed()).1,
        ProcessorFlags::OF,
    );
    flags.set_zf_if_zero(&result.0);
    flags.set_sf_if_negative(&result.0);

    result.0
}

#[inline]
pub fn sub<T, U>(flags: &mut U, a: T, b: T) -> T
where
    T: ArithmeticOperand,
    U: FlagRegister,
{
    let result = a.overflowing_sub(&b);
    flags.reset();
    flags.set_if(!result.1, ProcessorFlags::CF);
    flags.set_if(
        a.as_signed().overflowing_sub(&b.as_signed()).1,
        ProcessorFlags::OF,
    );
    flags.set_zf_if_zero(&result.0);
    flags.set_sf_if_negative(&result.0);

    result.0
}

#[inline]
pub fn cmp<T, U>(flags: &mut U, a: T, b: T) -> T
where
    T: ArithmeticOperand,
    U: FlagRegister,
{
    let result = a.overflowing_sub(&b);
    flags.reset();
    flags.set_if(!result.1, ProcessorFlags::CF);
    flags.set_if(
        a.as_signed().overflowing_sub(&b.as_signed()).1,
        ProcessorFlags::OF,
    );
    flags.set_zf_if_zero(&result.0);
    flags.set_sf_if_negative(&result.0);

    a
}

#[inline]
pub fn mul<T, U>(flags: &mut U, a: T, b: T) -> T
where
    T: ArithmeticOperand,
    U: FlagRegister,
{
    let result = a.overflowing_mul(&b);
    flags.reset();
    flags.set_if(result.1, ProcessorFlags::CF);
    flags.set_if(
        a.as_signed().overflowing_mul(&b.as_signed()).1,
        ProcessorFlags::OF,
    );
    flags.set_zf_if_zero(&result.0);
    flags.set_sf_if_negative(&result.0);

    result.0
}

#[inline]
pub fn div<T, U>(flags: &mut U, a: T, b: T) -> T
where
    T: ArithmeticOperand,
    U: FlagRegister,
{
    flags.reset();
    if b.is_zero() {
        flags.set(ProcessorFlags::CF);
        flags.set(ProcessorFlags::OF);
        return T::zero();
    }
    flags.set_zf_if_zero(&a);

    a / b
}

#[inline]
pub fn and<T, U>(flags: &mut U, a: T, b: T) -> T
where
    T: ArithmeticOperand,
    U: FlagRegister,
{
    let result = a & b;
    flags.reset();
    flags.set_zf_if_zero(&result);
    flags.set_sf_if_negative(&result);

    result
}

#[inline]
pub fn or<T, U>(flags: &mut U, a: T, b: T) -> T
where
    T: ArithmeticOperand,
    U: FlagRegister,
{
    let result = a | b;
    flags.reset();
    flags.set_zf_if_zero(&result);
    flags.set_sf_if_negative(&result);

    result
}

#[inline]
pub fn xor<T, U>(flags: &mut U, a: T, b: T) -> T
where
    T: ArithmeticOperand,
    U: FlagRegister,
{
    let result = a ^ b;
    flags.reset();
    flags.set_zf_if_zero(&result);
    flags.set_sf_if_negative(&result);

    result
}

#[inline]
pub fn not<T, U>(flags: &mut U, a: T) -> T
where
    T: ArithmeticOperand,
    U: FlagRegister,
{
    let result = !a;
    flags.reset();
    flags.set_zf_if_zero(&result);
    flags.set_if(!flags.get(ProcessorFlags::SF), ProcessorFlags::SF);

    result
}

#[inline]
pub fn shl<T, U>(flags: &mut U, a: T, b: T) -> T
where
    T: ArithmeticOperand,
    U: FlagRegister,
{
    let result = a << b;
    flags.reset();
    // TODO

    result
}

#[inline]
pub fn shr<T, U>(flags: &mut U, a: T, b: T) -> T
where
    T: ArithmeticOperand,
    U: FlagRegister,
{
    let result = a >> b;
    flags.reset();
    // TODO

    result
}
