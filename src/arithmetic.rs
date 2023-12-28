use super::flag_register::{FlagRegister, ProcessorFlags};
use super::num_type_conversions::ToSigned;
use num_traits::{
    ops::overflowing::{OverflowingAdd, OverflowingMul, OverflowingSub},
    Zero,
};

pub trait ArithmeticOperand:
    OverflowingAdd + OverflowingSub + OverflowingMul + Zero + PartialEq + ToSigned
{
}
impl<T: OverflowingAdd + OverflowingSub + OverflowingMul + Zero + PartialEq + ToSigned>
    ArithmeticOperand for T
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
