use std::{
    marker::PhantomData,
    ops::{Index, IndexMut},
};

use num_traits::{PrimInt, WrappingAdd};

pub trait DoublablePrecision {
    type DoublePrecision: PrimInt;
    fn combine(a: Self, b: Self) -> Self::DoublePrecision;
    fn split(a: Self::DoublePrecision) -> (Self, Self)
    where
        Self: Sized;
}
macro_rules! impl_doublable_precision {
    ($a:ty, $b:ty) => {
        impl DoublablePrecision for $a {
            type DoublePrecision = $b;

            #[inline]
            fn combine(a: Self, b: Self) -> Self::DoublePrecision {
                (a as Self::DoublePrecision) << (std::mem::size_of::<Self>() * 8)
                    | (b as Self::DoublePrecision)
            }

            #[inline]
            fn split(a: Self::DoublePrecision) -> (Self, Self) {
                ((a >> (std::mem::size_of::<Self>() * 8)) as Self, a as Self)
            }
        }
    };
}
impl_doublable_precision!(u8, u16);
impl_doublable_precision!(u16, u32);
impl_doublable_precision!(u32, u64);
impl_doublable_precision!(u64, u128);
impl_doublable_precision!(u128, u128);
impl_doublable_precision!(i8, i16);
impl_doublable_precision!(i16, i32);
impl_doublable_precision!(i32, i64);
impl_doublable_precision!(i64, i128);
impl_doublable_precision!(i128, i128);

pub struct Memory<T> {
    pub memory: Vec<T>,
}

impl<T> Memory<T>
where
    T: Copy,
{
    pub fn new(initial: T, size: usize) -> Memory<T> {
        Memory {
            memory: vec![initial; size],
        }
    }

    pub fn load_binary<U>(&mut self, binary: &[U])
    where
        U: Into<T> + Copy,
    {
        for (i, byte) in binary.iter().enumerate() {
            self.memory[i] = (*byte).into();
        }
    }
}

impl<T> Index<usize> for Memory<T> {
    type Output = T;

    #[inline]
    fn index(&self, index: usize) -> &Self::Output {
        &self.memory[index]
    }
}

impl<T> IndexMut<usize> for Memory<T> {
    #[inline]
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.memory[index]
    }
}

pub struct TwoByteMemory<'a, T: 'a, U, V>
where
    T: IndexMut<V>,
    U: Into<V>,
    T::Output: DoublablePrecision + Sized,
{
    data: &'a mut T,
    underlying_index: PhantomData<V>,
    index: PhantomData<U>,
}

impl<'a, T, U, V> TwoByteMemory<'a, T, U, V>
where
    T: IndexMut<V>,
    U: Into<V> + From<u8> + WrappingAdd<Output = U> + Copy,
    T::Output: DoublablePrecision + Copy,
{
    pub fn new(data: &'a mut T) -> Self {
        TwoByteMemory {
            data,
            underlying_index: PhantomData,
            index: PhantomData,
        }
    }

    #[inline]
    pub fn read(&self, address: U) -> <T::Output as DoublablePrecision>::DoublePrecision {
        let address_first = address.into();
        let address_second = address.wrapping_add(&U::from(1)).into();
        T::Output::combine(self.data[address_first], self.data[address_second])
    }
    #[inline]
    pub fn write(&mut self, address: U, value: <T::Output as DoublablePrecision>::DoublePrecision) {
        let address_first = address.into();
        let address_second = address.wrapping_add(&U::from(1)).into();
        let (a, b) = T::Output::split(value);
        self.data[address_first] = a;
        self.data[address_second] = b;
    }
}

macro_rules! two_byte_memory {
    // two_byte_memory!(two_byte_: data_memory[u16] -> 2 * u8)
    ($name:ident: $data:ident[$index:ty] -> 2 * $underlying:ty) => {
        #[inline]
        pub fn $name(&mut self) -> TwoByteMemory<Memory<$underlying>, $index, usize> {
            TwoByteMemory::new(&mut self.$data)
        }
    };
}
pub(crate) use two_byte_memory;
