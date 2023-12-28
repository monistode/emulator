use num_traits::{WrappingAdd, WrappingSub};

use super::memory::{DoublablePrecision, TwoByteMemory};
use std::{
    marker::PhantomData,
    ops::{Index, IndexMut},
};

// All of this file's magic fuckery is just for you to be able to do this:
// TwoByteStack::new(&mut self.memory, &mut self.registers.sp, true).push(0)

pub trait Stack<T> {
    fn push(&mut self, value: T);
    fn pop(&mut self) -> T;
    fn peek(&self) -> T;
}

pub struct TwoByteStack<'a, T: 'a, U, V>
where
    T: IndexMut<V>,
    U: Into<V>,
    T::Output: DoublablePrecision + Sized,
{
    data: TwoByteMemory<'a, T, U, V>,
    pointer: &'a mut U,
    downward: bool,
    index: PhantomData<V>,
}

impl<'a, T, U, V> TwoByteStack<'a, T, U, V>
where
    T: IndexMut<V>,
    U: Into<V> + From<u8> + WrappingAdd<Output = U> + WrappingSub<Output = U> + Copy,
    T::Output: DoublablePrecision + Sized + Copy,
{
    #[inline]
    pub fn new(data: &'a mut T, pointer: &'a mut U, downward: bool) -> Self {
        TwoByteStack {
            data: TwoByteMemory::new(data),
            pointer,
            downward,
            index: PhantomData,
        }
    }
}

impl<'a, T: 'a, U, V> Stack<<<T as Index<V>>::Output as DoublablePrecision>::DoublePrecision>
    for TwoByteStack<'a, T, U, V>
where
    T: IndexMut<V>,
    U: Into<V> + From<u8> + WrappingAdd<Output = U> + WrappingSub<Output = U> + Copy,
    T::Output: DoublablePrecision + Sized + Copy,
{
    #[inline]
    fn push(&mut self, value: <T::Output as DoublablePrecision>::DoublePrecision) {
        if self.downward {
            *self.pointer = (*self.pointer).wrapping_sub(&U::from(2));
        } else {
            *self.pointer = (*self.pointer).wrapping_add(&U::from(2));
        }
        self.data.write(*self.pointer, value);
    }

    #[inline]
    fn pop(&mut self) -> <T::Output as DoublablePrecision>::DoublePrecision {
        let result = self.data.read(*self.pointer);
        if self.downward {
            *self.pointer = (*self.pointer).wrapping_add(&U::from(2));
        } else {
            *self.pointer = (*self.pointer).wrapping_sub(&U::from(2));
        }
        result
    }

    #[inline]
    fn peek(&self) -> <T::Output as DoublablePrecision>::DoublePrecision {
        self.data.read(*self.pointer)
    }
}

macro_rules! two_byte_stack {
    // two_byte_stack!(memory_stack[registers.sp: u16] -> u8, basd on data_memory);
    // two_byte_stack!(memory_stack[registers.sp: u16] -> u8, based on data_memory, growing downward);
    ($name:ident[$register:ident: $indextype:ty] -> $itemtype:ty, based on $memory:ident) => {
        #[inline]
        pub fn $name(&mut self) -> TwoByteStack<'_, Memory<$itemtype>, $indextype, usize> {
            TwoByteStack::new(&mut self.$memory, &mut self.registers.$register, false)
        }
    };
    ($name:ident[$register:ident: $indextype:ty] -> $itemtype:ty, based on $memory:ident, growing downward) => {
        #[inline]
        pub fn $name(&mut self) -> TwoByteStack<'_, Memory<$itemtype>, $indextype, usize> {
            TwoByteStack::new(&mut self.$memory, &mut self.registers.$register, true)
        }
    };
}

pub(crate) use two_byte_stack;
