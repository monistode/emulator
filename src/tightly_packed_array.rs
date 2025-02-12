use std::{
    marker::PhantomData,
    ops::{AddAssign, ShlAssign},
};

use bitvec::vec::BitVec;
use num_traits::Zero;

pub struct TightlyPackedArray<T> {
    data: BitVec,
    byte_size: usize,
    _phantom: PhantomData<T>,
}

impl<T> TightlyPackedArray<T>
where
    T: Zero + From<u8> + AddAssign + ShlAssign,
{
    pub fn new(data: BitVec, byte_size: usize) -> TightlyPackedArray<T> {
        TightlyPackedArray {
            data,
            byte_size,
            _phantom: PhantomData,
        }
    }

    pub fn at(&self, index: usize) -> T {
        // We find all the bytes that can contribute - the size of our T doesn't have
        // to be a multiple of 8, so we have to be careful.
        let start = index * self.byte_size;
        let end = start + self.byte_size;
        let mut value = T::zero();
        for i in start..end {
            value <<= T::from(1);
            if self.data[i] {
                value += T::from(1);
            }
        }
        value
    }
}
