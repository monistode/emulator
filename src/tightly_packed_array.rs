use std::{
    marker::PhantomData,
    ops::{AddAssign, ShlAssign},
};

use num_traits::Zero;

pub struct TightlyPackedArray<T> {
    bytes: Vec<u8>,
    byte_size: usize,
    _phantom: PhantomData<T>,
}

impl<T> TightlyPackedArray<T>
where
    T: Zero + From<u8> + AddAssign + ShlAssign,
{
    pub fn new(bytes: Vec<u8>, byte_size: usize) -> TightlyPackedArray<T> {
        TightlyPackedArray {
            bytes,
            byte_size,
            _phantom: PhantomData,
        }
    }

    pub fn at(&self, index: usize) -> T {
        // We find all the bytes that can contribute - the size of our T doesn't have
        // to be a multiple of 8, so we have to be careful.
        let start = index * self.byte_size / 8;
        let start_offset = index * self.byte_size % 8;
        let immediate_end_offset = 8 - 8.min(start_offset + self.byte_size);
        let mut bits = 8 - start_offset;
        let mut value = T::zero();
        value += T::from(
            (self.bytes.get(start).unwrap_or(&0) & (((1u16 << bits) - 1) as u8))
                >> immediate_end_offset,
        );
        let mut i = 1;
        while bits < self.byte_size {
            let bits_to_add = std::cmp::min(8, self.byte_size - bits);
            value <<= T::from(bits_to_add as u8);
            value += T::from(self.bytes.get(start + i).unwrap_or(&0) >> (8 - bits_to_add));
            bits += bits_to_add;
            i += 1;
        }
        value
    }
}
