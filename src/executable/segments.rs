use num_traits::Zero;
use std::ops::{AddAssign, ShlAssign};

use crate::tightly_packed_array::TightlyPackedArray;

use super::segment_meta::ExecutableSegmentMetadata;

#[derive(Debug)]
pub struct ExecutableSegment {
    metadata: ExecutableSegmentMetadata,
    bytes: Vec<u8>,
}

impl ExecutableSegment {
    pub fn metadata(&self) -> &ExecutableSegmentMetadata {
        &self.metadata
    }

    pub fn bytes(&self) -> &[u8] {
        &self.bytes
    }

    pub fn tightly_packed_array<T>(&self) -> TightlyPackedArray<T>
    where
        T: Zero + From<u8> + AddAssign + ShlAssign,
    {
        TightlyPackedArray::new(self.bytes.clone(), self.metadata.byte_size as usize)
    }
}

#[derive(Debug)]
pub struct ExecutableSegments {
    pub segments: Vec<ExecutableSegment>,
}

impl ExecutableSegments {
    pub fn new(bytes: &[u8]) -> ExecutableSegments {
        let mut segments = Vec::new();
        let mut offset = 0;
        while offset < bytes.len() {
            if offset + 16 > bytes.len() {
                break;
            }
            let metadata = ExecutableSegmentMetadata::new(&bytes[offset..]);
            offset += 17;
            let mut segment_bytes = Vec::new();
            for i in 0..metadata.disk_size() {
                if offset + i as usize >= bytes.len() {
                    break;
                }
                segment_bytes.push(bytes[offset + i as usize]);
            }
            offset += metadata.disk_size() as usize;
            segments.push(ExecutableSegment {
                metadata,
                bytes: segment_bytes,
            });
        }
        ExecutableSegments { segments }
    }
}
