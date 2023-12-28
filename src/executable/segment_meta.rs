use super::flags::ExecutableSegmentFlags;

#[derive(Debug)]
pub struct ExecutableSegmentMetadata {
    pub start: u32,
    pub byte_size: u32,
    pub size: u32,
    pub vsize: u32,
    pub flags: ExecutableSegmentFlags,
}

impl ExecutableSegmentMetadata {
    pub fn new(bytes: &[u8]) -> ExecutableSegmentMetadata {
        ExecutableSegmentMetadata {
            start: u32::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]),
            byte_size: u32::from_le_bytes([bytes[4], bytes[5], bytes[6], bytes[7]]),
            size: u32::from_le_bytes([bytes[8], bytes[9], bytes[10], bytes[11]]),
            vsize: u32::from_le_bytes([bytes[12], bytes[13], bytes[14], bytes[15]]),
            flags: ExecutableSegmentFlags::new(bytes[16]),
        }
    }

    pub fn disk_size(&self) -> u32 {
        (self.size * 8 + self.byte_size - 1) / self.byte_size
    }
}
