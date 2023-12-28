use super::segments::ExecutableSegments;

#[derive(Debug)]
pub struct ExecutableHeader {
    pub harvard: bool,
    pub entry_point: u32,
}

#[derive(Debug)]
pub struct Executable {
    pub header: ExecutableHeader,
    pub segments: ExecutableSegments,
}

impl Executable {
    pub fn new(bytes: &[u8]) -> Executable {
        Executable {
            header: ExecutableHeader {
                harvard: bytes[0] == 0,
                entry_point: u32::from_le_bytes([bytes[1], bytes[2], bytes[3], bytes[4]]),
            },
            segments: ExecutableSegments::new(&bytes[5..]),
        }
    }

    pub fn segments(&self) -> &Vec<super::segments::ExecutableSegment> {
        &self.segments.segments
    }
}
