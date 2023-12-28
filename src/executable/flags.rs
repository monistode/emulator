#[derive(Debug)]
pub struct ExecutableSegmentFlags {
    pub executable: bool,
    pub writable: bool,
    pub readable: bool,
    pub special: bool,
    pub stripped: bool,
}

impl ExecutableSegmentFlags {
    pub fn new(byte: u8) -> ExecutableSegmentFlags {
        ExecutableSegmentFlags {
            executable: byte & 0b00000001 != 0,
            writable: byte & 0b00000010 != 0,
            readable: byte & 0b00000100 != 0,
            special: byte & 0b00001000 != 0,
            stripped: byte & 0b00010000 != 0,
        }
    }
}
