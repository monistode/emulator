use monistode_binutils::Executable;

pub trait Processor<Byte, Pc, IOPort, IOItem> {
    fn next(&mut self) -> Byte;
    fn at_pc_plus(&self, offset: u16) -> Byte;
    fn pc(&self) -> Pc;
    fn run_command<T, U>(&mut self, output: T, input: U) -> ProcessorContinue
    where
        T: Fn(IOPort, IOItem),
        U: Fn(IOPort) -> IOItem;
    fn load_executable(&mut self, executable: &Executable) -> Result<(), String>;
    fn peek_stack(&mut self, n: u8) -> u16;
}

pub enum ProcessorContinue {
    KeepRunning,
    Error,
    Halt,
}
