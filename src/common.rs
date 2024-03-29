use crate::executable::executable::Executable;

pub trait Processor<Byte, Pc, IOPort, IOItem> {
    fn next(&mut self) -> Byte;
    fn at_pc_plus(&self, offset: u16) -> Byte;
    fn pc(&self) -> Pc;
    fn run_command<T, U>(&mut self, output: T, input: U) -> ProcessorContinue
    where
        T: Fn(IOPort, IOItem),
        U: Fn(IOPort) -> IOItem;
    fn load_executable(&mut self, executable: &Executable) -> Result<(), String>;
}

pub enum ProcessorContinue {
    KeepRunning,
    Error,
    Halt,
}
