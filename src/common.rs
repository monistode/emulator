use crate::executable::executable::Executable;

pub trait Processor<Byte, Pc, OutputPort, OutputItem> {
    fn next(&mut self) -> Byte;
    fn at_pc_plus(&self, offset: u16) -> Byte;
    fn pc(&self) -> Pc;
    fn run_command<T>(&mut self, output: T) -> ProcessorContinue
    where
        T: Fn(OutputPort, OutputItem);
    fn load_executable(&mut self, executable: &Executable);
}

pub enum ProcessorContinue {
    KeepRunning,
    Halt,
}
