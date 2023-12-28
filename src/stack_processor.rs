use crate::executable::executable::Executable;

use super::{
    arithmetic,
    common::{Processor, ProcessorContinue},
    flag_register::{implement_flag_register, FlagRegister, ProcessorFlags},
    memory::Memory,
    memory::{two_byte_memory, TwoByteMemory},
    stack::{two_byte_stack, Stack, TwoByteStack},
    system,
};
use num_derive::FromPrimitive;
use num_traits::FromPrimitive;
use std::{fs::File, io::Read};
use ux::u6;

#[derive(Debug, FromPrimitive)]
enum Opcode {
    Halt = 0b000000,
    Load = 0b000001,
    LoadFr = 0b000010,
    LoadMem = 0b100000,
    Mov = 0b100010,
    Push = 0b001001,
    PushFr = 0b001010,
    Pop = 0b001100,
    PopFr = 0b001101,
    Out = 0b101011,
    Dup = 0b000111,
    Dup2 = 0b001000,
    Add = 0b001110,
    Sub = 0b001111,
    Mul = 0b010000,
    Div = 0b10001,
    Swap = 0b000110,
    Cmpe = 0b011000,
    CmpeImm = 0b100110,
    Cmpb = 0b011001,
    CmpbImm = 0b100111,
    Jmp = 0b011010,
    JmpImm = 0b101000,
    Jc = 0b011011,
    JcImm = 0b101001,
}

macro_rules! to_bool {
    ($value:expr) => {{
        if $value {
            0b1111111111111111
        } else {
            0b0000000000000000
        }
    }};
}

macro_rules! with_immediate {
    ($processor:ident, $op:expr) => {{
        let immediate = $processor.load_immediate();
        $op(immediate);
        ProcessorContinue::KeepRunning
    }};
}

macro_rules! arithmetic {
    ($processor:ident, $op:expr) => {{
        let a = $processor.register_stack().pop();
        let b = $processor.register_stack().pop();
        let result = $op(&mut $processor.registers.fr, a, b);
        $processor.register_stack().push(result);
        ProcessorContinue::KeepRunning
    }};
}

macro_rules! arithmetic_imm {
    ($processor:ident, $op:expr) => {
        with_immediate!($processor, |immediate| {
            let stack_value = $processor.register_stack().pop();
            let result = $op(&mut $processor.registers.fr, immediate, stack_value);
            $processor.register_stack().push(result);
            ProcessorContinue::KeepRunning
        })
    };
}

implement_flag_register!(StackProcessorFlagRegister(u16));

pub struct StackProcessorRegisters {
    pub pc: u16,
    pub fr: StackProcessorFlagRegister,
    pub tos: u16,
    pub sp: u16,
}

pub struct StackProcessor {
    pub text_memory: Memory<u6>,
    pub data_memory: Memory<u8>,
    pub registers: StackProcessorRegisters,
}

impl StackProcessor {
    pub fn new() -> StackProcessor {
        StackProcessor {
            text_memory: Memory::new(u6::new(0), 65536),
            data_memory: Memory::new(0, 65536),
            registers: StackProcessorRegisters {
                pc: 0,
                fr: StackProcessorFlagRegister::new(),
                tos: 256,
                sp: 1024,
            },
        }
    }

    pub fn load_from_bytes(&mut self, bytes: &[u8]) {
        self.text_memory
            .load_binary(&bytes.iter().map(|x| u6::new(*x)).collect::<Vec<_>>());
    }

    pub fn load_text_binary_file(&mut self, binary: &str) {
        let mut file = File::open(binary).expect("File not found");
        let mut buffer = Vec::new();
        file.read_to_end(&mut buffer).expect("Could not read file");
        self.text_memory
            .load_binary(&buffer.iter().map(|x| u6::new(*x)).collect::<Vec<_>>());
    }

    two_byte_stack!(register_stack[tos: u16] -> u8, based on data_memory, growing downward);
    two_byte_stack!(memory_stack[sp: u16] -> u8, based on data_memory);
    two_byte_memory!(two_byte_data_memory: data_memory[u16] -> 2 * u8);

    #[inline]
    fn load_immediate(&mut self) -> u16 {
        let low = self.text_memory[(self.registers.pc.wrapping_add(2)) as usize];
        let middle = self.text_memory[(self.registers.pc.wrapping_add(1)) as usize];
        let high = self.text_memory[self.registers.pc as usize];
        self.registers.pc = self.registers.pc.wrapping_add(3);
        (u16::from(high)) << 12 | (u16::from(middle)) << 6 | u16::from(low)
    }
}

impl Processor<u6, u16, u16, u16> for StackProcessor {
    fn next(&mut self) -> u6 {
        let instruction = self.text_memory[self.registers.pc as usize];
        self.registers.pc = self.registers.pc.wrapping_add(1);
        instruction
    }
    fn at_pc_plus(&self, offset: u16) -> u6 {
        self.text_memory[(self.registers.pc.wrapping_add(offset)) as usize]
    }
    fn pc(&self) -> u16 {
        self.registers.pc
    }

    fn run_command<T>(&mut self, output: T) -> ProcessorContinue
    where
        T: Fn(u16, u16),
    {
        let next_instruction = self.next();
        let next_instruction_as_enum =
            Opcode::from_u8(next_instruction.into()).unwrap_or_else(|| {
                panic!(
                    "Unknown opcode: {:b} at address {:x}",
                    next_instruction,
                    self.registers.pc - 1
                )
            });

        match next_instruction_as_enum {
            Opcode::Halt => system::halt(),
            Opcode::Load => {
                let address = self.register_stack().pop();
                let result = self.two_byte_data_memory().read(address);
                self.register_stack().push(result);
                ProcessorContinue::KeepRunning
            }
            Opcode::LoadFr => {
                let result = self.registers.fr.0;
                self.register_stack().push(result);
                ProcessorContinue::KeepRunning
            }
            Opcode::LoadMem => with_immediate!(self, |address| {
                let source = self.two_byte_data_memory().read(address);
                self.register_stack().push(source);
            }),
            Opcode::Mov => with_immediate!(self, |immediate| self.register_stack().push(immediate)),
            Opcode::Push => {
                let source = self.register_stack().pop();
                self.memory_stack().push(source);
                ProcessorContinue::KeepRunning
            }
            Opcode::PushFr => {
                let source = self.registers.fr.0;
                self.memory_stack().push(source);
                ProcessorContinue::KeepRunning
            }
            Opcode::Pop => {
                let source = self.memory_stack().pop();
                self.register_stack().push(source);
                ProcessorContinue::KeepRunning
            }
            Opcode::PopFr => {
                let source = self.memory_stack().pop();
                self.registers.fr.0 = source;
                ProcessorContinue::KeepRunning
            }
            Opcode::Out => with_immediate!(self, |port| output(port, self.register_stack().pop())),
            Opcode::Dup => {
                let source = self.register_stack().pop();
                self.register_stack().push(source);
                self.register_stack().push(source);
                ProcessorContinue::KeepRunning
            }
            Opcode::Dup2 => {
                let source1 = self.register_stack().pop();
                let source2 = self.register_stack().pop();
                self.register_stack().push(source2);
                self.register_stack().push(source1);
                self.register_stack().push(source2);
                ProcessorContinue::KeepRunning
            }
            // The arithmetic operations push the result onto the register stack
            Opcode::Add => arithmetic!(self, arithmetic::add),
            Opcode::Sub => arithmetic!(self, arithmetic::sub),
            Opcode::Mul => arithmetic!(self, arithmetic::mul),
            Opcode::Div => arithmetic!(self, arithmetic::div),
            Opcode::Swap => {
                let source1 = self.register_stack().pop();
                let source2 = self.register_stack().pop();
                self.register_stack().push(source1);
                self.register_stack().push(source2);
                ProcessorContinue::KeepRunning
            }
            Opcode::Cmpe => arithmetic!(self, |_, a, b| to_bool!(a == b)),
            Opcode::CmpeImm => arithmetic_imm!(self, |_, immediate, stack_value| to_bool!(
                immediate == stack_value
            )),
            Opcode::Cmpb => arithmetic!(self, |_, a, b| to_bool!(a > b)),
            Opcode::CmpbImm => arithmetic_imm!(self, |_, immediate, stack_value| to_bool!(
                immediate > stack_value
            )),
            Opcode::Jmp => {
                self.registers.pc = self.register_stack().pop().wrapping_add(self.registers.pc);
                ProcessorContinue::KeepRunning
            }
            Opcode::JmpImm => with_immediate!(self, |target: u16| self.registers.pc =
                target.wrapping_add(self.registers.pc)),
            Opcode::Jc => {
                if self.register_stack().pop() == 0b1111111111111111 {
                    self.registers.pc = self.register_stack().pop().wrapping_add(self.registers.pc);
                }
                ProcessorContinue::KeepRunning
            }
            Opcode::JcImm => with_immediate!(self, |destination: u16| {
                if self.register_stack().pop() == 0b1111111111111111 {
                    self.registers.pc = destination.wrapping_add(self.registers.pc);
                }
                ProcessorContinue::KeepRunning
            }),
        }
    }

    fn load_executable(&mut self, executable: &Executable) {
        for segment in executable.segments() {
            if segment.metadata().flags.executable {
                for i in 0..segment.metadata().vsize {
                    self.text_memory[(segment.metadata().start + i) as usize] =
                        u6::new(segment.tightly_packed_array::<u8>().at(i as usize));
                }
            } else {
                for i in 0..segment.metadata().vsize {
                    self.data_memory[(segment.metadata().start + i) as usize] =
                        segment.tightly_packed_array::<u8>().at(i as usize);
                }
            }
        }
    }
}
