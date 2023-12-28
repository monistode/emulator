use crate::executable::executable::Executable;

use super::{
    common::{Processor, ProcessorContinue},
    flag_register::{implement_flag_register, FlagRegister, ProcessorFlags},
    memory::Memory,
    stack::{two_byte_stack, TwoByteStack},
};
use num_derive::FromPrimitive;
use num_traits::FromPrimitive;

#[derive(Debug, FromPrimitive)]
enum Opcode {
    Halt = 0b00000000,
}

implement_flag_register!(CiscProcessorFlagRegister(u8));

pub struct CiscRegisters {
    pub pc: u16,
    pub fr: CiscProcessorFlagRegister,
    pub r: [u16; 4],
    pub bp: u16,
    pub sp: u16,
}

pub struct CiscProcessor {
    pub memory: Memory<u8>,
    pub registers: CiscRegisters,
}

impl CiscProcessor {
    pub fn new() -> CiscProcessor {
        CiscProcessor {
            memory: Memory::new(0, 65536),
            registers: CiscRegisters {
                pc: 0,
                fr: CiscProcessorFlagRegister::new(),
                r: [0; 4],
                bp: 0,
                sp: 1024,
            },
        }
    }

    two_byte_stack!(memory_stack[sp: u16] -> u8, based on memory, growing downward);
}

impl Processor<u8, u16, u16, u16> for CiscProcessor {
    fn next(&mut self) -> u8 {
        let instruction = self.memory[self.registers.pc as usize];
        self.registers.pc = self.registers.pc.wrapping_add(1);
        instruction
    }
    fn at_pc_plus(&self, offset: u16) -> u8 {
        self.memory[self.registers.pc.wrapping_add(offset) as usize]
    }
    fn pc(&self) -> u16 {
        self.registers.pc
    }

    fn run_command<T, U>(&mut self, output: T, input: U) -> ProcessorContinue
    where
        T: Fn(u16, u16),
        U: Fn(u16) -> u16,
    {
        let next_instruction = self.next();
        let next_instruction_as_enum = Opcode::from_u8(next_instruction).unwrap_or_else(|| {
            panic!(
                "Unknown opcode: {:b} at address {:x}",
                next_instruction,
                self.registers.pc - 1
            )
        });
        match next_instruction_as_enum {
            Opcode::Halt => ProcessorContinue::Halt,
        }
    }

    fn load_executable(&mut self, executable: &Executable) -> Result<(), String> {
        if executable.header.harvard {
            return Err("Harvard architecture not supported".to_string());
        }
        for segment in executable.segments() {
            if segment.metadata().flags.readable {
                if segment.metadata().byte_size != 8 {
                    return Err("Only 8-bit bytes are supported".to_string());
                }
                for i in 0..segment.metadata().vsize {
                    self.memory[(segment.metadata().start + i) as usize] =
                        *segment.bytes().get(i as usize).unwrap_or(&0);
                }
            }
        }
        self.registers.pc = executable.header.entry_point as u16;
        Ok(())
    }
}
