use monistode_binutils::Executable;

use super::{
    arithmetic,
    common::{Processor, ProcessorContinue},
    flag_register::{implement_flag_register, FlagRegister, ProcessorFlags},
    memory::{two_byte_memory, Memory, TwoByteMemory},
    stack::{two_byte_stack, Stack, TwoByteStack},
};
use num_derive::FromPrimitive;
use num_traits::FromPrimitive;

#[derive(Debug, FromPrimitive)]
enum Opcode {
    Halt = 0b00000,
    Load = 0b000001,
    Store = 0b000010,
    MovRegImm = 0b000110,
    MovRegReg = 0b000101,
    Push = 0b101000,
    Pop = 0b101001,
    Add = 0b000011,
    Addc = 0b100100,
    Sub = 0b000100,
    Mul = 0b001010,
    Div = 0b001011,
    And = 0b001100,
    Or = 0b001101,
    Xor = 0b001110,
    Not = 0b001111,
    Lsh = 0b010000,
    Rsh = 0b010001,
    CallAddr = 0b010010,
    CallRegAddr = 0b010011,
    Ret = 0b010100,
    CmpRegReg = 0b010101,
    CmpRegImm = 0b010110,
    TestRegReg = 0b010111,
    TestRegImm = 0b011000,
    JmpAddr = 0b011001,
    JmpReg = 0b011010,
    Je = 0b001000,
    Jne = 0b011011,
    Jg = 0b011100,
    Jge = 0b011101,
    Jl = 0b011110,
    Jle = 0b011111,
    In = 0b100000,
    // OutImmImm = 0b100001,
    // OutImmReg = 0b100010,
    Nop = 0b100011,
}

macro_rules! with_registers {
    ($processor:ident, $args_head:ident, $op:expr) => {{
        let next_byte = $processor.next();
        // I'm not gonna do the checks for valid padding
        let register_3 = (next_byte >> 1) & 0b111;
        let register_2 = (next_byte >> 4) & 0b111;
        let register_1 = (next_byte >> 7) & 0b1 | $args_head << 1;
        $op(register_1, register_2, register_3)
    }};
}

macro_rules! with_immediate {
    ($processor: ident, $args_head:ident, $op:expr) => {{
        let mut immediate = ($args_head as u16) << 14;
        immediate = immediate | ($processor.next() as u16) << 6;
        immediate = immediate | ($processor.next() as u16) >> 2;
        $op(immediate)
    }};
}

macro_rules! with_reg_and_immediate {
    ($processor: ident, $args_head:ident, $op:expr) => {{
        with_registers!(
            $processor,
            $args_head,
            |register_1, register_2, register_3| {
                let mut immediate = ($processor.next() as u16) << 8;
                immediate = immediate | ($processor.next() as u16);
                $op(register_1, register_2, register_3, immediate)
            }
        )
    }};
}

macro_rules! unary_arithmetic {
    ($processor:ident, $args_head:ident, $op:expr) => {{
        with_registers!($processor, $args_head, |register_1, register_2, _| {
            let Some(value_2) = $processor.get_register_value(register_2) else {
                return ProcessorContinue::Error;
            };
            let result = $op(&mut $processor.registers.fr, value_2);
            $processor.output_into_register(register_1, result)
        })
    }};
}

macro_rules! arithmetic {
    ($processor:ident, $args_head:ident, $op:expr) => {{
        with_registers!(
            $processor,
            $args_head,
            |register_1, register_2, register_3| {
                let Some(value_2) = $processor.get_register_value(register_2) else {
                    return ProcessorContinue::Error;
                };
                let Some(value_3) = $processor.get_register_value(register_3) else {
                    return ProcessorContinue::Error;
                };
                let result = $op(&mut $processor.registers.fr, value_2, value_3);
                $processor.output_into_register(register_1, result)
            }
        )
    }};
}

implement_flag_register!(RiscProcessorFlagRegister(u8));

pub struct RiscRegisters {
    pub pc: u16,
    pub fr: RiscProcessorFlagRegister,
    pub r: [u16; 4],
    pub sp: u16,
}

pub struct RiscProcessor {
    pub memory: Memory<u8>,
    pub registers: RiscRegisters,
}

impl RiscProcessor {
    pub fn new() -> RiscProcessor {
        RiscProcessor {
            memory: Memory::new(0, 65536),
            registers: RiscRegisters {
                pc: 0,
                fr: RiscProcessorFlagRegister::new(),
                r: [0; 4],
                sp: 1024,
            },
        }
    }

    two_byte_stack!(memory_stack[sp: u16] -> u8, based on memory, growing downward);
    two_byte_memory!(two_byte_memory: memory[u16] -> 2 * u8);

    fn get_register_value(&self, register_id: u8) -> Option<u16> {
        self.registers
            .r
            .get(register_id as usize)
            .copied()
            .or(match register_id {
                4 => Some(self.registers.sp),
                _ => None,
            })
    }

    fn set_register_value(&mut self, register_id: u8, value: u16) -> bool {
        match register_id {
            0..=3 => {
                self.registers.r[register_id as usize] = value;
                true
            }
            4 => {
                self.registers.sp = value;
                true
            }
            _ => false,
        }
    }

    fn output_into_register(&mut self, register_id: u8, value: u16) -> ProcessorContinue {
        if self.set_register_value(register_id, value) {
            ProcessorContinue::KeepRunning
        } else {
            ProcessorContinue::Error
        }
    }
}

impl Processor<u8, u16, u16, u16> for RiscProcessor {
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

    fn run_command<T, U>(&mut self, _: T, input: U) -> ProcessorContinue
    where
        T: Fn(u16, u16),
        U: Fn(u16) -> u16,
    {
        let next_byte = self.next();
        let next_instruction = next_byte >> 2;
        let next_instruction_as_enum = match Opcode::from_u8(next_instruction.into()) {
            Some(opcode) => opcode,
            None => {
                return ProcessorContinue::Error;
            }
        };

        let args_head = next_byte & 0b11;

        match next_instruction_as_enum {
            Opcode::Halt => ProcessorContinue::Halt,
            Opcode::Load => with_registers!(self, args_head, |register_1, register_2, _| {
                let Some(address) = self.get_register_value(register_2) else {
                    return ProcessorContinue::Error;
                };
                let value = self.two_byte_memory().read(address);
                self.output_into_register(register_1, value)
            }),
            Opcode::Store => with_registers!(self, args_head, |register_1, register_2, _| {
                let Some(address) = self.get_register_value(register_1) else {
                    return ProcessorContinue::Error;
                };
                let Some(value) = self.get_register_value(register_2) else {
                    return ProcessorContinue::Error;
                };
                self.two_byte_memory().write(address, value);
                ProcessorContinue::KeepRunning
            }),
            Opcode::MovRegImm => {
                with_reg_and_immediate!(self, args_head, |register_1, _, _, immediate| {
                    self.output_into_register(register_1, immediate)
                })
            }
            Opcode::MovRegReg => with_registers!(self, args_head, |register_1, register_2, _| {
                let Some(value) = self.get_register_value(register_2) else {
                    return ProcessorContinue::Error;
                };
                self.output_into_register(register_1, value)
            }),
            Opcode::Push => with_registers!(self, args_head, |register_1, _, _| {
                let Some(value) = self.get_register_value(register_1) else {
                    return ProcessorContinue::Error;
                };
                self.memory_stack().push(value);
                ProcessorContinue::KeepRunning
            }),
            Opcode::Pop => with_registers!(self, args_head, |register_1, _, _| {
                let value = self.memory_stack().pop();
                self.output_into_register(register_1, value)
            }),
            Opcode::Add => arithmetic!(self, args_head, arithmetic::add),
            Opcode::Addc => arithmetic!(self, args_head, arithmetic::addc),
            Opcode::Sub => arithmetic!(self, args_head, arithmetic::sub),
            Opcode::Mul => arithmetic!(self, args_head, arithmetic::mul),
            Opcode::Div => arithmetic!(self, args_head, arithmetic::div),
            Opcode::And => arithmetic!(self, args_head, arithmetic::and),
            Opcode::Or => arithmetic!(self, args_head, arithmetic::or),
            Opcode::Xor => arithmetic!(self, args_head, arithmetic::xor),
            Opcode::Not => unary_arithmetic!(self, args_head, arithmetic::not),
            Opcode::Lsh => arithmetic!(self, args_head, arithmetic::shl),
            Opcode::Rsh => arithmetic!(self, args_head, arithmetic::shr),
            Opcode::CallAddr => with_immediate!(self, args_head, |immediate| {
                let return_address = self.registers.pc;
                self.memory_stack().push(return_address);
                self.registers.pc = immediate;
                ProcessorContinue::KeepRunning
            }),
            Opcode::CallRegAddr => with_registers!(self, args_head, |register_1, _, _| {
                let Some(address) = self.get_register_value(register_1) else {
                    return ProcessorContinue::Error;
                };
                let return_address = self.registers.pc;
                self.memory_stack().push(return_address);
                self.registers.pc = address;
                ProcessorContinue::KeepRunning
            }),
            Opcode::Ret => {
                let return_address = self.memory_stack().pop();
                self.registers.pc = return_address;
                ProcessorContinue::KeepRunning
            }
            Opcode::CmpRegReg => with_registers!(self, args_head, |register_1, register_2, _| {
                let Some(value_1) = self.get_register_value(register_1) else {
                    return ProcessorContinue::Error;
                };
                let Some(value_2) = self.get_register_value(register_2) else {
                    return ProcessorContinue::Error;
                };
                arithmetic::cmp(&mut self.registers.fr, value_1, value_2);
                ProcessorContinue::KeepRunning
            }),
            Opcode::CmpRegImm => {
                with_reg_and_immediate!(self, args_head, |register_1, _, _, immediate| {
                    let Some(value) = self.get_register_value(register_1) else {
                        return ProcessorContinue::Error;
                    };
                    arithmetic::cmp(&mut self.registers.fr, value, immediate);
                    ProcessorContinue::KeepRunning
                })
            }
            Opcode::TestRegReg => with_registers!(self, args_head, |register_1, register_2, _| {
                let Some(value_1) = self.get_register_value(register_1) else {
                    return ProcessorContinue::Error;
                };
                let Some(value_2) = self.get_register_value(register_2) else {
                    return ProcessorContinue::Error;
                };
                arithmetic::test(&mut self.registers.fr, value_1, value_2);
                ProcessorContinue::KeepRunning
            }),
            Opcode::TestRegImm => {
                with_reg_and_immediate!(self, args_head, |register_1, _, _, immediate| {
                    let Some(value) = self.get_register_value(register_1) else {
                        return ProcessorContinue::Error;
                    };
                    arithmetic::test(&mut self.registers.fr, value, immediate);
                    ProcessorContinue::KeepRunning
                })
            }
            Opcode::JmpAddr => with_immediate!(self, args_head, |immediate| {
                self.registers.pc = immediate;
                ProcessorContinue::KeepRunning
            }),
            Opcode::JmpReg => with_registers!(self, args_head, |register_1, _, _| {
                let Some(address) = self.get_register_value(register_1) else {
                    return ProcessorContinue::Error;
                };
                self.registers.pc = address;
                ProcessorContinue::KeepRunning
            }),
            Opcode::Je => with_immediate!(self, args_head, |immediate| {
                if self.registers.fr.get(ProcessorFlags::ZF) {
                    self.registers.pc = immediate;
                }
                ProcessorContinue::KeepRunning
            }),
            Opcode::Jne => with_immediate!(self, args_head, |immediate| {
                if !self.registers.fr.get(ProcessorFlags::ZF) {
                    self.registers.pc = immediate;
                }
                ProcessorContinue::KeepRunning
            }),
            Opcode::Jg => with_immediate!(self, args_head, |address| {
                if !self.registers.fr.get(ProcessorFlags::ZF)
                    && (self.registers.fr.get(ProcessorFlags::SF)
                        == self.registers.fr.get(ProcessorFlags::OF))
                {
                    self.registers.pc = address;
                }
                ProcessorContinue::KeepRunning
            }),
            Opcode::Jge => with_immediate!(self, args_head, |address| {
                if !self.registers.fr.get(ProcessorFlags::CF) {
                    self.registers.pc = address;
                }
                ProcessorContinue::KeepRunning
            }),
            Opcode::Jl => with_immediate!(self, args_head, |address| {
                if self.registers.fr.get(ProcessorFlags::SF)
                    != self.registers.fr.get(ProcessorFlags::OF)
                {
                    self.registers.pc = address;
                }
                ProcessorContinue::KeepRunning
            }),
            Opcode::Jle => with_immediate!(self, args_head, |address| {
                if self.registers.fr.get(ProcessorFlags::ZF)
                    || self.registers.fr.get(ProcessorFlags::CF)
                {
                    self.registers.pc = address;
                }
                ProcessorContinue::KeepRunning
            }),
            Opcode::In => {
                with_reg_and_immediate!(self, args_head, |register_1, _, _, immediate| self
                    .output_into_register(register_1, input(immediate)))
            }
            Opcode::Nop => ProcessorContinue::KeepRunning,
        }
    }

    fn load_executable(&mut self, executable: &Executable) -> Result<(), String> {
        unimplemented!()
    }
}
