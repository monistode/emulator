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
    Halt = 0b000000,
    Load = 0b000001,
    LoadImm = 0b10010000,
    Loadf = 0b000010,
    LoadIR1 = 0b1000001,
    LoadIR2 = 0b1000010,
    MovAccIR1 = 0b011111,
    MovAccIR2 = 0b100000,
    StoreIR1 = 0b000011,
    StoreIR1Imm = 0b10010001,
    StoreIR2 = 0b100011,
    StoreIR2Imm = 0b11110010,
    Storef = 0b000100,
    MovIR1Acc = 0b100001,
    MovIR2Acc = 0b100010,
    MovIR2IR1 = 0b111111,
    MovIR1IR2 = 0b1000000,
    MovImm = 0b10000000,
    Push = 0b000101,
    Pop = 0b000111,
    Pushf = 0b000110,
    Popf = 0b001000,
    PushIR1 = 0b111101,
    PopIR1 = 0b001001,
    PushIR2 = 0b100100,
    PopIR2 = 0b100101,
    AddAddr = 0b001010,
    AddIR1 = 0b100110,
    AddIR2 = 0b100111,
    SubAddr = 0b001011,
    SubIR1 = 0b101000,
    SubIR2 = 0b101001,
    MulAddr = 0b001110,
    MulIR1 = 0b101010,
    MulIR2 = 0b101011,
    DivAddr = 0b001111,
    DivIR1 = 0b101100,
    DivIR2 = 0b101101,
    Inc = 0b001100,
    IncIR1 = 0b101110,
    IncIR2 = 0b101111,
    Dec = 0b001101,
    DecIR1 = 0b110000,
    DecIR2 = 0b110001,
    AndAddr = 0b010000,
    AndIR1 = 0b110010,
    AndIR2 = 0b110011,
    OrAddr = 0b010001,
    OrIR1 = 0b110100,
    OrIR2 = 0b110101,
    XorAddr = 0b010010,
    XorIR1 = 0b110110,
    XorIR2 = 0b110111,
    NotAddr = 0b010011,
    NotIR1 = 0b111000,
    NotIR2 = 0b111110,
    Lsh = 0b10000001,
    Rsh = 0b10000010,
    CallAddr = 0b10000100,
    Call = 0b010100,
    Ret = 0b010101,
    CmpAddr = 0b010110,
    CmpImm = 0b10000101,
    CmpIR1 = 0b111001,
    CmpIR2 = 0b111010,
    TestImm = 0b10000110,
    TestAddr = 0b10001111,
    TestIR1 = 0b111011,
    TestIR2 = 0b111100,
    JmpAddr = 0b10000111,
    Jmp = 0b010111,
    JeAddr = 0b10001000,
    Je = 0b011000,
    JneAddr = 0b10001001,
    Jne = 0b011001,
    JgAddr = 0b10001010,
    Jg = 0b011010,
    JgeAddr = 0b10001011,
    Jge = 0b011011,
    JlAddr = 0b10001100,
    Jl = 0b011100,
    JleAddr = 0b10001101,
    Jle = 0b011101,
    In = 0b10001110,
    Out = 0b10011000,
}

macro_rules! with_immediate {
    ($processor:ident, $op:expr) => {{
        let immediate = $processor.load_immediate();
        $op(immediate);
        ProcessorContinue::KeepRunning
    }};
}

macro_rules! arithmetic_imm {
    ($processor:ident, $op:expr) => {{
        let immediate = $processor.load_immediate();
        let acc = $processor.registers.acc;
        let result = $op(&mut $processor.registers.fr, acc, immediate);
        $processor.registers.acc = result;
        ProcessorContinue::KeepRunning
    }};
}

macro_rules! arithmetic_addr {
    ($processor:ident, $op:expr) => {{
        let address = $processor.load_immediate();
        let value = $processor.two_byte_memory().read(address);
        let acc = $processor.registers.acc;
        let result = $op(&mut $processor.registers.fr, acc, value);
        $processor.registers.acc = result;
        ProcessorContinue::KeepRunning
    }};
}

macro_rules! arithmetic_ir1 {
    ($processor:ident, $op:expr) => {{
        let address = $processor.registers.ir1;
        let value = $processor.two_byte_memory().read(address);
        let acc = $processor.registers.acc;
        let result = $op(&mut $processor.registers.fr, acc, value);
        $processor.registers.acc = result;
        ProcessorContinue::KeepRunning
    }};
}

macro_rules! arithmetic_ir2 {
    ($processor:ident, $op:expr) => {{
        let address = $processor.registers.ir2;
        let value = $processor.two_byte_memory().read(address);
        let acc = $processor.registers.acc;
        let result = $op(&mut $processor.registers.fr, acc, value);
        $processor.registers.acc = result;
        ProcessorContinue::KeepRunning
    }};
}

macro_rules! arithmetic_acc {
    ($processor:ident, $op:expr) => {{
        let acc = $processor.registers.acc;
        let result = $op(&mut $processor.registers.fr, acc);
        $processor.registers.acc = result;
        ProcessorContinue::KeepRunning
    }};
}

macro_rules! arithmetic_ir1_single_operand {
    ($processor:ident, $op:expr) => {{
        let previous = $processor.registers.ir1;
        let result = $op(&mut $processor.registers.fr, previous);
        $processor.registers.ir1 = result;
        ProcessorContinue::KeepRunning
    }};
}

macro_rules! arithmetic_ir2_single_operand {
    ($processor:ident, $op:expr) => {{
        let previous = $processor.registers.ir2;
        let result = $op(&mut $processor.registers.fr, previous);
        $processor.registers.ir2 = result;
        ProcessorContinue::KeepRunning
    }};
}

macro_rules! arithmetic_acc_addr_into_acc {
    ($processor:ident, $op:expr) => {{
        let address = $processor.registers.acc;
        let value = $processor.two_byte_memory().read(address);
        let result = $op(&mut $processor.registers.fr, value);
        $processor.registers.acc = result;
        ProcessorContinue::KeepRunning
    }};
}

macro_rules! arithmetic_ir1_into_acc {
    ($processor:ident, $op:expr) => {{
        let address = $processor.registers.ir1;
        let value = $processor.two_byte_memory().read(address);
        let result = $op(&mut $processor.registers.fr, value);
        $processor.registers.acc = result;
        ProcessorContinue::KeepRunning
    }};
}

macro_rules! arithmetic_ir2_into_acc {
    ($processor:ident, $op:expr) => {{
        let address = $processor.registers.ir2;
        let value = $processor.two_byte_memory().read(address);
        let result = $op(&mut $processor.registers.fr, value);
        $processor.registers.acc = result;
        ProcessorContinue::KeepRunning
    }};
}

implement_flag_register!(AccProcessorFlagRegister(u8));

pub struct AccRegisters {
    pub pc: u16,
    pub fr: AccProcessorFlagRegister,
    pub sp: u16,
    pub ir1: u16,
    pub ir2: u16,
    pub acc: u16,
}

pub struct AccProcessor {
    pub memory: Memory<u8>,
    pub registers: AccRegisters,
}

impl AccProcessor {
    pub fn new() -> AccProcessor {
        AccProcessor {
            memory: Memory::new(0, 65536),
            registers: AccRegisters {
                pc: 0,
                fr: AccProcessorFlagRegister::new(),
                sp: 1024,
                ir1: 0,
                ir2: 0,
                acc: 0,
            },
        }
    }

    two_byte_stack!(memory_stack[sp: u16] -> u8, based on memory, growing downward);
    two_byte_memory!(two_byte_memory: memory[u16] -> 2 * u8);

    fn load_immediate(&mut self) -> u16 {
        let immediate = self.next() as u16;
        immediate << 8 | self.next() as u16
    }
}

impl Processor<u8, u16, u16, u16> for AccProcessor {
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
        let next_instruction_as_enum = match Opcode::from_u8(next_instruction.into()) {
            Some(opcode) => opcode,
            None => {
                return ProcessorContinue::Error;
            }
        };
        match next_instruction_as_enum {
            Opcode::Halt => ProcessorContinue::Halt,
            Opcode::Load => {
                let address = self.registers.acc;
                self.registers.acc = self.two_byte_memory().read(address);
                ProcessorContinue::KeepRunning
            }
            Opcode::LoadImm => with_immediate!(self, |immediate| {
                self.registers.acc = immediate;
            }),
            Opcode::Loadf => {
                let value: u8 = self.registers.fr.into();
                self.registers.acc = value as u16;
                ProcessorContinue::KeepRunning
            }
            Opcode::LoadIR1 => {
                let address = self.registers.ir1;
                self.registers.acc = self.two_byte_memory().read(address);
                ProcessorContinue::KeepRunning
            }
            Opcode::LoadIR2 => {
                let address = self.registers.ir2;
                self.registers.acc = self.two_byte_memory().read(address);
                ProcessorContinue::KeepRunning
            }
            Opcode::MovAccIR1 => {
                self.registers.acc = self.registers.ir1;
                ProcessorContinue::KeepRunning
            }
            Opcode::MovAccIR2 => {
                self.registers.acc = self.registers.ir2;
                ProcessorContinue::KeepRunning
            }
            Opcode::StoreIR1 => {
                let address = self.registers.ir1;
                let value = self.registers.acc;
                self.two_byte_memory().write(address, value);
                ProcessorContinue::KeepRunning
            }
            Opcode::StoreIR1Imm => with_immediate!(self, |immediate| {
                let address = self.registers.ir1;
                self.two_byte_memory().write(address, immediate);
            }),
            Opcode::StoreIR2 => {
                let address = self.registers.ir2;
                let value = self.registers.acc;
                self.two_byte_memory().write(address, value);
                ProcessorContinue::KeepRunning
            }
            Opcode::StoreIR2Imm => with_immediate!(self, |immediate| {
                let address = self.registers.ir2;
                self.two_byte_memory().write(address, immediate);
            }),
            Opcode::Storef => {
                let value = (self.registers.acc & 0xFF) as u8;
                self.registers.fr.0 = value;
                ProcessorContinue::KeepRunning
            }
            Opcode::MovIR1Acc => {
                self.registers.ir1 = self.registers.acc;
                ProcessorContinue::KeepRunning
            }
            Opcode::MovIR2Acc => {
                self.registers.ir2 = self.registers.acc;
                ProcessorContinue::KeepRunning
            }
            Opcode::MovIR2IR1 => {
                self.registers.ir2 = self.registers.ir1;
                ProcessorContinue::KeepRunning
            }
            Opcode::MovIR1IR2 => {
                self.registers.ir1 = self.registers.ir2;
                ProcessorContinue::KeepRunning
            }
            Opcode::MovImm => with_immediate!(self, |immediate| {
                self.registers.acc = immediate;
            }),
            Opcode::Push => {
                let value = self.registers.acc;
                self.memory_stack().push(value);
                ProcessorContinue::KeepRunning
            }
            Opcode::Pop => {
                let value = self.memory_stack().pop();
                self.registers.acc = value;
                ProcessorContinue::KeepRunning
            }
            Opcode::Pushf => {
                let value = self.registers.fr.0 as u16;
                self.memory_stack().push(value);
                ProcessorContinue::KeepRunning
            }
            Opcode::Popf => {
                let value = self.memory_stack().pop();
                self.registers.fr.0 = value as u8;
                ProcessorContinue::KeepRunning
            }
            Opcode::PushIR1 => {
                let value = self.registers.ir1;
                self.memory_stack().push(value);
                ProcessorContinue::KeepRunning
            }
            Opcode::PopIR1 => {
                let value = self.memory_stack().pop();
                self.registers.ir1 = value;
                ProcessorContinue::KeepRunning
            }
            Opcode::PushIR2 => {
                let value = self.registers.ir2;
                self.memory_stack().push(value);
                ProcessorContinue::KeepRunning
            }
            Opcode::PopIR2 => {
                let value = self.memory_stack().pop();
                self.registers.ir2 = value;
                ProcessorContinue::KeepRunning
            }
            Opcode::AddAddr => arithmetic_addr!(self, arithmetic::add),
            Opcode::AddIR1 => arithmetic_ir1!(self, arithmetic::add),
            Opcode::AddIR2 => arithmetic_ir2!(self, arithmetic::add),
            Opcode::SubAddr => arithmetic_addr!(self, arithmetic::sub),
            Opcode::SubIR1 => arithmetic_ir1!(self, arithmetic::sub),
            Opcode::SubIR2 => arithmetic_ir2!(self, arithmetic::sub),
            Opcode::MulAddr => arithmetic_addr!(self, arithmetic::mul),
            Opcode::MulIR1 => arithmetic_ir1!(self, arithmetic::mul),
            Opcode::MulIR2 => arithmetic_ir2!(self, arithmetic::mul),
            Opcode::DivAddr => arithmetic_addr!(self, arithmetic::div),
            Opcode::DivIR1 => arithmetic_ir1!(self, arithmetic::div),
            Opcode::DivIR2 => arithmetic_ir2!(self, arithmetic::div),
            Opcode::Inc => arithmetic_acc!(self, arithmetic::inc),
            Opcode::IncIR1 => arithmetic_ir1_single_operand!(self, arithmetic::inc),
            Opcode::IncIR2 => arithmetic_ir2_single_operand!(self, arithmetic::inc),
            Opcode::Dec => arithmetic_acc!(self, arithmetic::dec),
            Opcode::DecIR1 => arithmetic_ir1_single_operand!(self, arithmetic::dec),
            Opcode::DecIR2 => arithmetic_ir2_single_operand!(self, arithmetic::dec),
            Opcode::AndAddr => arithmetic_addr!(self, arithmetic::and),
            Opcode::AndIR1 => arithmetic_ir1!(self, arithmetic::and),
            Opcode::AndIR2 => arithmetic_ir2!(self, arithmetic::and),
            Opcode::OrAddr => arithmetic_addr!(self, arithmetic::or),
            Opcode::OrIR1 => arithmetic_ir1!(self, arithmetic::or),
            Opcode::OrIR2 => arithmetic_ir2!(self, arithmetic::or),
            Opcode::XorAddr => arithmetic_addr!(self, arithmetic::xor),
            Opcode::XorIR1 => arithmetic_ir1!(self, arithmetic::xor),
            Opcode::XorIR2 => arithmetic_ir2!(self, arithmetic::xor),
            Opcode::NotAddr => arithmetic_acc_addr_into_acc!(self, arithmetic::not),
            Opcode::NotIR1 => arithmetic_ir1_into_acc!(self, arithmetic::not),
            Opcode::NotIR2 => arithmetic_ir2_into_acc!(self, arithmetic::not),
            Opcode::Lsh => arithmetic_imm!(self, arithmetic::shl),
            Opcode::Rsh => arithmetic_imm!(self, arithmetic::shr),
            Opcode::CallAddr => with_immediate!(self, |address| {
                let return_address = self.registers.pc;
                self.memory_stack().push(return_address);
                self.registers.pc = address;
                ProcessorContinue::KeepRunning
            }),
            Opcode::Call => {
                let return_address = self.registers.pc;
                self.memory_stack().push(return_address);
                self.registers.pc = self.registers.acc;
                ProcessorContinue::KeepRunning
            }
            Opcode::Ret => {
                let return_address = self.memory_stack().pop();
                self.registers.pc = return_address;
                ProcessorContinue::KeepRunning
            }
            Opcode::CmpAddr => arithmetic_addr!(self, arithmetic::cmp),
            Opcode::CmpImm => arithmetic_imm!(self, arithmetic::cmp),
            Opcode::CmpIR1 => arithmetic_ir1!(self, arithmetic::cmp),
            Opcode::CmpIR2 => arithmetic_ir2!(self, arithmetic::cmp),
            Opcode::TestImm => arithmetic_imm!(self, arithmetic::test),
            Opcode::TestAddr => arithmetic_addr!(self, arithmetic::test),
            Opcode::TestIR1 => arithmetic_ir1!(self, arithmetic::test),
            Opcode::TestIR2 => arithmetic_ir2!(self, arithmetic::test),
            Opcode::JmpAddr => with_immediate!(self, |address| {
                self.registers.pc = address;
            }),
            Opcode::Jmp => {
                self.registers.pc = self.registers.acc;
                ProcessorContinue::KeepRunning
            }
            Opcode::JeAddr => with_immediate!(self, |address| {
                if self.registers.fr.get(ProcessorFlags::ZF) {
                    self.registers.pc = address;
                }
            }),
            Opcode::Je => {
                if self.registers.fr.get(ProcessorFlags::ZF) {
                    self.registers.pc = self.registers.acc;
                }
                ProcessorContinue::KeepRunning
            }
            Opcode::JneAddr => with_immediate!(self, |address| {
                if !self.registers.fr.get(ProcessorFlags::ZF) {
                    self.registers.pc = address;
                }
            }),
            Opcode::Jne => {
                if !self.registers.fr.get(ProcessorFlags::ZF) {
                    self.registers.pc = self.registers.acc;
                }
                ProcessorContinue::KeepRunning
            }
            Opcode::JgAddr => with_immediate!(self, |address| {
                if !self.registers.fr.get(ProcessorFlags::ZF)
                    && (self.registers.fr.get(ProcessorFlags::SF)
                        == self.registers.fr.get(ProcessorFlags::OF))
                {
                    self.registers.pc = address;
                }
            }),
            Opcode::Jg => {
                if !self.registers.fr.get(ProcessorFlags::ZF)
                    && (self.registers.fr.get(ProcessorFlags::SF)
                        == self.registers.fr.get(ProcessorFlags::OF))
                {
                    self.registers.pc = self.registers.acc;
                }
                ProcessorContinue::KeepRunning
            }
            Opcode::JgeAddr => with_immediate!(self, |address| {
                if !self.registers.fr.get(ProcessorFlags::CF) {
                    self.registers.pc = address;
                }
            }),
            Opcode::Jge => {
                if !self.registers.fr.get(ProcessorFlags::CF) {
                    self.registers.pc = self.registers.acc;
                }
                ProcessorContinue::KeepRunning
            }
            Opcode::JlAddr => with_immediate!(self, |address| {
                if self.registers.fr.get(ProcessorFlags::SF)
                    != self.registers.fr.get(ProcessorFlags::OF)
                {
                    self.registers.pc = address;
                }
            }),
            Opcode::Jl => {
                if self.registers.fr.get(ProcessorFlags::SF)
                    != self.registers.fr.get(ProcessorFlags::OF)
                {
                    self.registers.pc = self.registers.acc;
                }
                ProcessorContinue::KeepRunning
            }
            Opcode::JleAddr => with_immediate!(self, |address| {
                if self.registers.fr.get(ProcessorFlags::ZF)
                    || self.registers.fr.get(ProcessorFlags::CF)
                {
                    self.registers.pc = address;
                }
            }),
            Opcode::Jle => {
                if self.registers.fr.get(ProcessorFlags::ZF)
                    || self.registers.fr.get(ProcessorFlags::CF)
                {
                    self.registers.pc = self.registers.acc;
                }
                ProcessorContinue::KeepRunning
            }
            Opcode::In => with_immediate!(self, |port| {
                self.registers.acc = input(port);
            }),
            Opcode::Out => with_immediate!(self, |port| {
                output(port, self.registers.acc);
            }),
        }
    }

    fn load_executable(&mut self, executable: &Executable) -> Result<(), String> {
        unimplemented!()
    }
}
