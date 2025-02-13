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
    Halt = 0b00000000,
    MovRegImm = 0b10000000,
    MovRegReg = 0b01100000,
    MovRegRegAdr = 0b01100001,
    MovRegRegAdrOff = 0b10100000,
    MovRegAdrReg = 0b01100010,
    MovRegAdrImm = 0b10000001,
    MovRegAdrOffReg = 0b10100001,
    MovRegAdrOffImm = 0b11000000,
    PushReg = 0b00000001,
    PushImm = 0b01000000,
    PopReg = 0b00000010,
    EnterImm = 0b01000001,
    Leave = 00100000,
    AddRegRegAdr = 0b01100011,
    AddRegReg = 0b01100100,
    AddRegRegAdrOff = 0b10100010,
    AddRegAdrReg = 0b01100101,
    SubRegRegAdr = 0b01100110,
    SubRegReg = 0b01100111,
    SubRegRegAdrOff = 0b10100011,
    SubRegAdrReg = 0b01101000,
    IncReg = 0b00011111,
    IncRegAdr = 0b00000100,
    IncRegAdrOff = 0b100000010,
    DecReg = 0b00000101,
    DecRegAdr = 0b00000110,
    DecRegAdrOff = 0b10000011,
    MulRegReg = 0b01101001,
    MulRegRegAdr = 0b01101010,
    MulRegAdrReg = 0b01101011,
    MulRegImm = 0b10000100,
    MulRegRegAdrOff = 0b10100100,
    DivRegReg = 0b01101100,
    DivRegRegAdr = 0b01101101,
    DivRegAdrReg = 0b01101110,
    DivRegImm = 0b10000101,
    DivRegRegAdrOff = 0b10100101,
    AndRegReg = 0b01101111,
    AndRegRegAdr = 0b01110000,
    OrRegReg = 0b01110001,
    OrRegRegAdr = 0b01110010,
    XorRegReg = 0b01110011,
    XorRegRegAdr = 0b01110100,
    NotReg = 0b00000111,
    NotRegAdr = 0b00001000,
    LshRegImm = 0b10000110,
    LshRegAdrImm = 0b10000111,
    LshRegAdrOffImm = 0b11000001,
    RshRegImm = 0b10001000,
    RshRegAdrImm = 0b10001001,
    RshRegAdrOffImm = 0b11000010,
    CallImm = 0b01000010,
    CallReg = 0b00001001,
    CallRegOff = 145,
    Ret = 0b00100001,
    CmpRegReg = 0b01110101,
    CmpRegImm = 0b10001011,
    CmpRegRegAdr = 146,
    CmpRegRegAdrOff = 0b10100110,
    TestRegReg = 0b01110111,
    TestRegRegAdr = 0b01111000,
    TestRegRegAdrOff = 0b10100111,
    JmpImm = 0b01000011,
    JmpReg = 0b00001010,
    JmpRegOff = 0b10001100,
    JeImm = 0b01000100,
    JneImm = 0b01000101,
    JgImm = 0b01000110,
    JgeImm = 0b01000111,
    JlImm = 0b01001000,
    JleImm = 0b01001001,
    InRegPort = 0b10001101,
    InRegAdrPort = 0b10001110,
    InRegAdrOffPort = 0b11000011,
    OutPortImm = 0b11001111,
    OutPortReg = 0b10001111,
    OutPortRegAdr = 0b10010000,
    OutPortRegAdrOff = 0b11000100,
    //Load4SimdregAddr = 0b00001011,
    //Store4SimdregAddr = 0b00001100,
    //Add4Simdreg1Simdreg2 = 0b01111010,
    //Sub4Simdreg1Simdreg2 = 0b01111011,
    //Mul4Simdreg1Imdreg2 = 0b01111101,
    //Div4Simdreg1Simdreg2 = 255,
    Nop = 0b00100010,
}

macro_rules! with_register_value {
    ($processor:ident, $op:expr) => {{
        let register_value = match $processor.load_register() {
            Some(value) => value,
            None => return ProcessorContinue::Error,
        };
        $op(register_value);
        ProcessorContinue::KeepRunning
    }};
}

macro_rules! with_register_id_and_register_value {
    ($processor:ident, $op:expr) => {{
        let (register_id, address_register_id) = $processor.load_two_register_ids();
        let value = match $processor.get_register_value(address_register_id) {
            Some(value) => value,
            None => return ProcessorContinue::Error,
        };
        $op(register_id, value)
    }};
}

macro_rules! with_two_register_values {
    ($processor:ident, $op:expr) => {{
        let (register_id1, register_id2) = $processor.load_two_register_ids();
        let value1 = match $processor.get_register_value(register_id1) {
            Some(value) => value,
            None => return ProcessorContinue::Error,
        };
        let value2 = match $processor.get_register_value(register_id2) {
            Some(value) => value,
            None => return ProcessorContinue::Error,
        };
        $op(value1, value2)
    }};
}

macro_rules! with_register_id_and_register_address {
    ($processor:ident, $op:expr) => {{
        let (register_id, address_register_id) = $processor.load_two_register_ids();
        let value = match $processor.get_register_value(address_register_id) {
            Some(value) => $processor.two_byte_memory().read(value),
            None => return ProcessorContinue::Error,
        };
        $op(register_id, value)
    }};
}

macro_rules! with_register_address {
    ($processor:ident, $op:expr) => {{
        let value = $processor.load_register_address();
        match value {
            Some(value) => {
                $op(value);
                ProcessorContinue::KeepRunning
            }
            None => ProcessorContinue::Error,
        }
    }};
}

macro_rules! with_register_id_and_register_address_offset {
    ($processor:ident, $op:expr) => {{
        let (register_id, address_register_id) = $processor.load_two_register_ids();
        let address_register_value = match $processor.get_register_value(address_register_id) {
            Some(value) => value,
            None => return ProcessorContinue::Error,
        };
        with_immediate_raw!($processor, |immediate| {
            let value = $processor
                .two_byte_memory()
                .read(address_register_value.wrapping_add(immediate));
            $op(register_id, value)
        })
    }};
}

macro_rules! with_register_address_offset {
    ($processor:ident, $op:expr) => {{
        let value = $processor.load_register_address_offset();
        $op(value);
        ProcessorContinue::KeepRunning
    }};
}

macro_rules! with_register_address_offset_as_address {
    ($processor:ident, $op:expr) => {{
        let register_value = match $processor.load_register() {
            Some(value) => value,
            None => return ProcessorContinue::Error,
        };
        with_immediate!($processor, |immediate| {
            $op(register_value.wrapping_add(immediate));
        })
    }};
}

macro_rules! with_register_id {
    ($processor:ident, $op:expr) => {{
        let register_id = $processor.next();
        $op(register_id);
        ProcessorContinue::KeepRunning
    }};
}

macro_rules! with_immediate_raw {
    ($processor:ident, $op:expr) => {{
        let immediate = $processor.load_immediate();
        $op(immediate)
    }};
}

macro_rules! with_immediate {
    ($processor:ident, $op:expr) => {{
        let immediate = $processor.load_immediate();
        $op(immediate);
        ProcessorContinue::KeepRunning
    }};
}

macro_rules! save_to_register {
    ($processor:ident, $op:expr) => {{
        let register_id = $processor.next();
        let value = $op();
        match $processor.save_register(register_id, value) {
            Ok(_) => ProcessorContinue::KeepRunning,
            Err(_) => ProcessorContinue::Error,
        }
    }};
}

macro_rules! save_to_register_address {
    ($processor:ident, $op:expr) => {{
        let address = match $processor.load_register() {
            Some(value) => value,
            None => return ProcessorContinue::Error,
        };
        let value = $op();
        $processor.two_byte_memory().write(address, value);
        ProcessorContinue::KeepRunning
    }};
}

macro_rules! save_to_register_address_offset {
    ($processor:ident, $op:expr) => {{
        let address = match $processor.load_register() {
            Some(value) => value,
            None => return ProcessorContinue::Error,
        };
        let offset = $processor.load_immediate();
        let value = $op();
        $processor
            .two_byte_memory()
            .write(address.wrapping_add(offset), value);
        ProcessorContinue::KeepRunning
    }};
}

macro_rules! save_optional_to_register_address_offset {
    ($processor:ident, $op:expr) => {{
        let address = match $processor.load_register() {
            Some(value) => value,
            None => return ProcessorContinue::Error,
        };
        let offset = $processor.load_immediate();
        let value = match $op() {
            Some(value) => value,
            None => return ProcessorContinue::Error,
        };
        $processor
            .two_byte_memory()
            .write(address.wrapping_add(offset), value);
        ProcessorContinue::KeepRunning
    }};
}

macro_rules! push_result {
    ($processor:ident, $op:expr) => {{
        let value = $op();
        $processor.memory_stack().push(value);
        ProcessorContinue::KeepRunning
    }};
}

macro_rules! push_optional_result {
    ($processor:ident, $op:expr) => {{
        let value = match $op() {
            Some(value) => value,
            None => return ProcessorContinue::Error,
        };
        $processor.memory_stack().push(value);
        ProcessorContinue::KeepRunning
    }};
}

implement_flag_register!(CiscProcessorFlagRegister(u8));

macro_rules! arithmetic_reg_reg_addr {
    ($processor: ident, $op: expr) => {{
        with_register_id_and_register_address!($processor, |register_id, b| {
            let a = match $processor.get_register_value(register_id) {
                Some(value) => value,
                None => return ProcessorContinue::Error,
            };
            let result = $op(&mut $processor.registers.fr, a, b);
            match $processor.save_register(register_id, result) {
                Ok(_) => ProcessorContinue::KeepRunning,
                Err(_) => ProcessorContinue::Error,
            }
        })
    }};
}

macro_rules! arithmetic_reg_addr_reg {
    ($processor: ident, $op: expr) => {{
        with_two_register_values!($processor, |address, b| {
            let a = $processor.two_byte_memory().read(address);
            let result = $op(&mut $processor.registers.fr, a, b);
            $processor.two_byte_memory().write(address, result);
            ProcessorContinue::KeepRunning
        })
    }};
}

macro_rules! arithmetic_reg_reg {
    ($processor: ident, $op: expr) => {{
        with_register_id_and_register_value!($processor, |register_id, b| {
            let a = match $processor.get_register_value(register_id) {
                Some(value) => value,
                None => return ProcessorContinue::Error,
            };
            let result = $op(&mut $processor.registers.fr, a, b);
            match $processor.save_register(register_id, result) {
                Ok(_) => ProcessorContinue::KeepRunning,
                Err(_) => ProcessorContinue::Error,
            }
        })
    }};
}

macro_rules! arithmetic_reg_reg_addr_off {
    ($processor: ident, $op: expr) => {{
        with_register_id_and_register_address_offset!($processor, |register_id, b| {
            let a = match $processor.get_register_value(register_id) {
                Some(value) => value,
                None => return ProcessorContinue::Error,
            };
            let result = $op(&mut $processor.registers.fr, a, b);
            match $processor.save_register(register_id, result) {
                Ok(_) => ProcessorContinue::KeepRunning,
                Err(_) => ProcessorContinue::Error,
            }
        })
    }};
}

macro_rules! arithmetic_reg_imm {
    ($processor: ident, $op: expr) => {{
        with_register_id!($processor, |register_id| {
            with_immediate!($processor, |b| {
                let a = match $processor.get_register_value(register_id) {
                    Some(value) => value,
                    None => return ProcessorContinue::Error,
                };
                let result = $op(&mut $processor.registers.fr, a, b);
                match $processor.save_register(register_id, result) {
                    Ok(_) => ProcessorContinue::KeepRunning,
                    Err(_) => ProcessorContinue::Error,
                }
            })
        })
    }};
}

macro_rules! arithmetic_reg_addr_imm {
    ($processor: ident, $op: expr) => {{
        with_register_value!($processor, |address| {
            with_immediate!($processor, |b| {
                let a = $processor.two_byte_memory().read(address);
                let result = $op(&mut $processor.registers.fr, a, b);
                $processor.two_byte_memory().write(address, result);
                ProcessorContinue::KeepRunning
            })
        })
    }};
}

macro_rules! arithmetic_reg_addr_offset_imm {
    ($processor: ident, $op: expr) => {{
        with_register_address_offset_as_address!($processor, |address| {
            with_immediate!($processor, |b| {
                let a = $processor.two_byte_memory().read(address);
                let result = $op(&mut $processor.registers.fr, a, b);
                $processor.two_byte_memory().write(address, result);
                ProcessorContinue::KeepRunning
            })
        })
    }};
}

macro_rules! arithmetic_reg {
    ($processor: ident, $op: expr) => {{
        with_register_id!($processor, |register_id| {
            let a = match $processor.get_register_value(register_id) {
                Some(value) => value,
                None => return ProcessorContinue::Error,
            };
            let result = $op(&mut $processor.registers.fr, a);
            match $processor.save_register(register_id, result) {
                Ok(_) => ProcessorContinue::KeepRunning,
                Err(_) => ProcessorContinue::Error,
            }
        })
    }};
}

macro_rules! arithmetic_reg_addr {
    ($processor: ident, $op: expr) => {{
        with_register_value!($processor, |address| {
            let a = $processor.two_byte_memory().read(address);
            let result = $op(&mut $processor.registers.fr, a);
            $processor.two_byte_memory().write(address, result);
            ProcessorContinue::KeepRunning
        })
    }};
}

macro_rules! arithmetic_reg_addr_offset {
    ($processor: ident, $op: expr) => {{
        with_register_address_offset_as_address!($processor, |address| {
            let a = $processor.two_byte_memory().read(address);
            let result = $op(&mut $processor.registers.fr, a);
            $processor.two_byte_memory().write(address, result);
            ProcessorContinue::KeepRunning
        })
    }};
}

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
    two_byte_memory!(two_byte_memory: memory[u16] -> 2 * u8);

    fn load_immediate(&mut self) -> u16 {
        let immediate = self.next() as u16;
        immediate << 8 | self.next() as u16
    }

    fn load_register(&mut self) -> Option<u16> {
        let register = self.next();
        self.registers
            .r
            .get(register as usize)
            .copied()
            .or(match register {
                4 => Some(self.registers.bp),
                5 => Some(self.registers.sp),
                _ => None,
            })
    }

    fn load_two_register_ids(&mut self) -> (u8, u8) {
        let registers = self.next();
        return (registers >> 4, registers & 0b00001111);
    }

    fn get_register_value(&self, register_id: u8) -> Option<u16> {
        self.registers
            .r
            .get(register_id as usize)
            .copied()
            .or(match register_id {
                4 => Some(self.registers.bp),
                5 => Some(self.registers.sp),
                _ => None,
            })
    }

    fn load_register_address(&mut self) -> Option<u16> {
        let register = self.next();
        let address = self
            .registers
            .r
            .get(register as usize)
            .copied()
            .or(match register {
                4 => Some(self.registers.bp),
                5 => Some(self.registers.sp),
                _ => None,
            });
        address.map(|address| self.two_byte_memory().read(address))
    }

    fn load_register_address_offset(&mut self) -> Option<u16> {
        let register = self.next();
        let offset = self.load_immediate();
        let address = self
            .registers
            .r
            .get(register as usize)
            .copied()
            .or(match register {
                4 => Some(self.registers.bp),
                5 => Some(self.registers.sp),
                _ => None,
            });
        address.map(|address| self.two_byte_memory().read(address.wrapping_add(offset)))
    }

    fn save_register(&mut self, register_id: u8, value: u16) -> Result<(), ()> {
        if register_id > 5 {
            return Err(());
        }
        if register_id == 4 {
            self.registers.bp = value;
            return Ok(());
        }
        if register_id == 5 {
            self.registers.sp = value;
            return Ok(());
        }
        self.registers.r[register_id as usize] = value;
        Ok(())
    }
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

    fn peek_stack(&mut self, n: u8) -> u16 {
        self.memory_stack().peek_down_by(n * 2)
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
            Opcode::MovRegImm => save_to_register!(self, || self.load_immediate()),
            Opcode::MovRegReg => {
                with_register_id_and_register_value!(self, |destination, value| {
                    match self.save_register(destination, value) {
                        Ok(_) => ProcessorContinue::KeepRunning,
                        Err(_) => ProcessorContinue::Error,
                    }
                })
            }
            Opcode::MovRegRegAdr => {
                with_register_id_and_register_address!(self, |destination, value| {
                    match self.save_register(destination, value) {
                        Ok(_) => ProcessorContinue::KeepRunning,
                        Err(_) => ProcessorContinue::Error,
                    }
                })
            }
            Opcode::MovRegRegAdrOff => {
                with_register_id_and_register_address_offset!(self, |destination, value| {
                    match self.save_register(destination, value) {
                        Ok(_) => ProcessorContinue::KeepRunning,
                        Err(_) => ProcessorContinue::Error,
                    }
                })
            }
            Opcode::MovRegAdrReg => {
                let (destination, source) = self.load_two_register_ids();
                let value = match self.get_register_value(source) {
                    Some(value) => value,
                    None => return ProcessorContinue::Error,
                };
                match self.save_register(destination, value) {
                    Ok(_) => ProcessorContinue::KeepRunning,
                    Err(_) => ProcessorContinue::Error,
                }
            }
            Opcode::MovRegAdrImm => save_to_register_address!(self, || self.load_immediate()),
            Opcode::MovRegAdrOffReg => {
                save_optional_to_register_address_offset!(self, || self.load_register())
            }
            Opcode::MovRegAdrOffImm => {
                save_to_register_address_offset!(self, || self.load_immediate())
            }
            Opcode::PushReg => push_optional_result!(self, || self.load_register()),
            Opcode::PushImm => push_result!(self, || self.load_immediate()),
            Opcode::PopReg => save_to_register!(self, || self.memory_stack().pop()),
            Opcode::EnterImm => {
                let offset = self.load_immediate();
                let bp = self.registers.bp;
                self.memory_stack().push(bp);
                self.registers.bp = self.registers.sp;
                self.registers.sp = self.registers.sp.wrapping_sub(offset);
                ProcessorContinue::KeepRunning
            }
            Opcode::Leave => {
                let bp = self.memory_stack().pop();
                self.registers.sp = self.registers.bp;
                self.registers.bp = bp;
                ProcessorContinue::KeepRunning
            }

            // Arithmetic operations:
            Opcode::AddRegRegAdr => arithmetic_reg_reg_addr!(self, arithmetic::add),
            Opcode::AddRegReg => arithmetic_reg_reg!(self, arithmetic::add),
            Opcode::AddRegRegAdrOff => arithmetic_reg_reg_addr_off!(self, arithmetic::add),
            Opcode::AddRegAdrReg => arithmetic_reg_addr_reg!(self, arithmetic::add),

            Opcode::SubRegRegAdr => arithmetic_reg_reg_addr!(self, arithmetic::sub),
            Opcode::SubRegReg => arithmetic_reg_reg!(self, arithmetic::sub),
            Opcode::SubRegRegAdrOff => arithmetic_reg_reg_addr_off!(self, arithmetic::sub),
            Opcode::SubRegAdrReg => arithmetic_reg_addr_reg!(self, arithmetic::sub),

            Opcode::IncReg => arithmetic_reg!(self, arithmetic::inc),
            Opcode::IncRegAdr => arithmetic_reg_addr!(self, arithmetic::inc),
            Opcode::IncRegAdrOff => arithmetic_reg_addr_offset!(self, arithmetic::inc),
            Opcode::DecReg => arithmetic_reg!(self, arithmetic::dec),
            Opcode::DecRegAdr => arithmetic_reg_addr!(self, arithmetic::dec),
            Opcode::DecRegAdrOff => arithmetic_reg_addr_offset!(self, arithmetic::dec),

            Opcode::MulRegRegAdr => arithmetic_reg_reg_addr!(self, arithmetic::mul),
            Opcode::MulRegReg => arithmetic_reg_reg!(self, arithmetic::mul),
            Opcode::MulRegRegAdrOff => arithmetic_reg_reg_addr_off!(self, arithmetic::mul),
            Opcode::MulRegImm => arithmetic_reg_imm!(self, arithmetic::mul),
            Opcode::MulRegAdrReg => arithmetic_reg_addr_reg!(self, arithmetic::mul),

            Opcode::DivRegRegAdr => arithmetic_reg_reg_addr!(self, arithmetic::div),
            Opcode::DivRegReg => arithmetic_reg_reg!(self, arithmetic::div),
            Opcode::DivRegRegAdrOff => arithmetic_reg_reg_addr_off!(self, arithmetic::div),
            Opcode::DivRegImm => arithmetic_reg_imm!(self, arithmetic::div),
            Opcode::DivRegAdrReg => arithmetic_reg_addr_reg!(self, arithmetic::div),

            Opcode::AndRegReg => arithmetic_reg_reg!(self, arithmetic::and),
            Opcode::AndRegRegAdr => arithmetic_reg_reg_addr!(self, arithmetic::and),
            Opcode::OrRegReg => arithmetic_reg_reg!(self, arithmetic::or),
            Opcode::OrRegRegAdr => arithmetic_reg_reg_addr!(self, arithmetic::or),
            Opcode::XorRegReg => arithmetic_reg_reg!(self, arithmetic::xor),
            Opcode::XorRegRegAdr => arithmetic_reg_reg_addr!(self, arithmetic::xor),
            Opcode::NotReg => arithmetic_reg!(self, arithmetic::not),
            Opcode::NotRegAdr => arithmetic_reg_addr!(self, arithmetic::not),

            Opcode::LshRegImm => arithmetic_reg_imm!(self, arithmetic::shl),
            Opcode::LshRegAdrImm => arithmetic_reg_addr_imm!(self, arithmetic::shl),
            Opcode::LshRegAdrOffImm => arithmetic_reg_addr_offset_imm!(self, arithmetic::shl),

            Opcode::RshRegImm => arithmetic_reg_imm!(self, arithmetic::shr),
            Opcode::RshRegAdrImm => arithmetic_reg_addr_imm!(self, arithmetic::shr),
            Opcode::RshRegAdrOffImm => arithmetic_reg_addr_offset_imm!(self, arithmetic::shr),

            // End of arithmetic operations, other than for comparisons
            Opcode::CallImm => with_immediate!(self, |immediate| {
                let pc = self.registers.pc;
                self.memory_stack().push(pc);
                self.registers.pc = immediate;
            }),
            Opcode::CallReg => with_register_value!(self, |value| {
                let pc = self.registers.pc;
                self.memory_stack().push(pc);
                self.registers.pc = value;
            }),
            Opcode::CallRegOff => with_register_address_offset_as_address!(self, |address| {
                let pc = self.registers.pc;
                self.memory_stack().push(pc);
                self.registers.pc = address;
            }),
            Opcode::Ret => {
                let pc = self.memory_stack().pop();
                self.registers.pc = pc;
                ProcessorContinue::KeepRunning
            }

            // Comparison operations
            Opcode::CmpRegReg => arithmetic_reg_reg!(self, arithmetic::cmp),
            Opcode::CmpRegImm => arithmetic_reg_imm!(self, arithmetic::cmp),
            Opcode::CmpRegRegAdr => arithmetic_reg_reg_addr!(self, arithmetic::cmp),
            Opcode::CmpRegRegAdrOff => arithmetic_reg_reg_addr_off!(self, arithmetic::cmp),
            Opcode::TestRegReg => arithmetic_reg_reg!(self, arithmetic::test),
            Opcode::TestRegRegAdr => arithmetic_reg_reg_addr!(self, arithmetic::test),
            Opcode::TestRegRegAdrOff => arithmetic_reg_reg_addr_off!(self, arithmetic::test),
            // End of comparison operations
            Opcode::JmpImm => with_immediate!(self, |immediate| self.registers.pc = immediate),
            Opcode::JmpReg => with_register_value!(self, |value| self.registers.pc = value),
            Opcode::JmpRegOff => {
                with_register_address_offset_as_address!(self, |address| self.registers.pc =
                    address)
            }
            Opcode::JeImm => with_immediate!(self, |immediate| {
                if self.registers.fr.get(ProcessorFlags::ZF) {
                    self.registers.pc = immediate;
                }
            }),
            Opcode::JneImm => with_immediate!(self, |immediate| {
                if !self.registers.fr.get(ProcessorFlags::ZF) {
                    self.registers.pc = immediate;
                }
            }),
            Opcode::JgImm => with_immediate!(self, |immediate| {
                if !self.registers.fr.get(ProcessorFlags::ZF)
                    && (self.registers.fr.get(ProcessorFlags::SF)
                        == self.registers.fr.get(ProcessorFlags::OF))
                {
                    self.registers.pc = immediate;
                }
            }),
            Opcode::JgeImm => with_immediate!(self, |immediate| {
                if !self.registers.fr.get(ProcessorFlags::CF) {
                    self.registers.pc = immediate;
                }
            }),
            Opcode::JlImm => with_immediate!(self, |immediate| {
                if self.registers.fr.get(ProcessorFlags::SF)
                    != self.registers.fr.get(ProcessorFlags::OF)
                {
                    self.registers.pc = immediate;
                }
            }),
            Opcode::JleImm => with_immediate!(self, |immediate| {
                if self.registers.fr.get(ProcessorFlags::ZF)
                    || self.registers.fr.get(ProcessorFlags::CF)
                {
                    self.registers.pc = immediate;
                }
            }),

            Opcode::InRegPort => {
                save_to_register!(self, || with_immediate_raw!(self, input))
            }
            Opcode::InRegAdrPort => {
                save_to_register_address!(self, || with_immediate_raw!(self, input))
            }
            Opcode::InRegAdrOffPort => {
                save_to_register_address_offset!(self, || with_immediate_raw!(self, input))
            }
            Opcode::OutPortImm => {
                with_immediate!(self, |port| with_register_value!(self, |immediate| {
                    output(port, immediate)
                }))
            }
            Opcode::OutPortReg => {
                with_immediate!(self, |port| with_register_value!(self, |value| output(
                    port, value
                )))
            }
            Opcode::OutPortRegAdr => {
                with_immediate!(self, |port| with_register_address!(self, |address| output(
                    port, address
                )))
            }
            Opcode::OutPortRegAdrOff => {
                with_immediate_raw!(self, |port| with_register_address_offset!(self, |value| {
                    match value {
                        Some(value) => {
                            output(port, value);
                            ProcessorContinue::KeepRunning
                        }
                        None => ProcessorContinue::Error,
                    }
                }))
            }
            Opcode::Nop => ProcessorContinue::KeepRunning,
        }
    }

    fn load_executable(&mut self, executable: &Executable) -> Result<(), String> {
        unimplemented!()
    }
}
