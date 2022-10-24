#![no_std]

/** References: https://www.masswerk.at/6502/6502_instruction_set.html
*/

#[cfg(feature = "std")]
extern crate std;

use yaxpeax_arch::{AddressDiff, Arch, Decoder, LengthedInstruction, Reader};

mod display;

#[derive(Debug)]
pub struct N6502;

impl Arch for N6502 {
    type Address = u16;
    type Word = u8;
    type Instruction = Instruction;
    type DecodeError = DecodeError;
    type Decoder = InstDecoder;
    type Operand = Operand;
}

#[derive(Debug, Copy, Clone)]
pub struct Instruction {
    pub opcode: Opcode,
    pub operand: Operand,
}

impl Default for Instruction {
    fn default() -> Self {
        Instruction {
            opcode: Opcode::Invalid(0xff),
            operand: Operand::Implied,
        }
    }
}

impl LengthedInstruction for Instruction {
    type Unit = AddressDiff<<N6502 as Arch>::Address>;
    fn min_size() -> Self::Unit {
        AddressDiff::from_const(1)
    }

    fn len(&self) -> Self::Unit {
        // Each opcode is 1 byte, remaining insn size inherent in operand.
        AddressDiff::from_const(self.operand.width() + 1)
    }
}

impl yaxpeax_arch::Instruction for Instruction {
    // FIXME: Probably not correct.
    fn well_defined(&self) -> bool {
        true
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Width {
    W,
    B,
    None,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Opcode {
    Invalid(u8),
    ADC,
    AND,
    ASL,
    BCC,
    BCS,
    BEQ,
    BIT,
    BMI,
    BNE,
    BPL,
    BRK,
    BVC,
    BVS,
    CLC,
    CLD,
    CLI,
    CLV,
    CMP,
    CPX,
    CPY,
    DEC,
    DEX,
    DEY,
    EOR,
    INC,
    INX,
    INY,
    JMP,
    JSR,
    LDA,
    LDX,
    LDY,
    LSR,
    NOP,
    ORA,
    PHA,
    PHP,
    PLA,
    PLP,
    ROL,
    ROR,
    RTI,
    RTS,
    SBC,
    SEC,
    SED,
    SEI,
    STA,
    STX,
    STY,
    TAX,
    TAY,
    TSX,
    TXA,
    TXS,
    TYA,
}

#[derive(Debug, Copy, Clone)]
pub enum Operand {
    Accumulator,
    Absolute(u16),
    AbsoluteX(u16),
    AbsoluteY(u16),
    Immediate(u8),
    Implied,
    Indirect(u16),
    IndirectYIndexed(u8),
    XIndexedIndirect(u8),
    Relative(u8),
    ZeroPage(u8),
    ZeroPageX(u8),
    ZeroPageY(u8),
}

impl Operand {
    fn width(&self) -> <N6502 as Arch>::Address {
        match self {
            Operand::Accumulator | Operand::Implied => 0,

            Operand::Immediate(_)
            | Operand::IndirectYIndexed(_)
            | Operand::XIndexedIndirect(_)
            | Operand::Relative(_)
            | Operand::ZeroPage(_)
            | Operand::ZeroPageX(_)
            | Operand::ZeroPageY(_) => 1,

            Operand::Absolute(_)
            | Operand::AbsoluteX(_)
            | Operand::AbsoluteY(_)
            | Operand::Indirect(_) => 2,
        }
    }
}

pub type DecodeError = yaxpeax_arch::StandardDecodeError;

#[derive(Debug)]
pub struct InstDecoder;

/** An inherent implementation of `InstDecoder` is made public in case I want to use each part of
    the decoder individually, such as in a cycle-accurate emulator.
*/
impl InstDecoder {
    pub fn op_type(&self, opcode: u8) -> Result<(Opcode, Operand), DecodeError> {
        match opcode {
            0x00 => Ok((Opcode::BRK, Operand::Implied)),
            0x01 => Ok((Opcode::ORA, Operand::XIndexedIndirect(Default::default()))),
            0x05 => Ok((Opcode::ORA, Operand::ZeroPage(Default::default()))),
            0x06 => Ok((Opcode::ASL, Operand::ZeroPage(Default::default()))),
            0x08 => Ok((Opcode::PHP, Operand::Implied)),
            0x09 => Ok((Opcode::ORA, Operand::Immediate(Default::default()))),
            0x0a => Ok((Opcode::ASL, Operand::Accumulator)),
            0x0d => Ok((Opcode::ORA, Operand::Absolute(Default::default()))),
            0x0e => Ok((Opcode::ASL, Operand::Absolute(Default::default()))),

            0x10 => Ok((Opcode::BPL, Operand::Relative(Default::default()))),
            0x11 => Ok((Opcode::ORA, Operand::IndirectYIndexed(Default::default()))),
            0x15 => Ok((Opcode::ORA, Operand::ZeroPageX(Default::default()))),
            0x16 => Ok((Opcode::ASL, Operand::ZeroPageX(Default::default()))),
            0x18 => Ok((Opcode::CLC, Operand::Implied)),
            0x19 => Ok((Opcode::ORA, Operand::AbsoluteY(Default::default()))),
            0x1d => Ok((Opcode::ORA, Operand::AbsoluteX(Default::default()))),
            0x1e => Ok((Opcode::ASL, Operand::AbsoluteX(Default::default()))),

            0x20 => Ok((Opcode::JSR, Operand::Absolute(Default::default()))),
            0x21 => Ok((Opcode::AND, Operand::XIndexedIndirect(Default::default()))),
            0x24 => Ok((Opcode::BIT, Operand::ZeroPage(Default::default()))),
            0x25 => Ok((Opcode::AND, Operand::ZeroPage(Default::default()))),
            0x26 => Ok((Opcode::ROL, Operand::ZeroPage(Default::default()))),
            0x28 => Ok((Opcode::PLP, Operand::Implied)),
            0x29 => Ok((Opcode::AND, Operand::Immediate(Default::default()))),
            0x2a => Ok((Opcode::ROL, Operand::Accumulator)),
            0x2c => Ok((Opcode::BIT, Operand::Absolute(Default::default()))),
            0x2d => Ok((Opcode::AND, Operand::Absolute(Default::default()))),
            0x2e => Ok((Opcode::ROL, Operand::Absolute(Default::default()))),

            0x30 => Ok((Opcode::BMI, Operand::Relative(Default::default()))),
            0x31 => Ok((Opcode::AND, Operand::IndirectYIndexed(Default::default()))),
            0x35 => Ok((Opcode::AND, Operand::ZeroPageX(Default::default()))),
            0x36 => Ok((Opcode::ROL, Operand::ZeroPageX(Default::default()))),
            0x38 => Ok((Opcode::SEC, Operand::Implied)),
            0x39 => Ok((Opcode::AND, Operand::AbsoluteY(Default::default()))),
            0x3d => Ok((Opcode::AND, Operand::AbsoluteX(Default::default()))),
            0x3e => Ok((Opcode::ROL, Operand::AbsoluteX(Default::default()))),

            0x40 => Ok((Opcode::RTI, Operand::Implied)),
            0x41 => Ok((Opcode::EOR, Operand::XIndexedIndirect(Default::default()))),
            0x45 => Ok((Opcode::EOR, Operand::ZeroPage(Default::default()))),
            0x46 => Ok((Opcode::LSR, Operand::ZeroPage(Default::default()))),
            0x48 => Ok((Opcode::PHA, Operand::Implied)),
            0x49 => Ok((Opcode::EOR, Operand::Immediate(Default::default()))),
            0x4a => Ok((Opcode::LSR, Operand::Accumulator)),
            0x4c => Ok((Opcode::JMP, Operand::Absolute(Default::default()))),
            0x4d => Ok((Opcode::EOR, Operand::Absolute(Default::default()))),
            0x4e => Ok((Opcode::LSR, Operand::Absolute(Default::default()))),

            0x50 => Ok((Opcode::BVC, Operand::Relative(Default::default()))),
            0x51 => Ok((Opcode::EOR, Operand::IndirectYIndexed(Default::default()))),
            0x55 => Ok((Opcode::EOR, Operand::ZeroPageX(Default::default()))),
            0x56 => Ok((Opcode::LSR, Operand::ZeroPageX(Default::default()))),
            0x58 => Ok((Opcode::CLI, Operand::Implied)),
            0x59 => Ok((Opcode::EOR, Operand::AbsoluteY(Default::default()))),
            0x5d => Ok((Opcode::EOR, Operand::AbsoluteX(Default::default()))),
            0x5e => Ok((Opcode::LSR, Operand::AbsoluteX(Default::default()))),

            0x60 => Ok((Opcode::RTS, Operand::Implied)),
            0x61 => Ok((Opcode::ADC, Operand::XIndexedIndirect(Default::default()))),
            0x65 => Ok((Opcode::ADC, Operand::ZeroPage(Default::default()))),
            0x66 => Ok((Opcode::ROR, Operand::ZeroPage(Default::default()))),
            0x68 => Ok((Opcode::PLA, Operand::Implied)),
            0x69 => Ok((Opcode::ADC, Operand::Immediate(Default::default()))),
            0x6a => Ok((Opcode::ROR, Operand::Accumulator)),
            0x6c => Ok((Opcode::JMP, Operand::Indirect(Default::default()))),
            0x6d => Ok((Opcode::ADC, Operand::Absolute(Default::default()))),
            0x6e => Ok((Opcode::ROR, Operand::Absolute(Default::default()))),

            0x70 => Ok((Opcode::BVS, Operand::Relative(Default::default()))),
            0x71 => Ok((Opcode::ADC, Operand::IndirectYIndexed(Default::default()))),
            0x75 => Ok((Opcode::ADC, Operand::ZeroPageX(Default::default()))),
            0x76 => Ok((Opcode::ROR, Operand::ZeroPageX(Default::default()))),
            0x78 => Ok((Opcode::SEI, Operand::Implied)),
            0x79 => Ok((Opcode::ADC, Operand::AbsoluteY(Default::default()))),
            0x7d => Ok((Opcode::ADC, Operand::AbsoluteX(Default::default()))),
            0x7e => Ok((Opcode::ROR, Operand::AbsoluteX(Default::default()))),

            /* 0x80 */
            0x81 => Ok((Opcode::STA, Operand::XIndexedIndirect(Default::default()))),
            0x84 => Ok((Opcode::STY, Operand::ZeroPage(Default::default()))),
            0x85 => Ok((Opcode::STA, Operand::ZeroPage(Default::default()))),
            0x86 => Ok((Opcode::STX, Operand::ZeroPage(Default::default()))),
            0x88 => Ok((Opcode::DEY, Operand::Implied)),
            0x8a => Ok((Opcode::TXA, Operand::Implied)),
            0x8c => Ok((Opcode::STY, Operand::Absolute(Default::default()))),
            0x8d => Ok((Opcode::STA, Operand::Absolute(Default::default()))),
            0x8e => Ok((Opcode::STX, Operand::Absolute(Default::default()))),

            0x90 => Ok((Opcode::BCC, Operand::Relative(Default::default()))),
            0x91 => Ok((Opcode::STA, Operand::IndirectYIndexed(Default::default()))),
            0x94 => Ok((Opcode::STY, Operand::ZeroPageX(Default::default()))),
            0x95 => Ok((Opcode::STA, Operand::ZeroPageX(Default::default()))),
            0x96 => Ok((Opcode::STX, Operand::ZeroPageY(Default::default()))),
            0x98 => Ok((Opcode::TYA, Operand::Implied)),
            0x99 => Ok((Opcode::STA, Operand::AbsoluteY(Default::default()))),
            0x9a => Ok((Opcode::TXS, Operand::Implied)),
            0x9d => Ok((Opcode::STA, Operand::AbsoluteX(Default::default()))),

            0xa0 => Ok((Opcode::LDY, Operand::Immediate(Default::default()))),
            0xa1 => Ok((Opcode::LDA, Operand::XIndexedIndirect(Default::default()))),
            0xa2 => Ok((Opcode::LDX, Operand::Immediate(Default::default()))),
            0xa4 => Ok((Opcode::LDY, Operand::ZeroPage(Default::default()))),
            0xa5 => Ok((Opcode::LDA, Operand::ZeroPage(Default::default()))),
            0xa6 => Ok((Opcode::LDX, Operand::ZeroPage(Default::default()))),
            0xa8 => Ok((Opcode::TAY, Operand::Implied)),
            0xa9 => Ok((Opcode::LDA, Operand::Immediate(Default::default()))),
            0xaa => Ok((Opcode::TAX, Operand::Implied)),
            0xac => Ok((Opcode::LDY, Operand::Absolute(Default::default()))),
            0xad => Ok((Opcode::LDA, Operand::Absolute(Default::default()))),
            0xae => Ok((Opcode::LDX, Operand::Absolute(Default::default()))),

            0xb0 => Ok((Opcode::BCS, Operand::Relative(Default::default()))),
            0xb1 => Ok((Opcode::LDA, Operand::IndirectYIndexed(Default::default()))),
            0xb4 => Ok((Opcode::LDY, Operand::ZeroPageX(Default::default()))),
            0xb5 => Ok((Opcode::LDA, Operand::ZeroPageX(Default::default()))),
            0xb6 => Ok((Opcode::LDX, Operand::ZeroPageY(Default::default()))),
            0xb8 => Ok((Opcode::CLV, Operand::Implied)),
            0xb9 => Ok((Opcode::LDA, Operand::AbsoluteY(Default::default()))),
            0xba => Ok((Opcode::TSX, Operand::Implied)),
            0xbc => Ok((Opcode::LDY, Operand::AbsoluteX(Default::default()))),
            0xbd => Ok((Opcode::LDA, Operand::AbsoluteX(Default::default()))),
            0xbe => Ok((Opcode::LDX, Operand::AbsoluteY(Default::default()))),

            0xc0 => Ok((Opcode::CPY, Operand::Immediate(Default::default()))),
            0xc1 => Ok((Opcode::CMP, Operand::XIndexedIndirect(Default::default()))),
            0xc4 => Ok((Opcode::CPY, Operand::ZeroPage(Default::default()))),
            0xc5 => Ok((Opcode::CMP, Operand::ZeroPage(Default::default()))),
            0xc6 => Ok((Opcode::DEC, Operand::ZeroPage(Default::default()))),
            0xc8 => Ok((Opcode::INY, Operand::Implied)),
            0xc9 => Ok((Opcode::CMP, Operand::Immediate(Default::default()))),
            0xca => Ok((Opcode::DEX, Operand::Implied)),
            0xcc => Ok((Opcode::CPY, Operand::Absolute(Default::default()))),
            0xcd => Ok((Opcode::CMP, Operand::Absolute(Default::default()))),
            0xce => Ok((Opcode::DEC, Operand::Absolute(Default::default()))),

            0xd0 => Ok((Opcode::BNE, Operand::Relative(Default::default()))),
            0xd1 => Ok((Opcode::CMP, Operand::IndirectYIndexed(Default::default()))),
            0xd5 => Ok((Opcode::CMP, Operand::ZeroPageX(Default::default()))),
            0xd6 => Ok((Opcode::DEC, Operand::ZeroPageX(Default::default()))),
            0xd8 => Ok((Opcode::CLD, Operand::Implied)),
            0xd9 => Ok((Opcode::CMP, Operand::AbsoluteY(Default::default()))),
            0xdd => Ok((Opcode::CMP, Operand::AbsoluteX(Default::default()))),
            0xde => Ok((Opcode::DEC, Operand::AbsoluteX(Default::default()))),

            0xe0 => Ok((Opcode::CPX, Operand::Immediate(Default::default()))),
            0xe1 => Ok((Opcode::SBC, Operand::XIndexedIndirect(Default::default()))),
            0xe4 => Ok((Opcode::CPX, Operand::ZeroPage(Default::default()))),
            0xe5 => Ok((Opcode::SBC, Operand::ZeroPage(Default::default()))),
            0xe6 => Ok((Opcode::INC, Operand::ZeroPage(Default::default()))),
            0xe8 => Ok((Opcode::INX, Operand::Implied)),
            0xe9 => Ok((Opcode::SBC, Operand::Immediate(Default::default()))),
            0xea => Ok((Opcode::NOP, Operand::Implied)),
            0xec => Ok((Opcode::CPX, Operand::Absolute(Default::default()))),
            0xed => Ok((Opcode::SBC, Operand::Absolute(Default::default()))),
            0xee => Ok((Opcode::INC, Operand::Absolute(Default::default()))),

            0xf0 => Ok((Opcode::BEQ, Operand::Relative(Default::default()))),
            0xf1 => Ok((Opcode::SBC, Operand::IndirectYIndexed(Default::default()))),
            0xf5 => Ok((Opcode::SBC, Operand::ZeroPageX(Default::default()))),
            0xf6 => Ok((Opcode::INC, Operand::ZeroPageX(Default::default()))),
            0xf8 => Ok((Opcode::SED, Operand::Implied)),
            0xf9 => Ok((Opcode::SBC, Operand::AbsoluteY(Default::default()))),
            0xfd => Ok((Opcode::SBC, Operand::AbsoluteX(Default::default()))),
            0xfe => Ok((Opcode::INC, Operand::AbsoluteX(Default::default()))),

            _ => Err(DecodeError::InvalidOpcode),
        }
    }
}

impl Default for InstDecoder {
    fn default() -> Self {
        InstDecoder {}
    }
}

impl Decoder<N6502> for InstDecoder {
    fn decode_into<T: Reader<<N6502 as Arch>::Address, <N6502 as Arch>::Word>>(
        &self,
        inst: &mut Instruction,
        words: &mut T,
    ) -> Result<(), <N6502 as Arch>::DecodeError> {
        let opcode = words.next()?;

        let (op_type, operand) = self.op_type(opcode).map_err(|e| {
            inst.opcode = Opcode::Invalid(opcode);
            e
        })?;

        let mut op_byte: u8 = 0;
        let mut op_word: u16 = 0;

        match operand.width() {
            0 => {}
            1 => {
                op_byte = words.next()?;
            }
            2 => {
                let byte_lo = words.next()?;
                let byte_hi = words.next()?;

                op_word = u16::from_le_bytes([byte_lo, byte_hi]);
            }
            _ => {
                unreachable!()
            }
        }

        let operand = match operand {
            Operand::Accumulator => Operand::Accumulator,
            Operand::Implied => Operand::Implied,

            Operand::Immediate(_) => Operand::Immediate(op_byte),
            Operand::IndirectYIndexed(_) => Operand::IndirectYIndexed(op_byte),
            Operand::XIndexedIndirect(_) => Operand::XIndexedIndirect(op_byte),
            Operand::Relative(_) => Operand::Relative(op_byte),
            Operand::ZeroPage(_) => Operand::ZeroPage(op_byte),
            Operand::ZeroPageX(_) => Operand::ZeroPageX(op_byte),
            Operand::ZeroPageY(_) => Operand::ZeroPageY(op_byte),

            Operand::Absolute(_) => Operand::Absolute(op_word),
            Operand::AbsoluteX(_) => Operand::AbsoluteX(op_word),
            Operand::AbsoluteY(_) => Operand::AbsoluteY(op_word),
            Operand::Indirect(_) => Operand::Indirect(op_word),
        };

        inst.opcode = op_type;
        inst.operand = operand;

        Ok(())
    }
}

pub struct Bytes {
    b1: u8,
    b2: u8,
    b3: u8,
    num: usize,
}

impl Bytes {
    fn one(b1: u8) -> Self {
        Self {
            b1,
            b2: 0,
            b3: 0,
            num: 1,
        }
    }

    fn two(b1: u8, b2: u8) -> Self {
        Self {
            b1,
            b2,
            b3: 0,
            num: 2,
        }
    }

    fn three(b1: u8, b2: u8, b3: u8) -> Self {
        Self { b1, b2, b3, num: 3 }
    }
}

impl Iterator for Bytes {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        let ret = match self.num {
            3 => Some(self.b3),
            2 => Some(self.b2),
            1 => Some(self.b1),
            _ => None,
        };
        self.num -= 1;
        ret
    }
}

impl Instruction {
    pub fn decode(&self) -> Bytes {
        match (self.opcode, self.operand) {
            (Opcode::BRK, Operand::Implied) => Bytes::one(0x00),
            (Opcode::ORA, Operand::XIndexedIndirect(val)) => Bytes::two(0x01, val),
            (Opcode::ORA, Operand::ZeroPage(val)) => Bytes::two(0x05, val),
            (Opcode::ASL, Operand::ZeroPage(val)) => Bytes::two(0x06, val),
            (Opcode::PHP, Operand::Implied) => Bytes::one(0x08),
            (Opcode::ORA, Operand::Immediate(val)) => Bytes::two(0x09, val),
            (Opcode::ASL, Operand::Accumulator) => Bytes::one(0x0a),
            (Opcode::ORA, Operand::Absolute(addr)) => {
                Bytes::three(0x0d, addr.to_le_bytes()[0], addr.to_le_bytes()[1])
            }
            (Opcode::ASL, Operand::Absolute(addr)) => {
                Bytes::three(0x0e, addr.to_le_bytes()[0], addr.to_le_bytes()[1])
            }

            (Opcode::BPL, Operand::Relative(val)) => Bytes::two(0x10, val),
            (Opcode::ORA, Operand::IndirectYIndexed(val)) => Bytes::two(0x11, val),
            (Opcode::ORA, Operand::ZeroPageX(val)) => Bytes::two(0x15, val),
            (Opcode::ASL, Operand::ZeroPageX(val)) => Bytes::two(0x16, val),
            (Opcode::CLC, Operand::Implied) => Bytes::one(0x18),
            (Opcode::ORA, Operand::AbsoluteY(addr)) => {
                Bytes::three(0x19, addr.to_le_bytes()[0], addr.to_le_bytes()[1])
            }
            (Opcode::ORA, Operand::AbsoluteX(addr)) => {
                Bytes::three(0x1d, addr.to_le_bytes()[0], addr.to_le_bytes()[1])
            }
            (Opcode::ASL, Operand::AbsoluteX(addr)) => {
                Bytes::three(0x1e, addr.to_le_bytes()[0], addr.to_le_bytes()[1])
            }

            (Opcode::JSR, Operand::Absolute(addr)) => {
                Bytes::three(0x20, addr.to_le_bytes()[0], addr.to_le_bytes()[1])
            }
            (Opcode::AND, Operand::XIndexedIndirect(val)) => Bytes::two(0x21, val),
            (Opcode::BIT, Operand::ZeroPage(val)) => Bytes::two(0x24, val),
            (Opcode::AND, Operand::ZeroPage(val)) => Bytes::two(0x25, val),
            (Opcode::ROL, Operand::ZeroPage(val)) => Bytes::two(0x26, val),
            (Opcode::PLP, Operand::Implied) => Bytes::one(0x28),
            (Opcode::AND, Operand::Immediate(val)) => Bytes::two(0x29, val),
            (Opcode::ROL, Operand::Accumulator) => Bytes::one(0x2a),
            (Opcode::BIT, Operand::Absolute(addr)) => {
                Bytes::three(0x2c, addr.to_le_bytes()[0], addr.to_le_bytes()[1])
            }
            (Opcode::AND, Operand::Absolute(addr)) => {
                Bytes::three(0x2d, addr.to_le_bytes()[0], addr.to_le_bytes()[1])
            }

            (Opcode::ROL, Operand::Absolute(addr)) => {
                Bytes::three(0x2e, addr.to_le_bytes()[0], addr.to_le_bytes()[1])
            }
            (Opcode::BMI, Operand::Relative(val)) => Bytes::two(0x30, val),
            (Opcode::AND, Operand::IndirectYIndexed(val)) => Bytes::two(0x31, val),
            (Opcode::AND, Operand::ZeroPageX(val)) => Bytes::two(0x35, val),
            (Opcode::ROL, Operand::ZeroPageX(val)) => Bytes::two(0x36, val),
            (Opcode::SEC, Operand::Implied) => Bytes::one(0x38),
            (Opcode::AND, Operand::AbsoluteY(addr)) => {
                Bytes::three(0x39, addr.to_le_bytes()[0], addr.to_le_bytes()[1])
            }
            (Opcode::AND, Operand::AbsoluteX(addr)) => {
                Bytes::three(0x3d, addr.to_le_bytes()[0], addr.to_le_bytes()[1])
            }
            (Opcode::ROL, Operand::AbsoluteX(addr)) => {
                Bytes::three(0x3e, addr.to_le_bytes()[0], addr.to_le_bytes()[1])
            }

            (Opcode::RTI, Operand::Implied) => Bytes::one(0x40),
            (Opcode::EOR, Operand::XIndexedIndirect(val)) => Bytes::two(0x41, val),
            (Opcode::EOR, Operand::ZeroPage(val)) => Bytes::two(0x45, val),
            (Opcode::LSR, Operand::ZeroPage(val)) => Bytes::two(0x46, val),
            (Opcode::PHA, Operand::Implied) => Bytes::one(0x48),
            (Opcode::EOR, Operand::Immediate(val)) => Bytes::two(0x49, val),
            (Opcode::LSR, Operand::Accumulator) => Bytes::one(0x4a),
            (Opcode::JMP, Operand::Absolute(addr)) => {
                Bytes::three(0x4c, addr.to_le_bytes()[0], addr.to_le_bytes()[1])
            }
            (Opcode::EOR, Operand::Absolute(addr)) => {
                Bytes::three(0x4d, addr.to_le_bytes()[0], addr.to_le_bytes()[1])
            }
            (Opcode::LSR, Operand::Absolute(addr)) => {
                Bytes::three(0x4e, addr.to_le_bytes()[0], addr.to_le_bytes()[1])
            }

            (Opcode::BVC, Operand::Relative(val)) => Bytes::two(0x50, val),
            (Opcode::EOR, Operand::IndirectYIndexed(val)) => Bytes::two(0x51, val),
            (Opcode::EOR, Operand::ZeroPageX(val)) => Bytes::two(0x55, val),
            (Opcode::LSR, Operand::ZeroPageX(val)) => Bytes::two(0x56, val),
            (Opcode::CLI, Operand::Implied) => Bytes::one(0x58),
            (Opcode::EOR, Operand::AbsoluteY(addr)) => {
                Bytes::three(0x59, addr.to_le_bytes()[0], addr.to_le_bytes()[1])
            }
            (Opcode::EOR, Operand::AbsoluteX(addr)) => {
                Bytes::three(0x5d, addr.to_le_bytes()[0], addr.to_le_bytes()[1])
            }
            (Opcode::LSR, Operand::AbsoluteX(addr)) => {
                Bytes::three(0x5e, addr.to_le_bytes()[0], addr.to_le_bytes()[1])
            }

            (Opcode::RTS, Operand::Implied) => Bytes::one(0x60),
            (Opcode::ADC, Operand::XIndexedIndirect(val)) => Bytes::two(0x61, val),
            (Opcode::ADC, Operand::ZeroPage(val)) => Bytes::two(0x65, val),
            (Opcode::ROR, Operand::ZeroPage(val)) => Bytes::two(0x66, val),
            (Opcode::PLA, Operand::Implied) => Bytes::one(0x68),
            (Opcode::ADC, Operand::Immediate(val)) => Bytes::two(0x69, val),
            (Opcode::ROR, Operand::Accumulator) => Bytes::one(0x6a),
            (Opcode::JMP, Operand::Indirect(addr)) => {
                Bytes::three(0x6c, addr.to_le_bytes()[0], addr.to_le_bytes()[1])
            }
            (Opcode::ADC, Operand::Absolute(addr)) => {
                Bytes::three(0x6d, addr.to_le_bytes()[0], addr.to_le_bytes()[1])
            }
            (Opcode::ROR, Operand::Absolute(addr)) => {
                Bytes::three(0x6e, addr.to_le_bytes()[0], addr.to_le_bytes()[1])
            }
            (Opcode::BVS, Operand::Relative(val)) => Bytes::two(0x70, val),
            (Opcode::ADC, Operand::IndirectYIndexed(val)) => Bytes::two(0x71, val),
            (Opcode::ADC, Operand::ZeroPageX(val)) => Bytes::two(0x75, val),
            (Opcode::ROR, Operand::ZeroPageX(val)) => Bytes::two(0x76, val),
            (Opcode::SEI, Operand::Implied) => Bytes::one(0x78),
            (Opcode::ADC, Operand::AbsoluteY(addr)) => {
                Bytes::three(0x79, addr.to_le_bytes()[0], addr.to_le_bytes()[1])
            }
            (Opcode::ADC, Operand::AbsoluteX(addr)) => {
                Bytes::three(0x7d, addr.to_le_bytes()[0], addr.to_le_bytes()[1])
            }
            (Opcode::ROR, Operand::AbsoluteX(addr)) => {
                Bytes::three(0x7e, addr.to_le_bytes()[0], addr.to_le_bytes()[1])
            }

            (Opcode::STA, Operand::XIndexedIndirect(val)) => Bytes::two(0x81, val),
            (Opcode::STY, Operand::ZeroPage(val)) => Bytes::two(0x84, val),
            (Opcode::STA, Operand::ZeroPage(val)) => Bytes::two(0x85, val),
            (Opcode::STX, Operand::ZeroPage(val)) => Bytes::two(0x86, val),
            (Opcode::DEY, Operand::Implied) => Bytes::one(0x88),
            (Opcode::TXA, Operand::Implied) => Bytes::one(0x8a),
            (Opcode::STY, Operand::Absolute(addr)) => {
                Bytes::three(0x8c, addr.to_le_bytes()[0], addr.to_le_bytes()[1])
            }

            (Opcode::STA, Operand::Absolute(addr)) => {
                Bytes::three(0x8d, addr.to_le_bytes()[0], addr.to_le_bytes()[1])
            }
            (Opcode::STX, Operand::Absolute(addr)) => {
                Bytes::three(0x8e, addr.to_le_bytes()[0], addr.to_le_bytes()[1])
            }

            (Opcode::BCC, Operand::Relative(val)) => Bytes::two(0x90, val),
            (Opcode::STA, Operand::IndirectYIndexed(val)) => Bytes::two(0x91, val),
            (Opcode::STY, Operand::ZeroPageX(val)) => Bytes::two(0x94, val),
            (Opcode::STA, Operand::ZeroPageX(val)) => Bytes::two(0x95, val),
            (Opcode::STX, Operand::ZeroPageY(val)) => Bytes::two(0x96, val),
            (Opcode::TYA, Operand::Implied) => Bytes::one(0x98),
            (Opcode::STA, Operand::AbsoluteY(addr)) => {
                Bytes::three(0x99, addr.to_le_bytes()[0], addr.to_le_bytes()[1])
            }
            (Opcode::TXS, Operand::Implied) => Bytes::one(0x9a),
            (Opcode::STA, Operand::AbsoluteX(addr)) => {
                Bytes::three(0x9d, addr.to_le_bytes()[0], addr.to_le_bytes()[1])
            }

            (Opcode::LDY, Operand::Immediate(val)) => Bytes::two(0xa0, val),
            (Opcode::LDA, Operand::XIndexedIndirect(val)) => Bytes::two(0xa1, val),
            (Opcode::LDX, Operand::Immediate(val)) => Bytes::two(0xa2, val),
            (Opcode::LDY, Operand::ZeroPage(val)) => Bytes::two(0xa4, val),
            (Opcode::LDA, Operand::ZeroPage(val)) => Bytes::two(0xa5, val),
            (Opcode::LDX, Operand::ZeroPage(val)) => Bytes::two(0xa6, val),
            (Opcode::TAY, Operand::Implied) => Bytes::one(0xa8),
            (Opcode::LDA, Operand::Immediate(val)) => Bytes::two(0xa9, val),
            (Opcode::TAX, Operand::Implied) => Bytes::one(0xaa),
            (Opcode::LDY, Operand::Absolute(addr)) => {
                Bytes::three(0xac, addr.to_le_bytes()[0], addr.to_le_bytes()[1])
            }
            (Opcode::LDA, Operand::Absolute(addr)) => {
                Bytes::three(0xad, addr.to_le_bytes()[0], addr.to_le_bytes()[1])
            }
            (Opcode::LDX, Operand::Absolute(addr)) => {
                Bytes::three(0xae, addr.to_le_bytes()[0], addr.to_le_bytes()[1])
            }

            (Opcode::BCS, Operand::Relative(val)) => Bytes::two(0xb0, val),
            (Opcode::LDA, Operand::IndirectYIndexed(val)) => Bytes::two(0xb1, val),
            (Opcode::LDY, Operand::ZeroPageX(val)) => Bytes::two(0xb4, val),
            (Opcode::LDA, Operand::ZeroPageX(val)) => Bytes::two(0xb5, val),
            (Opcode::LDX, Operand::ZeroPageY(val)) => Bytes::two(0xb6, val),
            (Opcode::CLV, Operand::Implied) => Bytes::one(0xb8),
            (Opcode::LDA, Operand::AbsoluteY(addr)) => {
                Bytes::three(0xb9, addr.to_le_bytes()[0], addr.to_le_bytes()[1])
            }
            (Opcode::TSX, Operand::Implied) => Bytes::one(0xba),
            (Opcode::LDY, Operand::AbsoluteX(addr)) => {
                Bytes::three(0xbc, addr.to_le_bytes()[0], addr.to_le_bytes()[1])
            }
            (Opcode::LDA, Operand::AbsoluteX(addr)) => {
                Bytes::three(0xbd, addr.to_le_bytes()[0], addr.to_le_bytes()[1])
            }
            (Opcode::LDX, Operand::AbsoluteY(addr)) => {
                Bytes::three(0xbe, addr.to_le_bytes()[0], addr.to_le_bytes()[1])
            }

            (Opcode::CPY, Operand::Immediate(val)) => Bytes::two(0xc0, val),
            (Opcode::CMP, Operand::XIndexedIndirect(val)) => Bytes::two(0xc1, val),
            (Opcode::CPY, Operand::ZeroPage(val)) => Bytes::two(0xc4, val),
            (Opcode::CMP, Operand::ZeroPage(val)) => Bytes::two(0xc5, val),
            (Opcode::DEC, Operand::ZeroPage(val)) => Bytes::two(0xc6, val),
            (Opcode::INY, Operand::Implied) => Bytes::one(0xc8),
            (Opcode::CMP, Operand::Immediate(val)) => Bytes::two(0xc9, val),
            (Opcode::DEX, Operand::Implied) => Bytes::one(0xca),
            (Opcode::CPY, Operand::Absolute(addr)) => {
                Bytes::three(0xcc, addr.to_le_bytes()[0], addr.to_le_bytes()[1])
            }
            (Opcode::CMP, Operand::Absolute(addr)) => {
                Bytes::three(0xcd, addr.to_le_bytes()[0], addr.to_le_bytes()[1])
            }
            (Opcode::DEC, Operand::Absolute(addr)) => {
                Bytes::three(0xce, addr.to_le_bytes()[0], addr.to_le_bytes()[1])
            }

            (Opcode::BNE, Operand::Relative(val)) => Bytes::two(0xd0, val),
            (Opcode::CMP, Operand::IndirectYIndexed(val)) => Bytes::two(0xd1, val),
            (Opcode::CMP, Operand::ZeroPageX(val)) => Bytes::two(0xd5, val),
            (Opcode::DEC, Operand::ZeroPageX(val)) => Bytes::two(0xd6, val),
            (Opcode::CLD, Operand::Implied) => Bytes::one(0xd8),
            (Opcode::CMP, Operand::AbsoluteY(addr)) => {
                Bytes::three(0xd9, addr.to_le_bytes()[0], addr.to_le_bytes()[1])
            }
            (Opcode::CMP, Operand::AbsoluteX(addr)) => {
                Bytes::three(0xdd, addr.to_le_bytes()[0], addr.to_le_bytes()[1])
            }
            (Opcode::DEC, Operand::AbsoluteX(addr)) => {
                Bytes::three(0xde, addr.to_le_bytes()[0], addr.to_le_bytes()[1])
            }

            (Opcode::CPX, Operand::Immediate(val)) => Bytes::two(0xe0, val),
            (Opcode::SBC, Operand::XIndexedIndirect(addr)) => Bytes::two(0xe1, addr),
            (Opcode::CPX, Operand::ZeroPage(val)) => Bytes::two(0xe4, val),
            (Opcode::SBC, Operand::ZeroPage(val)) => Bytes::two(0xe5, val),
            (Opcode::INC, Operand::ZeroPage(val)) => Bytes::two(0xe6, val),
            (Opcode::INX, Operand::Implied) => Bytes::one(0xe8),
            (Opcode::SBC, Operand::Immediate(val)) => Bytes::two(0xe9, val),
            (Opcode::NOP, Operand::Implied) => Bytes::one(0xea),
            (Opcode::CPX, Operand::Absolute(addr)) => {
                Bytes::three(0xec, addr.to_le_bytes()[0], addr.to_le_bytes()[1])
            }
            (Opcode::SBC, Operand::Absolute(addr)) => {
                Bytes::three(0xed, addr.to_le_bytes()[0], addr.to_le_bytes()[1])
            }
            (Opcode::INC, Operand::Absolute(addr)) => {
                Bytes::three(0xee, addr.to_le_bytes()[0], addr.to_le_bytes()[1])
            }

            (Opcode::BEQ, Operand::Relative(val)) => Bytes::two(0xf0, val),
            (Opcode::SBC, Operand::IndirectYIndexed(val)) => Bytes::two(0xf1, val),
            (Opcode::SBC, Operand::ZeroPageX(val)) => Bytes::two(0xf5, val),
            (Opcode::INC, Operand::ZeroPageX(val)) => Bytes::two(0xf6, val),
            (Opcode::SED, Operand::Implied) => Bytes::one(0xf8),
            (Opcode::SBC, Operand::AbsoluteY(addr)) => {
                Bytes::three(0xf9, addr.to_le_bytes()[0], addr.to_le_bytes()[1])
            }
            (Opcode::SBC, Operand::AbsoluteX(addr)) => {
                Bytes::three(0xfd, addr.to_le_bytes()[0], addr.to_le_bytes()[1])
            }
            (Opcode::INC, Operand::AbsoluteX(addr)) => {
                Bytes::three(0xfe, addr.to_le_bytes()[0], addr.to_le_bytes()[1])
            }
            (_, _) => panic!(),
        }
    }
}
