/** References: https://www.masswerk.at/6502/6502_instruction_set.html
*/
use std::fmt;

use yaxpeax_arch::{AddressDiff, Arch, Decoder, LengthedInstruction};

#[derive(Debug)]
pub struct N6502;

impl Arch for N6502 {
    type Address = u16;
    type Instruction = Instruction;
    type DecodeError = DecodeError;
    type Decoder = InstDecoder;
    type Operand = Operand;
}

#[derive(Debug, Copy, Clone)]
pub struct Instruction {
    pub opcode: Opcode,
    pub op_width: Width,
    pub operand: Operand,
}

impl Default for Instruction {
    fn default() -> Self {
        Instruction {
            opcode: Opcode::Invalid(0xff),
            op_width: Width::None,
            operand: Operand::Implied,
        }
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), std::fmt::Error> {
        unimplemented!();
    }
}

impl LengthedInstruction for Instruction {
    type Unit = AddressDiff<<N6502 as Arch>::Address>;
    fn min_size() -> Self::Unit {
        AddressDiff::from_const(1)
    }

    fn len(&self) -> Self::Unit {
        match self.operand {
            Operand::Accumulator | Operand::Implied => AddressDiff::from_const(1),

            Operand::Immediate(_)
            | Operand::IndirectYIndexed(_)
            | Operand::XIndexedIndirect(_)
            | Operand::Relative(_)
            | Operand::ZeroPage(_)
            | Operand::ZeroPageX(_)
            | Operand::ZeroPageY(_) => AddressDiff::from_const(2),

            Operand::Absolute(_)
            | Operand::AbsoluteX(_)
            | Operand::AbsoluteY(_)
            | Operand::Indirect(_) => AddressDiff::from_const(3),
        }
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

#[derive(Debug, PartialEq)]
pub enum DecodeError {
    ExhaustedInput,
    InvalidOpcode,
    InvalidOperand,
}

impl fmt::Display for DecodeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        unimplemented!();
    }
}

impl yaxpeax_arch::DecodeError for DecodeError {
    fn data_exhausted(&self) -> bool {
        self == &DecodeError::ExhaustedInput
    }
    fn bad_opcode(&self) -> bool {
        self == &DecodeError::InvalidOpcode
    }
    fn bad_operand(&self) -> bool {
        self == &DecodeError::InvalidOperand
    }
}

#[derive(Debug)]
pub struct InstDecoder;

impl Default for InstDecoder {
    fn default() -> Self {
        InstDecoder {}
    }
}

impl Decoder<Instruction> for InstDecoder {
    type Error = DecodeError;

    fn decode_into<T: IntoIterator<Item = u8>>(
        &self,
        inst: &mut Instruction,
        bytes: T,
    ) -> Result<(), Self::Error> {
        unimplemented!();
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
