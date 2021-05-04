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
    pub operands: Operand,
}

impl Default for Instruction {
    fn default() -> Self {
        unimplemented!();
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
        unimplemented!();
    }
    fn len(&self) -> Self::Unit {
        unimplemented!();
    }
}

impl yaxpeax_arch::Instruction for Instruction {
    fn well_defined(&self) -> bool {
        unimplemented!();
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Width {
    W,
    B,
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
        unimplemented!();
    }
    fn bad_opcode(&self) -> bool {
        unimplemented!();
    }
    fn bad_operand(&self) -> bool {
        unimplemented!();
    }
}

#[derive(Debug)]
pub struct InstDecoder;

impl Default for InstDecoder {
    fn default() -> Self {
        unimplemented!();
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
