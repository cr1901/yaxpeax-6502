/** References: https://www.masswerk.at/6502/6502_instruction_set.html
*/
use std::fmt;

use take_mut;
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

/** An inherent implementation of `InstDecoder` is made public in case I want to use each part of
    the decoder individually, such as in a cycle-accurate emulator.
*/
impl InstDecoder {
    pub fn is_legal(&self, opcode: u8) -> bool {
        let nib_hi = (opcode & 0xf0) >> 4;
        let nib_lo = opcode & 0x0f;

        match nib_lo {
            0x00 => opcode != 0x80,
            0x01 | 0x05 | 0x06 | 0x08 | 0x0d => true,
            0x02 => opcode == 0xA2,
            0x03 | 0x07 | 0x0b | 0x0f => false,
            0x04 => match nib_hi {
                0x02 | 0x08 | 0x09 | 0x0a | 0x0b | 0x0c | 0x0e => true,
                _ => false,
            },
            0x09 => opcode != 0x89,
            0x0a => match nib_hi {
                0x01 | 0x03 | 0x05 | 0x07 | 0x0d | 0x0f => false,
                _ => true,
            },
            0x0c => match nib_hi {
                0x00 | 0x01 | 0x03 | 0x05 | 0x07 | 0x09 | 0x0d | 0x0f => false,
                _ => true,
            },
            0x0e => opcode != 0x9e,
            _ => {
                unreachable!()
            }
        }
    }

    /** Find the opcode type, given that it's a legal opcode. Will not return valid data if
        an illegal opcode was supplied.

        The `Operand` variant returned contains the default value for the variant's type.
        It is expected that the user will fill in the data.
    */
    pub fn legal_op_type(&self, opcode: u8) -> (Opcode, Operand) {
        unimplemented!()
    }
}

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
        let mut bytes_iter = bytes.into_iter();
        let opcode = bytes_iter.next().ok_or(DecodeError::ExhaustedInput)?;

        if self.is_legal(opcode) {
            let (op_type, mut operand) = self.legal_op_type(opcode);

            let mut op_byte: u8 = 0;
            let mut op_word: u16 = 0;

            match operand.width() {
                0 => {}
                1 => {
                    op_byte = bytes_iter.next().ok_or(DecodeError::ExhaustedInput)?;
                }
                2 => {
                    let byte_lo = bytes_iter.next().ok_or(DecodeError::ExhaustedInput)?;
                    let byte_hi = bytes_iter.next().ok_or(DecodeError::ExhaustedInput)?;

                    op_word = u16::from_le_bytes([byte_lo, byte_hi]);
                }
                _ => {
                    unreachable!()
                }
            }

            take_mut::take(&mut operand, |op| match op {
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
            });

            inst.opcode = op_type;
            inst.operand = operand;
        } else {
            // TODO: We should support the illegal opcodes that are used in real-world code.
            inst.opcode = Opcode::Invalid(opcode);
            return Err(DecodeError::InvalidOpcode);
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
