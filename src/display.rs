use std::fmt::{self, Write};

use super::{DecodeError, Instruction, Opcode, Operand};

impl fmt::Display for DecodeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use yaxpeax_arch::DecodeError;
        f.write_str(self.description())
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        // TODO: Use fixed-len buffer instead?
        let mut s = String::new();

        write!(s, "{}", self.opcode)?;
        write!(s, " {}", self.operand)?;
        write!(f, "{}", s)
    }
}

impl fmt::Display for Opcode {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            Opcode::Invalid(o) => {
                write!(f, "invalid(#${:02x})", o)
            }
            Opcode::ADC => {
                write!(f, "ADC")
            }
            Opcode::AND => {
                write!(f, "AND")
            }
            Opcode::ASL => {
                write!(f, "ASL")
            }
            Opcode::BCC => {
                write!(f, "BCC")
            }
            Opcode::BCS => {
                write!(f, "BCS")
            }
            Opcode::BEQ => {
                write!(f, "BEQ")
            }
            Opcode::BIT => {
                write!(f, "BIT")
            }
            Opcode::BMI => {
                write!(f, "BMI")
            }
            Opcode::BNE => {
                write!(f, "BNE")
            }
            Opcode::BPL => {
                write!(f, "BPL")
            }
            Opcode::BRK => {
                write!(f, "BRK")
            }
            Opcode::BVC => {
                write!(f, "BVC")
            }
            Opcode::BVS => {
                write!(f, "BVS")
            }
            Opcode::CLC => {
                write!(f, "CLC")
            }
            Opcode::CLD => {
                write!(f, "CLD")
            }
            Opcode::CLI => {
                write!(f, "CLI")
            }
            Opcode::CLV => {
                write!(f, "CLV")
            }
            Opcode::CMP => {
                write!(f, "CMP")
            }
            Opcode::CPX => {
                write!(f, "CPX")
            }
            Opcode::CPY => {
                write!(f, "CPY")
            }
            Opcode::DEC => {
                write!(f, "DEC")
            }
            Opcode::DEX => {
                write!(f, "DEX")
            }
            Opcode::DEY => {
                write!(f, "DEY")
            }
            Opcode::EOR => {
                write!(f, "EOR")
            }
            Opcode::INC => {
                write!(f, "INC")
            }
            Opcode::INX => {
                write!(f, "INX")
            }
            Opcode::INY => {
                write!(f, "INY")
            }
            Opcode::JMP => {
                write!(f, "JMP")
            }
            Opcode::JSR => {
                write!(f, "JSR")
            }
            Opcode::LDA => {
                write!(f, "LDA")
            }
            Opcode::LDX => {
                write!(f, "LDX")
            }
            Opcode::LDY => {
                write!(f, "LDY")
            }
            Opcode::LSR => {
                write!(f, "LSR")
            }
            Opcode::NOP => {
                write!(f, "NOP")
            }
            Opcode::ORA => {
                write!(f, "ORA")
            }
            Opcode::PHA => {
                write!(f, "PHA")
            }
            Opcode::PHP => {
                write!(f, "PHP")
            }
            Opcode::PLA => {
                write!(f, "PLA")
            }
            Opcode::PLP => {
                write!(f, "PLP")
            }
            Opcode::ROL => {
                write!(f, "ROL")
            }
            Opcode::ROR => {
                write!(f, "ROR")
            }
            Opcode::RTI => {
                write!(f, "RTI")
            }
            Opcode::RTS => {
                write!(f, "RTS")
            }
            Opcode::SBC => {
                write!(f, "SBC")
            }
            Opcode::SEC => {
                write!(f, "SEC")
            }
            Opcode::SED => {
                write!(f, "SED")
            }
            Opcode::SEI => {
                write!(f, "SEI")
            }
            Opcode::STA => {
                write!(f, "STA")
            }
            Opcode::STX => {
                write!(f, "STX")
            }
            Opcode::STY => {
                write!(f, "STY")
            }
            Opcode::TAX => {
                write!(f, "TAX")
            }
            Opcode::TAY => {
                write!(f, "TAY")
            }
            Opcode::TSX => {
                write!(f, "TSX")
            }
            Opcode::TXA => {
                write!(f, "TXA")
            }
            Opcode::TXS => {
                write!(f, "TXS")
            }
            Opcode::TYA => {
                write!(f, "TYA")
            }
        }
    }
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            Operand::Accumulator => {
                write!(f, "A")
            }
            Operand::Absolute(w) => {
                write!(f, "${:04x}", w)
            }
            Operand::AbsoluteX(w) => {
                write!(f, "${:04x}, X", w)
            }
            Operand::AbsoluteY(w) => {
                write!(f, "${:04x}, Y", w)
            }
            Operand::Immediate(b) => {
                write!(f, "#${:02x}", b)
            }
            Operand::Implied => Ok(()),
            Operand::Indirect(w) => {
                write!(f, "(${:04x})", w)
            }
            Operand::IndirectYIndexed(b) => {
                write!(f, "(${:02x}), Y", b)
            }
            Operand::XIndexedIndirect(b) => {
                write!(f, "(${:02x}, X)", b)
            }
            Operand::Relative(b) | Operand::ZeroPage(b) => {
                write!(f, "${:02x}", b)
            }
            Operand::ZeroPageX(b) => {
                write!(f, "${:02x}, X", b)
            }
            Operand::ZeroPageY(b) => {
                write!(f, "${:02x}, Y", b)
            }
        }
    }
}
