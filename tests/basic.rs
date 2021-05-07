use yaxpeax_arch::{Arch, Decoder};
use yaxpeax_6502::{Opcode, Operand, N6502};

#[test]
fn test_decode() {
    let decoder = <N6502 as Arch>::Decoder::default();

    let data = [0xea];
    let instr = decoder.decode(data.iter().cloned()).unwrap();
    assert!(instr.opcode == Opcode::NOP);

    let data = [0x00];
    let instr = decoder.decode(data.iter().cloned()).unwrap();
    assert!(instr.opcode == Opcode::BRK);

    let data = [0xa2, 0xff];
    let instr = decoder.decode(data.iter().cloned()).unwrap();
    assert!(instr.opcode == Opcode::LDX);
}
