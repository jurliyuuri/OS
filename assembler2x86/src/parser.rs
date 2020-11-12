use super::mnemonic::*;
use super::mnemonic::Opcode::*;
use super::mnemonic::Operand::*;
use super::mnemonic::Register::*;


pub fn hoge() -> Opcode {
    let hoge: Opcode = Krz(Reg(F0), Reg(F1));
    hoge
}
