use crate::mnemonic::Opcode;
//use super::mnemonic::Operand::*;
//use super::mnemonic::Register::*;

pub fn huga(op: Opcode) {
    match op {
        Opcode::Krz(_, _) => {println!("hoge")},
        _ => {}
    }
}
