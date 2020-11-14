use crate::mnemonic::Opcode;
mod x86_16;
mod x86_32;
//use super::mnemonic::Operand::*;
//use super::mnemonic::Register::*;

pub fn generate(ops: Vec<Opcode>, mode: &str) -> String {
    if mode == "16bit" {
        x86_16::generate(ops)
    } else {
        x86_32::generate(ops)
    }
}

pub fn huga(op: Opcode) {
    match op {
        Opcode::Krz(_, _) => {println!("hoge")},
        _ => {}
    }
}
