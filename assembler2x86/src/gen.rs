use crate::mnemonic::*;
mod x86_16;
mod x86_32;
//use super::mnemonic::Operand::*;
//use super::mnemonic::Register::*;

pub fn generate(ops: Vec<Instruction>, mode: &str) -> String {
    if mode == "16bit" {
        x86_16::generate(ops)
    } else {
        x86_32::generate(ops)
    }
}
