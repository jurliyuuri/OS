use super::Token;
use super::Opcode;
use super::Operand;
use super::Register;

pub struct Parser {}

impl Parser {
    pub fn new() -> Parser {
        Parser{}
    }

    pub fn parse(&mut self, tokens: Vec<Token>) -> Vec<Opcode> {
        Vec::new()
    }
}
