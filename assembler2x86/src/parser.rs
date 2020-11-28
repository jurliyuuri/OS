use super::mnemonic::*;
mod token;
mod syntax;

#[derive(Debug)]
pub enum Token {
    Ic,
    Ci,

    Krz,
    Malkrz,
    Fen,
    Inj,

    Ata,
    Nta,
    Lat,
    Latsna,
    Kak,
    Ada,
    Ekc,
    Nac,
    Dal,
    Dto,
    Dro,
    Dtosna,

    Fi,
    Xtlo,
    Xylo,
    Clo,
    Xolo,
    Llo,
    Niv,
    Xtlonys,
    Xylonys,
    Xolonys,
    Llonys,

    Krz8i,
    Krz8c,
    Krz16i,
    Krz16c,

    Imm(String),
    Mem(String),
    Mem2(String, String),

    L,
    Nll,

    Comment(String),
}

pub fn parse(prog: String) -> Result<Vec<Instruction>, String> {
    let tokens = token::tokenize(prog);
    let mut result: Vec<Instruction> = Vec::new();
    let mut parser = syntax::Parser::new(tokens, &mut result);
    parser.parse()?;
    Ok(result)
}
