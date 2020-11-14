use super::mnemonic::*;
mod token;
mod syntax;

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

    Imm(i32),
    Label(String),
    Reg(i32),
    Mem(i32),
    Meml(String),
    Memi(i32, i32),
    Memil(i32, String),
    Memr(i32, i32),

    L,
    Nll,

    Comment(String),
    Operand(String), // TODO 消せ
}

pub fn parse(prog: String) -> Result<Vec<Opcode>, String> {
    let tokens = token::tokenize(prog);
    let mut parser = syntax::Parser::new();
    Ok(parser.parse(tokens))
}
