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
    Drosna,

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

    Reg(i32),
    Mem(i32),
    Memi(i32, i32),
    Memr(i32, i32),

    L,
    Nll,

    Imm(i32),
    Name(String),
    Comment(String),
}

pub fn parse(prog: String) -> Result<Vec<Opcode>, String> {
    token::tokenize(prog);
    Ok(Vec::new())
}
