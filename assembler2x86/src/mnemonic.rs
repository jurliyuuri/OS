pub enum Register {
    F0,
    F1,
    F2,
    F3,
    F4,
    F5,
    F6,
    XX,
}

pub enum Operand {
    Reg(Register),
    Mem(Register),
    Memi(Register, i32),
    Memr(Register, Register),
}

pub enum Opcode {
    Krz(Operand, Operand),
    Malkrz(Operand, Operand),
    Fen,
    Inj(Operand, Operand, Operand),

    Ata(Operand, Operand),
    Nta(Operand, Operand),
    Lat(Operand, Operand),
    Latsna(Operand, Operand),
    Kak(Operand, Operand),
    Ada(Operand, Operand),
    Ekc(Operand, Operand),
    Nac(Operand),
    Dal(Operand, Operand),
    Dto(Operand, Operand),
    Dro(Operand, Operand),
    Drosna(Operand, Operand),

    Fixtlo(Operand, Operand),
    Fixylo(Operand, Operand),
    Ficlo(Operand, Operand),
    Fixolo(Operand, Operand),
    Fillo(Operand, Operand),
    Finiv(Operand, Operand),
    Fixtlonys(Operand, Operand),
    Fixylonys(Operand, Operand),
    Fixolonys(Operand, Operand),
    Fillonys(Operand, Operand),

    Krz8i(Operand, Operand),
    Krz8c(Operand, Operand),
    Krz16i(Operand, Operand),
    Krz16c(Operand, Operand),

    Naen(String), // ny aftlaten - raw code
}

