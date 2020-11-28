use super::Token;
use super::Instruction;
use super::Label;
use super::Opcode;
use super::Operand;
use super::Register;


pub struct Parser<'a> {
    tokens: std::iter::Peekable<std::vec::IntoIter<Token>>,
    result: &'a mut Vec<Instruction>,
    next_label: String,
}

impl Parser<'_> {
    pub fn new<'a>(tokens: Vec<Token>, result: &'a mut Vec<Instruction>) -> Parser {
        Parser{tokens:tokens.into_iter().peekable(), result, next_label: String::from("")}
    }

    fn read_operand(&mut self) -> Result<Operand, String> {
        let token = self.tokens.next();
        let str2opr = |s| match s {
            "f0" => Register::F0,
            "f1" => Register::F1,
            "f2" => Register::F2,
            "f3" => Register::F3,
            "f4" => Register::F4,
            "f5" => Register::F5,
            "f6" => Register::F6,
            "xx" => Register::XX,
            _ => if let Ok(i) = String::from(s).parse() {Register::Imm(i)} else {Register::Label(String::from(s))}
        };
        if let Some(token) = token {
            match token {
                Token::Imm(s) => Ok(Operand::Reg(str2opr(s.as_str()))),
                Token::Mem(s) => Ok(Operand::Mem(str2opr(s.as_str()))),
                Token::Mem2(s1, s2) => Ok(Operand::Mem2(str2opr(s1.as_str()), str2opr(s2.as_str()))),
                _ => Err("expected operand".to_string())
            }
        } else {Err("expected operand".to_string())}
    }

    fn get_label(&mut self) -> Result<Option<Label>, String> {
        let label = if self.next_label.as_str() != "" {
            let flag = if let Ok(i) = self.next_label.parse() {Some(Label::Num(i))} else {Some(Label::Name(self.next_label.clone()))};
            self.next_label = String::from("");
            flag
        } else if let Some(i) = self.tokens.peek() {
            if let Token::L = i {
                self.tokens.next();
                if let Some(token) = self.tokens.next() {
                    if let Token::Imm(s) = token {
                        if let Ok(i) = s.parse() {Some(Label::Num(i))} else {Some(Label::Name(s))}
                    } else {return Err("expected label".to_string())}
                } else {return Err("expected label".to_string())}
            } else {
                None
            }
        } else {None};
        Ok(label)
    }

    pub fn parse(&mut self) -> Result<(), String> {
        macro_rules! decl_parse0 {
            ($t: ident) => {
                {let label = self.get_label()?; self.result.push(Instruction{
                    op: Opcode::$t,
                    label
                })}
            };
        }
        macro_rules! decl_parse1 {
            ($t: ident) => {
                {let op1 = self.read_operand()?; let label = self.get_label()?; self.result.push(Instruction{
                    op: Opcode::$t(op1),
                    label
                })}
            };
        }
        macro_rules! decl_parse2 {
            ($t: ident) => {
                {let op1 = self.read_operand()?; let op2 = self.read_operand()?; let label = self.get_label()?; self.result.push(Instruction{
                    op: Opcode::$t(op1, op2),
                    label
                })}
            };
        }
        macro_rules! decl_parse3 {
            ($t: ident) => {
                {let op1 = self.read_operand()?; let op2 = self.read_operand()?; let op3 = self.read_operand()?; let label = self.get_label()?; self.result.push(Instruction{
                    op: Opcode::$t(op1, op2, op3),
                    label
                })}
            };
        }
        while let Some(token) = self.tokens.next() {
            match token {
                Token::Ci => (),

                Token::Krz => decl_parse2!(Krz),
                Token::Malkrz => decl_parse2!(Malkrz),
                Token::Fen => decl_parse0!(Fen),
                Token::Inj => decl_parse3!(Inj),

                Token::Ata => decl_parse2!(Ata),
                Token::Nta => decl_parse2!(Nta),
                Token::Lat => decl_parse2!(Lat),
                Token::Latsna => decl_parse2!(Latsna),
                Token::Kak => decl_parse2!(Kak),
                Token::Ada => decl_parse2!(Ada),
                Token::Ekc => decl_parse2!(Ekc),
                Token::Nac => decl_parse1!(Nac),
                Token::Dal => decl_parse2!(Dal),
                Token::Dto => decl_parse2!(Dto),
                Token::Dro => decl_parse2!(Dro),
                Token::Dtosna => decl_parse2!(Dtosna),

                Token::Fi => {
                    let op1 = self.read_operand()?; let op2 = self.read_operand()?; let label = self.get_label()?;
                    if let Some(token) = self.tokens.next() {
                        let op = match token {
                            Token::Xtlo => Opcode::Fixtlo(op1, op2),
                            Token::Xylo => Opcode::Fixylo(op1, op2),
                            Token::Clo => Opcode::Ficlo(op1, op2),
                            Token::Xolo => Opcode::Fixolo(op1, op2),
                            Token::Llo => Opcode::Fillo(op1, op2),
                            Token::Niv => Opcode::Finiv(op1, op2),
                            Token::Xtlonys => Opcode::Fixtlonys(op1, op2),
                            Token::Xylonys => Opcode::Fixylonys(op1, op2),
                            Token::Xolonys => Opcode::Fixolonys(op1, op2),
                            Token::Llonys => Opcode::Fillonys(op1, op2),
                            _ => return Err("expected operation for `fi`".to_string())
                        };
                        self.result.push(Instruction{op, label});
                    } else {return Err("expected operation for `fi`".to_string());}
                },

                Token::Krz8i => decl_parse2!(Krz8i),
                Token::Krz8c => decl_parse2!(Krz8c),
                Token::Krz16i => decl_parse2!(Krz16i),
                Token::Krz16c => decl_parse2!(Krz16c),

                Token::Nll => {
                    self.next_label = if let Some(token) = self.tokens.next() {
                        if let Token::Imm(s) = token {
                            s
                        } else {return Err("expected label".to_string())}
                    } else {return Err("expected label".to_string())} 
                },

                _ => return Err("unexpected token ".to_string() + format!("{:?}", token).as_str()),
            }
        }
        Ok(())
    }
}
