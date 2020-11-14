use super::Token;

pub fn tokenize(prog: String) -> Vec<Token> {
    let mut tokens = Vec::new();
    for line in prog.lines() {
        let mut line = line.split(";");
        if let Some(content) = line.next() {
            let content = content.split_whitespace();
            for tokstr in content {
                let token = match tokstr {
                    "'i'c" => Token::Ic,
                    "'c'i" => Token::Ci,

                    "krz" => Token::Krz,
                    "kRz" => Token::Krz,
                    "malkrz" => Token::Malkrz,
                    "malkRz" => Token::Malkrz,
                    "fen" => Token::Fen,
                    "Inj" => Token::Inj,

                    "ata" => Token::Ata,
                    "nta" => Token::Nta,
                    "lat" => Token::Lat,
                    "latsna" => Token::Latsna,
                    "kak" => Token::Kak,
                    "ada" => Token::Ada,
                    "ekc" => Token::Ekc,
                    "nac" => Token::Nac,
                    "dal" => Token::Dal,
                    "dto" => Token::Dto,
                    "dro" => Token::Dro,
                    "dRo" => Token::Dro,
                    "dtosna" => Token::Dtosna,

                    "fi" => Token::Fi,
                    "xtlo" => Token::Xtlo,
                    "xylo" => Token::Xylo,
                    "clo" => Token::Clo,
                    "xolo" => Token::Xolo,
                    "llo" => Token::Llo,
                    "niv" => Token::Niv,
                    "xtlonys" => Token::Xtlonys,
                    "xylonys" => Token::Xylonys,
                    "xolonys" => Token::Xolonys,
                    "llonys" => Token::Llonys,
                    
                    "krz8i" => Token::Krz8i,
                    "krz8c" => Token::Krz8c,
                    "krz16i" => Token::Krz16i,
                    "krz16c" => Token::Krz16c,

                    "l'" => Token::L,
                    "nll" => Token::Nll,

                    _ => Token::Operand(String::from(tokstr)),
                };
                tokens.push(token);
            }

            let comment: Vec<String> = line.map(|s| String::from(s)).collect();
            let comment = comment.join(";");
            tokens.push(Token::Comment(comment));
        }
    }
    tokens
}
