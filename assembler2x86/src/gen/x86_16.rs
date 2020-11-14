use super::Opcode::{self, *};

pub fn generate(ops: Vec<Opcode>) -> String {
    let mut out = String::new();
    for op in ops {
        out.push_str(match op {
            Krz(_, _) => "Hoge",
            _ => "",
        });
    }
    out
}
