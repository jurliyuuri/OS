use super::Instruction;
use super::Opcode::{self, *};
use super::Label;
use super::Operand;
use super::Register;

fn gen_register(reg: Register) -> String {
    match reg {
        Register::F0 => "ax".to_string(),
        Register::F1 => "dx".to_string(),
        Register::F5 => "sp".to_string(),
        Register::XX => "ip".to_string(),
        Register::Imm(i) => format!("{}", i),
        Register::Label(s) => s,
        _ => "".to_string(),
    }
}

fn gen_operand(op: Operand) -> String {
    match op {
        Operand::Reg(r) => gen_register(r),
        Operand::Mem(r) => format!("[{}]", gen_register(r)),
        Operand::Mem2(r1, r2) => format!("[{} + {}]", gen_register(r1), gen_register(r2)),
    }
}

pub fn generate(ops: Vec<Instruction>) -> String {
    let mut out = String::new();
    let mut label_num = -1;
    let mut gen_label = || {label_num+=1;format!("ZZ{}", label_num)};
    for op in ops {
        if let Some(label) = op.label {
            out.push_str(match label {
                Label::Num(i) => "org ".to_string() + format!("{}", i).as_str() + "\n",
                Label::Name(s) => s + ":\n"
            }.as_str())
        }
        out.push_str(match op.op {
            Krz(op1, op2) => "\tmov ".to_string() + gen_operand(op1).as_str() + "," + gen_operand(op2).as_str() + "\n",
            Ata(op1, op2) => "\tadd ".to_string() + gen_operand(op1).as_str() + "," + gen_operand(op2).as_str() + "\n",
            Nta(op1, op2) => "\tsub ".to_string() + gen_operand(op1).as_str() + "," + gen_operand(op2).as_str() + "\n",
            Inj(op1, op2, op3) => {
                let o1 = gen_operand(op1);
                let o2 = gen_operand(op2);
                let o3 = gen_operand(op3);
                "\tmov  di,".to_string() + o2.as_str() + "\n" +
                "\tmov si," + o3.as_str() + "\n" +
                "\tmov " + o1.as_str() + "," + o2.as_str() + "\n" +
                "\tmov " + o2.as_str() + "," + o3.as_str() + "\n"
            },
            Malkrz(op1, op2) => {
                let label = gen_label();
                "\tje ".to_string() + label.as_str() + "\n" +
                "\tmov " + gen_operand(op1).as_str() + "," + gen_operand(op2).as_str() + "\n" +
                label.as_str() + ":\n"
            },
            Fen => "\tnop\n".to_string(),
            Fixylo(op1, op2) => {
                let label1 = gen_label(); let label2 = gen_label();
                "\tcmp ".to_string() + gen_operand(op1).as_str() + "," + gen_operand(op2).as_str() + "\n" +
                "\tjl " + label1.as_str() + "\n" +
                "\tcmp ax,ax\n" +
                "\tjmp " + label2.as_str() + "\n" +
                label1.as_str() + ":\n" +
                "\tcmp ip,0\n" +
                label2.as_str() + ":\n"
            },
            _ => "".to_string(),
        }.as_str());
    }
    out
}
