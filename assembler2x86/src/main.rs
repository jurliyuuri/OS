#![warn(clippy::pedantic)]
#[macro_use]
extern crate clap;

use std::fs::{self, File};
use std::io::Write;

use clap::Arg;
use mnemonic::Instruction;

pub mod mnemonic;
mod parser;
mod gen;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let app = app_from_crate!()
        .arg(Arg::from_usage("-o [file] 'output file'"))
        .arg(Arg::from_usage("-x --architecture [arch] 'select whether it is 16bit or 32bit'")
            .possible_values(&["16bit", "32bit"])
            .default_value("32bit"))
        .arg(Arg::from_usage("<files>... 'input files'"));
    
    let matches = app.get_matches();

    let mode = matches.value_of("architecture").unwrap();

    let mut ops: Vec<Instruction> = Vec::new();
    if let Some(o) = matches.values_of("files") {
        for file in o {
            let content = fs::read_to_string(file)?;
            let mut vec = parser::parse(content)?;
            ops.append(&mut vec);
        }
    }

    let output = gen::generate(ops, mode);

    if let Some(o) = matches.value_of("file") {
        write!(File::create(o)?, "{}", output)?;
    } else {
        print!("{}", output);
    }

    Ok(())
}
