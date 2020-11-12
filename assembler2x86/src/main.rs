pub mod mnemonic;
mod parser;
mod gen;

fn main() {
    println!("Hello, world!");
    let hoge = parser::hoge();
    gen::huga(hoge);
}
