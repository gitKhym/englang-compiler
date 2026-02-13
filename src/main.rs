use std::{env, fs};

mod ast;
mod lexer;
mod parser;

use lexer::Lexer;
use parser::Parser;
fn main() {
    let args: Vec<String> = env::args().collect();
    let file_path = &args[1];

    let source_code = fs::read_to_string(file_path).expect("Unable to read file");

    let lexer = Lexer::new(source_code);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();

    println!("{:#?}", program);
}
