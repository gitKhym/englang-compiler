use std::{
    env,
    fs::File,
    io::{BufRead, BufReader},
};

mod ast;
mod lexer;
mod parser;

use lexer::Lexer;
use parser::Parser;
fn main() {
    let args: Vec<String> = env::args().collect();
    let file_path = &args[1];

    let file = File::open(file_path).expect("Unable to open file");
    let lines = BufReader::new(file).lines();

    // let mut tokens: Vec<Token> = Vec::new();
    for line in lines.map_while(Result::ok) {
        let lexer = Lexer::new(line);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        println!("{:#?}", program);
    }
}
