use std::{
    env,
    fs::File,
    io::{BufRead, BufReader},
};

mod ast;
mod lexer;
mod parser;

use lexer::{Lexer, TokenType};
fn main() {
    let args: Vec<String> = env::args().collect();
    let file_path = &args[1];

    let file = File::open(file_path).expect("Unable to open file");
    let lines = BufReader::new(file).lines();

    for line in lines.map_while(Result::ok) {
        let mut lexer = Lexer::new(line);

        loop {
            let token = lexer.get_token();
            if token.token_type == TokenType::Eof {
                break;
            }
            println!("{:?}", token);
        }
    }
}
