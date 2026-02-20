use std::{env, fs, path::Path};

mod ast;
mod evaluate;
mod lexer;
mod parser;

use evaluate::Evaluator;
use lexer::{Lexer, TokenType};
use parser::Parser;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <file.eng>", args[0]);
        std::process::exit(1);
    }
    let file_path = &args[1];

    if Path::new(file_path).extension().and_then(|s| s.to_str()) != Some("eng") {
        eprintln!("Error: Input file must have a .eng extension");
        std::process::exit(1);
    }

    let source_code = fs::read_to_string(file_path).expect("Unable to read file");

    let mut lexer = Lexer::new(source_code);

    // loop {
    //     let token = lexer.get_token().token_type;
    //     if token == TokenType::Eof {
    //         break;
    //     }
    //     println!(":{:?}", token);
    // }

    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();

    let mut evaluator = Evaluator::new(program.clone());
    let evaluation_result = evaluator.eval_program();

    match evaluation_result {
        Some(obj) => println!("Evaluation Result: {:#?}", obj),
        None => println!("Evaluation did not produce a final value."),
    }
}
