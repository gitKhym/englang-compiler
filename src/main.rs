use std::{env, fs, path::Path};

mod ast;
mod lexer;
mod parser;
mod ast_visualizer;

use lexer::Lexer;
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

    let lexer = Lexer::new(source_code);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();

    ast_visualizer::generate_dot_graph(&program, "ast.dot").expect("Failed to generate AST graph");
    println!("AST graph generated in ast.dot");
}
