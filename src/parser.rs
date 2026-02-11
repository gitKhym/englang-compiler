use std::mem::replace;

use crate::lexer::{Lexer, Token};

pub struct Parser {
    lexer: Lexer,
    curr_token: Token,
    next_token: Token,
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Self {
        let parser = Parser {
            curr_token: lexer.get_token(),
            next_token: lexer.get_token(),
            lexer,
        };

        parser
    }

    pub fn get_token(&mut self) {
        self.curr_token = replace(&mut self.next_token, self.lexer.get_token());
    }

    pub fn parse_program() {}
}

