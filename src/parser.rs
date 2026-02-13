use std::mem::replace;

use crate::{
    ast::{Expr, Identifier, Program, Statement, VarDeclStatement},
    lexer::{Lexer, Token, TokenType},
};

pub struct Parser {
    pub lexer: Lexer,
    pub curr_token: Token,
    pub next_token: Token,
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

    pub fn consume_token(&mut self) {
        self.curr_token = replace(&mut self.next_token, self.lexer.get_token());
    }

    pub fn expect(&mut self, token_type: TokenType) -> Result<(), &'static str> {
        if self.next_token.token_type == token_type {
            self.consume_token();
            Ok(())
        } else {
            Err("Unexpected token type")
        }
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program {
            statements: Vec::new(),
        };

        while self.curr_token.token_type != TokenType::Eof {
            let statement = self.parse_statement();
            program.statements.push(statement);
            self.consume_token();
        }

        program
    }

    fn infix_binding_power(&mut self, op: &TokenType) -> (u8, u8) {
        match op {
            TokenType::Plus | TokenType::Minus => (1, 2),
            TokenType::Mult | TokenType::Div => (3, 4),
            _ => panic!("bad op: {:?}", op),
        }
    }

    fn parse_statement(&mut self) -> Statement {
        match self.curr_token.token_type {
            TokenType::Type(_) => self.parse_var_decl_statement(),
            _ => Statement::Illegal,
        }
    }

    fn parse_expression(&mut self, min_bp: u8) -> Expr {
        let mut left = match &self.curr_token.token_type {
            TokenType::Digit => {
                let lexeme = self.curr_token.lexeme.clone();
                self.consume_token(); // consume digit
                Expr::Atom(lexeme)
            }
            TokenType::LParen => {
                self.consume_token(); // consume (

                let expr = self.parse_expression(0);

                assert_eq!(self.curr_token.token_type, TokenType::RParen);
                self.consume_token(); // consume )

                expr
            }
            token => panic!("bad token: {:?}", token),
        };

        loop {
            let op = match &self.curr_token.token_type {
                TokenType::Eof | TokenType::Semi | TokenType::RParen => break,
                TokenType::Plus | TokenType::Minus | TokenType::Div | TokenType::Mult => {
                    self.curr_token.token_type
                }
                token => panic!("bad token: {:?}", token),
            };

            let (l_bp, r_bp) = self.infix_binding_power(&op);
            if l_bp < min_bp {
                break;
            }

            self.consume_token();
            let right = self.parse_expression(r_bp);

            left = Expr::Op(op, vec![left, right])
        }
        left
    }

    fn parse_var_decl_statement(&mut self) -> Statement {
        let explicit_type = self.curr_token.clone();

        if self.expect(TokenType::Ident).is_err() {
            return Statement::Illegal;
        }

        let identifier = Identifier {
            token_type: self.curr_token.token_type,
            value: self.curr_token.lexeme.clone(),
        };

        if self.expect(TokenType::Assign).is_err() {
            return Statement::Illegal;
        }

        self.consume_token(); // Advance to the expression

        let expression = self.parse_expression(1);

        let decl = VarDeclStatement {
            explicit_type,
            identifier,
            expression,
        };

        if self.next_token.token_type == TokenType::Semi {
            self.consume_token();
        }

        Statement::VarDecl(decl)
    }
}
