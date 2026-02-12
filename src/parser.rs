use std::mem::replace;

use crate::{
    ast::{BinaryExpr, Expr, Identifier, Program, Statement, VarDeclStatement},
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

    pub fn get_token(&mut self) {
        self.curr_token = replace(&mut self.next_token, self.lexer.get_token());
    }

    pub fn expect(&mut self, token_type: TokenType) -> Result<(), &'static str> {
        if self.next_token.token_type == token_type {
            self.get_token();
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
            self.get_token();
        }

        program
    }

    fn parse_statement(&mut self) -> Statement {
        match self.curr_token.token_type {
            TokenType::Type(_) => self.parse_var_decl_statement(),
            _ => Statement::Illegal,
        }
    }

    fn parse_expression(&mut self) -> Option<Expr> {
        let mut left = match self.curr_token.token_type {
            TokenType::Digit => {
                let value = self.curr_token.lexeme.clone();
                self.get_token();
                Expr::Int(value)
            }
            _ => return None,
        };

        while self.curr_token.token_type == TokenType::Plus {
            let operator = self.curr_token.token_type.clone();
            self.get_token();

            let right = match self.curr_token.token_type {
                TokenType::Digit => {
                    let value = self.curr_token.lexeme.clone();
                    self.get_token();
                    Expr::Int(value)
                }
                _ => return None,
            };

            left = Expr::BinaryExpr(BinaryExpr {
                left: Box::new(left),
                operator,
                right: Box::new(right),
            });
        }

        Some(left)
    }

    fn parse_var_decl_statement(&mut self) -> Statement {
        let explicit_type = self.curr_token.clone();

        if self.expect(TokenType::Ident).is_err() {
            return Statement::Illegal;
        }

        let identifier = Identifier {
            token_type: self.curr_token.token_type.clone(),
            value: self.curr_token.lexeme.clone(),
        };

        if self.expect(TokenType::Assign).is_err() {
            return Statement::Illegal;
        }

        self.get_token(); // Advance to the expression

        let value = match self.parse_expression() {
            Some(expr) => expr,
            None => return Statement::Illegal,
        };

        let decl = VarDeclStatement {
            explicit_type,
            identifier,
            value,
        };

        if self.next_token.token_type == TokenType::Semi {
            self.get_token();
        }

        Statement::VarDecl(decl)
    }
}
