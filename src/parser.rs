use std::mem::replace;

use crate::{
    ast::{
        BlockStatement, Expr, Identifier, IfStatement, Program, ReturnStatement, Statement,
        VarDeclStatement,
    },
    lexer::{Lexer, Token, TokenType, VarType},
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

    pub fn expect(&mut self, token_type: TokenType) -> Result<Token, String> {
        if self.curr_token.token_type == token_type {
            let old_token = self.curr_token.clone();
            self.consume_token();
            Ok(old_token)
        } else {
            Err(format!(
                "Expected {:?}, but found {:?}",
                token_type, self.curr_token.token_type
            ))
        }
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program {
            statements: Vec::new(),
        };

        while self.curr_token.token_type != TokenType::Eof {
            let statement = self.parse_statement();

            if !matches!(statement, Statement::Illegal) {
                program.statements.push(statement);
            } else {
                self.consume_token();
            }
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

    fn parse_expression(&mut self, min_bp: u8) -> Expr {
        let mut left = match &self.curr_token.token_type {
            TokenType::Digit | TokenType::Ident => {
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

    // STATEMENTS

    fn parse_statement(&mut self) -> Statement {
        match self.curr_token.token_type {
            TokenType::Type(_) => self.parse_var_decl_statement(),
            TokenType::Return => self.parse_return_statement(),
            TokenType::If => self.parse_if_statement(),
            _ => Statement::Illegal,
        }
    }

    fn parse_block_statement(&mut self) -> BlockStatement {
        if self.expect(TokenType::LCurl).is_err() {
            panic!(
                "Expected: {:#?}, got: {:#?}",
                TokenType::LCurl,
                self.curr_token.token_type
            )
        }

        let mut block_statement = BlockStatement {
            statements: Vec::new(),
        };

        while self.curr_token.token_type != TokenType::RCurl {
            let statement = self.parse_statement();

            if matches!(statement, Statement::Illegal) {
                panic!("Expected a statement, got {:#?}", statement)
            }
            block_statement.statements.push(statement);
        }

        self.consume_token();

        block_statement
    }

    // ReturnStatement: RETURN EXPR
    fn parse_return_statement(&mut self) -> Statement {
        self.consume_token(); // Consume return

        let statement = Statement::Return(ReturnStatement {
            expression: self.parse_expression(0),
        });

        match self.expect(TokenType::Semi) {
            Ok(_) => statement,
            Err(_) => Statement::Illegal,
        }
    }

    // IfStatement: IF LPAREN EXPR RPAREN BlockStatement ELSE BlockStatement
    fn parse_if_statement(&mut self) -> Statement {
        // IF
        self.consume_token();

        // LPAREN
        if self.expect(TokenType::LParen).is_err() {
            panic!(
                "Expected: {:#?}, got: {:#?}",
                TokenType::LParen,
                self.curr_token.token_type
            )
        }

        // EXPR
        let expression = self.parse_expression(0);

        // RPAREN
        if self.expect(TokenType::RParen).is_err() {
            panic!(
                "Expected: {:#?}, got: {:#?}",
                TokenType::RParen,
                self.curr_token.token_type
            )
        }

        // BlockStatement
        let consequence = self.parse_block_statement();

        if self.expect(TokenType::Else).is_err() {
            panic!(
                "Expected: {:#?}, got: {:#?}",
                TokenType::Else,
                self.curr_token.token_type
            )
        }

        let alternative = self.parse_block_statement();

        let statement = Statement::If(IfStatement {
            condition: expression,
            consequence,
            alternative,
        });

        statement
    }

    // VarDeclStatement: TYPE IDENT ASSIGN EXPR
    fn parse_var_decl_statement(&mut self) -> Statement {
        // TYPE
        let explicit_type = match self.curr_token.token_type {
            TokenType::Type(var_type) => {
                self.consume_token();
                var_type
            }
            _ => return Statement::Illegal,
        };

        // IDENT
        let identifier = match self.expect(TokenType::Ident) {
            Ok(identifier) => identifier.clone(),
            Err(_) => return Statement::Illegal,
        };

        // ASSIGN
        if self.expect(TokenType::Assign).is_err() {
            return Statement::Illegal;
        }

        // EXPR
        let expression = self.parse_expression(0);

        let var_decl_statement = Statement::VarDecl(VarDeclStatement {
            explicit_type,
            identifier: Identifier {
                token_type: identifier.token_type,
                value: identifier.lexeme,
            },
            expression,
        });

        match self.expect(TokenType::Semi) {
            Ok(_) => var_decl_statement,
            Err(_) => Statement::Illegal,
        }
    }
}
