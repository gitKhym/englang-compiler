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

    pub fn curr_token_is(&mut self, token_type: TokenType) -> bool {
        self.curr_token.token_type == token_type
    }

    pub fn next_token_is(&mut self, token_type: TokenType) -> bool {
        self.next_token.token_type == token_type
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

    fn prefix_binding_power(&self, op: &TokenType) -> ((), u8) {
        match op {
            TokenType::Plus | TokenType::Minus => ((), 8),
            _ => panic!("bad prefix operator"),
        }
    }

    fn infix_binding_power(&mut self, op: &TokenType) -> (u8, u8) {
        match op {
            TokenType::Eq | TokenType::Neq => (0, 1),
            TokenType::Plus | TokenType::Minus => (1, 2),
            TokenType::Mult | TokenType::Div => (3, 4),
            _ => panic!("bad op: {:?}", op),
        }
    }

    fn parse_expression(&mut self, min_bp: u8) -> Box<Expr> {
        let mut left: Box<Expr> = match &self.curr_token.token_type {
            TokenType::Digit | TokenType::Ident => {
                let lexeme = self.curr_token.lexeme.clone();
                self.consume_token(); // consume digit
                Box::new(Expr::Atom(lexeme))
            }

            TokenType::True => {
                self.consume_token(); // consume True
                Box::new(Expr::Bool(true))
            }

            TokenType::False => {
                self.consume_token(); // consume False
                Box::new(Expr::Bool(false))
            }

            TokenType::LParen => {
                self.consume_token(); // consume (

                let expr = self.parse_expression(0);

                assert_eq!(self.curr_token.token_type, TokenType::RParen);
                self.consume_token(); // consume )

                expr
            }

            TokenType::Minus => {
                let op = self.curr_token.token_type;
                self.consume_token();

                let ((), r_bp) = self.prefix_binding_power(&op);
                let right = self.parse_expression(r_bp);

                Box::new(Expr::UnOp(op, right))
            }

            token => panic!("bad token: {:?}", token),
        };

        loop {
            let op = match &self.curr_token.token_type {
                TokenType::Eof | TokenType::Semi | TokenType::RParen => break,
                TokenType::Plus
                | TokenType::Minus
                | TokenType::Div
                | TokenType::Mult
                | TokenType::Eq
                | TokenType::Neq => self.curr_token.token_type,
                token => panic!("bad token: {:?}", token),
            };

            let (l_bp, r_bp) = self.infix_binding_power(&op);
            if l_bp < min_bp {
                break;
            }

            self.consume_token();
            let right = self.parse_expression(r_bp);

            left = Box::new(Expr::BinOp { op, left, right })
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

    // BlockStatement: LCURL STATEMENT[] RCURL
    fn parse_block_statement(&mut self) -> BlockStatement {
        // LCURL
        match self.expect(TokenType::LCurl) {
            Ok(_) => {}
            Err(e) => panic!("{e}"),
        };

        // STATEMENT[]
        let mut block_statement = BlockStatement {
            statements: Vec::new(),
        };

        while self.curr_token.token_type != TokenType::RCurl {
            let statement = self.parse_statement();

            if matches!(statement, Statement::Illegal) {
                panic!("Block not closed")
            }
            block_statement.statements.push(statement);
        }

        // RCURL
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
        match self.expect(TokenType::LParen) {
            Ok(_) => {}
            Err(e) => panic!("{e}"),
        };

        // EXPR
        let expression = self.parse_expression(0);

        // RPAREN
        match self.expect(TokenType::RParen) {
            Ok(_) => {}
            Err(e) => panic!("{e}"),
        };

        // BlockStatement
        let consequence = self.parse_block_statement();

        // ELSE?
        let alternative = if self.curr_token_is(TokenType::Else) {
            self.consume_token();

            if self.curr_token_is(TokenType::If) {
                // ELSE IF?
                let nested_if = self.parse_if_statement();

                Some(BlockStatement {
                    statements: vec![nested_if],
                })
            } else {
                // normal else
                Some(self.parse_block_statement())
            }
        } else {
            None
        };

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
            _ => panic!(
                "Expected: {:#?}, got: {:#?}",
                TokenType::Type(VarType::Any),
                self.curr_token
            ),
        };

        // IDENT
        let identifier = match self.expect(TokenType::Ident) {
            Ok(identifier) => identifier.clone(),
            Err(e) => panic!("{e}"),
        };

        // ASSIGN
        match self.expect(TokenType::Assign) {
            Ok(_) => {}
            Err(e) => panic!("{e}"),
        };

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
