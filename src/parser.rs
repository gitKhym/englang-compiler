use core::panic;
use std::mem::replace;

use crate::{
    ast::{
        ArrayLiteralExpr, AssignmentStatement, BinOpExpr, BlockStatement, ClassDefStatement, Expr,
        FuncCallExpr, FuncDefExpr, FuncDefStatement, IfStatement, IndexExpr, MemberAccessExpr,
        Program, ReturnStatement, Statement, TypedIdent, UnOpExpr, VarDeclStatement,
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

    pub fn _print_token(&self) {
        println!("{:#?}", self.curr_token)
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

    fn consume_var_type(&mut self) -> VarType {
        let mut var_type = match self.curr_token.token_type.clone() {
            TokenType::Type(var_type) => {
                self.consume_token();
                var_type
            }
            TokenType::Ident => {
                let name = self.curr_token.lexeme.clone();
                self.consume_token();
                VarType::Custom(name)
            }
            _ => panic!(),
        };

        while self.curr_token_is(TokenType::LSquare) {
            self.consume_token();
            self.expect(TokenType::RSquare).unwrap();
            var_type = VarType::Array(Box::new(var_type));
        }

        var_type
    }

    fn consume_ident(&mut self) -> String {
        if let TokenType::Ident = self.curr_token.token_type {
            let lexeme = self.curr_token.lexeme.clone();
            self.consume_token();
            lexeme
        } else {
            panic!("Expected identifier, got {:?}", self.curr_token.token_type);
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

    // STATEMENTS
    fn parse_statement(&mut self) -> Statement {
        match self.curr_token.token_type {
            TokenType::Type(_) => self.parse_var_decl_statement(),
            TokenType::Return => self.parse_return_statement(),
            TokenType::If => self.parse_if_statement(),
            TokenType::Class => self.parse_class_def_statement(),
            TokenType::Loop => self.parse_loop_statement(),
            TokenType::Fn => {
                // FN IDENT => FunctionDefStatement
                if self.next_token_is(TokenType::Ident) {
                    self.parse_function_def_statement()
                } else {
                    // FN => FunctionDefExpression
                    let expr = self.parse_expression(0);
                    Statement::Expression(expr)
                }
            }
            TokenType::Ident => {
                if self.next_token_is(TokenType::Ident) {
                    return self.parse_var_decl_statement();
                }

                Statement::Expression(self.parse_expression(0))
            }

            _ => Statement::Expression(self.parse_expression(0)),
        }
    }

    fn parse_expression(&mut self, min_bp: u8) -> Box<Expr> {
        // Parse the prefix
        let mut left: Box<Expr> = match &self.curr_token.token_type {
            TokenType::Fn => Box::new(self.parse_function_def_expression()),

            TokenType::Ident => {
                if self.next_token_is(TokenType::LParen) {
                    Box::new(self.parse_function_call_expression())
                } else {
                    Box::new(Expr::Ident(self.consume_ident()))
                }
            }
            TokenType::Digit => Box::new(self.parse_int_expression()),

            TokenType::Null => {
                self.expect(TokenType::Null).unwrap();
                Box::new(Expr::Null)
            }

            TokenType::True => {
                self.expect(TokenType::True).unwrap();
                Box::new(Expr::Bool(true))
            }

            TokenType::False => {
                self.expect(TokenType::False).unwrap();
                Box::new(Expr::Bool(false))
            }

            TokenType::LParen => {
                self.expect(TokenType::LParen).unwrap();
                let expr = self.parse_expression(0);
                self.expect(TokenType::RParen).unwrap();
                expr
            }

            TokenType::LSquare => {
                self.consume_token();
                let mut elements = Vec::new();

                if !self.curr_token_is(TokenType::RSquare) {
                    elements.push(self.parse_expression(0));
                    while self.curr_token_is(TokenType::Comma) {
                        self.consume_token();
                        elements.push(self.parse_expression(0));
                    }
                }

                self.expect(TokenType::RSquare).unwrap();
                Box::new(Expr::ArrayLiteral(ArrayLiteralExpr { elements }))
            }

            TokenType::Minus => {
                let op = self.curr_token.token_type.clone();
                self.consume_token();
                let (_, r_bp) = self.prefix_binding_power(&op);
                let right = self.parse_expression(r_bp);
                Box::new(Expr::UnOp(UnOpExpr { op, expr: right }))
            }

            token => panic!("Attempted to parse expression, got: {:?}", token),
        };

        // Parse postfix / infix operators
        loop {
            match self.curr_token.token_type {
                TokenType::Eof | TokenType::Semi | TokenType::RParen => break,

                // Binary / infix operators
                TokenType::Plus
                | TokenType::Minus
                | TokenType::Mult
                | TokenType::Div
                | TokenType::Eq
                | TokenType::Neq => {
                    let op = self.curr_token.token_type.clone();
                    let (l_bp, r_bp) = self.infix_binding_power(&op);
                    if l_bp < min_bp {
                        break;
                    }
                    self.consume_token();
                    let right = self.parse_expression(r_bp);
                    left = Box::new(Expr::BinOp(BinOpExpr { op, left, right }));
                }

                // Postfix operators
                TokenType::Dot => {
                    self.consume_token();
                    let member = match self.curr_token.token_type {
                        TokenType::Ident => self.consume_ident(),
                        _ => panic!("Expected identifier after '.'"),
                    };
                    left = Box::new(Expr::MemberAccess(MemberAccessExpr {
                        object: left,
                        field: member,
                    }));
                }

                TokenType::LSquare => {
                    self.consume_token(); // consume '['
                    let mut elements = Vec::new();

                    if !self.curr_token_is(TokenType::RSquare) {
                        elements.push(self.parse_expression(0));
                        while self.curr_token_is(TokenType::Comma) {
                            self.consume_token();
                            elements.push(self.parse_expression(0));
                        }
                    }

                    self.expect(TokenType::RSquare).unwrap();
                    left = Box::new(Expr::ArrayLiteral(ArrayLiteralExpr { elements }))
                }

                TokenType::Bang => {
                    self.consume_token();
                    left = Box::new(Expr::UnOp(UnOpExpr {
                        op: TokenType::Bang,
                        expr: left,
                    }));
                }

                _ => break,
            }
        }

        left
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
            TokenType::Dot => (9, 10),
            _ => panic!("bad op: {:?}", op),
        }
    }

    fn parse_int_expression(&mut self) -> Expr {
        let int: i64 = self.curr_token.lexeme.parse().expect("Unexpected string");

        self.consume_token();

        Expr::Int(int)
    }

    // (VARTYPE IDENT)[]
    fn parse_typed_ident_list(&mut self) -> TypedIdent {
        let explicit_type = self.consume_var_type();
        let identifier = self.consume_ident();

        TypedIdent {
            explicit_type,
            identifier,
        }
    }

    // FuncDefExpr: FN LPAREN VARDECL[] RPAREN RARROW BLOCKSTATEMENT
    fn parse_function_def_expression(&mut self) -> Expr {
        // FN
        self.expect(TokenType::Fn).unwrap();

        // LPAREN
        self.expect(TokenType::LParen).unwrap();

        let mut args: Vec<TypedIdent> = Vec::new();
        while !self.curr_token_is(TokenType::RParen) {
            let param = self.parse_typed_ident_list();
            args.push(param);

            match self.expect(TokenType::Comma) {
                Ok(_) => continue,
                Err(_) => break,
            };
        }
        // RPAREN
        self.expect(TokenType::RParen).unwrap();

        // RARROW
        match self.expect(TokenType::Rarrow) {
            Ok(_) => {}
            Err(e) => panic!("{e}"),
        };

        let return_type = self.consume_var_type();

        let function_block = self.parse_block_statement();

        Expr::FuncDef(FuncDefExpr {
            args,
            return_type,
            function_block,
        })
    }
    fn parse_function_call_expression(&mut self) -> Expr {
        let identifier = self.consume_ident(); // Consume IDENT

        self.expect(TokenType::LParen).unwrap(); // Consume LPAREN

        let mut args: Vec<Box<Expr>> = Vec::new();

        if self.curr_token_is(TokenType::RParen) {
            self.consume_token(); // consume RParen
            return Expr::FuncCall(FuncCallExpr { identifier, args });
        }

        args.push(self.parse_expression(0));

        while self.curr_token_is(TokenType::Comma) {
            self.expect(TokenType::Comma).unwrap(); // consume comma
            args.push(self.parse_expression(0));
        }

        self.expect(TokenType::RParen).unwrap(); // Consume RPAREN

        Expr::FuncCall(FuncCallExpr { identifier, args })
    }

    // BlockStatement: LCURL STATEMENT[] RCURL
    fn parse_block_statement(&mut self) -> BlockStatement {
        // LCURL
        self.expect(TokenType::LCurl).unwrap();

        let mut block_statement = BlockStatement {
            statements: Vec::new(),
        };

        while !self.curr_token_is(TokenType::RCurl) {
            let statement = self.parse_statement();

            if !matches!(statement, Statement::Illegal) {
                block_statement.statements.push(statement);
            } else {
                self.consume_token();
            }
        }

        // RCURL
        match self.expect(TokenType::RCurl) {
            Ok(_) => {}
            Err(e) => panic!("{e}"),
        };

        block_statement
    }

    // ReturnStatement: RETURN EXPRSTATEMENT
    fn parse_return_statement(&mut self) -> Statement {
        self.expect(TokenType::Return).unwrap();

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
        self.expect(TokenType::If).unwrap();

        // LPAREN
        self.expect(TokenType::LParen).unwrap();

        // EXPR
        let expression = self.parse_expression(0);

        // RPAREN
        self.expect(TokenType::RParen).unwrap();

        // BlockStatement
        let consequence = self.parse_block_statement();

        // ELSE?
        let alternative = if self.expect(TokenType::Else).is_ok() {
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
        let explicit_type = self.consume_var_type();

        // IDENT
        let identifier = self.consume_ident();

        // ASSIGN?
        let expression = if self.expect(TokenType::Assign).is_ok() {
            Some(self.parse_expression(0))
        } else {
            None
        };

        self.expect(TokenType::Semi).unwrap();

        Statement::VarDecl(VarDeclStatement {
            explicit_type,
            identifier,
            expression,
        })
    }

    // ClassDefStatement: CLASS IDENT LCURL TYPEDIDENT[] RCURL
    fn parse_class_def_statement(&mut self) -> Statement {
        self.expect(TokenType::Class).unwrap();

        let identifier = self.consume_ident();

        self.expect(TokenType::LCurl).unwrap();

        let mut fields: Vec<TypedIdent> = Vec::new();
        while !self.curr_token_is(TokenType::RCurl) {
            let field = self.parse_typed_ident_list();
            fields.push(field);

            match self.expect(TokenType::Comma) {
                Ok(_) => continue,
                Err(_) => break,
            };
        }

        self.expect(TokenType::RCurl).unwrap();

        Statement::ClassDef(ClassDefStatement { identifier, fields })
    }

    // FuncDefStatement: FN IDENT LPAREN TYPEDIDENT[] RPAREN RARROW VARTYPE BLOCKSTATEMENT
    fn parse_function_def_statement(&mut self) -> Statement {
        self.expect(TokenType::Fn).unwrap();

        let identifier = self.consume_ident();

        self.expect(TokenType::LParen).unwrap();

        let mut args: Vec<TypedIdent> = Vec::new();
        while !self.curr_token_is(TokenType::RParen) {
            let param = self.parse_typed_ident_list();
            args.push(param);

            match self.expect(TokenType::Comma) {
                Ok(_) => continue,
                Err(_) => break,
            };
        }

        self.expect(TokenType::RParen).unwrap();

        self.expect(TokenType::Rarrow).unwrap();

        let return_type = self.consume_var_type();

        let function_block = self.parse_block_statement();

        Statement::FuncDef(FuncDefStatement {
            identifier,
            args,
            return_type,
            function_block,
        })
    }

    fn parse_loop_statement(&mut self) -> Statement {
        self.expect(TokenType::Loop).unwrap();

        Statement::Illegal
    }
}
