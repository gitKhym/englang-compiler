use crate::lexer::{Token, TokenType};

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug)]
pub enum Statement {
    Illegal,
    VarDecl(VarDeclStatement),
}

#[derive(Debug)]
pub struct Identifier {
    pub token_type: TokenType,
    pub value: String,
}

// Basically an AST Node (through my understanding atleast)
#[derive(Debug)]
pub enum Expr {
    Atom(String),
    Op(TokenType, Vec<Expr>),
}

#[derive(Debug)]
pub struct VarDeclStatement {
    pub explicit_type: Token,
    pub identifier: Identifier,
    pub expression: Expr,
}
