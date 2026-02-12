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
pub enum Expr {
    Int(String),
    BinaryExpr(BinaryExpr),
}

#[derive(Debug)]
pub struct Identifier {
    pub token_type: TokenType,
    pub value: String,
}

#[derive(Debug)]
pub struct BinaryExpr {
    pub left: Box<Expr>,
    pub operator: TokenType,
    pub right: Box<Expr>,
}

#[derive(Debug)]
pub struct VarDeclStatement {
    pub explicit_type: Token,
    pub identifier: Identifier,
    pub value: Expr,
}
