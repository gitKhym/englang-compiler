use crate::lexer::{Token, TokenType, VarType};

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug)]
pub enum Statement {
    Illegal,
    VarDecl(VarDeclStatement),
    Return(ReturnStatement),
    If(IfStatement),
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
pub struct BlockStatement {
    pub statements: Vec<Statement>,
}

#[derive(Debug)]
pub struct VarDeclStatement {
    pub explicit_type: VarType,
    pub identifier: Identifier,
    pub expression: Expr,
}

#[derive(Debug)]
pub struct ReturnStatement {
    pub expression: Expr,
}

#[derive(Debug)]
pub struct IfStatement {
    pub condition: Expr,
    pub consequence: BlockStatement,
    pub alternative: BlockStatement,
}
