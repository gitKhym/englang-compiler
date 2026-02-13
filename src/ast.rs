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
    Expression(Box<Expr>),
}

// Basically an AST Node (through my understanding atleast)
#[derive(Debug)]
pub enum Expr {
    Atom(String),
    Bool(bool),
    BinOp {
        op: TokenType,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    UnOp(TokenType, Box<Expr>),
}

#[derive(Debug)]
pub struct Identifier {
    pub token_type: TokenType,
    pub value: String,
}

#[derive(Debug)]
pub struct BlockStatement {
    pub statements: Vec<Statement>,
}

#[derive(Debug)]
pub struct VarDeclStatement {
    pub explicit_type: VarType,
    pub identifier: Identifier,
    pub expression: Box<Expr>,
}

#[derive(Debug)]
pub struct ReturnStatement {
    pub expression: Box<Expr>,
}

#[derive(Debug)]
pub struct IfStatement {
    pub condition: Box<Expr>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}
