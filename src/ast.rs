use crate::lexer::{TokenType, VarType};

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

// Expressions
#[derive(Debug)]
pub enum Expr {
    Null,
    Int(i64),
    Ident(String),
    Bool(bool),
    BinOp(BinOpExpr),
    UnOp(UnOpExpr),
    FuncDef(FuncDefExpr),
    FuncCall(FuncCallExpr),
}

// Binary operation expression
#[derive(Debug)]
pub struct BinOpExpr {
    pub op: TokenType,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}

// Unary operation expression
#[derive(Debug)]
pub struct UnOpExpr {
    pub op: TokenType,
    pub expr: Box<Expr>,
}

// Function definition expression
#[derive(Debug)]
pub struct FuncDefExpr {
    pub args: Vec<VarDeclStatement>,
    pub return_type: VarType,
    pub function_block: BlockStatement,
}

// Function call expression
#[derive(Debug)]
pub struct FuncCallExpr {
    pub identifier: String,
    pub args: Vec<Box<Expr>>,
}

// Block statement
#[derive(Debug)]
pub struct BlockStatement {
    pub statements: Vec<Statement>,
}

// Statements
#[derive(Debug)]
pub enum Statement {
    Expression(Box<Expr>),
    Illegal,
    VarDecl(VarDeclStatement),
    Return(ReturnStatement),
    If(IfStatement),
    FuncDef(FuncDefStatement),
}

// Variable declaration statement
#[derive(Debug)]
pub struct VarDeclStatement {
    pub explicit_type: VarType,
    pub identifier: String,
    pub expression: Option<Box<Expr>>,
}

// Return statement
#[derive(Debug)]
pub struct ReturnStatement {
    pub expression: Box<Expr>,
}

// If statement
#[derive(Debug)]
pub struct IfStatement {
    pub condition: Box<Expr>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

// Function definition statement
#[derive(Debug)]
pub struct FuncDefStatement {
    pub identifier: String,
    pub args: Vec<VarDeclStatement>,
    pub return_type: VarType,
    pub function_block: BlockStatement,
}
