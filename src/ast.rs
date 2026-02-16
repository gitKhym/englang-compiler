use crate::lexer::{TokenType, VarType};

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

// Expressions
#[derive(Debug)]
pub enum Expr {
    Null,
    StringLiteral(String),
    Int(i64),
    Ident(String),
    Bool(bool),
    BinOp(BinOpExpr),
    UnOp(UnOpExpr),
    FuncDef(FuncDefExpr),
    FuncCall(FuncCallExpr),
    MemberAccess(MemberAccessExpr),
    Index(IndexExpr),
    ArrayLiteral(ArrayLiteralExpr),
    RecordLiteral(RecordLiteralExpr),
}

#[derive(Debug)]
pub enum Statement {
    Expression(Box<Expr>),
    Illegal,
    VarDecl(VarDeclStatement),
    Return(ReturnStatement),
    If(IfStatement),
    FuncDef(FuncDefStatement),
    ClassDef(ClassDefStatement),
    ImplDef(ImplDefStatement),
    Assignment(AssignmentStatement),
}

// Member access expression
#[derive(Debug)]
pub struct MemberAccessExpr {
    pub object: Box<Expr>,
    pub field: String,
}

#[derive(Debug)]
pub struct RecordLiteralExpr {
    pub identifier: String,
    pub fields: Vec<(String, Box<Expr>)>,
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
    pub args: Vec<TypedIdent>,
    pub return_type: VarType,
    pub function_block: BlockStatement,
}

// Function call expression
#[derive(Debug)]
pub struct FuncCallExpr {
    pub callee: Box<Expr>,
    pub args: Vec<Box<Expr>>,
}

#[derive(Debug)]
pub struct AssignmentStatement {
    pub target: Box<Expr>,
    pub value: Box<Expr>,
}

#[derive(Debug)]
pub struct ArrayLiteralExpr {
    pub elements: Vec<Box<Expr>>,
}

#[derive(Debug)]
pub struct IndexExpr {
    pub object: Box<Expr>,
    pub index: Box<Expr>,
}

// Block statement
#[derive(Debug)]
pub struct BlockStatement {
    pub statements: Vec<Statement>,
}

// Statements

// Class declaration statement
#[derive(Debug)]
pub struct ClassDeclStatement {
    pub identifier: String,
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

// Typed Ident used for classes and function defs
#[derive(Debug)]
pub struct TypedIdent {
    pub explicit_type: VarType,
    pub identifier: String,
}

// Function definition statement
#[derive(Debug)]
pub struct FuncDefStatement {
    pub identifier: String,
    pub args: Vec<TypedIdent>,
    pub return_type: VarType,
    pub function_block: BlockStatement,
}

// Class definition statement
#[derive(Debug)]
pub struct ClassDefStatement {
    pub identifier: String,
    pub fields: Vec<TypedIdent>,
}

#[derive(Debug)]
pub struct ImplDefStatement {
    pub identifier: String,
    pub implementations: Vec<FuncDefStatement>,
}
