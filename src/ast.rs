use crate::lexer::{TokenType, VarType};

// AstNode itself doesn't need Clone if it's just an enum over Clone types, but if it contained non-Clone fields, it would.
// For now, assume it's fine without a top-level derive.
pub enum AstNode {
    Expr(Expr),
    Statement(Statement),
}

#[derive(Debug, Clone)]
pub struct Program {
    pub statements: Vec<Statement>,
}

// Expressions
#[derive(Debug, Clone)]
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

#[derive(Debug,Clone)]
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
#[derive(Debug, Clone)]
pub struct MemberAccessExpr {
    pub object: Box<Expr>,
    pub field: String,
}

#[derive(Debug, Clone)]
pub struct RecordLiteralExpr {
    pub identifier: String,
    pub fields: Vec<(String, Box<Expr>)>,
}

// Binary operation expression
#[derive(Debug, Clone)]
pub struct BinOpExpr {
    pub op: TokenType,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}

// Unary operation expression
#[derive(Debug, Clone)]
pub struct UnOpExpr {
    pub op: TokenType,
    pub expr: Box<Expr>,
}

// Function definition expression
#[derive(Debug, Clone)]
pub struct FuncDefExpr {
    pub args: Vec<TypedIdent>,
    pub return_type: VarType,
    pub function_block: BlockStatement,
}

// Function call expression
#[derive(Debug, Clone)]
pub struct FuncCallExpr {
    pub callee: Box<Expr>,
    pub args: Vec<Box<Expr>>,
}

#[derive(Debug, Clone)]
pub struct AssignmentStatement {
    pub target: Box<Expr>,
    pub value: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct ArrayLiteralExpr {
    pub elements: Vec<Box<Expr>>,
}

#[derive(Debug, Clone)]
pub struct IndexExpr {
    pub object: Box<Expr>,
    pub index: Box<Expr>,
}

// Block statement
#[derive(Debug, Clone)]
pub struct BlockStatement {
    pub statements: Vec<Statement>,
}

// Statements

// Class declaration statement
#[derive(Debug, Clone)]
pub struct ClassDeclStatement {
    pub identifier: String,
}

// Variable declaration statement
#[derive(Debug, Clone)]
pub struct VarDeclStatement {
    pub explicit_type: VarType,
    pub identifier: String,
    pub expression: Option<Box<Expr>>,
}

// Return statement
#[derive(Debug, Clone)]
pub struct ReturnStatement {
    pub expression: Box<Expr>,
}

// If statement
#[derive(Debug, Clone)]
pub struct IfStatement {
    pub condition: Box<Expr>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

// Typed Ident used for classes and function defs
#[derive(Debug, Clone)]
pub struct TypedIdent {
    pub explicit_type: VarType,
    pub identifier: String,
}

// Function definition statement
#[derive(Debug, Clone)]
pub struct FuncDefStatement {
    pub identifier: String,
    pub args: Vec<TypedIdent>,
    pub return_type: VarType,
    pub function_block: BlockStatement,
}

// Class definition statement
#[derive(Debug, Clone)]
pub struct ClassDefStatement {
    pub identifier: String,
    pub fields: Vec<TypedIdent>,
}

#[derive(Debug, Clone)]
pub struct ImplDefStatement {
    pub identifier: String,
    pub implementations: Vec<FuncDefStatement>,
}