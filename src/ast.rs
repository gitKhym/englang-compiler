use crate::lexer::{Token, TokenType};

pub struct Expr {}
pub struct Stmt {}

pub struct Identifier {
    token: TokenType,
    value: String,
}

// Root
pub struct Program {
    statements: Vec<Stmt>,
}

// Expressions
pub enum ExprKind {
    Binary,
    Unary,
    FuncCall,
    Int,
    String,
    Bool,
}

pub struct BinaryExpr {
    left: Expr,
    token_type: TokenType,
    right: Expr,
}

// Statements
pub enum StmtKind {
    VarDecl,
}

// TYPE(VarType) ID EQ EXPR
pub struct VarDeclStmt {
    token: Token,
    identifier: Identifier,
    value: Expr,
}
