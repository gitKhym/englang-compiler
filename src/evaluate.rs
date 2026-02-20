use std::collections::HashMap;

use crate::ast::{self, VarDeclStatement};

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    Integer(i64),
    String(String),
    Boolean(bool),
    Null,
}

pub struct Evaluator {
    env: HashMap<String, Object>,
    statements: ast::Program,
}

impl Evaluator {
    pub fn new(statements: ast::Program) -> Self {
        Self {
            env: HashMap::new(),
            statements,
        }
    }

    pub fn eval_program(&mut self) -> Option<Object> {
        let mut last = None;

        for stmt in self.statements.statements.clone().into_iter() {
            last = Some(self.eval_statement(&stmt));
        }

        last
    }

    pub fn eval_expr(&mut self, node: &ast::Expr) -> Object {
        match node {
            ast::Expr::Int(n) => Object::Integer(*n),
            ast::Expr::StringLiteral(s) => Object::String(s.clone()),
            ast::Expr::Bool(b) => Object::Boolean(*b),
            ast::Expr::Null => Object::Null,

            ast::Expr::Ident(name) => match self.env.get(name) {
                Some(obj) => obj.clone(),
                None => Object::Null,
            },

            ast::Expr::BinOp(bin_op) => {
                let left_obj = self.eval_expr(&bin_op.left);
                let right_obj = self.eval_expr(&bin_op.right);

                match bin_op.op {
                    crate::lexer::TokenType::Plus => {
                        self.eval_infix_plus_expression(left_obj, right_obj)
                    }
                    crate::lexer::TokenType::Minus => {
                        self.eval_infix_minus_expression(left_obj, right_obj)
                    }
                    crate::lexer::TokenType::Mult => {
                        self.eval_infix_multiply_expression(left_obj, right_obj)
                    }
                    crate::lexer::TokenType::Div => {
                        self.eval_infix_divide_expression(left_obj, right_obj)
                    }
                    crate::lexer::TokenType::Eq => {
                        self.eval_infix_equal_expression(left_obj, right_obj)
                    }
                    crate::lexer::TokenType::Neq => {
                        self.eval_infix_not_equal_expression(left_obj, right_obj)
                    }
                    crate::lexer::TokenType::Lt => {
                        self.eval_infix_less_than_expression(left_obj, right_obj)
                    }
                    crate::lexer::TokenType::Gt => {
                        self.eval_infix_greater_than_expression(left_obj, right_obj)
                    }
                    _ => Object::Null,
                }
            }
            _ => Object::Null,
        }
    }

    pub fn eval_statement(&mut self, node: &ast::Statement) -> Object {
        match node {
            ast::Statement::Expression(expr) => self.eval_expr(expr),

            ast::Statement::VarDecl(VarDeclStatement {
                explicit_type,
                identifier,
                expression,
            }) => {
                let val = if let Some(expr_box) = expression {
                    self.eval_expr(&*expr_box)
                } else {
                    Object::Null
                };
                self.env.insert(identifier.clone(), val.clone());
                val
            }

            ast::Statement::Return(return_stmt) => self.eval_expr(&return_stmt.expression),

            ast::Statement::If(if_stmt) => {
                let condition_obj = self.eval_expr(&if_stmt.condition);
                if self.is_truthy(condition_obj) {
                    self.eval_block_statement(&if_stmt.consequence)
                } else if let Some(alternative) = &if_stmt.alternative {
                    self.eval_block_statement(alternative)
                } else {
                    Object::Null
                }
            }
            _ => Object::Null,
        }
    }

    fn eval_infix_plus_expression(&self, left: Object, right: Object) -> Object {
        match (left, right) {
            (Object::Integer(l), Object::Integer(r)) => Object::Integer(l + r),
            (Object::String(l), Object::String(r)) => Object::String(l + &r),
            _ => Object::Null,
        }
    }

    fn eval_infix_minus_expression(&self, left: Object, right: Object) -> Object {
        match (left, right) {
            (Object::Integer(l), Object::Integer(r)) => Object::Integer(l - r),
            _ => Object::Null,
        }
    }

    fn eval_infix_multiply_expression(&self, left: Object, right: Object) -> Object {
        match (left, right) {
            (Object::Integer(l), Object::Integer(r)) => Object::Integer(l * r),
            _ => Object::Null,
        }
    }

    fn eval_infix_divide_expression(&self, left: Object, right: Object) -> Object {
        match (left, right) {
            (Object::Integer(l), Object::Integer(r)) => {
                if r == 0 {
                    Object::Null
                } else {
                    Object::Integer(l / r)
                }
            }
            _ => Object::Null,
        }
    }

    fn eval_infix_equal_expression(&self, left: Object, right: Object) -> Object {
        Object::Boolean(left == right)
    }

    fn eval_infix_not_equal_expression(&self, left: Object, right: Object) -> Object {
        Object::Boolean(left != right)
    }

    fn eval_infix_less_than_expression(&self, left: Object, right: Object) -> Object {
        match (left, right) {
            (Object::Integer(l), Object::Integer(r)) => Object::Boolean(l < r),
            _ => Object::Null,
        }
    }

    fn eval_infix_greater_than_expression(&self, left: Object, right: Object) -> Object {
        match (left, right) {
            (Object::Integer(l), Object::Integer(r)) => Object::Boolean(l > r),
            _ => Object::Null,
        }
    }

    fn eval_block_statement(&mut self, block: &ast::BlockStatement) -> Object {
        let mut result = Object::Null;
        for statement in &block.statements {
            result = self.eval_statement(statement);
        }
        result
    }

    fn is_truthy(&self, obj: Object) -> bool {
        match obj {
            Object::Boolean(b) => b,
            Object::Null => false,
            Object::Integer(i) => i != 0,
            Object::String(s) => !s.is_empty(),
            _ => unreachable!(""),
        }
    }
}
