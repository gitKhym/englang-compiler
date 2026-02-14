use crate::ast::{BlockStatement, Expr, Program, Statement};
use crate::lexer::VarType;
use std::fmt::{Display, Formatter};
use std::io::{Result, Write};
use uuid::Uuid;

pub fn generate_dot_graph(program: &Program, output_path: &str) -> Result<()> {
    let mut file = std::fs::File::create(output_path)?;
    let mut graph = Vec::new();
    let root_id = Uuid::new_v4().to_string();
    graph.push(format!("  \"{}\" [label=\"Program\"];", root_id));
    for stmt in &program.statements {
        let stmt_id = Uuid::new_v4().to_string();
        graph.push(format!("  \"{}\" -> \"{}\";", root_id, stmt_id));
        walk_statement(stmt, &stmt_id, &mut graph);
    }
    let output = format!("digraph ast {{\n{}\n}}", graph.join("\n"));
    file.write_all(output.as_bytes())
}

fn walk_statement(statement: &Statement, id: &str, graph: &mut Vec<String>) {
    match statement {
        Statement::Expression(expr) => {
            graph.push(format!("\"{}\" [label=\"ExpressionStatement\"];", id));
            let expr_id = Uuid::new_v4().to_string();
            graph.push(format!("\"{}\" -> \"{}\";", id, expr_id));
            walk_expression(expr, &expr_id, graph);
        }
        Statement::VarDecl(decl) => {
            graph.push(format!(
                "\"{}\" [label=\"VarDecl: {} {}\"];",
                id, decl.explicit_type, decl.identifier
            ));
            if let Some(expr) = &decl.expression {
                let expr_id = Uuid::new_v4().to_string();
                graph.push(format!("  \"{}\" -> \"{}\";", id, expr_id));
                walk_expression(expr, &expr_id, graph);
            }
        }
        Statement::Return(ret) => {
            graph.push(format!("\"{}\" [label=\"Return\"];", id));
            let expr_id = Uuid::new_v4().to_string();
            graph.push(format!("\"{}\" -> \"{}\";", id, expr_id));
            walk_expression(&ret.expression, &expr_id, graph);
        }
        Statement::FuncDef(func_def) => {
            graph.push(format!(
                "\"{}\" [label=\"FuncDef: {}\"];",
                id, func_def.identifier
            ));
            let block_id = Uuid::new_v4().to_string();
            graph.push(format!("  \"{}\" -> \"{}\";", id, block_id));
            walk_block_statement(&func_def.function_block, &block_id, graph);
        }
        Statement::If(if_stmt) => {
            graph.push(format!("\"{}\" [label=\"If\"];", id));
            let cond_id = Uuid::new_v4().to_string();
            graph.push(format!(
                "\"{}\" -> \"{}\" [label=\"condition\"];",
                id, cond_id
            ));
            walk_expression(&if_stmt.condition, &cond_id, graph);

            let consequence_id = Uuid::new_v4().to_string();
            graph.push(format!(
                "\"{}\" -> \"{}\" [label=\"consequence\"];",
                id, consequence_id
            ));
            walk_block_statement(&if_stmt.consequence, &consequence_id, graph);

            if let Some(alternative) = &if_stmt.alternative {
                let alternative_id = Uuid::new_v4().to_string();
                graph.push(format!(
                    "\"{}\" -> \"{}\" [label=\"alternative\"];",
                    id, alternative_id
                ));
                walk_block_statement(alternative, &alternative_id, graph);
            }
        }
        Statement::Illegal => {
            graph.push(format!("\"{}\" [label=\"Illegal\"];", id));
        }
    }
}

fn walk_expression(expression: &Expr, id: &str, graph: &mut Vec<String>) {
    match expression {
        Expr::Int(val) => {
            graph.push(format!("\"{}\" [label=\"Int: {}\"];", id, val));
        }
        Expr::Ident(name) => {
            graph.push(format!("\"{}\" [label=\"Ident: {}\"];", id, name));
        }
        Expr::Bool(val) => {
            graph.push(format!("\"{}\" [label=\"Bool: {}\"];", id, val));
        }
        Expr::BinOp(bin_op) => {
            graph.push(format!("\"{}\" [label=\"BinOp: {:?}\"];", id, bin_op.op));
            let left_id = Uuid::new_v4().to_string();
            let right_id = Uuid::new_v4().to_string();
            graph.push(format!("\"{}\" -> \"{}\" [label=\"left\"];", id, left_id));
            graph.push(format!("\"{}\" -> \"{}\" [label=\"right\"];", id, right_id));
            walk_expression(&bin_op.left, &left_id, graph);
            walk_expression(&bin_op.right, &right_id, graph);
        }
        Expr::UnOp(un_op) => {
            graph.push(format!("\"{}\" [label=\"UnOp: {:?}\"];", id, un_op.op));
            let expr_id = Uuid::new_v4().to_string();
            graph.push(format!("\"{}\" -> \"{}\";", id, expr_id));
            walk_expression(&un_op.expr, &expr_id, graph);
        }
        Expr::FuncCall(call) => {
            graph.push(format!(
                "\"{}\" [label=\"FuncCall: {}\"];",
                id, call.identifier
            ));
            for arg in &call.args {
                let arg_id = Uuid::new_v4().to_string();
                graph.push(format!("\"{}\" -> \"{}\";", id, arg_id));
                walk_expression(arg, &arg_id, graph);
            }
        }
        Expr::FuncDef(func_def) => {
            graph.push(format!("\"{}\" [label=\"FuncDef\"];", id));
            let block_id = Uuid::new_v4().to_string();
            graph.push(format!("\"{}\" -> \"{}\";", id, block_id));
            walk_block_statement(&func_def.function_block, &block_id, graph);
        }
        Expr::Null => {
            graph.push(format!("\"{}\" [label=\"Null\"];", id));
        }
    }
}

fn walk_block_statement(block: &BlockStatement, id: &str, graph: &mut Vec<String>) {
    graph.push(format!("\"{}\" [label=\"Block\"];", id));
    for stmt in &block.statements {
        let stmt_id = Uuid::new_v4().to_string();
        graph.push(format!("\"{}\" -> \"{}\";", id, stmt_id));
        walk_statement(stmt, &stmt_id, graph);
    }
}

impl Display for VarType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}
