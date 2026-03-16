use crate::ast::id::ExprId;
use stock_source::Symbol;

#[derive(Debug)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
}

#[derive(Debug)]
pub enum UnaryOp {
    Negate,
    Not,
}

#[derive(Debug)]
pub enum AstExpr {
    Integer(Symbol),
    Float(Symbol),

    Binary {
        op: BinaryOp,
        lhs: ExprId,
        rhs: ExprId,
    },

    Unary {
        op: UnaryOp,
        operand: ExprId,
    },
}

#[derive(Debug)]
pub enum AstStmt {}

#[derive(Debug)]
pub enum AstItem {}
