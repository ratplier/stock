use crate::id::{ExprId, StmtId};
use stock_source::Symbol;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum UnaryOp {
    Negate,
    Not,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AstExpr {
    Integer(Symbol),
    Float(Symbol),

    Identifier(Symbol),

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
pub enum AstStmt {
    Let {
        name: Symbol,
        value: ExprId,
    },

    Block {
        statements: Vec<StmtId>,
        expr: Option<ExprId>,
    },
}

#[derive(Debug)]
pub enum AstItem {}
