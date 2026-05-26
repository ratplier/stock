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
pub struct Block {
    pub stmts: Vec<StmtId>,
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

    Call {
        callee: ExprId,
        args: Vec<ExprId>,
    },

    Block(Block),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AstStmt {
    Let { name: Symbol, value: ExprId },

    Expr { expr: ExprId, has_semicolon: bool },

    Block(Block),
}

#[derive(Debug)]
pub enum AstItem {}
