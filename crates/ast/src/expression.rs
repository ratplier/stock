use crate::operators::{BinaryOp, UnaryOp};
use stock_span::{Span, Symbol};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ExprId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LiteralKind {
    Integer,
    Float,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Literal {
        kind: LiteralKind,
        value: Symbol, 
    },

    Binary {
        op: BinaryOp,
        lhs: ExprId,
        rhs: ExprId,
    },

    Unary {
        op: UnaryOp,
        rhs: ExprId,
    },
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}
