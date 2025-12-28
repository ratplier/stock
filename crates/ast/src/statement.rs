use crate::expression::ExprId;
use stock_span::{Span, Symbol};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StmtId(pub u32);

#[derive(Debug, Clone)]
pub enum StmtKind {
    Let { name: Symbol, value: ExprId },
    Return { value: ExprId },
}

#[derive(Debug, Clone)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}
