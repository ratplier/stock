use crate::{BinaryOp, LiteralKind, UnaryOp};
use stock_span::{Span, Symbol};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ExprId(pub u32);

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

#[derive(Debug, Default)]
pub struct Ast {
    statements: Vec<Stmt>,
    expressions: Vec<Expr>,
}

impl Ast {
    pub fn new() -> Self {
        Ast::default()
    }

    pub fn add_expr(&mut self, kind: ExprKind, span: Span) -> ExprId {
        self.expressions.push(Expr { kind, span });
        ExprId(self.expressions.len() as u32)
    }

    pub fn add_expr_stmt(&mut self, kind: StmtKind, span: Span) -> StmtId {
        self.statements.push(Stmt { kind, span });
        StmtId(self.statements.len() as u32)
    }

    pub fn get_expr(&self, id: ExprId) -> &Expr {
        &self.expressions[(id.0 - 1) as usize]
    }

    pub fn get_stmt(&self, id: StmtId) -> &Stmt {
        &self.statements[(id.0 - 1) as usize]
    }
}

impl Ast {
    pub fn binary(&mut self, op: BinaryOp, lhs: ExprId, rhs: ExprId, span: Span) -> ExprId {
        self.add_expr(ExprKind::Binary { op, lhs, rhs }, span)
    }

    pub fn unary(&mut self, op: UnaryOp, rhs: ExprId, span: Span) -> ExprId {
        self.add_expr(ExprKind::Unary { op, rhs }, span)
    }

    pub fn literal(&mut self, kind: LiteralKind, value: Symbol, span: Span) -> ExprId {
        self.add_expr(ExprKind::Literal { kind, value }, span)
    }

    pub fn let_stmt(&mut self, name: Symbol, value: ExprId, span: Span) -> StmtId {
        self.add_expr_stmt(StmtKind::Let { name, value }, span)
    }

    pub fn return_stmt(&mut self, value: ExprId, span: Span) -> StmtId {
        self.add_expr_stmt(StmtKind::Return { value }, span)
    }
}
