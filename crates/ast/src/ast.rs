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
    pub id: ExprId,
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
    pub id: StmtId,
    pub kind: StmtKind,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ItemId(pub u32);

#[derive(Debug, Clone)]
pub enum ItemKind {
    Function {
        name: Symbol,
        params: Vec<Symbol>,
        body: Vec<StmtId>,
    },
}

#[derive(Debug, Clone)]
pub struct Item {
    pub id: ItemId,
    pub kind: ItemKind,
    pub span: Span,
}

#[derive(Debug, Default)]
pub struct Ast {
    statements: Vec<Stmt>,
    expressions: Vec<Expr>,
    items: Vec<Item>,
}

impl Ast {
    pub fn new() -> Self {
        Ast::default()
    }

    pub fn add_expr(&mut self, kind: ExprKind, span: Span) -> ExprId {
        let id = ExprId(self.expressions.len() as u32 + 1);
        let expr = Expr { id, kind, span };

        self.expressions.push(expr);
        id
    }

    pub fn add_stmt(&mut self, kind: StmtKind, span: Span) -> StmtId {
        let id = StmtId(self.statements.len() as u32 + 1);
        let stmt = Stmt { id, kind, span };

        self.statements.push(stmt);
        id
    }

    pub fn add_item(&mut self, kind: ItemKind, span: Span) -> ItemId {
        let id = ItemId(self.items.len() as u32 + 1);
        let item = Item { id, kind, span };

        self.items.push(item);
        id
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
        self.add_stmt(StmtKind::Let { name, value }, span)
    }

    pub fn return_stmt(&mut self, value: ExprId, span: Span) -> StmtId {
        self.add_stmt(StmtKind::Return { value }, span)
    }
}
