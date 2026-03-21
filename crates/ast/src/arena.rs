use crate::id::{ExprId, ItemId, StmtId};
use crate::nodes::{AstExpr, AstItem, AstStmt, BinaryOp, UnaryOp};
use stock_source::{Span, Symbol};

#[derive(Debug, Default)]
pub struct AstArena {
    exprs: Vec<AstExpr>,
    expr_spans: Vec<Span>,

    stmts: Vec<AstStmt>,
    stmt_spans: Vec<Span>,

    items: Vec<AstItem>,
    item_spans: Vec<Span>,
}

impl AstArena {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_expr(&mut self, expr: AstExpr, span: Span) -> ExprId {
        let id = self.exprs.len();

        self.exprs.push(expr);
        self.expr_spans.push(span);

        ExprId(id as u32)
    }

    pub fn add_stmt(&mut self, stmt: AstStmt, span: Span) -> StmtId {
        let id = self.stmts.len();

        self.stmts.push(stmt);
        self.stmt_spans.push(span);

        StmtId(id as u32)
    }

    pub fn add_item(&mut self, item: AstItem, span: Span) -> ItemId {
        let id = self.items.len();

        self.items.push(item);
        self.item_spans.push(span);

        ItemId(id as u32)
    }

    pub fn get_expr(&self, id: ExprId) -> &AstExpr {
        &self.exprs[id.0 as usize]
    }

    pub fn get_stmt(&self, id: StmtId) -> &AstStmt {
        &self.stmts[id.0 as usize]
    }

    pub fn get_item(&self, id: ItemId) -> &AstItem {
        &self.items[id.0 as usize]
    }

    pub fn get_expr_span(&self, id: ExprId) -> Span {
        self.expr_spans[id.0 as usize]
    }

    pub fn get_stmt_span(&self, id: StmtId) -> Span {
        self.stmt_spans[id.0 as usize]
    }

    pub fn get_item_span(&self, id: ItemId) -> Span {
        self.item_spans[id.0 as usize]
    }
}

impl AstArena {
    pub fn integer(&mut self, value: Symbol, span: Span) -> ExprId {
        self.add_expr(AstExpr::Integer(value), span)
    }

    pub fn float(&mut self, value: Symbol, span: Span) -> ExprId {
        self.add_expr(AstExpr::Float(value), span)
    }

    pub fn identifier(&mut self, name: Symbol, span: Span) -> ExprId {
        self.add_expr(AstExpr::Identifier(name), span)
    }

    pub fn binary(&mut self, op: BinaryOp, lhs: ExprId, rhs: ExprId, span: Span) -> ExprId {
        self.add_expr(AstExpr::Binary { op, lhs, rhs }, span)
    }

    pub fn unary(&mut self, op: UnaryOp, operand: ExprId, span: Span) -> ExprId {
        self.add_expr(AstExpr::Unary { op, operand }, span)
    }
}

impl AstArena {
    pub fn let_stmt(&mut self, name: Symbol, value: ExprId, span: Span) -> StmtId {
        self.add_stmt(AstStmt::Let { name, value }, span)
    }
}
