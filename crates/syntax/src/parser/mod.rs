#[cfg(test)]
mod tests;

use stock_diagnostics::DiagnosticSink;
use stock_source::{Interner, Span, Token, TokenKind};

use crate::lexer::Lexer;
use stock_ast::{AstArena, BinaryOp, ExprId, StmtId, UnaryOp};

const RECOVERY_TOKENS: &[TokenKind] = &[TokenKind::Let, TokenKind::Semicolon];

#[derive(Debug)]
pub struct Parser<'a> {
    lexer: Lexer<'a>,
    buffer: [Token; 2],

    interner: &'a mut Interner,
    sink: &'a mut DiagnosticSink,
    ast: &'a mut AstArena,
}

impl<'a> Parser<'a> {
    pub fn new(
        mut lexer: Lexer<'a>,
        interner: &'a mut Interner,
        sink: &'a mut DiagnosticSink,
        ast: &'a mut AstArena,
    ) -> Self {
        let t0 = lexer.next_token(interner, sink);
        let t1 = lexer.next_token(interner, sink);

        Self {
            lexer,
            buffer: [t0, t1],
            interner,
            sink,
            ast,
        }
    }

    pub fn parse_program(&mut self) -> Vec<StmtId> {
        let mut stmts = Vec::new();

        while !self.peek_kind().is_eof() {
            if let Some(stmt) = self.parse_stmt() {
                stmts.push(stmt);
            } else {
                self.synchronize(RECOVERY_TOKENS);
            }
        }

        stmts
    }
}

impl Parser<'_> {
    #[inline]
    fn peek(&self) -> Token {
        self.buffer[0]
    }

    #[inline(always)]
    fn peek_kind(&self) -> TokenKind {
        self.peek().kind
    }

    #[inline(always)]
    fn peek_span(&self) -> Span {
        self.peek().span
    }

    fn advance(&mut self) -> Token {
        let token = self.buffer[0];
        self.buffer[0] = self.buffer[1];
        self.buffer[1] = self.lexer.next_token(self.interner, self.sink);
        token
    }

    fn at(&mut self, kind: TokenKind) -> bool {
        self.peek_kind() == kind
    }

    fn consume(&mut self, kind: TokenKind) -> bool {
        if self.at(kind) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn expect(&mut self, kind: TokenKind) -> Option<Token> {
        if self.at(kind) {
            Some(self.advance())
        } else {
            let token = self.peek();
            self.sink.expected_token(kind, token.kind, token.span);

            None
        }
    }

    fn synchronize(&mut self, recovery: &[TokenKind]) {
        loop {
            let kind = self.peek_kind();
            if kind.is_eof() || recovery.contains(&kind) {
                break;
            }
            self.advance();
        }
    }

    fn position(&self) -> u32 {
        self.peek().span.start
    }
}

mod token_kind {
    use super::{BinaryOp, TokenKind, UnaryOp};

    pub fn infix_binding_power(token: TokenKind) -> Option<(u8, u8)> {
        match token {
            TokenKind::Plus | TokenKind::Minus => Some((1, 2)),
            TokenKind::Star | TokenKind::Slash => Some((3, 4)),

            _ => None,
        }
    }

    pub fn to_binary_op(token: TokenKind) -> BinaryOp {
        match token {
            TokenKind::Plus => BinaryOp::Add,
            TokenKind::Minus => BinaryOp::Subtract,
            TokenKind::Star => BinaryOp::Multiply,
            TokenKind::Slash => BinaryOp::Divide,
            _ => unreachable!("token should be a binary operator"),
        }
    }

    pub fn to_unary_op(token: TokenKind) -> UnaryOp {
        match token {
            TokenKind::Minus => UnaryOp::Negate,
            TokenKind::Bang => UnaryOp::Not,
            _ => unreachable!("token should be a unary operator"),
        }
    }

    pub fn is_unary_op(token: TokenKind) -> bool {
        matches!(token, TokenKind::Minus | TokenKind::Bang)
    }
}

// parsing helpers
impl Parser<'_> {
    fn expect_semicolon(&mut self) -> Option<Token> {
        let token = self.expect(TokenKind::Semicolon);

        if token.is_none() {
            let position = self.position();
            let span = Span::from_position(position);
            self.sink.expected_semicolon(span);

            self.synchronize(RECOVERY_TOKENS);
        }

        token
    }

    fn parse_call(&mut self, callee: ExprId) -> Option<ExprId> {
        let start = self.ast.get_expr_span(callee);
        self.expect(TokenKind::LParen)?;

        let mut args = Vec::new();

        while !self.at(TokenKind::RParen) && !self.at(TokenKind::EndOfFile) {
            if let Some(arg) = self.parse_expr() {
                args.push(arg);
            } else {
                // TODO: emit expected expr error
                return None;
            }

            if self.consume(TokenKind::Comma) {
                break;
            }
        }

        let closing_span = self.expect(TokenKind::RParen)?.span;
        let span = Span::merge(start, closing_span);

        Some(self.ast.call(callee, args, span))
    }
}

// expression parsing
impl Parser<'_> {
    fn parse_expr(&mut self) -> Option<ExprId> {
        self.parse_infix_expr(0)
    }

    fn parse_infix_expr(&mut self, min_bp: u8) -> Option<ExprId> {
        let mut lhs = self.parse_prefix_expr()?;

        loop {
            let token = self.peek();
            let kind = token.kind;

            if kind.is_eof() {
                break;
            }

            if let Some((l_bp, r_bp)) = token_kind::infix_binding_power(kind) {
                if l_bp < min_bp {
                    break;
                }

                self.advance();

                let rhs = self.parse_infix_expr(r_bp)?;
                let op = token_kind::to_binary_op(kind);

                let left_span = self.ast.get_expr_span(lhs);
                let right_span = self.ast.get_expr_span(rhs);
                let span = Span::merge(left_span, right_span);

                lhs = self.ast.binary(op, lhs, rhs, span);
            } else {
                // next token is not an infix operator
                break;
            }
        }

        Some(lhs)
    }

    fn parse_prefix_expr(&mut self) -> Option<ExprId> {
        let token = self.peek();
        let kind = token.kind;

        if token_kind::is_unary_op(kind) {
            self.advance();

            let rhs = self.parse_prefix_expr()?;

            let op = token_kind::to_unary_op(kind);
            let right_span = self.ast.get_expr_span(rhs);

            let span = Span::merge(token.span, right_span);
            return Some(self.ast.unary(op, rhs, span));
        }

        self.parse_postfix_expr()
    }

    fn parse_postfix_expr(&mut self) -> Option<ExprId> {
        let mut expr = self.parse_primary()?;

        #[allow(clippy::while_let_loop)]
        loop {
            match self.peek_kind() {
                TokenKind::LParen => expr = self.parse_call(expr)?,

                _ => break, // not a postfix operator
            }
        }

        Some(expr)
    }

    fn parse_primary(&mut self) -> Option<ExprId> {
        let token = self.peek();

        if token.has_symbol() {
            self.advance();

            let kind = token.kind;
            let symbol = token.symbol;

            let ast_node = match kind {
                TokenKind::Integer => self.ast.integer(symbol, token.span),
                TokenKind::Float => self.ast.float(symbol, token.span),
                TokenKind::Identifier => self.ast.identifier(symbol, token.span),
                _ => unreachable!("token should have a symbol"),
            };

            return Some(ast_node);
        }

        if self.consume(TokenKind::LParen) {
            let expr = self.parse_expr()?;
            self.expect(TokenKind::RParen)?;

            return Some(expr);
        }

        self.sink.expected_expression(token.span);
        None
    }
}

// statement parsing
impl Parser<'_> {
    fn parse_stmt(&mut self) -> Option<StmtId> {
        match self.peek_kind() {
            TokenKind::Let => self.parse_let_stmt(),

            _ => self.parse_expr_stmt(),
        }
    }

    fn parse_let_stmt(&mut self) -> Option<StmtId> {
        let start = self.peek_span();
        self.expect(TokenKind::Let)?;

        let name_token = self.expect(TokenKind::Identifier)?;
        let name = name_token.symbol;

        // TODO: lookahead for annotations (a: Type)

        self.expect(TokenKind::Eq)?;

        let value = self.parse_expr()?;
        let semicolon = self.expect_semicolon()?;

        let span = Span::merge(start, semicolon.span);
        Some(self.ast.let_stmt(name, value, span))
    }

    fn parse_expr_stmt(&mut self) -> Option<StmtId> {
        let expr = self.parse_expr()?;
        let semicolon = self.expect_semicolon()?;

        let span = Span::merge(self.ast.get_expr_span(expr), semicolon.span);
        Some(self.ast.expr_stmt(expr, span))
    }
}
