#[cfg(test)]
mod tests;

use stock_source::{Span, Symbol};

use crate::lexer::{Token, TokenKind};
use stock_ast::{AstArena, ExprId, StmtId};

pub struct Parser<'a> {
    tokens: &'a [Token],
    ast: AstArena,
    cursor: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Self {
            tokens,
            ast: AstArena::new(),
            cursor: 0,
        }
    }

    pub fn parse(&mut self) {
        todo!("parse")
    }
}

impl Parser<'_> {
    fn peek(&self) -> Token {
        // token buffer should never be empty
        self.tokens.get(self.cursor).cloned().unwrap()
    }

    fn advance(&mut self) -> Token {
        let token = self.peek();

        // last token must be EOF
        if !token.kind.is_eof() {
            self.cursor += 1;
        }

        token
    }

    fn expect_token(&mut self, kind: TokenKind) -> Option<Token> {
        let token = self.peek();
        if token.kind == kind {
            Some(self.advance())
        } else {
            // TODO: report error
            None
        }
    }

    fn expect(&mut self, kind: TokenKind) -> Option<Span> {
        self.expect_token(kind).map(|token| token.span)
    }

    fn expect_id(&mut self, kind: TokenKind) -> Option<Symbol> {
        self.expect_token(kind).and_then(|token| token.symbol)
    }

    fn span_from(&self, start: usize) -> Span {
        Span::new(start as u32, self.cursor as u32)
    }
}

impl Parser<'_> {
    fn parse_infix_expr(&mut self, min_bp: u8) -> ExprId {
        let mut lhs = self.parse_prefix_expr();

        loop {
            let op_token = self.peek();
            let kind = op_token.kind;

            if kind.is_eof() {
                break;
            }

            if let Some((l_bp, r_bp)) = kind.infix_binding_power() {
                if l_bp < min_bp {
                    break;
                }

                self.advance();

                let rhs = self.parse_infix_expr(r_bp);
                let op = kind.to_binary_op();

                let left_span = self.ast.get_expr_span(lhs);
                let right_span = self.ast.get_expr_span(rhs);
                let span = Span::merge(left_span, right_span);

                lhs = self.ast.binary(op, lhs, rhs, span);
            } else {
                break;
            }
        }

        lhs
    }

    fn parse_prefix_expr(&mut self) -> ExprId {
        let token = self.advance();

        if token.has_symbol() {
            let kind = token.kind;
            let symbol = token.symbol.unwrap();

            return match kind {
                TokenKind::Integer => self.ast.integer(symbol, token.span),
                TokenKind::Float => self.ast.float(symbol, token.span),
                TokenKind::Identifier => self.ast.identifier(symbol, token.span),

                _ => unreachable!("token should have a symbol"),
            };
        }

        match token.kind {
            kind if kind.is_unary_op() => {
                let r_bp = kind.prefix_binding_power().unwrap();
                let rhs = self.parse_infix_expr(r_bp);

                let op = kind.to_unary_op();
                let right_span = self.ast.get_expr_span(rhs);
                let span = Span::merge(token.span, right_span);

                self.ast.unary(op, rhs, span)
            }

            _ => todo!("error handling"),
        }
    }
}

impl Parser<'_> {
    fn parse_stmt(&mut self) -> StmtId {
        let token = self.peek();

        match token.kind {
            TokenKind::Let => self.parse_let_stmt(),
            _ => todo!("error handling"),
        }
    }

    fn parse_let_stmt(&mut self) -> StmtId {
        // TODO: proper error handling
        let start = self.cursor;
        self.expect(TokenKind::Let).unwrap();

        let name = self.expect_id(TokenKind::Identifier).unwrap();
        self.expect(TokenKind::Eq).unwrap();

        let value = self.parse_infix_expr(0);
        self.expect(TokenKind::Semicolon).unwrap();

        self.ast.let_stmt(name, value, self.span_from(start))
    }
}
