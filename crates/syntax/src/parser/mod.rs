#[cfg(test)]
mod tests;

use stock_source::Span;

use crate::ast::{Ast, ExprId};
use crate::lexer::{Token, TokenKind};

pub struct Parser<'a> {
    tokens: &'a [Token],
    ast: Ast,
    cursor: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Self {
            tokens,
            ast: Ast::new(),
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

    fn expect(&mut self, kind: TokenKind) -> Option<Span> {
        let token = self.peek();
        if token.kind == kind {
            Some(self.advance().span)
        } else {
            // TODO: report error
            None
        }
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

        match token.kind {
            TokenKind::Integer(symbol) => self.ast.integer(symbol, token.span),
            TokenKind::Float(symbol) => self.ast.float(symbol, token.span),

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
