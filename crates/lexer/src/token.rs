use stock_span::{Span, Symbol};
use crate::error::LexerError;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenKind {
    Number,

    Plus, Minus, Star, Slash,
    LParen, RParen,

    EndOfFile,
    Error(LexerError),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
    pub symbol: Option<Symbol>,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span, symbol: None }
    }

    pub fn symbol(kind: TokenKind, span: Span, symbol: Symbol) -> Self {
        Self { kind, span, symbol: Some(symbol) }
    }

    pub fn error(err: LexerError, span: Span) -> Self {
        Token::new(TokenKind::Error(err), span)
    }

    pub fn is(&self, kind: TokenKind) -> bool {
        self.kind == kind
    }
}