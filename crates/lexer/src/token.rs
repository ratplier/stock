use stock_source::Span;
use crate::error::LexerError;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenKind {
    Number,

    Plus, Minus, Star, Slash,
    LParen, RParen,

    EndOfFile,
    Error(LexerError),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn error(err: LexerError, span: Span) -> Self {
        Self { kind: TokenKind::Error(err), span }
    }

    pub fn is(&self, kind: TokenKind) -> bool {
        self.kind == kind
    }
}