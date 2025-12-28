use crate::define_keywords;
use crate::error::LexerError;
use stock_span::{Span, Symbol};

define_keywords! {
    "let" => Let,
    "return" => Return,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenKind {
    Number,

    Plus,
    Minus,
    Star,
    Slash,
    LParen,
    RParen,

    EndOfFile,

    Identifier,
    Keyword(Keyword),
    Error(LexerError),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
    pub symbol: Option<Symbol>,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self {
            kind,
            span,
            symbol: None,
        }
    }

    pub fn symbol(kind: TokenKind, span: Span, symbol: Symbol) -> Self {
        Self {
            kind,
            span,
            symbol: Some(symbol),
        }
    }

    pub fn error(err: LexerError, span: Span) -> Self {
        Token::new(TokenKind::Error(err), span)
    }

    pub fn is(&self, kind: TokenKind) -> bool {
        self.kind == kind
    }
}
