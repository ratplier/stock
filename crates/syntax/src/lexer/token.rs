use stock_source::{Span, Symbol};

#[repr(u8)]
#[rustfmt::skip]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenKind {
    // literals
    Integer(Symbol), // 123, 1_000
    Float(Symbol),   // 1.0, 1e10

    // operators
    Plus, Minus, Star, Slash,
    PlusEq, MinusEq, StarEq, SlashEq,
    Eq, EqEq, BangEq, Bang,
    Lt, Gt, LtEq, GtEq,

    // delimiters
    LParen, RParen, LBrace, RBrace, LBracket, RBracket,
    Comma, Dot, Colon, Semicolon,

    EndOfFile, Error
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn eof(span: Span) -> Self {
        Self::new(TokenKind::EndOfFile, span)
    }
}
