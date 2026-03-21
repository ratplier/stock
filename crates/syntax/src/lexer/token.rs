use stock_ast::{BinaryOp, UnaryOp};
use stock_source::{Span, Symbol};

#[repr(u8)]
#[rustfmt::skip]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenKind {
    // literals
    Integer, // 123, 1_000
    Float,   // 1.0, 1e10

    // identifiers
    Identifier, // foo

    Let,
    If, Else, Loop, Break,


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

impl TokenKind {
    pub fn infix_binding_power(&self) -> Option<(u8, u8)> {
        match self {
            TokenKind::Plus | TokenKind::Minus => Some((1, 2)),
            TokenKind::Star | TokenKind::Slash => Some((3, 4)),
            _ => None,
        }
    }

    pub fn prefix_binding_power(&self) -> Option<u8> {
        match self {
            TokenKind::Minus | TokenKind::Bang => Some(5),
            _ => None,
        }
    }

    pub fn to_binary_op(&self) -> BinaryOp {
        match self {
            TokenKind::Plus => BinaryOp::Add,
            TokenKind::Minus => BinaryOp::Subtract,
            TokenKind::Star => BinaryOp::Multiply,
            TokenKind::Slash => BinaryOp::Divide,
            _ => unreachable!("token should be a binary operator"),
        }
    }

    pub fn to_unary_op(&self) -> UnaryOp {
        match self {
            TokenKind::Minus => UnaryOp::Negate,
            TokenKind::Bang => UnaryOp::Not,
            _ => unreachable!("token should be a unary operator"),
        }
    }

    pub fn is_binary_op(&self) -> bool {
        matches!(
            self,
            TokenKind::Plus | TokenKind::Minus | TokenKind::Star | TokenKind::Slash
        )
    }

    pub fn is_unary_op(&self) -> bool {
        matches!(self, TokenKind::Minus | TokenKind::Bang)
    }

    pub fn is_eof(&self) -> bool {
        matches!(self, TokenKind::EndOfFile)
    }

    pub fn keyword_from_str(keyword: &[u8]) -> Option<TokenKind> {
        match keyword {
            b"let" => Some(TokenKind::Let),
            b"if" => Some(TokenKind::If),
            b"else" => Some(TokenKind::Else),
            b"loop" => Some(TokenKind::Loop),
            b"break" => Some(TokenKind::Break),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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

    pub fn with_symbol(kind: TokenKind, symbol: Symbol, span: Span) -> Self {
        Self {
            kind,
            span,
            symbol: Some(symbol),
        }
    }

    pub fn eof(span: Span) -> Self {
        Self::new(TokenKind::EndOfFile, span)
    }

    pub fn has_symbol(&self) -> bool {
        self.symbol.is_some()
    }
}
