use crate::interner::Symbol;
use crate::span::Span;

#[repr(u8)]
#[rustfmt::skip]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenKind {
    // literals
    Integer, // 123, 1_000
    Float,   // 1.0, 1e10

    // identifiers
    Identifier, // foo_bar

    Let,
    If, Else, Loop, Break,

    // operators
    Plus, Minus, Star, Slash,
    Eq, EqEq, Bang, BangEq,
    Lt, Gt, Le, Ge,

    // delimiters
    LParen, RParen, LBrace, RBrace, LBracket, RBracket,
    Comma, Dot, Colon, Semicolon,

    EndOfFile, Unknown
}

impl TokenKind {
    pub fn is_eof(&self) -> bool {
        matches!(self, TokenKind::EndOfFile)
    }

    pub fn is_unknown(&self) -> bool {
        matches!(self, TokenKind::Unknown)
    }

    pub fn from_symbol(symbol: Symbol) -> Self {
        match symbol {
            Symbol::LET => TokenKind::Let,
            Symbol::IF => TokenKind::If,
            Symbol::ELSE => TokenKind::Else,
            Symbol::LOOP => TokenKind::Loop,
            Symbol::BREAK => TokenKind::Break,

            _ => unreachable!("symbol should be a keyword"),
        }
    }

    pub fn to_readable_str(&self) -> &str {
        match self {
            TokenKind::Integer => "integer",
            TokenKind::Float => "float",
            TokenKind::Identifier => "identifier",
            TokenKind::Let => "let",
            TokenKind::If => "if",
            TokenKind::Else => "else",
            TokenKind::Loop => "loop",
            TokenKind::Break => "break",

            TokenKind::Plus => "+",
            TokenKind::Minus => "-",
            TokenKind::Star => "*",
            TokenKind::Slash => "/",
            TokenKind::Eq => "=",
            TokenKind::EqEq => "==",
            TokenKind::Bang => "!",
            TokenKind::BangEq => "!=",
            TokenKind::Lt => "<",
            TokenKind::Gt => ">",
            TokenKind::Le => "<=",
            TokenKind::Ge => ">=",

            TokenKind::LParen => "(",
            TokenKind::RParen => ")",
            TokenKind::LBrace => "{",
            TokenKind::RBrace => "}",
            TokenKind::LBracket => "[",
            TokenKind::RBracket => "]",
            TokenKind::Comma => ",",
            TokenKind::Dot => ".",
            TokenKind::Colon => ":",
            TokenKind::Semicolon => ";",

            TokenKind::EndOfFile => "end of file",

            _ => todo!("cannot convert token kind '{:?}' to readable string", self),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
    pub symbol: Symbol,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self {
            kind,
            span,
            symbol: Symbol::EMPTY,
        }
    }

    pub fn with_symbol(kind: TokenKind, symbol: Symbol, span: Span) -> Self {
        Self { kind, span, symbol }
    }

    pub fn eof(span: Span) -> Self {
        Self::new(TokenKind::EndOfFile, span)
    }

    pub fn has_symbol(&self) -> bool {
        self.symbol != Symbol::EMPTY
    }
}
