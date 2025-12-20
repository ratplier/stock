use crate::span::Span;

#[derive(Debug, PartialEq)]
pub enum LexerError {
    UnexpectedSymbol,
    UnexpectedCharacter,
    InvalidNumber,
}

#[derive(Debug, PartialEq)]
pub enum TokenType {
    Number,

    PLUS, MINUS, STAR, SLASH,
    LPAREN, RPAREN,

    EndOfFile,
    Error(LexerError, Span),
}

impl TokenType {
    pub fn to_token<'src>(self) -> Token<'src> {
        Token::new(self)
    }

    pub fn to_literal<'src>(self, literal: &'src [u8]) -> Token<'src> {
        Token::literal(self, literal)
    }
}

#[derive(Debug, PartialEq)]
pub struct Token<'src> {
    pub kind: TokenType,
    pub literal: Option<&'src [u8]>,
}

impl<'src> Token<'src> {
    pub fn new(kind: TokenType) -> Self {
        Self { kind, literal: None }
    }

    pub fn error(err: LexerError, span: Span) -> Self {
        Self { kind: TokenType::Error(err, span), literal: None }
    }

    pub fn literal(kind: TokenType, literal: &'src [u8]) -> Self {
        Self { kind, literal: Some(literal) }
    }

    pub fn end_of_file() -> Self {
        Token::new(TokenType::EndOfFile)
    }
}