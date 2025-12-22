use stock_source::Span;

#[derive(Debug, PartialEq, Eq)]
pub enum LexerError {
    UnexpectedCharacter,
    InvalidNumber,
}

#[derive(Debug, PartialEq, Eq)]
pub enum TokenType {
    Number,

    Plus, Minus, Star, Slash,
    LParen, RParen,

    EndOfFile,
    Error(LexerError),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenType,
    pub span: Span,
}

impl Token {
    pub fn new(kind: TokenType, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn error(err: LexerError, span: Span) -> Self {
        Self { kind: TokenType::Error(err), span }
    }

    pub fn is(&self, kind: TokenType) -> bool {
        self.kind == kind
    }
}