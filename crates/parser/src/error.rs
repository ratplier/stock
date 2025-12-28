use stock_span::Span;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParserErrorKind {
    ExpectedRParen,

    UnexpectedToken,
    UnexpectedCharacter,
    InvalidNumber,

    ExpectedIdentifierAfterLet,
    ExpectedEqualAfterLetIdentifier,
    ExpectedExpressionAfterEqual,
    ExpectedEndOfStatement,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParserError {
    pub kind: ParserErrorKind,
    pub span: Span,
}

impl ParserError {
    pub fn new(kind: ParserErrorKind, span: Span) -> Self {
        ParserError { kind, span }
    }
}
