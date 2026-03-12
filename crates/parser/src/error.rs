use stock_lexer::LexerError;
use stock_span::Span;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseErrorKind {
    LexerError(LexerError),

    // structural / punctuation
    ExpectedLParen,
    ExpectedRParen,
    ExpectedLBrace,
    ExpectedRBrace,
    ExpectedSemicolon,
    MissingCommaBetweenParameters,

    // stream issues
    UnexpectedEOF,
    UnexpectedToken,

    // grammar expectations
    ExpectedStatement,
    ExpectedExpression,
    ExpectedItem,

    // identifier / name related
    ExpectedIdentifierAfterLet,
    ExpectedEqualAfterLetIdentifier,
    ExpectedExpressionAfterEqual,

    ExpectedIdentifierAfterFn,
    ExpectedLParenBeforeFnParameters,
    ExpectedParameterIdentifier,
    ExpectedRParenAfterFnParameters,
    UnterminatedBlock,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseError {
    pub kind: ParseErrorKind,
    pub span: Span,
}

impl ParseError {
    pub fn new(kind: ParseErrorKind, span: Span) -> Self {
        ParseError { kind, span }
    }
}
