use crate::{Diagnostic, DiagnosticCode, DiagnosticSink, Label};
use stock_source::Span;

// emit lexing errors
use crate::error::LexError;

impl DiagnosticSink {
    fn lex_error(&self, error: LexError) -> Diagnostic {
        Diagnostic::error(DiagnosticCode::LexError(error))
    }

    pub fn unknown_byte(&mut self, byte: u8, span: Span) -> &mut Self {
        let error = self
            .lex_error(LexError::UnknownByte { byte })
            .with_label(Label::primary(span, "could not match any known token"));

        self.emit(error)
    }

    pub fn trailing_decimal(&mut self, span: Span) -> &mut Self {
        let error = self
            .lex_error(LexError::TrailingDecimal)
            .with_label(Label::primary(span, "trailing decimal/exponent (1e, 1.)"));

        self.emit(error)
    }

    pub fn invalid_suffix(&mut self, span: Span) -> &mut Self {
        let error = self
            .lex_error(LexError::InvalidSuffix)
            .with_label(Label::primary(span, "invalid suffix"));

        self.emit(error)
    }
}

// emit parsing errors
use crate::error::ParseError;
use stock_source::TokenKind;

impl DiagnosticSink {
    fn parse_error(&self, error: ParseError) -> Diagnostic {
        Diagnostic::error(DiagnosticCode::ParseError(error))
    }

    pub fn expected_token(&mut self, expected: TokenKind, got: TokenKind, span: Span) -> &mut Self {
        let note = format!(
            "expected {} got {}",
            expected.to_readable_str(),
            got.to_readable_str()
        );

        let error = self
            .parse_error(ParseError::ExpectedToken { expected, got })
            .with_label(Label::primary(span, "unexpected token"))
            .with_note(note);

        self.emit(error)
    }

    pub fn unexpected_token(&mut self, token: TokenKind, span: Span) -> &mut Self {
        let note = format!("expected {}", token.to_readable_str());

        let error = self
            .parse_error(ParseError::UnexpectedToken { token })
            .with_label(Label::primary(span, "unexpected token"))
            .with_note(note);

        self.emit(error)
    }
}

// expression parsing errors
impl DiagnosticSink {
    pub fn unexpected_eof(&mut self, span: Span) -> &mut Self {
        let error = self
            .parse_error(ParseError::UnexpectedToken {
                token: TokenKind::EndOfFile,
            })
            .with_label(Label::primary(span, "unexpected end of file"));

        self.emit(error)
    }

    pub fn expected_binary_op(&mut self, span: Span) -> &mut Self {
        let error = self
            .parse_error(ParseError::ExpectedBinaryOp)
            .with_label(Label::primary(span, "expected binary operator"));

        self.emit(error)
    }

    pub fn expected_expression(&mut self, span: Span) -> &mut Self {
        let error = self
            .parse_error(ParseError::ExpectedExpression)
            .with_label(Label::primary(span, "expected expression"));

        self.emit(error)
    }
}

// statement parsing errors
impl DiagnosticSink {
    pub fn expected_statement(&mut self, span: Span) -> &mut Self {
        let error = self
            .parse_error(ParseError::ExpectedStatement)
            .with_label(Label::primary(span, "expected statement"));

        self.emit(error)
    }

    pub fn expected_semicolon(&mut self, span: Span) -> &mut Self {
        let error = self
            .parse_error(ParseError::ExpectedSemicolon)
            .with_label(Label::primary(span, "expected semicolon"));

        self.emit(error)
    }
}
