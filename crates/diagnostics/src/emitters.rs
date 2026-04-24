use crate::{Diagnostic, DiagnosticCode, DiagnosticSink, Label};
use stock_source::Span;

use crate::error::LexError;
/// emit lexing errors
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
