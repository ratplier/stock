use crate::{Diagnostic, DiagnosticCode, DiagnosticSink, Label, error::LexError};
use stock_source::Span;

/// emit lexing errors
impl DiagnosticSink {
    pub fn unknown_byte(&mut self, byte: u8, span: Span) -> &mut Self {
        let code = DiagnosticCode::LexError(LexError::UnknownByte { byte });

        self.emit(
            Diagnostic::error(code)
                .with_label(Label::primary(span, "could not match any known token")),
        )
    }

    pub fn trailing_decimal(&mut self, span: Span) -> &mut Self {
        let code = DiagnosticCode::LexError(LexError::TrailingDecimal);

        self.emit(
            Diagnostic::error(code)
                .with_label(Label::primary(span, "trailing decimal/exponent (1e, 1.)")),
        )
    }
}
