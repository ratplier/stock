use crate::{Diagnostic, Severity};

#[derive(Debug, Default)]
pub struct DiagnosticSink {
    diagnostics: Vec<Diagnostic>,
    error_count: u32,
}

impl DiagnosticSink {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn emit(&mut self, diagnostic: Diagnostic) -> &mut Self {
        if diagnostic.severity == Severity::Error {
            self.error_count += 1;
        }

        self.diagnostics.push(diagnostic);
        self
    }

    pub fn has_errors(&self) -> bool {
        self.error_count > 0
    }

    pub fn error_count(&self) -> u32 {
        self.error_count
    }

    pub fn drain(self) -> Vec<Diagnostic> {
        self.diagnostics
    }
}
