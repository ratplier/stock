use crate::{Label, error::LexError};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
    Note,
}

#[derive(Debug, Clone)]
pub enum DiagnosticCode {
    LexError(LexError),
}

#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub severity: Severity,
    pub code: DiagnosticCode,

    /// labels to attach to the diagnostic (^^^ error...)
    pub labels: Vec<Label>,

    /// a short description of the diagnostic
    /// only use when needed (hints, etc)
    pub note: Option<String>,
}

impl Diagnostic {
    pub fn error(code: DiagnosticCode) -> Self {
        Self {
            severity: Severity::Error,
            code,
            labels: Vec::new(),
            note: None,
        }
    }

    pub fn with_label(mut self, label: Label) -> Self {
        self.labels.push(label);
        self
    }

    pub fn with_note(mut self, note: impl Into<String>) -> Self {
        self.note = Some(note.into());
        self
    }
}
