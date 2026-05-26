use std::borrow::Cow;

use crate::{Label, error::LexError, error::ParseError};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
    Note,
}

#[derive(Debug, Clone)]
pub enum DiagnosticCode {
    LexError(LexError),
    ParseError(ParseError),
}

#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub severity: Severity,
    pub code: DiagnosticCode,

    /// labels to attach to the diagnostic (^^^ error...)
    pub labels: Vec<Label>,

    /// a short description of the diagnostic
    /// only use when needed (hints, etc)
    note: Cow<'static, str>,

    /// a quick guide to resolve the diagnostic
    help: Cow<'static, str>,
}

impl Diagnostic {
    pub fn error(code: DiagnosticCode) -> Self {
        Self {
            severity: Severity::Error,
            code,
            labels: Vec::new(),

            note: "".into(),
            help: "".into(),
        }
    }

    pub fn with_label(mut self, label: Label) -> Self {
        self.labels.push(label);
        self
    }

    pub fn with_note(mut self, note: impl Into<Cow<'static, str>>) -> Self {
        self.note = note.into();
        assert!(!self.note.is_empty(), "expected non-empty note");
        self
    }

    pub fn with_help(mut self, help: impl Into<Cow<'static, str>>) -> Self {
        self.help = help.into();
        assert!(!self.help.is_empty(), "expected non-empty help");
        self
    }

    pub fn get_note(&self) -> Option<&str> {
        if self.note.is_empty() {
            None
        } else {
            Some(&self.note)
        }
    }

    pub fn get_help(&self) -> Option<&str> {
        if self.help.is_empty() {
            None
        } else {
            Some(&self.help)
        }
    }
}
