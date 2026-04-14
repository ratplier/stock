mod diagnostic;
mod emitters;
mod error;
mod label;
mod sink;

pub use crate::{
    diagnostic::{Diagnostic, DiagnosticCode, Severity},
    label::{Label, LabelStyle},
    sink::DiagnosticSink,
};
