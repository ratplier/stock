use stock_source::Span;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LabelStyle {
    Primary,
    Secondary,
}

#[derive(Debug, Clone)]
pub struct Label {
    pub style: LabelStyle,
    pub span: Span,
    pub message: &'static str,
}

impl Label {
    pub fn primary(span: Span, message: &'static str) -> Self {
        Self {
            style: LabelStyle::Primary,
            span,
            message,
        }
    }

    pub fn secondary(span: Span, message: &'static str) -> Self {
        Self {
            style: LabelStyle::Secondary,
            span,
            message,
        }
    }
}
