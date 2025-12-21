
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

impl Span {
    pub fn new(start: u32, end: u32) -> Self {
        assert!(start < end, "start must be less than end");
        Span { start, end }
    }

    pub fn char(position: u32) -> Self {
        Span::new( position, position + 1)
    }

    pub fn len(&self) -> u32 {
        self.end - self.start
    }
}
