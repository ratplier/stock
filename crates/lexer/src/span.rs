#[derive(Debug, PartialEq)]
pub struct Span {
    start: usize,
    end: usize,
}

impl Span {
    pub const fn new(start: usize, end: usize) -> Self {
        assert!(start < end, "start must be less than end");
        Span { start, end }
    }

    pub fn char(position: usize) -> Self {
        Span::new(position, position + 1)
    }

    pub fn from_length(start: usize, length: usize) -> Self {
        Span::new(start, start + length)
    }

    pub fn len(&self) -> usize {
        self.end - self.start
    }

    pub fn is_empty(&self) -> bool {
        self.start >= self.end
    }

    pub fn read<'src>(&self, source: &'src [u8]) -> &'src [u8] {
        &source[self.start..self.end]
    }
}