#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

impl Span {
    pub fn new(start: u32, end: u32) -> Self {
        assert!(start <= end, "start must be less than or equal to end");
        Span { start, end }
    }

    pub fn location(position: u32) -> Self {
        Span::new(position, position)
    }

    pub fn char(position: u32) -> Self {
        Span::new(position, position + 1)
    }

    pub fn read<'src>(&self, source: &'src str) -> &'src str {
        &source[self.start as usize..self.end as usize]
    }

    pub fn len(&self) -> u32 {
        self.end - self.start
    }
}
