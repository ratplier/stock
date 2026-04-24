use crate::span::Span;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SourceId(pub u32);

#[derive(Debug)]
pub struct SourceMap<'a> {
    files: Vec<&'a [u8]>,
    file_starts: Vec<u32>,

    offset: u32,
}

impl<'a> SourceMap<'a> {
    pub fn new() -> Self {
        Self {
            files: Vec::new(),
            file_starts: Vec::new(),

            offset: 0,
        }
    }

    pub fn add_source(&mut self, source: &'a [u8]) -> SourceId {
        let source_id = self.files.len() as u32;
        let source_len = source.len() as u32;
        let offset = self.offset;

        self.files.push(source);
        self.file_starts.push(offset);
        self.offset += source_len + 1;

        SourceId(source_id)
    }

    pub fn get_source(&self, source_id: SourceId) -> &[u8] {
        self.files[source_id.0 as usize]
    }

    fn upper_bound(&self, position: u32) -> usize {
        self.file_starts.partition_point(|&start| start <= position)
    }

    pub fn lookup(&self, span: Span) -> (SourceId, &[u8]) {
        let file_index = {
            let span_start_file = self.upper_bound(span.start) - 1;
            let span_end_file = self.upper_bound(span.end) - 1;

            assert_eq!(span_start_file, span_end_file, "span crosses file boundary");

            span_start_file
        };

        let file_start = self.file_starts[file_index];

        let local_start = span.start - file_start;
        let local_end = span.end - file_start;

        let source_id = SourceId(file_index as u32);
        let text_slice = Span::new(local_start, local_end).read(self.files[file_index]);

        (source_id, text_slice)
    }
}

impl Default for SourceMap<'_> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_source() {
        let mut source = SourceMap::new();

        let src1 = source.add_source(b"foo");
        let src2 = source.add_source(b"bar");

        assert_eq!(source.get_source(src1), b"foo");
        assert_eq!(source.get_source(src2), b"bar");
    }

    #[test]
    fn test_lookup() {
        let mut source = SourceMap::new();

        let src1 = source.add_source(b"hello");
        let src2 = source.add_source(b"world");

        let result1 = source.lookup(Span::new(0, 5));
        let result2 = source.lookup(Span::new(6, 11));

        assert_eq!(result1, (src1, b"hello" as &[u8]));
        assert_eq!(result2, (src2, b"world" as &[u8]));
    }
}
