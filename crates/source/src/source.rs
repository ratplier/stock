use crate::span::Span;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FileId(pub u32);

pub struct SourceFile<'source> {
    pub name: &'source str,
    pub src: &'source str,
}

pub struct SourceMap<'source> {
    file: Vec<SourceFile<'source>>,
}

impl<'source> SourceMap<'source> {
    pub fn new() -> Self {
        SourceMap { file: Vec::new() }
    }

    pub fn add(&mut self, name: &'source str, src: &'source str) -> FileId {
        let file = SourceFile { name, src };
        self.file.push(file);

        FileId(self.file.len() as u32)
    }

    pub fn get_file(&self, id: FileId) -> &SourceFile<'_> {
        &self.file[(id.0 - 1) as usize]
    }

    pub fn read_span(&self, file: FileId, span: Span) -> &str {
        let file = self.get_file(file);
        let (start, end) = (span.start as usize, span.end as usize);
        &file.src[start..end]
    }
}
