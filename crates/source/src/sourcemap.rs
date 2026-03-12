use crate::span::Span;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SourceId(pub u32);

pub struct SourceMap {
    files: Vec<String>,
    file_starts: Vec<u32>,

    offset: u32,
}

impl SourceMap {
    pub fn new() -> Self {
        Self {
            files: Vec::new(),
            file_starts: Vec::new(),

            offset: 0,
        }
    }

    pub fn add_source(&mut self, source: String) -> SourceId {
        let source_length = source.len() as u32;
        let offset = self.offset;

        self.files.push(source.clone());
        self.file_starts.push(offset);

        self.offset += source_length + 1;

        SourceId(source_length)
    }

    pub fn get_source(&self, file_id: SourceId) -> &str {
        &self.files[file_id.0 as usize]
    }

    pub fn lookup(&self, span: Span) -> (SourceId, &str) {
        let file_index = self.file_starts
            .partition_point(|&start| start <= span.start) - 1;

        let file_id = SourceId(file_index as u32);
        let local_start = span.start - self.file_starts[file_index];
        let local_end = span.end - self.file_starts[file_index];
        
        let text_slice = &self.files[file_index][local_start as usize .. local_end as usize];

        (file_id, text_slice)
    }
}

impl Default for SourceMap {
    fn default() -> Self {
        Self::new()
    }
}
