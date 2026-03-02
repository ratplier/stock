use crate::span::Span;

#[derive(Debug)]
pub struct SourceInfo {
    pub name: String,
    pub source: String,
    pub start_position: u32,
}

#[derive(Debug, Default)]
pub struct SourceMap {
    files: Vec<SourceInfo>,
    current_offset: u32,
}

impl SourceMap {
    pub fn new() -> Self {
        SourceMap {
            files: Vec::new(),
            current_offset: 0,
        }
    }

    pub fn add_file(&mut self, name: String, source: String) -> u32 {
        let start_position = self.current_offset;
        let file_length = source.len() as u32;

        let file_info = SourceInfo {
            name,
            source,
            start_position,
        };

        self.current_offset += file_length + 1;
        self.files.push(file_info);

        start_position
    }

    pub fn find(&self, position: u32) -> Option<&SourceInfo> {
        use std::cmp::Ordering;

        self.files
            .binary_search_by(|file| {
                if position < file.start_position {
                    Ordering::Greater
                } else if position >= file.start_position + file.source.len() as u32 {
                    Ordering::Less
                } else {
                    Ordering::Equal
                }
            })
            .ok()
            .map(|index| &self.files[index])
    }

    pub fn find_from_span(&self, span: &Span) -> Option<&SourceInfo> {
        self.find(span.start)
    }
}
