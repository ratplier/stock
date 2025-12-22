#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum LexerError {
    UnexpectedCharacter,
    InvalidNumber,
}
