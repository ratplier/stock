#[derive(Debug, PartialEq, Eq)]
pub enum LexerError {
    UnexpectedCharacter,
    InvalidNumber,
}
