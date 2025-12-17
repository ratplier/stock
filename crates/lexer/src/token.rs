use crate::{LexerError};

#[derive(Debug, PartialEq)]
pub enum Token {
    Number(String),

    PLUS, MINUS, STAR, SLASH,
    LPAREN, RPAREN,

    Error(LexerError),
}