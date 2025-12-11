#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Number(String),

    PLUS, MINUS, STAR, SLASH
}