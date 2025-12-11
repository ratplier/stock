use crate::Token;

#[derive(Debug, PartialEq, Clone)]
pub enum LexerError {
    MultipleDecimalPoints,
    TrailingUnderscore,
    TrailingDecimal,
    InvalidSymbol,
}

pub struct Lexer<'src> {
    source: &'src [u8],
    cursor: usize,
}

impl<'src> Lexer<'src> {
    fn new(source: &'src str) -> Self {
        // TODO: support utf8 for strings
        assert!(source.is_ascii(), "source must be ascii");
        Self { source: source.as_bytes(), cursor: 0 }    
    }

    fn peek(&self, offset: usize) -> Option<char> {
        let index = self.cursor + offset;
        if index >= self.source.len() {
            return None
        }
        
        Some(self.source[self.cursor] as char)
    }

    fn advance(&mut self) {
        self.cursor += 1;
    }
}

impl Lexer<'_> {
    fn consume_whitespace(&mut self) {
        while let Some(char) = self.peek(0) {
            if char.is_whitespace() {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn consume_number(&mut self) -> Token {
        let start = self.cursor;
        let mut decimal_seen = false;

        while let Some(char) = self.peek(0) {
            if char.is_digit(10) || char == '_' {
                self.advance();
            } else if char == '.' {
                if decimal_seen {
                    return Token::Error(LexerError::MultipleDecimalPoints)
                }
                
                decimal_seen = true;
                self.advance();
            } else {
                break;
            }
        }

        let slice = &self.source[start..self.cursor];
        let check_trailing = |c: u8| {
            slice[slice.len() - 1] == c
        };

        if check_trailing(b'_') {
            return Token::Error(LexerError::TrailingUnderscore)
        }

        if decimal_seen && check_trailing(b'.') {
            return Token::Error(LexerError::TrailingDecimal)
        }

        let number = String::from_utf8_lossy(slice)
            .replace("_", "");

        Token::Number(number)
    }

    fn consume_symbol(&mut self) -> Token {
        match self.peek(0) {
            Some('+') => { self.advance(); Token::PLUS },
            Some('-') => { self.advance(); Token::MINUS },
            Some('*') => { self.advance(); Token::STAR },
            Some('/') => { self.advance(); Token::SLASH },
            
            _ => Token::Error(LexerError::InvalidSymbol),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_consume_whitespace() {
        let mut lexer = Lexer::new("   1");
        lexer.consume_whitespace();
        assert_eq!(lexer.peek(0), Some('1'));
    }

    #[test]
    fn test_consume_number() {
        let mut lexer = Lexer::new("123");
        let token = lexer.consume_number();
        assert_eq!(token, Token::Number("123".to_string()));

        let mut lexer = Lexer::new("1.23");
        let token = lexer.consume_number();
        assert_eq!(token, Token::Number("1.23".to_string()));

        let mut lexer = Lexer::new("_1_2_3_");
        let token = lexer.consume_number();
        assert_eq!(token, Token::Number("123".to_string()));
    }

    #[test]
    fn test_consume_symbol() {
        let mut lexer = Lexer::new("+");
        let token = lexer.consume_symbol();
        assert_eq!(token, Token::PLUS);

        let mut lexer = Lexer::new("-");
        let token = lexer.consume_symbol();
        assert_eq!(token, Token::MINUS);

        let mut lexer = Lexer::new("*");
        let token = lexer.consume_symbol();
        assert_eq!(token, Token::STAR);

        let mut lexer = Lexer::new("/");
        let token = lexer.consume_symbol();
        assert_eq!(token, Token::SLASH);

        let mut lexer = Lexer::new("p");
        let token = lexer.consume_symbol();
        assert_eq!(token, Token::Error(LexerError::InvalidSymbol));
    }
}