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
    
    fn next_token(&mut self) -> Option<Token> {
        self.consume_whitespace();

        let char = self.peek(0)?;
        match char {
            '0'..='9' => Some(self.consume_number()),
            _ => self.consume_symbol(),
        }
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

    fn consume_symbol(&mut self) -> Option<Token> {
        match self.peek(0) {
            Some('+') => { self.advance(); Some(Token::PLUS) },
            Some('-') => { self.advance(); Some(Token::MINUS) },
            Some('*') => { self.advance(); Some(Token::STAR) },
            Some('/') => { self.advance(); Some(Token::SLASH) },
            Some('(') => { self.advance(); Some(Token::LPAREN) },
            Some(')') => { self.advance(); Some(Token::RPAREN) },

            _ => None,
        }
    }
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parsing() {
        let lexer = Lexer::new("1 + (2 - 3) * 4 / 5");
        let tokens = lexer.collect::<Vec<Token>>();
        let expected = vec![
            Token::Number("1".to_string()),
            Token::PLUS,
            Token::LPAREN,
            Token::Number("2".to_string()),
            Token::MINUS,
            Token::Number("3".to_string()),
            Token::RPAREN,
            Token::STAR,
            Token::Number("4".to_string()),
            Token::SLASH,
            Token::Number("5".to_string()),
        ];

        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_consume_whitespace() {
        let mut lexer = Lexer::new("   1");
        lexer.consume_whitespace();
        assert_eq!(lexer.peek(0), Some('1'));
    }

    #[test]
    fn test_consume_number() {
        // integer
        let mut lexer = Lexer::new("123");
        let token = lexer.consume_number();
        assert_eq!(token, Token::Number("123".to_string()));

        // decimal
        let mut lexer = Lexer::new("1.23");
        let token = lexer.consume_number();
        assert_eq!(token, Token::Number("1.23".to_string()));

        // underscore
        let mut lexer = Lexer::new("_1_2_3");
        let token = lexer.consume_number();
        assert_eq!(token, Token::Number("123".to_string()));

        // trailing underscore
        let mut lexer = Lexer::new("1_");
        let token = lexer.consume_number();
        assert_eq!(token, Token::Error(LexerError::TrailingUnderscore));  

        // trailing decimal
        let mut lexer = Lexer::new("1.");
        let token = lexer.consume_number();
        assert_eq!(token, Token::Error(LexerError::TrailingDecimal));
    }

    #[test]
    fn test_consume_symbol() {
        let mut lexer = Lexer::new("+");
        let token = lexer.consume_symbol();
        assert_eq!(token, Some(Token::PLUS));

        let mut lexer = Lexer::new("-");
        let token = lexer.consume_symbol();
        assert_eq!(token, Some(Token::MINUS));

        let mut lexer = Lexer::new("*");
        let token = lexer.consume_symbol();
        assert_eq!(token, Some(Token::STAR));

        let mut lexer = Lexer::new("/");
        let token = lexer.consume_symbol();
        assert_eq!(token, Some(Token::SLASH));

        let mut lexer = Lexer::new("p");
        let token = lexer.consume_symbol();
        assert_eq!(token, None);
    }
}