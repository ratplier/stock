use stock_source::Span;
use crate::token::{Token, TokenType, LexerError};

pub struct Lexer<'source> {
    source: &'source str,
    cursor: usize,
}

impl<'source> Lexer<'source> {
    pub fn new(source: &'source str) -> Self {
        Lexer { source, cursor: 0 }
    }

    #[inline]
    fn peek_at(&self, offset: usize) -> Option<char> {
        self.source.get(self.cursor..)?.chars().nth(offset)
    }

    fn span(&self, start: usize) -> Span {
        Span::new(
            start as u32,
            self.cursor as u32 
        )
    }

    pub fn next_token(&mut self) -> Token {
        self.consume_whitespace();

        let start = self.cursor;
        let char = match self.peek_at(0) {
            Some(c) => c,
            None => return Token::new(TokenType::EndOfFile, self.span(start)),
        };

        match char {
            char if char.is_ascii_digit() => self.consume_number(),

            '+' => { self.consume_character(); Token::new(TokenType::Plus, self.span(start)) },
            '-' => { self.consume_character(); Token::new(TokenType::Minus, self.span(start)) },
            '*' => { self.consume_character(); Token::new(TokenType::Star, self.span(start)) },
            '/' => { self.consume_character(); Token::new(TokenType::Slash, self.span(start)) },
            '(' => { self.consume_character(); Token::new(TokenType::LParen, self.span(start)) },
            ')' => { self.consume_character(); Token::new(TokenType::RParen, self.span(start)) },

            _ => {
                self.consume_character();
                Token::error(LexerError::UnexpectedCharacter, self.span(start))
            },
        }
    }
    
    // consumers
    fn consume_character(&mut self) {
        let char_length = self.peek_at(0)
            .map(|c| c.len_utf8())
            .unwrap_or(0);

        self.cursor += char_length;
    }

    fn consume_whitespace(&mut self) {
        while let Some(char) = self.peek_at(0) {
            if char.is_whitespace() {
                self.consume_character();
            } else {
                break;
            }
        }
    }

    fn consume_number(&mut self) -> Token {
        let start = self.cursor;
        let mut valid = true;
        let mut has_dot = false;

        while let Some(char) = self.peek_at(0) {
            if char.is_ascii_digit() || char == '_' {
                self.consume_character();
            } else if char == '.' {
                if has_dot {
                    valid = false;
                }

                has_dot = true;
                self.consume_character();
            } else {
                break;
            }
        }

        if valid {
            Token::new(TokenType::Number, self.span(start))
        } else {
            Token::error(LexerError::InvalidNumber, self.span(start))
        }
    }
}

impl<'source> Iterator for Lexer<'source> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.next_token();
        if token.kind == TokenType::EndOfFile {
            None
        } else {
            Some(token)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_tokens(src: &str, expected: Vec<TokenType>) {
        let lexer = Lexer::new( src);
        let tokens: Vec<TokenType> = lexer.map(|t| t.kind).collect();

        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_utf8_char() {
        assert_tokens("α + β", vec![
            TokenType::Error(LexerError::UnexpectedCharacter),
            TokenType::Plus,
            TokenType::Error(LexerError::UnexpectedCharacter),
        ]);
    }

    #[test]
    fn test_basic_math() {
        assert_tokens("1 + 2", vec![
            TokenType::Number, 
            TokenType::Plus, 
            TokenType::Number
        ]);
    }

    #[test]
    fn test_decimals() {
        let mut lexer = Lexer::new("1.23");
        let token = lexer.next().unwrap();
        
        let (start, end) = (token.span.start as usize, token.span.end as usize);
        assert_eq!(token.kind, TokenType::Number);
        assert_eq!(&lexer.source[start..end], "1.23");
    }

    #[test]
    fn test_number_underscores() {
        assert_tokens("1_000", vec![TokenType::Number]);
        assert_tokens("1_000.50", vec![TokenType::Number]);
    }

    #[test]
    fn test_number_invalid() {
        // 1.2.3 is invalid
        let mut lexer = Lexer::new("1.2.3");
        let t = lexer.next_token();
        assert_eq!(t.kind, TokenType::Error(LexerError::InvalidNumber));
    }
}