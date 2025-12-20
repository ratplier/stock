use crate::{Span, token::{LexerError, Token, TokenType}};

pub struct Lexer<'src> {
    source: &'src [u8],
    cursor: usize,
}

impl<'src> Lexer<'src> {
    pub fn new(source: &'src str) -> Self {
        // TODO: support utf8 for strings
        assert!(source.is_ascii(), "source must be ascii");
        Self { source: source.as_bytes(), cursor: 0 }    
    }

    pub fn peek_at(&self, offset: usize) -> Option<&u8> {
        self.source.get(self.cursor + offset)
    }

    pub fn next_token(&mut self) -> Token<'src> {
        self.consume_whitespace();

        match self.peek_at(0) {
            Some(char) if char.is_ascii_digit() => self.consume_number(),
            Some(char) if char.is_ascii_punctuation() => self.consume_symbol(),
            Some(_) => {
                let span = Span::char(self.cursor);
                self.consume_character();
                Token::error(LexerError::UnexpectedCharacter, span)
            }

            None => Token::end_of_file(),
        }
    }

    // consumers
    fn consume_character(&mut self) {
        self.cursor += 1;
    }

    fn consume_whitespace(&mut self) {
        while let Some(char) = self.peek_at(0) {
            if char.is_ascii_whitespace() {
                self.consume_character();
            } else {
                break;
            }
        }
    }

    fn consume_number(&mut self) -> Token<'src> {
        let start = self.cursor;
        let mut valid = true;
        let mut has_dot = false;

        while let Some(char) = self.peek_at(0) {
            if char.is_ascii_digit() {
                self.consume_character();
            } else if *char == b'.' {
                if has_dot {
                    valid = false;
                }

                has_dot = true;
                self.consume_character();
            } else if *char == b'_' {
                if let Some(next_char) = self.peek_at(1) && *next_char == b'_' {
                    valid = false;
                }
            } else {
                break;
            }
        }

        let slice = &self.source[start..self.cursor];
        let leading_trailing = |char: u8| {
            slice.first() == Some(&char) || slice.last() == Some(&char)
        };

        if valid && !slice.is_empty() && !leading_trailing(b'.') && !leading_trailing(b'_') {
            TokenType::Number.to_literal(slice)
        } else {
            let span = Span::new(start, self.cursor);
            TokenType::Error(LexerError::InvalidNumber, span).to_token()
        }
    }

    fn consume_symbol(&mut self) -> Token<'src> {
        let symbol = self.peek_at(0).unwrap_or(&b'\0');
        let token = match symbol {
            b'+' => TokenType::PLUS.to_token(),
            b'-' => TokenType::MINUS.to_token(),
            b'*' => TokenType::STAR.to_token(),
            b'/' => TokenType::SLASH.to_token(),

            b'(' => TokenType::LPAREN.to_token(),
            b')' => TokenType::RPAREN.to_token(),

            _ => {
                let span = Span::char(self.cursor);
                Token::error(LexerError::UnexpectedSymbol, span)
            }
        };
        
        self.consume_character();
        token
    }
}

impl<'src> Iterator for Lexer<'src> {
    type Item = Token<'src>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            token if token.kind == TokenType::EndOfFile => None,
            token => Some(token)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexing() {
        let lexer = Lexer::new("1 + (2 - 3) * 4 / 5");
        let tokens: Vec<Token> = lexer.collect();

        let expected = vec![
            TokenType::Number.to_literal(b"1"),
            TokenType::PLUS.to_token(),
            TokenType::LPAREN.to_token(),
            TokenType::Number.to_literal(b"2"),
            TokenType::MINUS.to_token(),
            TokenType::Number.to_literal(b"3"),
            TokenType::RPAREN.to_token(),
            TokenType::STAR.to_token(),
            TokenType::Number.to_literal(b"4"),
            TokenType::SLASH.to_token(),
            TokenType::Number.to_literal(b"5")
        ];

        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_consume_whitespace() {
        let mut lexer = Lexer::new("   1");
        lexer.consume_whitespace();

        assert_eq!(lexer.peek_at(0), Some(&b'1'));
    }

    #[test]
    fn test_consume_number_integer() {
        let mut lexer = Lexer::new("123");
        let token = lexer.consume_number();

        assert_eq!(token.kind, TokenType::Number);
    }

    #[test]
    fn test_consume_number_decimal() {
        let mut lexer = Lexer::new("1.23");
        let token = lexer.consume_number();

        assert_eq!(token.kind, TokenType::Number);
    }

    #[test]
    fn test_consume_number_error() {
        let mut lexer = Lexer::new("1.2.3");
        let token = lexer.consume_number();

        assert_eq!(token.kind, TokenType::Error(
            LexerError::InvalidNumber, Span::new(0, 5)
        ));
    }

    #[test]
    fn test_consume_symbol_plus() {
        let mut lexer = Lexer::new("+");
        let token = lexer.consume_symbol();

        assert_eq!(token.kind, TokenType::PLUS);
    }

    #[test]
    fn test_consume_symbol_invalid() {
        let mut lexer = Lexer::new("~");
        let token = lexer.consume_symbol();

        assert_eq!(token.kind, TokenType::Error(
            LexerError::UnexpectedSymbol, Span::char(0)
        ));
    }

    #[test]
    fn test_next_token_eof() {
        let mut lexer = Lexer::new("");
        let token = lexer.next_token();

        assert_eq!(token.kind, TokenType::EndOfFile);
    }

    #[test]
    fn test_iterator_stops_at_eof() {
        let lexer = Lexer::new("1 + 2");
        let tokens: Vec<Token> = lexer.collect();

        assert_eq!(tokens.len(), 3);
        assert_ne!(tokens.last().map(|t| &t.kind), Some(&TokenType::EndOfFile))
    }
}
