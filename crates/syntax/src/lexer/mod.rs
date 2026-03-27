mod token;

use stock_source::{Interner, Span};
pub use token::{Token, TokenKind};

pub struct Lexer<'a> {
    source: &'a [u8],
    cursor: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a [u8]) -> Self {
        Self { source, cursor: 0 }
    }

    pub fn next_token(&mut self, interner: &mut Interner) -> Token {
        self.skip_whitespace();

        let start = self.cursor;
        let byte = match self.peek() {
            Some(byte) => byte,
            None => return Token::eof(self.span_from(start)),
        };

        match byte {
            byte if byte.is_ascii_digit() => self.lex_number(interner, start),
            byte if byte.is_ascii_alphanumeric() || byte == b'_' => {
                self.lex_identifier(interner, start)
            }

            byte if byte.is_ascii_punctuation() => self.lex_symbol(byte, start),

            _ => {
                self.advance();
                Token::new(TokenKind::Error, self.span_from(start))
            }
        }
    }
}

impl Lexer<'_> {
    fn advance(&mut self) -> Option<u8> {
        self.source.get(self.cursor).map(|&byte| {
            self.cursor += 1;
            byte
        })
    }

    fn peek(&self) -> Option<u8> {
        self.source.get(self.cursor).copied()
    }

    fn consume(&mut self, expected: u8) -> bool {
        if self.peek() == Some(expected) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn lookahead(&self, offset: u32) -> Option<u8> {
        self.source.get(self.cursor + (offset as usize)).copied()
    }

    fn span_from(&self, start: usize) -> Span {
        Span::new(start as u32, self.cursor as u32)
    }
}

impl Lexer<'_> {
    fn skip_whitespace(&mut self) {
        while self.peek().is_some_and(|byte| byte.is_ascii_whitespace()) {
            self.advance();
        }
    }

    fn lex_symbol(&mut self, byte: u8, start: usize) -> Token {
        self.advance();

        let kind = match byte {
            b'+' => {
                if self.consume(b'=') {
                    TokenKind::PlusEq
                } else {
                    TokenKind::Plus
                }
            }

            b'-' => {
                if self.consume(b'=') {
                    TokenKind::MinusEq
                } else {
                    TokenKind::Minus
                }
            }

            b'*' => {
                if self.consume(b'=') {
                    TokenKind::StarEq
                } else {
                    TokenKind::Star
                }
            }

            b'/' => {
                if self.consume(b'=') {
                    TokenKind::SlashEq
                } else {
                    TokenKind::Slash
                }
            }

            b'=' => {
                if self.consume(b'=') {
                    TokenKind::EqEq
                } else {
                    TokenKind::Eq
                }
            }

            b'!' => {
                if self.consume(b'=') {
                    TokenKind::BangEq
                } else {
                    TokenKind::Bang
                }
            }

            b'<' => {
                if self.consume(b'=') {
                    TokenKind::LtEq
                } else {
                    TokenKind::Lt
                }
            }

            b'>' => {
                if self.consume(b'=') {
                    TokenKind::GtEq
                } else {
                    TokenKind::Gt
                }
            }

            b'(' => TokenKind::LParen,
            b')' => TokenKind::RParen,
            b'{' => TokenKind::LBrace,
            b'}' => TokenKind::RBrace,
            b'[' => TokenKind::LBracket,
            b']' => TokenKind::RBracket,

            b',' => TokenKind::Comma,
            b'.' => TokenKind::Dot,
            b':' => TokenKind::Colon,
            b';' => TokenKind::Semicolon,

            // TODO: handle error
            _ => return Token::new(TokenKind::Error, self.span_from(start)),
        };

        Token::new(kind, self.span_from(start))
    }

    fn lex_number(&mut self, interner: &mut Interner, start: usize) -> Token {
        let mut kind = TokenKind::Integer;
        self.consume_digits();

        let byte = {
            let byte = self.peek();
            if byte.is_none() {
                todo!("error handling");
            }

            byte.unwrap()
        };

        if byte == b'.' && self.lookahead(1).is_some_and(|b| b.is_ascii_digit()) {
            kind = TokenKind::Float;

            self.advance();
            self.consume_digits();
        }

        if byte == b'e' || byte == b'E' {
            kind = TokenKind::Float;

            self.advance();

            // handle e+ and e-
            if self.lookahead(1).is_some_and(|b| b == b'+' || b == b'-') {
                self.advance();
            }

            self.consume_digits();
        }

        let span = self.span_from(start);
        let symbol = interner.intern_source(self.source, span);

        Token::with_symbol(kind, symbol, span)
    }

    fn consume_digits(&mut self) {
        while self
            .peek()
            .is_some_and(|byte| byte.is_ascii_digit() || byte == b'_')
        {
            self.advance();
        }
    }

    fn lex_identifier(&mut self, interner: &mut Interner, start: usize) -> Token {
        while let Some(byte) = self.peek() {
            if byte.is_ascii_alphanumeric() || byte == b'_' {
                self.advance();
            } else {
                break;
            }
        }

        let span = self.span_from(start);
        let identifier = &self.source[start..self.cursor];

        if let Some(kind) = TokenKind::keyword_from_str(identifier) {
            return Token::new(kind, span);
        }

        let symbol = interner.intern(identifier);
        Token::with_symbol(TokenKind::Identifier, symbol, span)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lex_all(source: &str) -> Vec<Token> {
        let mut lexer = Lexer::new(source.as_bytes());
        let mut interner = Interner::new();

        let mut tokens = Vec::new();
        loop {
            let token = lexer.next_token(&mut interner);
            if token.kind == TokenKind::EndOfFile {
                break;
            }
            tokens.push(token);
        }
        tokens
    }

    #[test]
    fn test_lex_basic_symbols() {
        let tokens = lex_all("+-*/!<> =");
        let expected = vec![
            TokenKind::Plus,
            TokenKind::Minus,
            TokenKind::Star,
            TokenKind::Slash,
            TokenKind::Bang,
            TokenKind::Lt,
            TokenKind::Gt,
            TokenKind::Eq,
        ];

        assert_eq!(tokens.len(), expected.len());
        for (token, expected) in tokens.iter().zip(expected.iter()) {
            assert_eq!(
                token.kind, *expected,
                "{:?} should match {:?}",
                token.kind, expected
            );
        }
    }

    #[test]
    fn test_lex_compound_symbols() {
        let tokens = lex_all("+= -= *= /= == != <= >= ");
        let expected = vec![
            TokenKind::PlusEq,
            TokenKind::MinusEq,
            TokenKind::StarEq,
            TokenKind::SlashEq,
            TokenKind::EqEq,
            TokenKind::BangEq,
            TokenKind::LtEq,
            TokenKind::GtEq,
        ];

        assert_eq!(tokens.len(), expected.len());
        for (token, expected) in tokens.iter().zip(expected.iter()) {
            assert_eq!(
                token.kind, *expected,
                "{:?} should match {:?}",
                token.kind, expected
            );
        }
    }

    #[test]
    fn test_lex_delimiters() {
        let tokens = lex_all("() {} [] , . : ;");
        let expected = vec![
            TokenKind::LParen,
            TokenKind::RParen,
            TokenKind::LBrace,
            TokenKind::RBrace,
            TokenKind::LBracket,
            TokenKind::RBracket,
            TokenKind::Comma,
            TokenKind::Dot,
            TokenKind::Colon,
            TokenKind::Semicolon,
        ];

        assert_eq!(tokens.len(), expected.len());
        for (token, expected) in tokens.iter().zip(expected.iter()) {
            assert_eq!(
                token.kind, *expected,
                "{:?} should match {:?}",
                token.kind, expected
            );
        }
    }

    #[test]
    fn test_lex_numbers() {
        let tokens = lex_all("123 1_000 1.0 1e10");

        assert_eq!(tokens.len(), 4);
        assert_eq!(tokens[0].kind, TokenKind::Integer);
        assert_eq!(tokens[1].kind, TokenKind::Integer);
        assert_eq!(tokens[2].kind, TokenKind::Float);
        assert_eq!(tokens[3].kind, TokenKind::Float);
    }

    #[test]
    fn test_lex_identifiers_keywords() {
        let tokens = lex_all("if else variable _under_score");

        assert_eq!(tokens.len(), 4);
        assert_eq!(tokens[0].kind, TokenKind::If);
        assert_eq!(tokens[1].kind, TokenKind::Else);
        assert_eq!(tokens[2].kind, TokenKind::Identifier);
        assert_eq!(tokens[3].kind, TokenKind::Identifier);
    }
}
