mod token;

use stock_source::{Interner, Span};
pub use token::{Token, TokenKind};

pub struct Lexer<'a> {
    source: &'a [u8],
    cursor: u32,
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

        if byte.is_ascii_digit() {
            return self.lex_number(interner);
        }

        if let Some(kind) = self.symbol_kind(byte) {
            return Token::new(kind, self.span_from(start));
        }

        // unrecognized token
        self.advance();
        Token::new(TokenKind::Error, self.span_from(start))
    }
}

impl Lexer<'_> {
    fn advance(&mut self) -> Option<u8> {
        self.source.get(self.cursor as usize).map(|&byte| {
            self.cursor += 1;
            byte
        })
    }

    fn peek(&self) -> Option<u8> {
        self.source.get(self.cursor as usize).copied()
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
        self.source.get((self.cursor + offset) as usize).copied()
    }

    fn span_from(&self, start: u32) -> Span {
        Span::new(start, self.cursor)
    }
}

impl Lexer<'_> {
    fn skip_whitespace(&mut self) {
        while self.peek().is_some_and(|byte| byte.is_ascii_whitespace()) {
            self.advance();
        }
    }

    fn symbol_kind(&mut self, byte: u8) -> Option<TokenKind> {
        self.advance();

        let mut matches = |expected: u8, found: TokenKind, default: TokenKind| -> TokenKind {
            if self.consume(expected) {
                found
            } else {
                default
            }
        };

        let kind = match byte {
            b'+' => matches(b'=', TokenKind::PlusEq, TokenKind::Plus),
            b'-' => matches(b'=', TokenKind::MinusEq, TokenKind::Minus),
            b'*' => matches(b'=', TokenKind::StarEq, TokenKind::Star),
            b'/' => matches(b'=', TokenKind::SlashEq, TokenKind::Slash),

            b'=' => matches(b'=', TokenKind::EqEq, TokenKind::Eq),
            b'!' => matches(b'=', TokenKind::BangEq, TokenKind::Bang),

            b'<' => matches(b'=', TokenKind::LtEq, TokenKind::Lt),
            b'>' => matches(b'=', TokenKind::GtEq, TokenKind::Gt),

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

            _ => return None,
        };

        Some(kind)
    }

    fn lex_number(&mut self, interner: &mut Interner) -> Token {
        let start = self.cursor;
        let mut is_float = false;

        while let Some(byte) = self.peek() {
            if byte.is_ascii_digit() || byte == b'_' {
                self.advance();
            } else if (byte == b'.' || byte == b'e')
                && !is_float
                && self.lookahead(1).is_some_and(|b| b.is_ascii_digit())
            {
                is_float = true;
                self.advance();
                continue;
            } else {
                break;
            }
        }

        let span = self.span_from(start);
        let symbol = interner.intern_source(self.source, span);
        let kind = if is_float {
            TokenKind::Float(symbol)
        } else {
            TokenKind::Integer(symbol)
        };

        Token::new(kind, span)
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
        assert!(matches!(tokens[0].kind, TokenKind::Integer(_)));
        assert!(matches!(tokens[1].kind, TokenKind::Integer(_)));
        assert!(matches!(tokens[2].kind, TokenKind::Float(_)));
        assert!(matches!(tokens[3].kind, TokenKind::Float(_)));
    }
}
