mod token;

use stock_diagnostics::DiagnosticSink;
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

    pub fn next_token(&mut self, interner: &mut Interner, sink: &mut DiagnosticSink) -> Token {
        self.skip_whitespace();

        let start = self.cursor;
        match self.peek() {
            None => Token::eof(self.span_from(start)),

            Some(byte) if byte.is_ascii_digit() => self.lex_number(interner, sink, start),
            Some(byte) if byte.is_ascii_alphabetic() || byte == b'_' => {
                self.lex_identifier(interner, start)
            }
            Some(byte) if byte.is_ascii_punctuation() => self.lex_symbol(byte, start),

            Some(byte) => {
                self.advance();

                let span = self.span_from(start);
                sink.unknown_byte(byte, span);

                Token::new(TokenKind::Unknown, span)
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

        #[rustfmt::skip]
        let kind = match byte {
            // arithmetic
            b'+' => TokenKind::Plus,
            b'-' => TokenKind::Minus,
            b'*' => TokenKind::Star,
            b'/' => TokenKind::Slash,

            // comparison
            b'<' => if self.consume(b'=') { TokenKind::Le } else { TokenKind::Lt },
            b'>' => if self.consume(b'=') { TokenKind::Ge } else { TokenKind::Gt },

            // delimiters
            b'(' => TokenKind::LParen,
            b')' => TokenKind::RParen,
            b'{' => TokenKind::LBrace,
            b'}' => TokenKind::RBrace,
            b'[' => TokenKind::LBracket,
            b']' => TokenKind::RBracket,

            // other
            b'=' => if self.consume(b'=') { TokenKind::EqEq } else { TokenKind::Eq },
            b'!' => if self.consume(b'=') { TokenKind::BangEq } else { TokenKind::Bang },

            b',' => TokenKind::Comma,
            b'.' => TokenKind::Dot,
            b':' => TokenKind::Colon,
            b';' => TokenKind::Semicolon,

            _ => TokenKind::Unknown,
        };

        Token::new(kind, self.span_from(start))
    }

    fn consume_digits(&mut self) {
        while self
            .peek()
            .is_some_and(|byte| byte.is_ascii_digit() || byte == b'_')
        {
            self.advance();
        }
    }

    fn lex_number(
        &mut self,
        interner: &mut Interner,
        sink: &mut DiagnosticSink,
        start: usize,
    ) -> Token {
        let mut kind = TokenKind::Integer;

        // TODO: support other bases (hex, binary)
        // also maybe add NumberInfo to define base, sign, etc
        if self.consume(b'0') {}

        self.consume_digits();

        // base decimal (1.3)
        if self.consume(b'.') {
            kind = TokenKind::Float;

            let decimal_start = self.cursor;
            self.consume_digits();

            if self.cursor == decimal_start {
                sink.trailing_decimal(self.span_from(start));
            }
        }

        // exponent, uses previous decimal (1.3) as base (1.3e-4)
        if self.consume(b'e') || self.consume(b'E') {
            kind = TokenKind::Float;

            // consume 1e+, 1e-
            if self.peek() == Some(b'+') || self.peek() == Some(b'-') {
                self.advance();
            }

            let exponent_start = self.cursor;
            self.consume_digits();

            if self.cursor == exponent_start {
                sink.trailing_decimal(self.span_from(start));
            }
        }

        // check for invalid suffix (123abc, 1_)
        if self
            .peek()
            .is_some_and(|b| b.is_ascii_alphabetic() || b == b'_')
        {
            while self
                .peek()
                .is_some_and(|b| b.is_ascii_alphanumeric() || b == b'_')
            {
                self.advance();
            }

            let span = self.span_from(start);
            // TODO: report invalid number suffix (_, abc)

            return Token::new(TokenKind::Unknown, span);
        }

        let span = self.span_from(start);
        let symbol = interner.intern_source(self.source, span);

        Token::with_symbol(kind, symbol, span)
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
        let symbol = interner.intern(&self.source[start..self.cursor]);

        if symbol.is_keyword() {
            return Token::new(TokenKind::from_symbol(symbol), span);
        }

        Token::with_symbol(TokenKind::Identifier, symbol, span)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lex_all(source: &str) -> Vec<Token> {
        let mut lexer = Lexer::new(source.as_bytes());
        let mut interner = Interner::new();
        let mut sink = DiagnosticSink::new();

        let mut tokens = Vec::new();
        loop {
            let token = lexer.next_token(&mut interner, &mut sink);
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
        // TODO: implement compounds (+=, -=, *=, /=)

        let tokens = lex_all("== != <= >= ");
        let expected = vec![
            TokenKind::EqEq,
            TokenKind::BangEq,
            TokenKind::Le,
            TokenKind::Ge,
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
