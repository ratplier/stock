use std::str::FromStr;

use crate::error::LexerError;
use crate::token::{Keyword, Token, TokenKind};
use stock_span::{Interner, Span};

pub struct Lexer<'source, 'interner> {
    source: &'source str,
    cursor: usize,
    interner: &'interner mut Interner,
}

impl<'source, 'interner> Lexer<'source, 'interner> {
    pub fn new(source: &'source str, interner: &'interner mut Interner) -> Self {
        Lexer {
            source,
            cursor: 0,
            interner,
        }
    }

    pub fn lex(source: &str, interner: &mut Interner) -> Vec<Token> {
        Lexer::new(source, interner).tokenize()
    }

    pub fn next_token(&mut self) -> Token {
        self.consume_whitespace();

        let start = self.cursor;
        let char = match self.peek_at(0) {
            Some(c) => c,
            None => return Token::new(TokenKind::EndOfFile, self.span(start)),
        };

        let mut create_token = |kind: TokenKind| {
            self.consume_character();
            Token::new(kind, self.span(start))
        };

        match char {
            char if char.is_ascii_digit() => self.consume_number(),
            char if char.is_ascii_alphabetic() || char == '_' => self.consume_identifier(),

            '+' => create_token(TokenKind::Plus),
            '-' => create_token(TokenKind::Minus),
            '*' => create_token(TokenKind::Star),
            '/' => create_token(TokenKind::Slash),
            '(' => create_token(TokenKind::LParen),
            ')' => create_token(TokenKind::RParen),

            _ => {
                self.consume_character();
                Token::error(LexerError::UnexpectedCharacter, self.span(start))
            }
        }
    }

    pub fn tokenize(mut self) -> Vec<Token> {
        let mut tokens = Vec::new();

        loop {
            let token = self.next_token();
            let is_eof = token.kind == TokenKind::EndOfFile;

            tokens.push(token);

            if is_eof {
                break;
            }
        }

        tokens
    }

    fn peek_at(&self, offset: usize) -> Option<char> {
        self.source.get(self.cursor..)?.chars().nth(offset)
    }

    fn span(&self, start: usize) -> Span {
        Span::new(start as u32, self.cursor as u32)
    }
}

impl Lexer<'_, '_> {
    // consumers
    fn consume_character(&mut self) {
        let char_length = self.peek_at(0).map(|c| c.len_utf8()).unwrap_or(0);

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

        let span = self.span(start);
        if valid {
            let raw = span.read(self.source);
            let symbol = self.interner.intern(raw);

            Token::symbol(TokenKind::Number, span, symbol)
        } else {
            Token::error(LexerError::InvalidNumber, span)
        }
    }

    pub fn consume_identifier(&mut self) -> Token {
        let start = self.cursor;

        while let Some(char) = self.peek_at(0) {
            if char.is_ascii_alphanumeric() || char == '_' {
                self.consume_character();
            } else {
                break;
            }
        }

        let span = self.span(start);
        let raw = span.read(self.source);

        if let Ok(keyword) = Keyword::from_str(raw) {
            Token::new(TokenKind::Keyword(keyword), span)
        } else {
            let symbol = self.interner.intern(raw);
            Token::symbol(TokenKind::Identifier, span, symbol)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn get_tokens(src: &str) -> Vec<Token> {
        let mut interner = Interner::new();
        Lexer::new(src, &mut interner).tokenize()
    }

    fn assert_tokens(src: &str, expected: Vec<TokenKind>) {
        let tokens: Vec<TokenKind> = get_tokens(src)
            .into_iter()
            .map(|t| t.kind)
            .take_while(|&k| k != TokenKind::EndOfFile)
            .collect();

        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_utf8_char() {
        assert_tokens(
            "α + β",
            vec![
                TokenKind::Error(LexerError::UnexpectedCharacter),
                TokenKind::Plus,
                TokenKind::Error(LexerError::UnexpectedCharacter),
            ],
        );
    }

    #[test]
    fn test_basic_math() {
        assert_tokens(
            "1 + 2",
            vec![TokenKind::Number, TokenKind::Plus, TokenKind::Number],
        );
    }

    #[test]
    fn test_decimals() {
        let source = "1.23 + 4.56";
        let token = &get_tokens(source)[2];

        let (start, end) = (token.span.start as usize, token.span.end as usize);
        assert_eq!(token.kind, TokenKind::Number);
        assert_eq!(&source[start..end], "4.56");
    }

    #[test]
    fn test_number_underscores() {
        assert_tokens("1_000", vec![TokenKind::Number]);
        assert_tokens("1_000.50", vec![TokenKind::Number]);
    }

    #[test]
    fn test_number_invalid() {
        // 1.2.3 is invalid
        let token = &get_tokens("1.2.3")[0];

        assert_eq!(token.kind, TokenKind::Error(LexerError::InvalidNumber));
    }

    #[test]
    fn test_keyword() {
        assert_tokens(
            "let return",
            vec![
                TokenKind::Keyword(Keyword::Let),
                TokenKind::Keyword(Keyword::Return),
            ],
        );
    }

    #[test]
    fn test_identifier() {
        assert_tokens(
            "variable_name anotherVar",
            vec![TokenKind::Identifier, TokenKind::Identifier],
        );
    }

    #[test]
    fn test_interning() {
        use stock_span::Interner;

        let source = "123 456";
        let mut interner = Interner::new();
        let tokens = Lexer::lex(source, &mut interner);
        println!("{:?}", tokens);

        let symbols: Vec<_> = tokens
            .iter()
            .take_while(|t| t.kind != TokenKind::EndOfFile)
            .map(|t| t.symbol.expect("symbols should be occupied"))
            .collect();

        println!("{:?}", symbols);

        assert_eq!(interner.lookup(symbols[0]), "123");
        assert_eq!(interner.lookup(symbols[1]), "456");
    }
}
