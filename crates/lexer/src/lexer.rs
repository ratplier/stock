use crate::Token;

pub struct Lexer<'src> {
    source: &'src str,
    cursor: usize,
}

impl<'src> Lexer<'src> {
    fn new(source: &'src str) -> Self {
        Self { source, cursor: 0 }    
    }

    fn is_at_end(&self) -> bool {
        self.cursor >= self.source.len()
    }

    fn peek(&self, offset: usize) -> Option<char> {
        let index = self.cursor + offset;
        if index >= self.source.len() {
            return None
        }

        self.source.chars().nth(index)
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

    fn consume_number(&mut self) -> Option<Token> {
        let is_number = |c: char| {
            (c >= '0' && c <= '9') || c == '_'
        };
        
        let mut decimal = false;
        let mut number = String::new();
        while let Some(char) = self.peek(0) {
            if is_number(char) {
                if char != '_' {
                    number.push(char);
                }
                self.advance();
            } else if char == '.' {
                if decimal {
                    todo!("number parsing does not fail when there are multiple decimal points");
                }

                decimal = true;
                number.push('.');
                self.advance();
            } else {
                // TODO: handle error for invalid number
                return None;
            }
        }

        // TODO: handle error when number is incomplete (e.g. `1.` or `.1`)
        Some(Token::Number(number))
    }

    fn consume_symbol(&mut self) -> Option<Token> {
        match self.peek(0) {
            Some('+') => { self.advance(); Some(Token::PLUS) },
            Some('-') => { self.advance(); Some(Token::MINUS) },
            Some('*') => { self.advance(); Some(Token::STAR) },
            Some('/') => { self.advance(); Some(Token::SLASH) },
            
            _ => None, // TODO: handle error for invalid symbol
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_at_end() {
        let lexer = Lexer::new("");
        assert!(lexer.is_at_end());

        let lexer = Lexer::new("1");
        assert!(!lexer.is_at_end());
    }

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
        assert_eq!(token, Some(Token::Number("123".to_string())));

        let mut lexer = Lexer::new("1.23");
        let token = lexer.consume_number();
        assert_eq!(token, Some(Token::Number("1.23".to_string())));

        let mut lexer = Lexer::new("_1_2_3_");
        let token = lexer.consume_number();
        assert_eq!(token, Some(Token::Number("123".to_string())));
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
    }
}