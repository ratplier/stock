use stock_ast::{Ast, ExprId, LiteralKind, BinaryOp, UnaryOp};
use stock_lexer::{Token, TokenKind, LexerError};
use stock_span::{Span, Symbol};
use crate::error::{ParserError, ParserErrorKind};

pub struct Parser {
    tokens: Vec<Token>,
    cursor: usize,

    ast: Ast,
    errors: Vec<ParserError>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        assert!(!tokens.is_empty(), "parser must have at least one token");

        Parser {
            tokens,
            cursor: 0,
            ast: Ast::new(),
            errors: Vec::new(),
        }
    }

    pub fn parse(mut self) -> (Ast, Vec<ParserError>) {
        if !self.is_at_end() {
            let _ = self.parse_expression(0);
        }
        (self.ast, self.errors)
    }
}

impl Parser {
    fn error(&mut self, error: ParserErrorKind) {
        let span = self.peek(0).span;
        let error = ParserError::new(error, span);
        self.errors.push(error);
    }

    fn get_symbol(&self, token: Token) -> Symbol {
        token.symbol.expect("token should have a symbol")
    }

    fn peek(&self, offset: usize) -> Token {
        self.tokens.get(self.cursor + offset)
            .unwrap_or(self.tokens.last().unwrap())
            .clone()
    }

    fn peek_kind(&self, offset: usize) -> TokenKind {
        self.peek(offset).kind
    }

    fn advance(&mut self) -> Token {
        let token = self.peek(0);
        self.cursor += 1;
        token
    }

    fn consume(&mut self, expected: TokenKind, error: ParserErrorKind) -> Result<Token, ()> {
        if self.peek_kind(0) == expected {
            Ok(self.advance())
        } else {
            self.error(error);
            Err(())
        }
    }

    fn is_at_end(&self) -> bool {
        self.peek_kind(0) == TokenKind::EndOfFile
    }
}

impl Parser {
    fn parse_expression(&mut self, min_binding_power: u8) -> Result<ExprId, ()> {
        let mut lhs = self.parse_prefix()?;

        loop {
            let current_kind = self.peek_kind(0);
            if current_kind == TokenKind::EndOfFile {
                break;
            }

            let (l_bp, r_bp) = match self.infix_binding_power(current_kind) {
                Some(bp) => bp,
                None => break,
            };

            
            if l_bp < min_binding_power {
                break;
            }

            let op_token = self.advance();
            let op = self.token_to_binary_op(op_token.kind);

            let rhs = self.parse_expression(r_bp)?;

            let lhs_span = self.ast.get_expr(lhs).span;
            let rhs_span = self.ast.get_expr(rhs).span;
            let combined_span = Span::new(lhs_span.start,  rhs_span.end);

            lhs = self.ast.binary(op, lhs, rhs, combined_span);
        }

        Ok(lhs)
    }

    fn parse_prefix(&mut self) -> Result<ExprId, ()> {
        let token = self.advance();

        match token.kind {
            TokenKind::Number => {
                let span = token.span;
                let symbol = self.get_symbol(token);

                // TODO: convert to appropriate literal kind
                Ok(self.ast.literal(LiteralKind::Integer, symbol, span))
            },

            TokenKind::LParen => {
                let expr = self.parse_expression(0)?;
                self.consume(TokenKind::RParen, ParserErrorKind::ExpectedRParen)?;
                Ok(expr)
            },

            TokenKind::Minus => {
                let ((), r_bp) = self.prefix_binding_power(TokenKind::Minus);
                let rhs = self.parse_expression(r_bp)?;
                
                let rhs_span = self.ast.get_expr(rhs).span;
                let span = Span::new(token.span.start, rhs_span.end);
                
                Ok(self.ast.unary(UnaryOp::Negate, rhs, span))
            },

            TokenKind::Error(lexer_error) => {
                let kind = match lexer_error {
                    LexerError::UnexpectedCharacter => ParserErrorKind::UnexpectedCharacter,
                    LexerError::InvalidNumber => ParserErrorKind::InvalidNumber,
                };

                self.error(kind);
                Err(())
            },

            _ => {
                self.error(ParserErrorKind::UnexpectedToken);
                Err(())
            },
        }
    }
}

impl Parser {
    // unaries bind tighter than infixes
    fn prefix_binding_power(&self, kind: TokenKind) -> ((), u8) {
        match kind {
            TokenKind::Minus => ((), 9),
            _ => ((), 0),
        }
    }

    // add/sub: low precedence (1, 2)
    // mul/div: higher precedence (3, 4)
    fn infix_binding_power(&self, kind: TokenKind) -> Option<(u8, u8)> {
        match kind {
            TokenKind::Plus | TokenKind::Minus => Some((1, 2)),
            TokenKind::Star | TokenKind::Slash => Some((3, 4)),
            _ => None,
        }
    }

    fn token_to_binary_op(&self, kind: TokenKind) -> BinaryOp {
        match kind {
            TokenKind::Plus => BinaryOp::Add,
            TokenKind::Minus => BinaryOp::Subtract,
            TokenKind::Star => BinaryOp::Multiply,
            TokenKind::Slash => BinaryOp::Divide,

            _ => unreachable!("token should be a binary operator"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use stock_lexer::Lexer;
    use stock_span::{Interner};
    use stock_ast::{ExprKind};

    // small utility to print ast nodes in a readable format
    fn tostring_ast(ast: &Ast, interner: &Interner, expr_id: ExprId) -> String {
        tostring_ast_inner(ast, interner, expr_id, 0)
    }

    fn tostring_ast_inner(ast: &Ast, interner: &Interner, expr_id: ExprId, depth: usize) -> String {
        let expr = ast.get_expr(expr_id);
        let indent = "  ".repeat(depth);
        let next_indent = "  ".repeat(depth + 1);
        
        match expr.kind {
            ExprKind::Literal { kind, value } => {
                format!("{:?}({:?})", kind, interner.lookup(value))
            },
            ExprKind::Binary { op, lhs, rhs } => {
                let lhs_str = tostring_ast_inner(ast, interner, lhs, depth + 1);
                let rhs_str = tostring_ast_inner(ast, interner, rhs, depth + 1);
                format!("{:?} {{\n{}lhs: {},\n{}rhs: {}\n{}}}", op, next_indent, lhs_str, next_indent, rhs_str, indent)
            },
            ExprKind::Unary { op, rhs } => {
                let rhs_str = tostring_ast_inner(ast, interner, rhs, depth + 1);
                format!("{:?}({})", op, rhs_str)
            },
        }
    }

    #[test]
    fn playground() {
        let source = "(4 + 5) * 67";

        let mut interner = Interner::new();
        let tokens = Lexer::lex(source, &mut interner);

        let mut parser = Parser::new(tokens);
        let expr_id = parser.parse_expression(0);

        if let Ok(expr_id) = expr_id {
            let str = tostring_ast(&parser.ast, &interner, expr_id);
            println!("{}", str);
        } else {
            for error in &parser.errors {
                println!("{:?}", error);
            }
        }
    }
}