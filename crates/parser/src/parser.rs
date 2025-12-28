use crate::error::{ParserError, ParserErrorKind};
use stock_ast::{Ast, BinaryOp, ExprId, LiteralKind, StmtId, UnaryOp};
use stock_lexer::{Keyword, LexerError, Token, TokenKind};
use stock_span::{Interner, Span, Symbol};

pub struct Parser<'interner> {
    tokens: Vec<Token>,
    cursor: usize,

    ast: Ast,
    errors: Vec<ParserError>,
    interner: &'interner Interner,
}

impl<'interner> Parser<'interner> {
    pub fn new(tokens: Vec<Token>, interner: &'interner Interner) -> Self {
        assert!(!tokens.is_empty(), "parser must have at least one token");

        Parser {
            tokens,
            cursor: 0,

            ast: Ast::new(),
            errors: Vec::new(),
            interner,
        }
    }

    pub fn parse(mut self) -> (Ast, Vec<StmtId>, Vec<ParserError>) {
        let mut roots = Vec::new();

        while !self.is_at_end() {
            let kind = self.peek_kind(0);

            match kind {
                TokenKind::Keyword(_) => match self.parse_statement() {
                    Ok(stmt) => roots.push(stmt),
                    Err(_) => self.recover(),
                },

                _ => {
                    self.error(ParserErrorKind::UnexpectedToken);
                    self.recover();
                }
            }
        }

        (self.ast, roots, self.errors)
    }
}

impl Parser<'_> {
    fn error(&mut self, error: ParserErrorKind) {
        let span = self.peek(0).span;
        let error = ParserError::new(error, span);
        self.errors.push(error);
    }

    fn get_symbol(&self, token: Token) -> Symbol {
        token.symbol.expect("token should have a symbol")
    }

    fn peek(&self, offset: usize) -> Token {
        self.tokens
            .get(self.cursor + offset)
            .copied()
            .unwrap_or(*self.tokens.last().unwrap())
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

    fn recover(&mut self) {
        self.advance();

        while !self.is_at_end() {
            let kind = self.peek_kind(0);
            if matches!(
                kind,
                TokenKind::Keyword(Keyword::Let) | TokenKind::Keyword(Keyword::Return)
            ) {
                return;
            }

            self.advance();
        }
    }
}

// expression parsing, uses pratt parsing technique
impl Parser<'_> {
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
            let combined_span = Span::new(lhs_span.start, rhs_span.end);

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

                let decimal_found = self.interner.lookup(symbol).find('.').is_some();

                let literal_kind = if decimal_found {
                    LiteralKind::Float
                } else {
                    LiteralKind::Integer
                };
                Ok(self.ast.literal(literal_kind, symbol, span))
            }

            TokenKind::LParen => {
                let expr = self.parse_expression(0)?;
                self.consume(TokenKind::RParen, ParserErrorKind::ExpectedRParen)?;
                Ok(expr)
            }

            TokenKind::Minus => {
                let ((), r_bp) = self.prefix_binding_power(TokenKind::Minus);
                let rhs = self.parse_expression(r_bp)?;

                let rhs_span = self.ast.get_expr(rhs).span;
                let span = Span::new(token.span.start, rhs_span.end);

                Ok(self.ast.unary(UnaryOp::Negate, rhs, span))
            }

            TokenKind::Error(lexer_error) => {
                let kind = match lexer_error {
                    LexerError::UnexpectedCharacter => ParserErrorKind::UnexpectedCharacter,
                    LexerError::InvalidNumber => ParserErrorKind::InvalidNumber,
                };

                self.error(kind);
                Err(())
            }

            _ => {
                self.error(ParserErrorKind::UnexpectedToken);
                Err(())
            }
        }
    }

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

// statement parsing
impl Parser<'_> {
    fn parse_statement(&mut self) -> Result<StmtId, ()> {
        let kind = self.peek_kind(0);
        let keyword = if let TokenKind::Keyword(k) = kind {
            k
        } else {
            self.error(ParserErrorKind::UnexpectedToken);
            return Err(());
        };

        match keyword {
            Keyword::Let => self.parse_let_statement(),
            Keyword::Return => self.parse_return_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Result<StmtId, ()> {
        let let_token = self.advance();

        let name_token = self.consume(
            TokenKind::Identifier,
            ParserErrorKind::ExpectedIdentifierAfterLet,
        )?;
        let name_symbol = self.get_symbol(name_token);

        self.consume(
            TokenKind::Equal,
            ParserErrorKind::ExpectedEqualAfterLetIdentifier,
        )?;

        let value_expr = self.parse_expression(0)?;
        let value_node = self.ast.get_expr(value_expr);

        let (start, end) = (let_token.span.start, value_node.span.end);
        let span = Span::new(start, end);
        Ok(self.ast.let_stmt(name_symbol, value_expr, span))
    }

    fn parse_return_statement(&mut self) -> Result<StmtId, ()> {
        let return_token = self.advance();

        let value_expr = self.parse_expression(0)?;

        let span = Span::new(
            return_token.span.start,
            self.ast.get_expr(value_expr).span.end,
        );

        Ok(self.ast.return_stmt(value_expr, span))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use stock_ast::{ExprKind, StmtKind};
    use stock_lexer::Lexer;
    use stock_span::Interner;

    // small utilites to print ast nodes in a readable format
    fn tostring_ast_expr(ast: &Ast, interner: &Interner, expr_id: ExprId, depth: usize) -> String {
        let expr = ast.get_expr(expr_id);
        let indent = "  ".repeat(depth);
        let next_indent = "  ".repeat(depth + 1);

        match expr.kind {
            ExprKind::Literal { kind, value } => {
                format!("{:?}({})", kind, interner.lookup(value))
            }
            ExprKind::Binary { op, lhs, rhs } => {
                let lhs_str = tostring_ast_expr(ast, interner, lhs, depth + 1);
                let rhs_str = tostring_ast_expr(ast, interner, rhs, depth + 1);
                format!(
                    "{:?} {{\n{}lhs: {},\n{}rhs: {}\n{}}}",
                    op, next_indent, lhs_str, next_indent, rhs_str, indent
                )
            }
            ExprKind::Unary { op, rhs } => {
                let rhs_str = tostring_ast_expr(ast, interner, rhs, depth + 1);
                format!("{:?}({})", op, rhs_str)
            }
        }
    }

    fn tostring_ast_stmt(ast: &Ast, interner: &Interner, stmt_id: StmtId, depth: usize) -> String {
        let stmt = ast.get_stmt(stmt_id);
        let indent = "  ".repeat(depth);
        let next_indent = "  ".repeat(depth + 1);

        match stmt.kind {
            StmtKind::Let { name, value } => {
                let value_str = tostring_ast_expr(ast, interner, value, depth + 1);
                format!(
                    "Let {{\n{}name: {},\n{}value: {}\n{}}}",
                    next_indent,
                    interner.lookup(name),
                    next_indent,
                    value_str,
                    indent
                )
            }
            StmtKind::Return { value } => {
                let value_str = tostring_ast_expr(ast, interner, value, depth + 1);
                format!("Return({})", value_str)
            }
        }
    }

    #[test]
    fn playground() {
        let source = "let x = 10 + 5 return 2";

        let mut interner = Interner::new();
        let tokens = Lexer::lex(source, &mut interner);

        let parser = Parser::new(tokens, &interner);
        let (ast, roots, errors) = parser.parse();

        if !errors.is_empty() {
            for error in errors {
                println!("{:?}", error);
            }
        } else {
            for stmt_id in roots {
                println!("{}", tostring_ast_stmt(&ast, &interner, stmt_id, 0))
            }
        }
    }
}
