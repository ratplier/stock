use crate::{lexer::Lexer, parser::Parser};

use stock_ast::{AstArena, AstExpr, AstStmt, BinaryOp, ExprId, StmtId, UnaryOp};
use stock_diagnostics::DiagnosticSink;
use stock_source::{Interner, Span, Symbol};

struct TestEnv {
    arena: AstArena,
    interner: Interner,
    sink: DiagnosticSink,
}

impl TestEnv {
    fn new() -> Self {
        Self {
            arena: AstArena::new(),
            interner: Interner::new(),
            sink: DiagnosticSink::new(),
        }
    }

    fn parse_expr(&mut self, source: &str) -> ExprId {
        let lexer = Lexer::new(source.as_bytes());
        let mut parser = Parser::new(lexer, &mut self.interner, &mut self.sink, &mut self.arena);

        let expr = parser.parse_expr();
        if expr.is_none() || self.sink.error_count() > 0 {
            panic!("parsed with errors: {:#?}", self.sink.drain());
        }

        expr.unwrap()
    }

    fn parse_stmt(&mut self, source: &str) -> StmtId {
        let lexer = Lexer::new(source.as_bytes());
        let mut parser = Parser::new(lexer, &mut self.interner, &mut self.sink, &mut self.arena);

        let stmt = parser.parse_stmt();
        if stmt.is_none() || self.sink.error_count() > 0 {
            panic!("parsed with errors: {:#?}", self.sink.drain());
        }

        stmt.unwrap()
    }

    fn parse_program(&mut self, source: &str) -> Vec<StmtId> {
        let lexer = Lexer::new(source.as_bytes());
        let mut parser = Parser::new(lexer, &mut self.interner, &mut self.sink, &mut self.arena);
        parser.parse_program()
    }

    fn assert_err(&mut self, source: &str) {
        let lexer = Lexer::new(source.as_bytes());
        let mut parser = Parser::new(lexer, &mut self.interner, &mut self.sink, &mut self.arena);

        let _ = parser.parse_program();
        assert!(
            self.sink.error_count() > 0,
            "expected errors but found none: '{}'",
            source
        );
    }

    fn assert_error_count(&mut self, source: &str, expected: u32) {
        let lexer = Lexer::new(source.as_bytes());
        let mut parser = Parser::new(lexer, &mut self.interner, &mut self.sink, &mut self.arena);

        let _ = parser.parse_program();
        let count = self.sink.error_count();
        let diags = self.sink.drain();

        assert_eq!(
            count, expected,
            "source '{}': expected {} errors, got {}: {:#?}",
            source, expected, count, diags
        );
    }

    fn resolve(&self, sym: Symbol) -> &str {
        self.interner.resolve(sym)
    }

    fn get_expr(&self, id: ExprId) -> &AstExpr {
        self.arena.get_expr(id)
    }

    fn get_stmt(&self, id: StmtId) -> &AstStmt {
        self.arena.get_stmt(id)
    }

    fn get_expr_span(&self, id: ExprId) -> Span {
        self.arena.get_expr_span(id)
    }
}

macro_rules! assert_match {
    ($value:expr, $pattern:pat => $body:expr) => {
        match $value {
            $pattern => $body,
            _ => panic!(
                "assertion failed: `{:?}` does not match `{}`",
                $value,
                stringify!($pattern)
            ),
        }
    };
}

#[test]
fn playground() {
    let mut env = TestEnv::new();
    let src = r"{
        let x = 400 * a() + 200;
        let y = 300 * b() + 100;
    }";

    env.parse_stmt(src);
    println!("{:?}", env.sink.drain());
}

// --- literals ---

#[test]
fn test_integer_zero() {
    let mut env = TestEnv::new();

    let id = env.parse_expr("0");
    assert_match!(env.get_expr(id), AstExpr::Integer(s) => assert_eq!(env.resolve(*s), "0"));
}

#[test]
fn test_integer_large() {
    let mut env = TestEnv::new();

    let id = env.parse_expr("123456789");
    assert_match!(env.get_expr(id), AstExpr::Integer(s) => assert_eq!(env.resolve(*s), "123456789"));
}

#[test]
fn test_float_zero() {
    let mut env = TestEnv::new();

    let id = env.parse_expr("0.0");
    assert_match!(env.get_expr(id), AstExpr::Float(s) => assert_eq!(env.resolve(*s), "0.0"));
}

#[test]
fn test_float_decimal() {
    let mut env = TestEnv::new();

    let id = env.parse_expr("1.5");
    assert_match!(env.get_expr(id), AstExpr::Float(s) => assert_eq!(env.resolve(*s), "1.5"));
}

#[test]
fn test_identifier_simple() {
    let mut env = TestEnv::new();

    let id = env.parse_expr("x");
    assert_match!(env.get_expr(id), AstExpr::Identifier(s) => assert_eq!(env.resolve(*s), "x"));
}

#[test]
fn test_identifier_multiword() {
    let mut env = TestEnv::new();

    let id = env.parse_expr("hello_world");
    assert_match!(env.get_expr(id), AstExpr::Identifier(s) => assert_eq!(env.resolve(*s), "hello_world"));
}

// --- binary operators ---

#[test]
fn test_binary_add() {
    let mut env = TestEnv::new();

    let id = env.parse_expr("1 + 2");
    assert_match!(env.get_expr(id), AstExpr::Binary { op: BinaryOp::Add, lhs, rhs } => {
        assert_match!(env.get_expr(*lhs), AstExpr::Integer(s) => assert_eq!(env.resolve(*s), "1"));
        assert_match!(env.get_expr(*rhs), AstExpr::Integer(s) => assert_eq!(env.resolve(*s), "2"));
    });
}

#[test]
fn test_binary_subtract() {
    let mut env = TestEnv::new();

    let id = env.parse_expr("5 - 3");
    assert_match!(env.get_expr(id), AstExpr::Binary { op: BinaryOp::Subtract, .. } => {});
}

#[test]
fn test_binary_multiply() {
    let mut env = TestEnv::new();

    let id = env.parse_expr("4 * 6");
    assert_match!(env.get_expr(id), AstExpr::Binary { op: BinaryOp::Multiply, .. } => {});
}

#[test]
fn test_binary_divide() {
    let mut env = TestEnv::new();

    let id = env.parse_expr("8 / 2");
    assert_match!(env.get_expr(id), AstExpr::Binary { op: BinaryOp::Divide, .. } => {});
}

// --- unary operators ---

#[test]
fn test_unary_negate() {
    let mut env = TestEnv::new();

    let id = env.parse_expr("-x");
    assert_match!(env.get_expr(id), AstExpr::Unary { op: UnaryOp::Negate, operand } => {
        assert_match!(env.get_expr(*operand), AstExpr::Identifier(..) => {});
    });
}

#[test]
fn test_unary_not() {
    let mut env = TestEnv::new();

    let id = env.parse_expr("!flag");
    assert_match!(env.get_expr(id), AstExpr::Unary { op: UnaryOp::Not, operand } => {
        assert_match!(env.get_expr(*operand), AstExpr::Identifier(..) => {});
    });
}

// --- precedence & associativity ---

#[test]
fn test_precedence_mul_before_add() {
    let mut env = TestEnv::new();

    // 1 + 2 * 3  =>  Add(1, Mul(2, 3))
    let id = env.parse_expr("1 + 2 * 3");

    assert_match!(env.get_expr(id), AstExpr::Binary { op: BinaryOp::Add, lhs, rhs } => {
        assert_match!(env.get_expr(*lhs), AstExpr::Integer(s) => assert_eq!(env.resolve(*s), "1"));
        assert_match!(env.get_expr(*rhs), AstExpr::Binary { op: BinaryOp::Multiply, .. } => {});
    });
}

#[test]
fn test_precedence_mul_before_add_reversed() {
    let mut env = TestEnv::new();

    // 2 * 3 + 1  =>  Add(Mul(2, 3), 1)
    let id = env.parse_expr("2 * 3 + 1");

    assert_match!(env.get_expr(id), AstExpr::Binary { op: BinaryOp::Add, lhs, rhs } => {
        assert_match!(env.get_expr(*lhs), AstExpr::Binary { op: BinaryOp::Multiply, .. } => {});
        assert_match!(env.get_expr(*rhs), AstExpr::Integer(s) => assert_eq!(env.resolve(*s), "1"));
    });
}

#[test]
fn test_precedence_div_before_sub() {
    let mut env = TestEnv::new();

    // 10 / 2 - 1  =>  Sub(Div(10, 2), 1)
    let id = env.parse_expr("10 / 2 - 1");

    assert_match!(env.get_expr(id), AstExpr::Binary { op: BinaryOp::Subtract, lhs, .. } => {
        assert_match!(env.get_expr(*lhs), AstExpr::Binary { op: BinaryOp::Divide, .. } => {});
    });
}

#[test]
fn test_left_associativity_subtract() {
    let mut env = TestEnv::new();

    // 1 - 2 - 3  =>  Sub(Sub(1, 2), 3)
    let id = env.parse_expr("1 - 2 - 3");

    assert_match!(env.get_expr(id), AstExpr::Binary { op: BinaryOp::Subtract, lhs, rhs } => {
        assert_match!(env.get_expr(*lhs), AstExpr::Binary { op: BinaryOp::Subtract, .. } => {});
        assert_match!(env.get_expr(*rhs), AstExpr::Integer(s) => assert_eq!(env.resolve(*s), "3"));
    });
}

#[test]
fn test_left_associativity_divide() {
    let mut env = TestEnv::new();

    // 8 / 4 / 2  =>  Div(Div(8, 4), 2)
    let id = env.parse_expr("8 / 4 / 2");

    assert_match!(env.get_expr(id), AstExpr::Binary { op: BinaryOp::Divide, lhs, .. } => {
        assert_match!(env.get_expr(*lhs), AstExpr::Binary { op: BinaryOp::Divide, .. } => {});
    });
}

#[test]
fn test_grouping_overrides_precedence_left() {
    let mut env = TestEnv::new();

    // (1 + 2) * 3  =>  Mul(Add(1, 2), 3)
    let id = env.parse_expr("(1 + 2) * 3");

    assert_match!(env.get_expr(id), AstExpr::Binary { op: BinaryOp::Multiply, lhs, rhs } => {
        assert_match!(env.get_expr(*lhs), AstExpr::Binary { op: BinaryOp::Add, .. } => {});
        assert_match!(env.get_expr(*rhs), AstExpr::Integer(s) => assert_eq!(env.resolve(*s), "3"));
    });
}

#[test]
fn test_grouping_overrides_precedence_right() {
    let mut env = TestEnv::new();

    // 3 * (1 + 2)  =>  Mul(3, Add(1, 2))
    let id = env.parse_expr("3 * (1 + 2)");

    assert_match!(env.get_expr(id), AstExpr::Binary { op: BinaryOp::Multiply, lhs, rhs } => {
        assert_match!(env.get_expr(*lhs), AstExpr::Integer(s) => assert_eq!(env.resolve(*s), "3"));
        assert_match!(env.get_expr(*rhs), AstExpr::Binary { op: BinaryOp::Add, .. } => {});
    });
}

// --- calls ---

#[test]
fn test_call_single_arg() {
    let mut env = TestEnv::new();

    let id = env.parse_expr("f(42)");
    assert_match!(env.get_expr(id), AstExpr::Call { callee, args } => {
        assert_eq!(args.len(), 1);

        assert_match!(env.get_expr(*callee), AstExpr::Identifier(s) => assert_eq!(env.resolve(*s), "f"));
        assert_match!(env.get_expr(args[0]), AstExpr::Integer(s) => assert_eq!(env.resolve(*s), "42"));
    });
}

#[test]
fn test_call_multiple_args() {
    let mut env = TestEnv::new();

    let id = env.parse_expr("add(1, 2, 3)");
    assert_match!(env.get_expr(id), AstExpr::Call { args, .. } => {
        assert_eq!(args.len(), 3);

        assert_match!(env.get_expr(args[0]), AstExpr::Integer(s) => assert_eq!(env.resolve(*s), "1"));
        assert_match!(env.get_expr(args[1]), AstExpr::Integer(s) => assert_eq!(env.resolve(*s), "2"));
        assert_match!(env.get_expr(args[2]), AstExpr::Integer(s) => assert_eq!(env.resolve(*s), "3"));
    });
}

#[test]
fn test_call_with_expr_args() {
    let mut env = TestEnv::new();

    // nested calls
    let id = env.parse_expr("f(1 + 2, g(x))");

    assert_match!(env.get_expr(id), AstExpr::Call { args, .. } => {
        assert_eq!(args.len(), 2);

        assert_match!(env.get_expr(args[0]), AstExpr::Binary { op: BinaryOp::Add, .. } => {});
        assert_match!(env.get_expr(args[1]), AstExpr::Call { .. } => {});
    });
}

#[test]
fn test_call_result_used_in_binary() {
    let mut env = TestEnv::new();

    let id = env.parse_expr("f() + g()");
    assert_match!(env.get_expr(id), AstExpr::Binary { op: BinaryOp::Add, lhs, rhs } => {
        assert_match!(env.get_expr(*lhs), AstExpr::Call { .. } => {});
        assert_match!(env.get_expr(*rhs), AstExpr::Call { .. } => {});
    });
}

// --- blocks ---

#[test]
fn test_block_empty() {
    let mut env = TestEnv::new();

    let id = env.parse_expr("{}");
    assert_match!(env.get_expr(id), AstExpr::Block(block) => {
        assert!(block.stmts.is_empty());
    });
}

#[test]
fn test_block_with_tail() {
    let mut env = TestEnv::new();

    let id = env.parse_expr("{ 42 }");
    assert_match!(env.get_expr(id), AstExpr::Block(block) => {
        assert_eq!(block.stmts.len(), 1);

        assert_match!(env.get_stmt(block.stmts[0]), AstStmt::Expr { has_semicolon, .. } => assert!(!*has_semicolon));
    });
}

#[test]
fn test_block_multiple_stmts() {
    let mut env = TestEnv::new();

    let id = env.parse_expr("{ let x = 1; let y = 2; x }");
    assert_match!(env.get_expr(id), AstExpr::Block(block) => {
        assert_eq!(block.stmts.len(), 3);

        assert_match!(env.get_stmt(block.stmts[0]), AstStmt::Let { .. } => {});
        assert_match!(env.get_stmt(block.stmts[1]), AstStmt::Let { .. } => {});

        assert_match!(env.get_stmt(block.stmts[2]), AstStmt::Expr { has_semicolon, .. } => {
            assert!(!*has_semicolon);
        });
    });
}

#[test]
fn test_block_expr_stmts() {
    let mut env = TestEnv::new();

    // first two have semicolons, last is tail
    let id = env.parse_expr("{ 1; 2; 3 }");

    assert_match!(env.get_expr(id), AstExpr::Block(block) => {
        assert_eq!(block.stmts.len(), 3);

        assert_match!(env.get_stmt(block.stmts[0]), AstStmt::Expr { has_semicolon, .. } => assert!(*has_semicolon));
        assert_match!(env.get_stmt(block.stmts[1]), AstStmt::Expr { has_semicolon, .. } => assert!(*has_semicolon));
        assert_match!(env.get_stmt(block.stmts[2]), AstStmt::Expr { has_semicolon, .. } => assert!(!*has_semicolon));
    });
}

#[test]
fn test_block_nested() {
    let mut env = TestEnv::new();

    let id = env.parse_expr("{ { 1 } }");
    assert_match!(env.get_expr(id), AstExpr::Block(outer) => {
        assert_eq!(outer.stmts.len(), 1);

        assert_match!(env.get_stmt(outer.stmts[0]), AstStmt::Expr { expr, .. } => {
            assert_match!(env.get_expr(*expr), AstExpr::Block(inner) => {
                assert_eq!(inner.stmts.len(), 1);
            });
        });
    });
}

#[test]
fn test_block_in_let() {
    let mut env = TestEnv::new();

    let id = env.parse_stmt("let x = { 1 + 2 };");
    assert_match!(env.get_stmt(id), AstStmt::Let { name, value } => {
        assert_eq!(env.resolve(*name), "x");
        assert_match!(env.get_expr(*value), AstExpr::Block(..) => {});
    });
}

// --- statements ---

#[test]
fn test_let_simple() {
    let mut env = TestEnv::new();

    let id = env.parse_stmt("let n = 0;");
    assert_match!(env.get_stmt(id), AstStmt::Let { name, value } => {
        assert_eq!(env.resolve(*name), "n");
        assert_match!(env.get_expr(*value), AstExpr::Integer(s) => assert_eq!(env.resolve(*s), "0"));
    });
}

#[test]
fn test_let_complex() {
    let mut env = TestEnv::new();

    let id = env.parse_stmt("let result = f(1 + 2) * 3;");
    assert_match!(env.get_stmt(id), AstStmt::Let { name, value } => {
        assert_eq!(env.resolve(*name), "result");
        assert_match!(env.get_expr(*value), AstExpr::Binary { op: BinaryOp::Multiply, lhs, .. } => {
            assert_match!(env.get_expr(*lhs), AstExpr::Call { .. } => {});
        });
    });
}

#[test]
fn test_expr_stmt_with_semicolon() {
    let mut env = TestEnv::new();

    let id = env.parse_stmt("f();");
    assert_match!(env.get_stmt(id), AstStmt::Expr { expr, has_semicolon } => {
        assert_match!(env.get_expr(*expr), AstExpr::Call { .. } => {});
        assert!(*has_semicolon);
    });
}

// --- programs ---

#[test]
fn test_program_empty() {
    let mut env = TestEnv::new();
    let stmts = env.parse_program("");

    assert!(stmts.is_empty());
}

#[test]
fn test_program_multiple_let() {
    let mut env = TestEnv::new();
    let stmts = env.parse_program("let x = 1; let y = 2; let z = 3;");

    assert_eq!(stmts.len(), 3);

    for id in &stmts {
        assert_match!(env.get_stmt(*id), AstStmt::Let { .. } => {});
    }
}

#[test]
fn test_program_mixed_stmts() {
    let mut env = TestEnv::new();
    let stmts = env.parse_program("let x = 1; f(x); { let y = 2; }");

    assert_eq!(stmts.len(), 3);

    assert_match!(env.get_stmt(stmts[0]), AstStmt::Let { .. } => {});
    assert_match!(env.get_stmt(stmts[1]), AstStmt::Expr { .. } => {});
    assert_match!(env.get_stmt(stmts[2]), AstStmt::Expr { expr, .. } => {
        assert_match!(env.get_expr(*expr), AstExpr::Block(..) => {});
    });
}

// --- spans ---

#[test]
fn test_span_single_token() {
    let mut env = TestEnv::new();

    let id = env.parse_expr("42");
    let span = env.get_expr_span(id);

    assert_eq!(span.start, 0);
    assert_eq!(span.end, 2);
}

#[test]
fn test_span_binary_expr() {
    let mut env = TestEnv::new();

    // "1 + 2" -> span 0..5
    let id = env.parse_expr("1 + 2");
    let span = env.get_expr_span(id);

    assert_eq!(span.start, 0);
    assert_eq!(span.end, 5);
}

#[test]
fn test_span_unary_expr() {
    let mut env = TestEnv::new();

    // "-x" -> span 0..2
    let id = env.parse_expr("-x");
    let span = env.get_expr_span(id);

    assert_eq!(span.start, 0);
    assert_eq!(span.end, 2);
}

// --- errors ---

#[test]
fn test_error_missing_let_eq() {
    let mut env = TestEnv::new();
    env.assert_err("let x 5;");
}

#[test]
fn test_error_missing_let_identifier() {
    let mut env = TestEnv::new();
    env.assert_err("let = 5;");
}

#[test]
fn test_error_missing_let_value() {
    let mut env = TestEnv::new();
    env.assert_err("let x =;");
}

#[test]
fn test_error_empty_grouping() {
    let mut env = TestEnv::new();
    env.assert_err("()");
}

#[test]
fn test_error_unclosed_block() {
    let mut env = TestEnv::new();
    env.assert_err("{ 1 + 2");
}

#[test]
fn test_error_expr_stmt_missing_semicolon() {
    let mut env = TestEnv::new();
    env.assert_err("a()");
}

#[test]
fn test_error_non_unary_op() {
    let mut env = TestEnv::new();
    env.assert_err("1 + + 2");
}

#[test]
fn test_error_recovery_continues_parsing() {
    let mut env = TestEnv::new();

    // first let fails, synchronize to second let and parse it
    let stmts = env.parse_program("let x = +; let y = 1;");

    assert_eq!(stmts.len(), 1);
    assert!(env.sink.error_count() == 1);
}

#[test]
fn test_error_count_single() {
    let mut env = TestEnv::new();
    env.assert_error_count("let x = 5", 1);
}

#[test]
fn test_interning_same_symbol() {
    let mut env = TestEnv::new();

    // same identifier resolves to same Symbol
    let a = env.parse_expr("foo");
    let b = env.parse_expr("foo");

    assert_match!(env.get_expr(a), AstExpr::Identifier(s1) =>
        assert_match!(env.get_expr(b), AstExpr::Identifier(s2) =>
            assert_eq!(s1, s2)
        )
    );
}

#[test]
fn test_interning_different_symbol() {
    let mut env = TestEnv::new();

    // different identifiers resolve to different Symbols
    let a = env.parse_expr("foo");
    let b = env.parse_expr("bar");

    assert_match!(env.get_expr(a), AstExpr::Identifier(s1) =>
        assert_match!(env.get_expr(b), AstExpr::Identifier(s2) =>
            assert_ne!(s1, s2)
        )
    );
}
