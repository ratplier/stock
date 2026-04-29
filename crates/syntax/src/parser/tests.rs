use crate::{lexer::Lexer, parser::Parser};

use stock_ast::{AstArena, AstExpr, AstStmt, BinaryOp, ExprId, StmtId, UnaryOp};
use stock_diagnostics::DiagnosticSink;
use stock_source::{Interner, Symbol};

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
            panic!("parsed with errors: {:?}", self.sink.drain());
        }

        expr.unwrap()
    }

    fn parse_stmt(&mut self, source: &str) -> StmtId {
        let lexer = Lexer::new(source.as_bytes());
        let mut parser = Parser::new(lexer, &mut self.interner, &mut self.sink, &mut self.arena);

        let stmt = parser.parse_stmt();
        if stmt.is_none() || self.sink.error_count() > 0 {
            panic!("parsed with errors: {:?}", self.sink.drain());
        }

        stmt.unwrap()
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

    fn resolve(&self, sym: Symbol) -> &str {
        self.interner.resolve(sym)
    }

    fn get_expr(&self, id: ExprId) -> &AstExpr {
        self.arena.get_expr(id)
    }

    fn get_stmt(&self, id: StmtId) -> &AstStmt {
        self.arena.get_stmt(id)
    }
}

macro_rules! assert_match {
    ($value:expr, $pattern:pat => $body:expr) => {
        match $value {
            $pattern => $body,
            _ => panic!(
                "Assertion failed: `{:?}` does not match `{}`",
                $value,
                stringify!($pattern)
            ),
        }
    };
}

#[test]
fn test_primaries() {
    let mut env = TestEnv::new();

    // floats
    let id = env.parse_expr("3.1415");
    assert_match!(env.get_expr(id), AstExpr::Float(s) => assert_eq!(env.resolve(*s), "3.1415"));

    // complex identifiers (underscores/digits)
    let id = env.parse_expr("_var_123");
    assert_match!(env.get_expr(id), AstExpr::Identifier(s) => assert_eq!(env.resolve(*s), "_var_123"));

    // grouping
    let id = env.parse_expr("(42)");
    assert_match!(env.get_expr(id), AstExpr::Integer(s) => assert_eq!(env.resolve(*s), "42"));
}

#[test]
fn test_precedence_deep() {
    let mut env = TestEnv::new();

    // unary, binary, grouping
    // -(1 + 2) * 3

    let id = env.parse_expr("-(1 + 2) * 3");
    assert_match!(env.get_expr(id), AstExpr::Binary { op: BinaryOp::Multiply, lhs, rhs } => {

        // lhs -> op: -, operand: (1 + 2)
        assert_match!(env.get_expr(*lhs), AstExpr::Unary { op: UnaryOp::Negate, operand } => {
            assert_match!(env.get_expr(*operand), AstExpr::Binary { op: BinaryOp::Add, .. } => {});
        });

        // rhs -> 3
        assert_match!(env.get_expr(*rhs), AstExpr::Integer(s) => {
            assert_eq!(env.resolve(*s), "3")
        });
    });

    // right associativity
    // !!abc

    let id = env.parse_expr("!!abc");
    assert_match!(env.get_expr(id), AstExpr::Unary { op: UnaryOp::Not, operand: inner_id } => {
        assert_match!(env.get_expr(*inner_id), AstExpr::Unary { op: UnaryOp::Not, .. } => {});
    });
}

#[test]
fn test_call_variants() {
    let mut env = TestEnv::new();

    // zero arguments
    // init()

    let id = env.parse_expr("init()");
    assert_match!(env.get_expr(id), AstExpr::Call { args, .. } => {
        assert_eq!(args.len(), 0)
    });

    // nested calls
    // f(g(1))

    let id: ExprId = env.parse_expr("f(g(1))");
    assert_match!(env.get_expr(id), AstExpr::Call { args, .. } => {
        assert_match!(env.get_expr(args[0]), AstExpr::Call { .. } => {});
    });

    // chained calls
    // get_f()(1)

    let id = env.parse_expr("get_f()(1)");
    assert_match!(env.get_expr(id), AstExpr::Call { callee, .. } => {
        assert_match!(env.get_expr(*callee), AstExpr::Call { .. } => {});
    });
}

#[test]
fn test_statements() {
    let mut env = TestEnv::new();

    // expression as statement
    let id = env.parse_stmt("1 + 2;");
    assert_match!(env.get_stmt(id), AstStmt::Expr(expr_id) => {
        assert_match!(env.get_expr(*expr_id), AstExpr::Binary { .. } => {});
    });

    // reusing the same symbol
    let id = env.parse_stmt("let x = x;");
    assert_match!(env.get_stmt(id), AstStmt::Let { name, value } => {
        assert_match!(env.get_expr(*value), AstExpr::Identifier(s) => {
            assert_eq!(env.resolve(*s), "x");
            assert_eq!(*name, *s)
        });
    });
}

#[test]
fn test_parser_errors() {
    let mut env = TestEnv::new();

    // missing semicolon
    env.assert_err("let x = 5");

    // missing closing paren (TODO)
    // env.assert_err("f(1, 2");

    // expected expression but found operator
    env.assert_err("let x = +;");
}
