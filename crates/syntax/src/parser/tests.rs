use crate::{lexer::Lexer, parser::Parser};
use stock_ast::{AstArena, AstExpr, AstStmt, BinaryOp, ExprId, StmtId, UnaryOp};
use stock_diagnostics::DiagnosticSink;
use stock_source::{Interner, Symbol};

fn parse<U, T: Fn(&mut Parser<'_>) -> U>(
    source: &str,
    callback: T,
) -> (U, AstArena, Interner, DiagnosticSink) {
    let mut interner = Interner::new();
    let mut sink = DiagnosticSink::new();

    let lexer = Lexer::new(source.as_bytes());
    let mut parser = Parser::new(lexer, &mut interner, &mut sink);

    let result = callback(&mut parser);

    assert!(parser.sink.error_count() == 0, "{:?}", sink.drain());

    (result, parser.ast, interner, sink)
}

fn parse_expr(source: &str) -> (ExprId, AstArena, Interner) {
    let (expr, ast, interner, _) = parse(source, |parser| parser.parse_expr());

    (expr.expect("expected expression"), ast, interner)
}

fn parse_stmt(source: &str) -> (StmtId, AstArena, Interner) {
    let (stmt, ast, interner, _) = parse(source, |parser| parser.parse_stmt());

    (stmt.expect("expected statement"), ast, interner)
}

fn assert_symbol(interner: &Interner, symbol: &Symbol, expected: &str) {
    assert_eq!(interner.resolve(*symbol), expected, "expected {expected}");
}

macro_rules! assert_branch {
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
fn test_literal() {
    let (expr, ast, interner) = parse_expr("42");
    let node = ast.get_expr(expr);

    // integer
    assert_branch!(node, AstExpr::Integer(symbol) => {
        assert_symbol(&interner, symbol, "42");
    });

    let (expr, ast, interner) = parse_expr("abc");

    // boolean
    assert_branch!(ast.get_expr(expr), AstExpr::Identifier(symbol) => {
        assert_symbol(&interner, symbol, "abc");
    });
}

#[test]
fn test_fn_call() {
    let (expr, ast, interner) = parse_expr("f(1, 2)");
    let node = ast.get_expr(expr);

    assert_branch!(node, AstExpr::Call { callee, args } => {
        assert_branch!(ast.get_expr(*callee), AstExpr::Identifier(symbol) => {
            assert_symbol(&interner, symbol, "f");
        });

        assert_eq!(args.len(), 2);

        assert_branch!(ast.get_expr(args[0]), AstExpr::Integer(symbol) => {
            assert_symbol(&interner, symbol, "1");
        });

        assert_branch!(ast.get_expr(args[1]), AstExpr::Integer(symbol) => {
            assert_symbol(&interner, symbol, "2");
        });
    });
}

#[test]
fn test_binary_expr() {
    let (expr, ast, interner) = parse_expr("1 + 2");
    let node = ast.get_expr(expr);

    assert_branch!(node, AstExpr::Binary { op, lhs, rhs } => {
        assert_eq!(*op, BinaryOp::Add);

        let lhs = ast.get_expr(*lhs);
        let rhs = ast.get_expr(*rhs);

        assert_branch!(lhs, AstExpr::Integer(symbol) => {
            assert_symbol(&interner, symbol, "1");
        });

        assert_branch!(rhs, AstExpr::Integer(symbol) => {
            assert_symbol(&interner, symbol, "2");
        });
    })
}

#[test]
fn test_nested_precedence() {
    let (expr, ast, interner) = parse_expr("1 + 2 * 3");
    let node = ast.get_expr(expr);

    assert_branch!(node, AstExpr::Binary { op, lhs, rhs } => {
        assert_eq!(*op, BinaryOp::Add);

        let lhs = ast.get_expr(*lhs);
        let rhs = ast.get_expr(*rhs);

        assert_branch!(lhs, AstExpr::Integer(symbol) => {
            assert_symbol(&interner, symbol, "1");
        });

        assert_branch!(rhs, AstExpr::Binary { op, lhs, rhs } => {
            assert_eq!(*op, BinaryOp::Multiply);

            let lhs = ast.get_expr(*lhs);
            let rhs = ast.get_expr(*rhs);

            assert_branch!(lhs, AstExpr::Integer(symbol) => {
                assert_symbol(&interner, symbol, "2");
            });

            assert_branch!(rhs, AstExpr::Integer(symbol) => {
                assert_symbol(&interner, symbol, "3");
            });
        });
    })
}

#[test]
fn test_associativity() {
    let (expr, ast, interner) = parse_expr("1 + 2 + 3");
    let node = ast.get_expr(expr);

    assert_branch!(node, AstExpr::Binary { op, lhs, rhs } => {
        assert_eq!(*op, BinaryOp::Add);

        let lhs = ast.get_expr(*lhs);
        let rhs = ast.get_expr(*rhs);

        assert_branch!(lhs, AstExpr::Binary { op, lhs, rhs } => {
            assert_eq!(*op, BinaryOp::Add);

            let lhs = ast.get_expr(*lhs);
            let rhs = ast.get_expr(*rhs);

            assert_branch!(lhs, AstExpr::Integer(symbol) => {
                assert_symbol(&interner, symbol, "1");
            });

            assert_branch!(rhs, AstExpr::Integer(symbol) => {
                assert_symbol(&interner, symbol, "2");
            });
        });

        assert_branch!(rhs, AstExpr::Integer(symbol) => {
            assert_symbol(&interner, symbol, "3");
        });
    })
}

#[test]
fn test_unary_expr() {
    let (expr, ast, interner) = parse_expr("-1");
    let node = ast.get_expr(expr);

    assert_branch!(node, AstExpr::Unary { op, operand } => {
        assert_eq!(*op, UnaryOp::Negate);

        let operand = ast.get_expr(*operand);
        assert_branch!(operand, AstExpr::Integer(symbol) => {
            assert_symbol(&interner, symbol, "1");
        });
    })
}

#[test]
fn test_let_stmt() {
    let (stmt, ast, interner) = parse_stmt("let x = 1;");
    let node = ast.get_stmt(stmt);

    assert_branch!(node, AstStmt::Let { name, value } => {
        assert_symbol(&interner, name, "x");

        let value = ast.get_expr(*value);
        assert_branch!(value, AstExpr::Integer(symbol) => {
            assert_symbol(&interner, symbol, "1");
        });
    })
}
