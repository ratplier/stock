use crate::{
    ast::{Ast, AstExpr, BinaryOp, ExprId, UnaryOp},
    lexer::Lexer,
    parser::Parser,
};
use stock_source::{Interner, Symbol};

fn parse(source: &str) -> (ExprId, Ast, Interner) {
    let mut interner = Interner::new();

    let mut lexer = Lexer::new(source.as_bytes());
    let tokens = {
        let mut tokens = Vec::new();

        loop {
            let token = lexer.next_token(&mut interner);
            tokens.push(token);

            if token.kind.is_eof() {
                break;
            }
        }

        tokens
    };

    let mut parser = Parser::new(&tokens);
    let expr = parser.parse_infix_expr(0);

    (expr, parser.ast, interner)
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
fn test_integer_literal() {
    let (expr, ast, interner) = parse("42");
    let node = ast.get_expr(expr);

    assert_branch!(node, AstExpr::Integer(symbol) => {
        assert_symbol(&interner, symbol, "42");
    });
}

#[test]
fn test_binary_expr() {
    let (expr, ast, interner) = parse("1 + 2");
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
    let (expr, ast, interner) = parse("1 + 2 * 3");
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
    let (expr, ast, interner) = parse("1 + 2 + 3");
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
    let (expr, ast, interner) = parse("-1");
    let node = ast.get_expr(expr);

    assert_branch!(node, AstExpr::Unary { op, operand } => {
        assert_eq!(*op, UnaryOp::Negate);

        let operand = ast.get_expr(*operand);
        assert_branch!(operand, AstExpr::Integer(symbol) => {
            assert_symbol(&interner, symbol, "1");
        });
    })
}
