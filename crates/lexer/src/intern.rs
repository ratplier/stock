use stock_span::Interner;
use crate::token::Token;

pub fn intern(interner: &mut Interner, source: &str, tokens: &mut Vec<Token>) {
    for token in tokens {
        if let Some(_) = token.symbol {
            continue;
        }

        let symbol = interner.intern(
            token.span.read(source)
        );

        token.symbol = Some(symbol);
    };
}