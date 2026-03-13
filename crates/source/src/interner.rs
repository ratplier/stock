use crate::span::Span;
use lasso::{Rodeo, Spur};

pub type Symbol = Spur;

pub struct Interner {
    rodeo: Rodeo,
}

impl Interner {
    pub fn new() -> Self {
        Self {
            rodeo: Rodeo::new(),
        }
    }

    pub fn intern(&mut self, bytes: &[u8]) -> Symbol {
        let string = unsafe { std::str::from_utf8_unchecked(bytes) };
        self.rodeo.get_or_intern(string)
    }

    pub fn intern_source(&mut self, source: &[u8], span: Span) -> Symbol {
        self.intern(span.read(source))
    }

    pub fn resolve(&self, symbol: Symbol) -> &str {
        self.rodeo.resolve(&symbol)
    }
}

impl Default for Interner {
    fn default() -> Self {
        Self::new()
    }
}
