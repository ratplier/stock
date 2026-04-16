use crate::span::Span;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Symbol(pub u32);

#[derive(Debug, Default)]
pub struct Interner {
    store: Vec<String>,
    map: HashMap<String, Symbol>,
}

impl Interner {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn intern(&mut self, bytes: &[u8]) -> Symbol {
        let string_slice = unsafe { std::str::from_utf8_unchecked(bytes) };

        if let Some(&symbol) = self.map.get(string_slice) {
            return symbol;
        }

        let symbol = Symbol(self.store.len() as u32);
        let owned_string = string_slice.to_string();

        self.store.push(owned_string.clone());
        self.map.insert(owned_string, symbol);

        symbol
    }

    pub fn intern_source(&mut self, source: &[u8], span: Span) -> Symbol {
        self.intern(span.read(source))
    }

    pub fn resolve(&self, symbol: Symbol) -> &str {
        &self.store[symbol.0 as usize]
    }
}
