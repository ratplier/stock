use crate::span::Span;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Symbol(pub u32);

impl Symbol {
    pub const EMPTY: Symbol = Symbol(0);

    pub const LET: Symbol = Symbol(1);
    pub const IF: Symbol = Symbol(2);
    pub const ELSE: Symbol = Symbol(3);
    pub const LOOP: Symbol = Symbol(4);
    pub const BREAK: Symbol = Symbol(5);

    pub fn is_keyword(self) -> bool {
        matches!(
            self,
            Symbol::LET | Symbol::IF | Symbol::ELSE | Symbol::LOOP | Symbol::BREAK
        )
    }
}

#[derive(Debug)]
pub struct Interner {
    store: Vec<String>,
    map: HashMap<String, Symbol>,
}

impl Interner {
    pub fn new() -> Self {
        let mut interner = Interner {
            store: Vec::new(),
            map: HashMap::new(),
        };

        interner.pre_intern();
        interner
    }

    fn define(&mut self, string: &str, symbol: Symbol) {
        let index = self.store.len();

        self.store.push(string.to_string());
        self.map.insert(string.to_string(), symbol);

        assert_eq!(symbol.0 as usize, index);
    }

    fn pre_intern(&mut self) {
        self.define("", Symbol::EMPTY);

        self.define("let", Symbol::LET);
        self.define("if", Symbol::IF);
        self.define("else", Symbol::ELSE);
        self.define("loop", Symbol::LOOP);
        self.define("break", Symbol::BREAK);
    }

    pub fn intern(&mut self, bytes: &[u8]) -> Symbol {
        assert!(!bytes.is_empty(), "expected non-empty bytes");

        let string_slice = std::str::from_utf8(bytes).expect("expected valid utf8");
        if let Some(&symbol) = self.map.get(string_slice) {
            return symbol;
        }

        let symbol = Symbol(self.store.len() as u32);
        self.define(string_slice, symbol);

        symbol
    }

    pub fn intern_source(&mut self, source: &[u8], span: Span) -> Symbol {
        self.intern(span.read(source))
    }

    pub fn resolve(&self, symbol: Symbol) -> &str {
        &self.store[symbol.0 as usize]
    }
}

impl Default for Interner {
    fn default() -> Self {
        Self::new()
    }
}
