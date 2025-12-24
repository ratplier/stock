use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Symbol(pub u32);

pub struct Interner {
    store: Vec<String>,
    map: HashMap<String, Symbol>,
}

impl Interner {
    pub fn new() -> Self {
        Interner {
            store: Vec::new(),
            map: HashMap::new(),
        }
    }

    pub fn intern(&mut self, name: &str) -> Symbol {
        if let Some(&id) = self.map.get(name) {
            return id;
        }

        let id = Symbol(self.store.len() as u32);
        let owned_string = name.to_string();

        self.store.push(owned_string.clone());
        self.map.insert(owned_string, id);

        id
    }

    pub fn lookup(&self, symbol: Symbol) -> &str {
        &self.store[symbol.0 as usize]
    }
}
