use std::collections::HashMap;

use crate::interpreter::ast::Ident;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Scope {
    Global,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Symbol {
    name: Ident,
    scope: Scope,
    index: usize,
}

impl Symbol {
    pub const fn index(&self) -> usize {
        self.index
    }
}

#[derive(Debug, Clone)]
pub struct SymbolTable {
    store: HashMap<String, Symbol>,
    num_definitions: usize,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            store: HashMap::new(),
            num_definitions: 0,
        }
    }

    pub fn define(&mut self, name: String) -> Symbol {
        let symbol = Symbol {
            name: name.clone(),
            scope: Scope::Global,
            index: self.num_definitions,
        };
        self.store.insert(name, symbol.clone());
        self.num_definitions += 1;
        symbol
    }

    pub fn resolve(&self, name: &str) -> Option<&Symbol> {
        self.store.get(name)
    }
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use super::{Symbol, SymbolTable};

    #[test]
    fn define_test() {
        let mut expected: HashMap<String, Symbol> = HashMap::new();
        expected.insert(
            "a".to_owned(),
            Symbol {
                name: "a".to_owned(),
                scope: super::Scope::Global,
                index: 0,
            },
        );
        expected.insert(
            "b".to_owned(),
            Symbol {
                name: "b".to_owned(),
                scope: super::Scope::Global,
                index: 1,
            },
        );

        let mut global = SymbolTable::new();

        let a = global.define("a".to_owned());
        assert_eq!(a, *expected.get("a").unwrap());

        let b = global.define("b".to_owned());
        assert_eq!(b, *expected.get("b").unwrap());
    }

    #[test]
    fn resolve_test() {
        let mut global = SymbolTable::new();
        global.define("a".to_owned());
        global.define("b".to_owned());

        let expected = &[
            Symbol {
                name: "a".to_owned(),
                scope: super::Scope::Global,
                index: 0,
            },
            Symbol {
                name: "b".to_owned(),
                scope: super::Scope::Global,
                index: 1,
            },
        ];

        for sym in expected {
            assert!(global.resolve(&sym.name).is_some());
            assert_eq!(global.resolve(&sym.name).unwrap(), sym);
        }
    }
}
