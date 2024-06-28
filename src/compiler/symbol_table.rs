use std::{borrow::Borrow, cell::RefCell, collections::HashMap, rc::Rc};

use crate::interpreter::{ast::Ident, evaluator::builtin::BUILTINS};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Scope {
    Global,
    Local,
    Builtin,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Symbol {
    name: Ident,
    pub scope: Scope,
    index: usize,
}

impl Symbol {
    pub const fn index(&self) -> usize {
        self.index
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SymbolTable {
    pub outer: Option<Rc<RefCell<SymbolTable>>>,
    store: HashMap<String, Symbol>,
    pub num_definitions: usize,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            outer: None,
            store: HashMap::new(),
            num_definitions: 0,
        }
    }

    pub fn new_with_builtins() -> Self {
        let mut table = Self::new();

        for (idx, name, _) in BUILTINS {
            table.define_builtin((*name).to_string(), *idx);
        }

        table
    }

    pub fn new_enclosed(outer: Rc<RefCell<Self>>) -> Self {
        Self {
            outer: Some(outer),
            store: HashMap::new(),
            num_definitions: 0,
        }
    }

    pub fn define(&mut self, name: String) -> Symbol {
        let scope = match self.outer {
            Some(_) => Scope::Local,
            None => Scope::Global,
        };
        let symbol = Symbol {
            name: name.clone(),
            scope,
            index: self.num_definitions,
        };
        self.store.insert(name, symbol.clone());
        self.num_definitions += 1;
        symbol
    }

    pub fn define_builtin(&mut self, name: String, idx: usize) -> Symbol {
        let symbol = Symbol {
            name: name.clone(),
            scope: Scope::Builtin,
            index: idx,
        };
        self.store.insert(name, symbol.clone());
        symbol
    }

    pub fn resolve(&self, name: &str) -> Option<Symbol> {
        self.store.get(name).map_or_else(
            || {
                self.outer.as_ref().map_or_else(
                    || None,
                    |table| {
                        let table: &RefCell<Self> = table.borrow();
                        table.borrow().resolve(name)
                    },
                )
            },
            |sym| Some(sym.clone()),
        )
    }
}

#[cfg(test)]
mod test {
    use std::{cell::RefCell, collections::HashMap, rc::Rc};

    use super::{Scope, Symbol, SymbolTable};

    #[test]
    fn builtins_test() {
        let global = SymbolTable::new();

        let glob = Rc::new(RefCell::new(global));
        let first_local = SymbolTable::new_enclosed(Rc::clone(&glob));

        let first = Rc::new(RefCell::new(first_local));
        let second_local = SymbolTable::new_enclosed(Rc::clone(&first));

        let mut expected = HashMap::new();
        expected.insert(
            "a".to_owned(),
            Symbol {
                name: "a".to_owned(),
                scope: Scope::Builtin,
                index: 0,
            },
        );
        expected.insert(
            "c".to_owned(),
            Symbol {
                name: "c".to_owned(),
                scope: Scope::Builtin,
                index: 1,
            },
        );
        expected.insert(
            "e".to_owned(),
            Symbol {
                name: "e".to_owned(),
                scope: Scope::Builtin,
                index: 2,
            },
        );
        expected.insert(
            "f".to_owned(),
            Symbol {
                name: "f".to_owned(),
                scope: Scope::Builtin,
                index: 3,
            },
        );

        // for (idx, (k, _)) in expected.iter().enumerate() {
        //     let mut table = glob.borrow_mut();
        //     table.define_builtin(k.to_owned(), idx);
        // }
        {
            let mut table = glob.borrow_mut();
            table.define_builtin("a".to_owned(), 0);
            table.define_builtin("c".to_owned(), 1);
            table.define_builtin("e".to_owned(), 2);
            table.define_builtin("f".to_owned(), 3);
        }

        for table in &[glob, first, Rc::new(RefCell::new(second_local))] {
            for sym in expected.clone() {
                let res = table.borrow().resolve(&sym.0);
                assert!(res.is_some());
                assert_eq!(res.unwrap(), sym.1);
            }
        }
    }

    #[test]
    fn resolve_nested_local_test() {
        let mut global = SymbolTable::new();
        global.define("a".to_owned());
        global.define("b".to_owned());

        let glob = Rc::new(RefCell::new(global));
        // global;
        let mut first_local = SymbolTable::new_enclosed(Rc::clone(&glob));
        first_local.define("c".to_owned());
        first_local.define("d".to_owned());

        let first = Rc::new(RefCell::new(first_local));
        let mut second_local = SymbolTable::new_enclosed(Rc::clone(&first));
        second_local.define("e".to_owned());
        second_local.define("f".to_owned());

        let mut expected_first = HashMap::new();
        expected_first.insert(
            "a".to_owned(),
            Symbol {
                name: "a".to_owned(),
                scope: Scope::Global,
                index: 0,
            },
        );
        expected_first.insert(
            "b".to_owned(),
            Symbol {
                name: "b".to_owned(),
                scope: Scope::Global,
                index: 1,
            },
        );
        expected_first.insert(
            "c".to_owned(),
            Symbol {
                name: "c".to_owned(),
                scope: Scope::Local,
                index: 0,
            },
        );
        expected_first.insert(
            "d".to_owned(),
            Symbol {
                name: "d".to_owned(),
                scope: Scope::Local,
                index: 1,
            },
        );

        let mut expected_second = HashMap::new();
        expected_second.insert(
            "a".to_owned(),
            Symbol {
                name: "a".to_owned(),
                scope: Scope::Global,
                index: 0,
            },
        );
        expected_second.insert(
            "b".to_owned(),
            Symbol {
                name: "b".to_owned(),
                scope: Scope::Global,
                index: 1,
            },
        );
        expected_second.insert(
            "e".to_owned(),
            Symbol {
                name: "e".to_owned(),
                scope: Scope::Local,
                index: 0,
            },
        );
        expected_second.insert(
            "f".to_owned(),
            Symbol {
                name: "f".to_owned(),
                scope: Scope::Local,
                index: 1,
            },
        );

        let first_local = first.borrow();
        for (key, sym) in expected_first.iter() {
            let result = first_local.resolve(key);
            assert!(result.is_some());
            assert_eq!(result.unwrap(), *sym);
        }

        for (key, sym) in expected_second.iter() {
            let result = second_local.resolve(key);
            assert!(result.is_some());
            assert_eq!(result.unwrap(), *sym);
        }
    }

    #[test]
    fn resolve_local_test() {
        let mut global = SymbolTable::new();
        global.define("a".to_owned());
        global.define("b".to_owned());

        let glob = Rc::new(RefCell::new(global));
        // global;
        let mut local = SymbolTable::new_enclosed(Rc::clone(&glob));
        local.define("c".to_owned());
        local.define("d".to_owned());

        let mut expected = HashMap::new();
        expected.insert(
            "a".to_owned(),
            Symbol {
                name: "a".to_owned(),
                scope: Scope::Global,
                index: 0,
            },
        );
        expected.insert(
            "b".to_owned(),
            Symbol {
                name: "b".to_owned(),
                scope: Scope::Global,
                index: 1,
            },
        );
        expected.insert(
            "c".to_owned(),
            Symbol {
                name: "c".to_owned(),
                scope: Scope::Local,
                index: 0,
            },
        );
        expected.insert(
            "d".to_owned(),
            Symbol {
                name: "d".to_owned(),
                scope: Scope::Local,
                index: 1,
            },
        );

        for (key, sym) in expected.iter() {
            let result = local.resolve(key);
            assert!(result.is_some());
            assert_eq!(result.unwrap(), *sym);
        }
    }

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

        expected.insert(
            "c".to_owned(),
            Symbol {
                name: "c".to_owned(),
                scope: Scope::Local,
                index: 0,
            },
        );
        expected.insert(
            "d".to_owned(),
            Symbol {
                name: "d".to_owned(),
                scope: Scope::Local,
                index: 1,
            },
        );

        expected.insert(
            "e".to_owned(),
            Symbol {
                name: "e".to_owned(),
                scope: Scope::Local,
                index: 0,
            },
        );
        expected.insert(
            "f".to_owned(),
            Symbol {
                name: "f".to_owned(),
                scope: Scope::Local,
                index: 1,
            },
        );

        let mut global = SymbolTable::new();

        let a = global.define("a".to_owned());
        assert_eq!(a, *expected.get("a").unwrap());

        let b = global.define("b".to_owned());
        assert_eq!(b, *expected.get("b").unwrap());

        let global = Rc::new(RefCell::new(global));
        let mut first_local = SymbolTable::new_enclosed(Rc::clone(&global));
        assert_eq!(
            first_local.define("c".to_owned()),
            *expected.get("c").unwrap()
        );
        assert_eq!(
            first_local.define("d".to_owned()),
            *expected.get("d").unwrap()
        );

        let first_local = Rc::new(RefCell::new(first_local));
        let mut second_local = SymbolTable::new_enclosed(Rc::clone(&first_local));
        assert_eq!(
            second_local.define("e".to_owned()),
            *expected.get("e").unwrap()
        );
        assert_eq!(
            second_local.define("f".to_owned()),
            *expected.get("f").unwrap()
        );
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
            assert_eq!(global.resolve(&sym.name).unwrap(), *sym);
        }
    }
}
