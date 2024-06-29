use std::{borrow::Borrow, cell::RefCell, collections::HashMap, rc::Rc};

use crate::interpreter::{ast::Ident, evaluator::builtin::BUILTINS};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Scope {
    Global,
    Local,
    Builtin,
    Free,
    Function,
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
    pub free_symbols: Vec<Symbol>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            outer: None,
            store: HashMap::new(),
            num_definitions: 0,
            free_symbols: Vec::new(),
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
            free_symbols: Vec::new(),
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

    pub fn define_free(&self, original: Symbol) -> Symbol {
        let symbol = Symbol {
            name: original.name.clone(),
            scope: Scope::Free,
            index: self.free_symbols.len(),
        };
        let table = (self as *const Self).cast_mut();
        unsafe {
            (*table).store.insert(original.name.clone(), symbol.clone());
            (*table).free_symbols.push(original);
        }
        symbol
    }

    pub fn define_function(&mut self, name: String) -> Symbol {
        let symbol = Symbol {
            name: name.clone(),
            scope: Scope::Function,
            index: 0,
        };
        self.store.insert(name, symbol.clone());
        symbol
    }

    pub fn resolve(&self, name: &str) -> Option<Symbol> {
        self.store.get(name).map_or_else(
            || {
                self.outer.as_ref().map_or_else(
                    // None if no enclosing table
                    || None,
                    |table| {
                        let table: &RefCell<Self> = table.borrow();
                        match table.borrow().resolve(name) {
                            Some(sym) => {
                                // return it if from global or builtin
                                if sym.scope == Scope::Global || sym.scope == Scope::Builtin {
                                    return Some(sym);
                                }

                                // otherwise it's a local from an enclosing, so it's a free variable for this scope
                                let free = self.define_free(sym);
                                Some(free)
                            }
                            // None if not in enclosing table
                            None => None,
                        }
                    },
                )
            },
            // return it if is in table
            |sym| Some(sym.clone()),
        )
    }
}

#[cfg(test)]
mod test {
    use std::{cell::RefCell, collections::HashMap, rc::Rc};

    use super::{Scope, Symbol, SymbolTable};

    #[test]
    fn shadowing_function_name_test() {
        let mut global = SymbolTable::new();
        global.define_function("a".to_owned());
        global.define("a".to_owned());

        let result = global.resolve("a");
        assert!(result.is_some());
        assert_eq!(
            result.unwrap(),
            Symbol {
                name: "a".to_string(),
                scope: Scope::Global,
                index: 0
            }
        )
    }

    #[test]
    fn function_name_test() {
        let mut global = SymbolTable::new();
        global.define_function("a".to_owned());

        let result = global.resolve("a");
        assert!(result.is_some());
        assert_eq!(
            result.unwrap(),
            Symbol {
                name: "a".to_string(),
                scope: Scope::Function,
                index: 0
            }
        )
    }

    #[test]
    fn unresovable_free_test() {
        let mut global = SymbolTable::new();
        global.define("a".to_owned());

        let glob = Rc::new(RefCell::new(global));
        let mut first_local = SymbolTable::new_enclosed(Rc::clone(&glob));
        first_local.define("c".to_owned());

        let first = Rc::new(RefCell::new(first_local));
        let mut second_local = SymbolTable::new_enclosed(Rc::clone(&first));
        second_local.define("e".to_owned());
        second_local.define("f".to_owned());

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
            "c".to_owned(),
            Symbol {
                name: "c".to_owned(),
                scope: Scope::Free,
                index: 0,
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

        for (key, sym) in expected.iter() {
            let result = second_local.resolve(key);
            assert!(result.is_some());
            assert_eq!(result.unwrap(), *sym);
        }

        for unresolvable in ["b", "d"] {
            let x = second_local.resolve(unresolvable);
            assert!(x.is_none());
        }
    }

    #[test]
    fn free_test() {
        let mut global = SymbolTable::new();
        global.define("a".to_owned());
        global.define("b".to_owned());

        let glob = Rc::new(RefCell::new(global));
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
            "c".to_owned(),
            Symbol {
                name: "c".to_owned(),
                scope: Scope::Free,
                index: 0,
            },
        );
        expected_second.insert(
            "d".to_owned(),
            Symbol {
                name: "d".to_owned(),
                scope: Scope::Free,
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

        let mut second: Vec<(String, Symbol)> = expected_second.clone().into_iter().collect();
        second.sort_by_key(|(s, _)| s.clone());
        for (key, sym) in second.iter() {
            let result = second_local.resolve(key);
            assert!(result.is_some());
            assert_eq!(result.unwrap(), *sym);
        }

        // check free symbols in table 2
        assert_eq!(
            second_local.free_symbols[0],
            Symbol {
                name: "c".to_string(),
                scope: Scope::Local,
                index: 0
            }
        );
        assert_eq!(
            second_local.free_symbols[1],
            Symbol {
                name: "d".to_string(),
                scope: Scope::Local,
                index: 1
            }
        );
    }

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
