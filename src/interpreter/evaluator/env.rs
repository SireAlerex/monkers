use std::{cell::RefCell, collections::HashMap, rc::Rc};

use super::object::Object;

#[derive(Debug, Clone, PartialEq)]
pub struct Environment {
    store: HashMap<String, Object>,
    outer: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            store: HashMap::new(),
            outer: None,
        }
    }

    pub fn enclosed_env(outer: Rc<RefCell<Self>>) -> Self {
        let mut env = Self::new();
        env.outer = Some(outer);

        env
    }

    pub fn get(&self, name: &String) -> Option<Object> {
        self.store.get(name).map_or_else(
            || {
                self.outer
                    .as_ref()
                    .map_or_else(|| None, |env| env.borrow().get(name))
            },
            |obj| Some(obj.clone()),
        )
    }

    pub fn set(&mut self, name: String, val: Object) -> Object {
        self.store.insert(name, val.clone());
        val
    }
}
