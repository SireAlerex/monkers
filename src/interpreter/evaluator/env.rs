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

    pub fn enclosed_env(outer: Rc<RefCell<Environment>>) -> Self {
        let mut env = Self::new();
        env.outer = Some(outer);

        env
    }

    pub fn get(&self, name: &String) -> Option<Object> {
        if let Some(obj) = self.store.get(name) {
            Some(obj.clone())
        } else if let Some(ref env) = self.outer {
            env.borrow().get(name)
        } else {
            None
        }
    }

    pub fn set(&mut self, name: String, val: Object) -> Object {
        self.store.insert(name, val.clone());
        val
    }
}
