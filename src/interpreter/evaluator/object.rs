use std::{
    cell::RefCell,
    fmt::{Debug, Display},
    ops::{Add, Div, Mul, Sub},
    rc::Rc,
};

use crate::{
    interpreter::ast::{Block, Ident},
    utils,
};

use super::{builtin::BuiltinFunction, env::Environment};

macro_rules! error {
    ($($arg:tt)*) => {
        Object::Error(format!($($arg)*))
    };
}

macro_rules! null {
    () => {
        Object::Null
    };
}

macro_rules! int_op {
    ($trait: tt, $func: tt, $op: tt, $op_str: literal) => {
        impl $trait<Object> for Object {
            type Output = Object;

            fn $func(self, rhs: Object) -> Self::Output {
                match (self, rhs) {
                    (Object::Integer(a), Object::Integer(b)) => Object::Integer(a $op b),
                    (left, right) => Object::op_error(&left, &right, $op_str)
                }
            }
        }

    };
}

pub(crate) use error;
pub(crate) use null;

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    String(String),
    Array(Vec<Object>),
    Returned(Box<Object>),
    Null,
    Error(String),
    Function(Function),
    Builtin(BuiltinFunction),
}

impl Add<Object> for Object {
    type Output = Object;

    fn add(self, rhs: Object) -> Self::Output {
        match (self, rhs) {
            (Object::Integer(a), Object::Integer(b)) => Object::Integer(a + b),
            (Object::String(a), Object::String(b)) => Object::String(a + &b),
            (left, right) => Object::op_error(&left, &right, "+"),
        }
    }
}

int_op!(Sub, sub, -, "-");
int_op!(Mul, mul, *, "*");
int_op!(Div, div, /, "/");

impl Object {
    pub fn function(parameters: Vec<Ident>, body: Block, env: Rc<RefCell<Environment>>) -> Self {
        Object::Function(Function::new(parameters, body, env))
    }

    pub fn get(&self, index: &Object) -> Object {
        match (self, index) {
            (Object::Array(array), Object::Integer(i)) => {
                if *i < 0 {
                    return null!();
                }
                match TryInto::<usize>::try_into(*i) {
                    Ok(idx) => {
                        if idx >= array.len() {
                            return null!();
                        }
                        println!("idx:{idx} max:{}", array.len());

                        array[idx].clone()
                    }
                    Err(err) => error!("size can't be a 64bit integer ({err})"),
                }
            }
            (_, _) => error!(
                "index operation not supported: {}[{}]",
                self.get_type(),
                index.get_type()
            ),
        }
    }

    pub(crate) fn less(self, right: Object) -> Object {
        match (self, right) {
            (Object::Integer(a), Object::Integer(b)) => Object::Boolean(a < b),
            (Object::String(a), Object::String(b)) => Object::Boolean(a < b),
            (left, right) => Object::op_error(&left, &right, "<"),
        }
    }

    pub(crate) fn greater(self, right: Object) -> Object {
        match (self, right) {
            (Object::Integer(a), Object::Integer(b)) => Object::Boolean(a > b),
            (Object::String(a), Object::String(b)) => Object::Boolean(a > b),
            (left, right) => Object::op_error(&left, &right, ">"),
        }
    }

    fn op_error(left: &Object, right: &Object, op: &str) -> Object {
        if core::mem::discriminant(left) == core::mem::discriminant(right) {
            error!(
                "unknown operator: {} {op} {}",
                left.get_type(),
                right.get_type()
            )
        } else {
            error!(
                "type mismatch: {} {op} {}",
                left.get_type(),
                right.get_type()
            )
        }
    }

    pub(crate) fn is_truthy(&self) -> bool {
        match self {
            Object::Boolean(b) => *b,
            Object::Null => false,
            _ => true,
        }
    }

    pub fn get_type(&self) -> String {
        match self {
            Object::Integer(_) => String::from("INTEGER"),
            Object::Boolean(_) => String::from("BOOLEAN"),
            Object::String(_) => String::from("STRING"),
            Object::Array(_) => String::from("ARRAY"),
            Object::Returned(obj) => format!("RETURNED({})", obj.get_type()),
            Object::Null => String::from("NULL"),
            Object::Function { .. } => String::from("FUNCTION"),
            Object::Builtin(_) => String::from("BUILTIN_FUNCTION"),
            Object::Error(_) => String::from("ERROR"),
        }
    }

    pub fn is_error(&self) -> bool {
        matches!(self, Self::Error(_))
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Integer(x) => write!(f, "{x}"),
            Self::Boolean(bool) => write!(f, "{bool}"),
            Self::String(s) => write!(f, "{s}"),
            Self::Array(array) => write!(f, "{}", utils::join(array)),
            Self::Null => f.write_str("null"),
            Self::Returned(obj) => write!(f, "{obj}"),
            Self::Function(function) => write!(f, "{function}"),
            Self::Builtin(func) => write!(f, "builtin({func:?})"),
            Self::Error(s) => write!(f, "{s}"),
        }
    }
}

#[derive(Clone, PartialEq)]
pub struct Function {
    pub parameters: Vec<Ident>,
    pub body: Block,
    pub env: Rc<RefCell<Environment>>,
}

impl Function {
    pub fn new(parameters: Vec<Ident>, body: Block, env: Rc<RefCell<Environment>>) -> Self {
        Function {
            parameters,
            body,
            env,
        }
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "fn({}) {{\n{}\n}}",
            self.parameters.join(", "),
            self.body
        )
    }
}

impl Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Function")
            .field("parameters", &self.parameters)
            .field("body", &self.body)
            .finish()
    }
}
