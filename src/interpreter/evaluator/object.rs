use std::{
    cell::RefCell,
    collections::HashMap,
    fmt::{Debug, Display},
    ops::{Add, Div, Mul, Sub},
    rc::Rc,
};

use crate::{
    interpreter::ast::{Block, Ident, Literal},
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Object {
    Integer(i64),                   // 8
    Boolean(bool),                  // 1
    String(String),                 // 24
    Hash(HashMap<Literal, Object>), // 48
    Array(Vec<Object>),             //24
    Returned(Box<Object>),          // 8
    Null,                           // 0
    Uninit,                         //0
    Error(String),                  //24
    Function(Function),             // 56
    Builtin(BuiltinFunction),       // 8
}

impl Add<Self> for Object {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Integer(a), Self::Integer(b)) => Self::Integer(a + b),
            (Self::String(a), Self::String(b)) => Self::String(a + &b),
            (left, right) => Self::op_error(&left, &right, "+"),
        }
    }
}

int_op!(Sub, sub, -, "-");
int_op!(Mul, mul, *, "*");
int_op!(Div, div, /, "/");

impl Object {
    pub fn function(parameters: Vec<Ident>, body: Block, env: Rc<RefCell<Environment>>) -> Self {
        Self::Function(Function::new(parameters, body, env))
    }

    pub fn get(&self, index: &Self) -> Self {
        match (self, index) {
            (Self::Array(array), Self::Integer(i)) => {
                if *i < 0 {
                    return null!();
                }
                match TryInto::<usize>::try_into(*i) {
                    Ok(idx) => {
                        if idx >= array.len() {
                            return null!();
                        }

                        array[idx].clone()
                    }
                    Err(err) => error!("size can't be a 64bit integer ({err})"),
                }
            }
            (Self::Hash(hash), _) => index.literal().map_or_else(
                || error!("unusable as hash key: {}", index.get_type()),
                |key| hash.get(&key).map_or_else(|| null!(), Clone::clone),
            ),
            (_, _) => error!(
                "index operation not supported: {}[{}]",
                self.get_type(),
                index.get_type()
            ),
        }
    }

    pub(crate) fn less(self, right: Self) -> Self {
        match (self, right) {
            (Self::Integer(a), Self::Integer(b)) => Self::Boolean(a < b),
            (Self::String(a), Self::String(b)) => Self::Boolean(a < b),
            (left, right) => Self::op_error(&left, &right, "<"),
        }
    }

    pub(crate) fn greater(self, right: Self) -> Self {
        match (self, right) {
            (Self::Integer(a), Self::Integer(b)) => Self::Boolean(a > b),
            (Self::String(a), Self::String(b)) => Self::Boolean(a > b),
            (left, right) => Self::op_error(&left, &right, ">"),
        }
    }

    fn op_error(left: &Self, right: &Self, op: &str) -> Self {
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

    pub(crate) const fn is_truthy(&self) -> bool {
        match self {
            Self::Boolean(b) => *b,
            Self::Null => false,
            _ => true,
        }
    }

    pub fn get_type(&self) -> String {
        match self {
            Self::Integer(_) => String::from("INTEGER"),
            Self::Boolean(_) => String::from("BOOLEAN"),
            Self::String(_) => String::from("STRING"),
            Self::Hash(_) => String::from("HASH"),
            Self::Array(_) => String::from("ARRAY"),
            Self::Returned(obj) => format!("RETURNED({})", obj.get_type()),
            Self::Null => String::from("NULL"),
            Self::Uninit => panic!("Uninit object"),
            Self::Function { .. } => String::from("FUNCTION"),
            Self::Builtin(_) => String::from("BUILTIN_FUNCTION"),
            Self::Error(_) => String::from("ERROR"),
        }
    }

    pub const fn is_error(&self) -> bool {
        matches!(self, Self::Error(_))
    }

    pub fn literal(&self) -> Option<Literal> {
        match self {
            Self::Integer(x) => Some(Literal::Int(*x)),
            Self::String(s) => Some(Literal::String(s.to_owned())),
            Self::Boolean(b) => Some(Literal::Boolean(*b)),
            _ => None,
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Integer(x) => write!(f, "{x}"),
            Self::Boolean(bool) => write!(f, "{bool}"),
            Self::String(s) | Self::Error(s) => write!(f, "{s}"),
            Self::Hash(hash) => {
                write!(
                    f,
                    "{{{}}}",
                    utils::join(hash.iter(), |(key, value)| format!("{key}: {value}"))
                )
            }
            Self::Array(array) => {
                write!(f, "[{}]", utils::join(array.iter(), ToString::to_string))
            }
            Self::Null => f.write_str("null"),
            Self::Uninit => panic!("Uninit object"),
            Self::Returned(obj) => write!(f, "{obj}"),
            Self::Function(function) => write!(f, "{function}"),
            Self::Builtin(func) => write!(f, "builtin({func:?})"),
        }
    }
}

impl From<i64> for Object {
    fn from(value: i64) -> Self {
        Object::Integer(value)
    }
}

impl From<bool> for Object {
    fn from(value: bool) -> Self {
        Object::Boolean(value)
    }
}

impl From<String> for Object {
    fn from(value: String) -> Self {
        Object::String(value)
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct Function {
    pub parameters: Vec<Ident>,
    pub body: Block,
    pub env: Rc<RefCell<Environment>>,
}

impl Function {
    pub fn new(parameters: Vec<Ident>, body: Block, env: Rc<RefCell<Environment>>) -> Self {
        Self {
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

// Not showing env to avoid recursion (an function env can point to an env containing the function)
impl Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Function")
            .field("parameters", &self.parameters)
            .field("body", &self.body)
            .finish_non_exhaustive()
    }
}
