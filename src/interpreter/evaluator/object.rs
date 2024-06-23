use std::{
    cell::RefCell,
    fmt::{Debug, Display},
    ops::{Add, Div, Mul, Sub},
    rc::Rc,
};

macro_rules! error {
    ($($arg:tt)*) => {
        {
            Object::Error(format!($($arg)*))
        }
    };
}

macro_rules! null {
    () => {
        Object::Null
    };
}

pub(crate) use error;
pub(crate) use null;

use crate::interpreter::ast::{Block, Ident};

use super::env::Environment;

#[derive(Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    String(String),
    Returned(Box<Object>),
    Null,
    Error(String),
    Function {
        parameters: Vec<Ident>,
        body: Block,
        env: Rc<RefCell<Environment>>,
    },
}

macro_rules! op {
    ($self: ident, $right: ident, $op: tt, $op_str: literal, $enum: ident) => {
        match ($self, $right) {
            (Object::Integer(a), Object::Integer(b)) => Object::$enum(a $op b),
            (left, right) => Object::op_error(&left, &right, $op_str)
        }
    };
}

macro_rules! int_op {
    ($self: ident, $right: ident, $op: tt, $op_str: literal) => {
        op!($self, $right, $op, $op_str, Integer)
    };
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

impl Sub<Object> for Object {
    type Output = Object;

    fn sub(self, rhs: Object) -> Self::Output {
        int_op!(self, rhs, -, "-")
    }
}

impl Mul<Object> for Object {
    type Output = Object;

    fn mul(self, rhs: Object) -> Self::Output {
        int_op!(self, rhs, *, "*")
    }
}

impl Div<Object> for Object {
    type Output = Object;

    fn div(self, rhs: Object) -> Self::Output {
        int_op!(self, rhs, /, "/")
    }
}

impl Object {
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
            Object::Returned(obj) => format!("RETURNED({})", obj.get_type()),
            Object::Null => String::from("NULL"),
            Object::Function { .. } => String::from("FUNCTION"),
            Object::Error(_) => String::from("ERROR"),
        }
    }

    pub fn is_error(&self) -> bool {
        matches!(self, Self::Error(_))
    }
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Integer(l0), Self::Integer(r0)) => l0 == r0,
            (Self::Boolean(l0), Self::Boolean(r0)) => l0 == r0,
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            (Self::Returned(l0), Self::Returned(r0)) => l0 == r0,
            (Self::Returned(left), right) => **left == *right,
            (left, Self::Returned(right)) => *left == **right,
            (
                Self::Function {
                    parameters: parameters_left,
                    body: body_left,
                    env: environment_left,
                },
                Self::Function {
                    parameters: parameters_right,
                    body: body_right,
                    env: environment_right,
                },
            ) => {
                environment_left == environment_right
                    && parameters_left == parameters_right
                    && body_left == body_right
            }
            (Self::Error(left), Self::Error(right)) => left == right,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Integer(x) => write!(f, "{x}"),
            Self::Boolean(bool) => write!(f, "{bool}"),
            Self::String(s) => write!(f, "{s}"),
            Self::Null => f.write_str("null"),
            Self::Returned(obj) => write!(f, "{obj}"),
            Self::Function {
                parameters, body, ..
            } => write!(f, "fn({}) {{\n{}\n}}", parameters.join(", "), body),
            Self::Error(s) => write!(f, "{s}"),
        }
    }
}

impl Debug for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Integer(arg0) => f.debug_tuple("Integer").field(arg0).finish(),
            Self::Boolean(arg0) => f.debug_tuple("Boolean").field(arg0).finish(),
            Self::String(arg0) => f.debug_tuple("String").field(arg0).finish(),
            Self::Returned(arg0) => f.debug_tuple("Returned").field(arg0).finish(),
            Self::Null => write!(f, "Null"),
            Self::Error(arg0) => f.debug_tuple("Error").field(arg0).finish(),
            Self::Function {
                parameters, body, ..
            } => f
                .debug_struct("Function")
                .field("parameters", parameters)
                .field("body", body)
                .finish(),
        }
    }
}
