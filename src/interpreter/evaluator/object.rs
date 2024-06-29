use std::{
    cell::RefCell,
    collections::HashMap,
    fmt::{Debug, Display},
    ops::{Add, Div, Mul, Sub},
    rc::Rc,
};

use crate::{
    compiler::chunk::Instructions,
    interpreter::ast::{Block, Ident, Literal},
    utils,
};

use super::{builtin::BuiltinFunction, env::Environment};

macro_rules! error {
    ($($arg:tt)*) => {
        Object::Error(format!($($arg)*))
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

// TODO: try reduce type size with indirection
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Object {
    Integer(i64),                       //  8
    Boolean(bool),                      //  1
    String(String),                     // 24
    Hash(Box<HashMap<Literal, Object>>),//  8
    Array(Vec<Object>),                 // 24
    Returned(Box<Object>),              //  8
    Null,                               //  0
    Uninit,                             //  0
    Error(String),                      // 24
    Function(Box<Function>),            //  8
    CompiledFunction(CompiledFunction), // 40
    Closure(Closure),                   // 32
    Builtin(BuiltinFunction),           //  8
}

pub const NULL: Object = Object::Null;
pub const UNINT_OBJECT: Object = Object::Uninit;

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
        Self::Function(Box::new(Function::new(parameters, body, env)))
    }

    pub fn get(&self, index: &Self) -> Self {
        match (self, index) {
            (Self::Array(array), Self::Integer(i)) => {
                if *i < 0 || *i >= array.len() as i64 {
                    return NULL;
                }
                array[*i as usize].clone()
            }
            (Self::Hash(hash), _) => index.literal().map_or_else(
                || error!("unusable as hash key: {}", index.get_type()),
                |key| hash.get(&key).map_or_else(|| NULL, Clone::clone),
            ),
            (_, _) => error!(
                "index operation not supported: {}[{}]",
                self.get_type(),
                index.get_type()
            ),
        }
    }

    pub fn get_unchecked_key(&self, index: &Self) -> Self {
        match (self, index) {
            (Self::Array(array), Self::Integer(i)) => {
                if *i < 0 || *i >= array.len() as i64 {
                    return NULL;
                }
                array[*i as usize].clone()
            }
            (Self::Hash(hash), _) => hash
                .get(&Literal::try_from(index).unwrap())
                .map_or_else(|| NULL, Clone::clone),
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
        println!("op_error left={left:?} right={right:?} op={op:?}");
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
            Self::CompiledFunction { .. } => String::from("COMPILED_FUNCTION"),
            Self::Closure(_) => String::from("CLOSURE"),
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
            Self::CompiledFunction(func) => write!(f, "{func}"),
            Self::Closure(closure) => write!(f, "{closure}"),
            Self::Builtin(func) => write!(f, "builtin({func:?})"),
        }
    }
}

impl From<i64> for Object {
    fn from(value: i64) -> Self {
        Self::Integer(value)
    }
}

impl From<bool> for Object {
    fn from(value: bool) -> Self {
        Self::Boolean(value)
    }
}

impl From<String> for Object {
    fn from(value: String) -> Self {
        Self::String(value)
    }
}

impl From<&str> for Object {
    fn from(value: &str) -> Self {
        Self::String(value.to_owned())
    }
}

impl<T: Into<Self>> From<Vec<T>> for Object {
    fn from(value: Vec<T>) -> Self {
        Self::Array(value.into_iter().map(|elem| elem.into()).collect())
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompiledFunction {
    pub instructions: Instructions,
    pub locals_count: usize,
    pub parameters_count: usize,
}

impl Display for CompiledFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "CompiledFunction({} locals, {} parameters, code:\n{}",
            self.locals_count, self.parameters_count, self.instructions
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Closure {
    pub func: Box<CompiledFunction>,
    pub free_variables: Vec<Object>,
}

impl Closure {
    pub fn from_instructions(instructions: Instructions) -> Self {
        Self {
            func: Box::new(CompiledFunction {
                instructions,
                locals_count: 0,
                parameters_count: 0,
            }),
            free_variables: Vec::new(),
        }
    }
}

impl Display for Closure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Closure(free variables: ({:?})\n{}",
            self.free_variables, self.func
        )
    }
}
