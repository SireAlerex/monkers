pub mod cli;
mod compiler;
mod interpreter;
mod utils;

pub use interpreter::evaluator::object::{Object, Function, Closure, CompiledFunction};
pub use interpreter::ast::Literal;
pub use interpreter::evaluator::builtin::BuiltinFunction;
pub use utils::fmt_duration;
