pub mod cli;
mod compiler;
mod interpreter;
mod utils;

pub use utils::fmt_duration;
pub use interpreter::evaluator::object::Object;