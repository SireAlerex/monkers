pub mod cli;
mod compiler;
mod interpreter;
mod utils;

pub use interpreter::evaluator::object::Object;
pub use utils::fmt_duration;
