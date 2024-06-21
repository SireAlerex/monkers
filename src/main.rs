use std::io::{self, BufReader};

mod token;
mod lexer;
mod repl;

fn main() {
    repl::repl::start(BufReader::new(io::stdin()), io::stdout());
}
