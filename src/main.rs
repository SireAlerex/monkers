use std::io::{self, BufReader};

mod interpreter;
mod cli;

fn main() {
    cli::repl::start(BufReader::new(io::stdin()), io::stdout());
}
