use std::io::{self, BufReader};

mod cli;
mod interpreter;

fn main() {
    cli::repl::start(BufReader::new(io::stdin()), io::stdout());
}
