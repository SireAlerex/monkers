use std::io::{self, BufReader};

use interpreter::{lexer::Lexer, parser::Parser, token::Source};

mod cli;
mod interpreter;

fn main() {
    cli::repl::start(BufReader::new(io::stdin()), io::stdout());
    let mut parser = Parser::new(Lexer::new("foo;", Source::REPL));
    parser.parse_program();
}
