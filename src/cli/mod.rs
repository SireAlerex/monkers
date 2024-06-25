use std::io::{self, BufReader};

use clap::Parser;

#[allow(dead_code)]
pub mod repl;

#[derive(Debug, Parser)]
struct Args {
    file: Option<String>,
}

pub fn start() {
    let args = Args::parse();

    if let Err(e) = match args.file {
        Some(file) => repl::read(file, io::stdout()),
        None => repl::start(BufReader::new(io::stdin()), io::stdout()),
    } {
        println!("Error: {e}");
    }
}
