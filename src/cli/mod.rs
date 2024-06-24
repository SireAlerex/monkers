use std::io::{self, BufReader};

use clap::Parser;

pub mod repl;

#[derive(Debug, Parser)]
struct Args {
    file: Option<String>,
}

pub fn start() {
    let args = Args::parse();

    match args.file {
        Some(file) => {
            if let Err(e) = repl::read(file, io::stdout()) {
                println!("Error: {e}");
            }
        }
        None => repl::start(BufReader::new(io::stdin()), io::stdout()),
    }
}
