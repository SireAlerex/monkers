use std::io::{self, BufReader};

use clap::Parser;

#[allow(dead_code)]
pub mod repl;

#[derive(Debug, Parser)]
struct Args {
    file: Option<String>,
    #[arg(long)]
    tree_walk: bool,
    #[arg(short, long)]
    debug: bool
}

pub fn start() {
    let args = Args::parse();

    if let Err(e) = match args.file {
        Some(file) => repl::read(file, io::stdout(), args.tree_walk, args.debug),
        None => repl::start(BufReader::new(io::stdin()), io::stdout(), args.tree_walk, args.debug),
    } {
        println!("Error: {e}");
    }
}
