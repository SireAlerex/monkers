use mimalloc::MiMalloc;
use monkers::{cli, fmt_duration};

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

use std::time::Instant;

fn main() {
    let start = Instant::now();
    cli::start();
    let end = start.elapsed();
    println!("Program execution time: {}", fmt_duration(end));
}
