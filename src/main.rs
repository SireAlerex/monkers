use mimalloc::MiMalloc;
use monkers::cli;

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

fn main() {
    cli::start();
}
