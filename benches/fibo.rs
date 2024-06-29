use std::io::stdout;

use criterion::{criterion_group, criterion_main, Criterion};
use monkers::{cli::repl, Object};

#[inline]
fn fibonacci(file: &str) -> Object {
    repl::read(file.to_string(), stdout(), false, false).unwrap()
}

pub fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("fib 30", |b| b.iter(|| fibonacci(".//scripts/fibo30")));
}

criterion_group! {
    name = benches;
    config = Criterion::default().sample_size(20);
    targets = criterion_benchmark
}
criterion_main!(benches);
