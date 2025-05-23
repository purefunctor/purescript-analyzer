use std::{fs, hint::black_box, sync::Arc};

use criterion::{Criterion, criterion_group, criterion_main};
use rayon::prelude::*;
use tests_package_set::all_source_files;

fn criterion_benchmark(c: &mut Criterion) {
    let mut g = c.benchmark_group("parsing");

    let files: Arc<[String]> =
        all_source_files().iter().filter_map(|file| fs::read_to_string(file).ok()).collect();

    let files = Arc::clone(&files);
    g.bench_function("parse-single-core", |b| {
        b.iter(|| {
            files.iter().for_each(|file| {
                let lexed = lexing::lex(black_box(file));
                let tokens = lexing::layout(black_box(&lexed));
                let parsed = parsing::parse(black_box(&lexed), black_box(&tokens));
                black_box(parsed);
            });
        })
    });

    let files = Arc::clone(&files);
    g.bench_function("parse-multi-core", |b| {
        b.iter(|| {
            files.par_iter().for_each(|file| {
                let lexed = lexing::lex(black_box(file));
                let tokens = lexing::layout(black_box(&lexed));
                let parsed = parsing::parse(black_box(&lexed), black_box(&tokens));
                black_box(parsed);
            });
        })
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
