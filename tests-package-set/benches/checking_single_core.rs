use std::sync::Arc;
use std::time::Duration;

use criterion::{BatchSize, Criterion, criterion_group, criterion_main};
use tests_package_set::{
    acme_source_files, build_warmed_engine, core_source_files, load_sources, run_single_core,
};

const MEASUREMENT_TIME: Duration = Duration::from_secs(60);

fn criterion_benchmark(c: &mut Criterion) {
    let core_sources = load_sources(core_source_files());
    let acme_sources = load_sources(acme_source_files());

    let mut g = c.benchmark_group("checking-single-core");
    g.sample_size(10);
    g.measurement_time(MEASUREMENT_TIME);

    for (label, sources) in [("core", &core_sources), ("acme", &acme_sources)] {
        let sources = Arc::clone(sources);
        let name = format!("check-{label}");
        g.bench_function(name.as_str(), move |b| {
            b.iter_batched(
                || build_warmed_engine(&sources),
                |warmed| run_single_core(&warmed),
                BatchSize::PerIteration,
            )
        });
    }
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
