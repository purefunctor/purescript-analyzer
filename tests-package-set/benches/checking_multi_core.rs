use std::sync::Arc;
use std::time::Duration;

use criterion::{BatchSize, Criterion, criterion_group, criterion_main};
use tests_package_set::{
    acme_source_files, build_warmed_engine, core_source_files, load_sources, probe_iter_time,
    run_multi_core,
};

fn criterion_benchmark(c: &mut Criterion) {
    let core_sources = load_sources(core_source_files());
    let acme_sources = load_sources(acme_source_files());

    let core_probe = probe_iter_time(&core_sources, run_multi_core);
    let acme_probe = probe_iter_time(&acme_sources, run_multi_core);
    let measurement = (core_probe.max(acme_probe) * 12).max(Duration::from_secs(60));

    let mut g = c.benchmark_group("checking-multi-core");
    g.sample_size(10);
    g.measurement_time(measurement);

    for (label, sources) in [("core", &core_sources), ("acme", &acme_sources)] {
        let owned = Arc::clone(sources);
        let name = format!("check-{label}");
        g.bench_function(name.as_str(), move |b| {
            b.iter_batched(
                || build_warmed_engine(&owned),
                |warmed| run_multi_core(&warmed),
                BatchSize::PerIteration,
            )
        });
    }
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
