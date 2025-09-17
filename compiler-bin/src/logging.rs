use std::{env, fs, path::PathBuf, time::Instant};

use tracing::level_filters::LevelFilter;
use tracing_subscriber::{Layer, Registry, layer::SubscriberExt};

struct SpanTimingLayer;

impl<S> tracing_subscriber::Layer<S> for SpanTimingLayer
where
    S: tracing::Subscriber + for<'lookup> tracing_subscriber::registry::LookupSpan<'lookup>,
{
    fn on_enter(&self, id: &tracing::span::Id, ctx: tracing_subscriber::layer::Context<'_, S>) {
        if let Some(span) = ctx.span(id) {
            span.extensions_mut().insert(Instant::now());
        }
    }

    fn on_close(&self, id: tracing::span::Id, ctx: tracing_subscriber::layer::Context<'_, S>) {
        if let Some(span) = ctx.span(&id)
            && let Some(start) = span.extensions().get::<Instant>()
        {
            let duration = start.elapsed();
            let name = span.name();
            tracing::info!(duration = format!("{duration:?}"), "{name}");
        }
    }
}

pub fn temporary_log_file() -> PathBuf {
    let temporary_directory = env::temp_dir();
    temporary_directory.join("purescript-analyzer.log")
}

pub fn start() {
    let path = temporary_log_file();
    let file = fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(path)
        .expect("Failed to open log file");

    let fmt = tracing_subscriber::fmt::layer().with_writer(file).with_filter(LevelFilter::INFO);
    let subscriber = Registry::default().with(fmt).with(SpanTimingLayer);
    tracing::subscriber::set_global_default(subscriber).unwrap();
}
