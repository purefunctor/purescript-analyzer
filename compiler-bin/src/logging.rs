use std::path::PathBuf;
use std::sync::Arc;
use std::time::Instant;
use std::{env, fs};

use tracing::level_filters::LevelFilter;
use tracing_subscriber::layer::SubscriberExt;
use tracing_subscriber::{Layer, Registry, filter, fmt};

use crate::cli;

struct SpanTimingLayer;

impl<S> tracing_subscriber::Layer<S> for SpanTimingLayer
where
    S: tracing::Subscriber + for<'lookup> tracing_subscriber::registry::LookupSpan<'lookup>,
{
    fn on_enter(&self, id: &tracing::span::Id, ctx: tracing_subscriber::layer::Context<'_, S>) {
        if let Some(span) = ctx.span(id) {
            if span.extensions().get::<Instant>().is_none() {
                span.extensions_mut().insert(Instant::now());
            }
        }
    }

    fn on_close(&self, id: tracing::span::Id, ctx: tracing_subscriber::layer::Context<'_, S>) {
        if let Some(span) = ctx.span(&id)
            && let Some(start) = span.extensions().get::<Instant>()
            && ctx.enabled(span.metadata())
        {
            let duration = start.elapsed();
            let name = span.name();
            tracing::info!(target: "meta", span = name, span.duration = ?duration);
        }
    }
}

pub fn temporary_log_file() -> PathBuf {
    let temporary_directory = env::temp_dir();
    temporary_directory.join("purescript-analyzer.log")
}

pub fn start(config: Arc<cli::Config>) {
    let path = temporary_log_file();
    let file = fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(path)
        .expect("Failed to open log file");

    let fmt_filter = filter::Targets::new()
        .with_target("building::engine", config.query_log)
        .with_target("purescript_analyzer::lsp", config.lsp_log)
        .with_default(LevelFilter::INFO);
    let fmt = fmt::layer().with_writer(file).with_filter(fmt_filter);

    let timing_filter = filter::Targets::new()
        .with_target("purescript_analyzer::lsp", config.lsp_log)
        .with_default(LevelFilter::OFF);
    let timing = SpanTimingLayer.with_filter(timing_filter);

    let subscriber = Registry::default().with(fmt).with(timing);
    tracing::subscriber::set_global_default(subscriber).unwrap();
}
