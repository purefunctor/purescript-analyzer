//! Tracing capture utilities for tests.

use std::io::{self, Write};
use std::sync::{Arc, Mutex};

use tracing::Level;
use tracing_subscriber::fmt::MakeWriter;
use tracing_subscriber::fmt::format::FmtSpan;
use tracing_subscriber::layer::SubscriberExt;
use tracing_subscriber::EnvFilter;

/// A writer that appends to a shared buffer.
#[derive(Clone)]
struct BufferWriter(Arc<Mutex<Vec<u8>>>);

impl Write for BufferWriter {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.0.lock().unwrap().extend_from_slice(buf);
        Ok(buf.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

impl<'a> MakeWriter<'a> for BufferWriter {
    type Writer = Self;
    fn make_writer(&'a self) -> Self::Writer {
        self.clone()
    }
}

/// Captures tracing output during closure execution (human-readable format).
pub fn with_capture<F, T>(level: Level, f: F) -> (T, String)
where
    F: FnOnce() -> T,
{
    let buffer = BufferWriter(Arc::new(Mutex::new(Vec::new())));
    let buffer_ref = buffer.clone();

    let subscriber = tracing_subscriber::registry()
        .with(
            tracing_subscriber::fmt::layer()
                .with_writer(buffer_ref)
                .with_ansi(false)
                .with_target(true)
                .with_level(true)
                .with_span_events(FmtSpan::CLOSE)
                .compact(),
        )
        .with(EnvFilter::default().add_directive(level.into()));

    let result = tracing::subscriber::with_default(subscriber, f);
    let bytes = Arc::try_unwrap(buffer.0).unwrap().into_inner().unwrap();
    (result, String::from_utf8_lossy(&bytes).into_owned())
}

/// Captures tracing output in JSON format with timing information.
pub fn with_json_capture<F, T>(level: Level, f: F) -> (T, String)
where
    F: FnOnce() -> T,
{
    let buffer = BufferWriter(Arc::new(Mutex::new(Vec::new())));
    let buffer_ref = buffer.clone();

    let subscriber = tracing_subscriber::registry()
        .with(
            tracing_subscriber::fmt::layer()
                .json()
                .with_writer(buffer_ref)
                .with_target(true)
                .with_level(true)
                .with_span_events(FmtSpan::NEW | FmtSpan::CLOSE)
                .with_current_span(true)
                .with_span_list(true),
        )
        .with(EnvFilter::default().add_directive(level.into()));

    let result = tracing::subscriber::with_default(subscriber, f);
    let bytes = Arc::try_unwrap(buffer.0).unwrap().into_inner().unwrap();
    (result, String::from_utf8_lossy(&bytes).into_owned())
}
