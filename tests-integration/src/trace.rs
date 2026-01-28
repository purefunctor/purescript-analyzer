//! Tracing capture utilities for tests.

use std::fs;
use std::io::{BufWriter, Write};
use std::path::PathBuf;
use std::sync::{Arc, Mutex};

use tracing::Level;
use tracing_subscriber::EnvFilter;
use tracing_subscriber::fmt::MakeWriter;
use tracing_subscriber::fmt::format::FmtSpan;
use tracing_subscriber::layer::SubscriberExt;

#[derive(Clone)]
struct FileWriter(Arc<Mutex<BufWriter<fs::File>>>);

impl Write for FileWriter {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.0.lock().unwrap().write(buf)
    }
    fn flush(&mut self) -> std::io::Result<()> {
        self.0.lock().unwrap().flush()
    }
}

impl<'a> MakeWriter<'a> for FileWriter {
    type Writer = Self;
    fn make_writer(&'a self) -> Self::Writer {
        self.clone()
    }
}

/// Writes trace output to a file at the given level.
/// Pass `env!("CARGO_TARGET_TMPDIR")` from test code as `target`.
pub fn with_file_trace<F, T>(level: Level, target: &str, test_name: &str, f: F) -> (T, PathBuf)
where
    F: FnOnce() -> T,
{
    let directory =
        PathBuf::from(target).parent().expect("no parent directory").join("compiler-tracing");

    fs::create_dir_all(&directory).expect("failed to create trace directory");

    let log_file = directory.join(format!("{}.jsonl", test_name));
    let log_handle = fs::File::create(&log_file).expect("failed to create trace file");
    let file_writer = FileWriter(Arc::new(Mutex::new(BufWriter::new(log_handle))));
    let file_writer_ref = file_writer.clone();

    let formatter = tracing_subscriber::fmt::layer()
        .with_writer(file_writer_ref)
        .with_span_events(FmtSpan::CLOSE)
        .json();

    let filter = EnvFilter::default().add_directive(level.into());
    let subscriber = tracing_subscriber::registry().with(formatter).with(filter);

    let result = tracing::subscriber::with_default(subscriber, f);
    file_writer.0.lock().unwrap().flush().expect("failed to flush trace file");

    (result, log_file)
}
