//! Tracing capture utilities for tests.

use std::fs::{self, File};
use std::io::{BufWriter, Write};
use std::path::PathBuf;
use std::sync::{Arc, Mutex};

use tracing::Level;
use tracing_subscriber::fmt::MakeWriter;
use tracing_subscriber::fmt::format::FmtSpan;
use tracing_subscriber::layer::SubscriberExt;
use tracing_subscriber::EnvFilter;

#[derive(Clone)]
struct FileWriter(Arc<Mutex<BufWriter<File>>>);

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
/// Pass `env!("CARGO_TARGET_TMPDIR")` from test code as `target_tmp_dir`.
pub fn with_file_trace<F, T>(level: Level, target_tmp_dir: &str, test_name: &str, f: F) -> (T, PathBuf)
where
    F: FnOnce() -> T,
{
    let dir_path = PathBuf::from(target_tmp_dir)
        .parent()
        .expect("no parent directory")
        .join("compiler-tracing");

    fs::create_dir_all(&dir_path).expect("failed to create trace directory");

    let file_path = dir_path.join(format!("{}.jsonl", test_name));
    let file = File::create(&file_path).expect("failed to create trace file");
    let writer = FileWriter(Arc::new(Mutex::new(BufWriter::new(file))));
    let writer_ref = writer.clone();

    let subscriber = tracing_subscriber::registry()
        .with(
            tracing_subscriber::fmt::layer()
                .with_writer(writer_ref)
                .with_span_events(FmtSpan::CLOSE)
                .json(),
        )
        .with(EnvFilter::default().add_directive(level.into()));

    let result = tracing::subscriber::with_default(subscriber, f);
    writer.0.lock().unwrap().flush().expect("failed to flush trace file");

    (result, file_path)
}
