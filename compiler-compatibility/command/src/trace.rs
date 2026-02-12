//! Tracing setup with dynamically switchable file output for per-package logs.

use std::fs::{self, File};
use std::io::{self, BufWriter, Write};
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};
use std::time::{SystemTime, UNIX_EPOCH};

use tracing_subscriber::Layer;
use tracing_subscriber::filter::{LevelFilter, Targets};
use tracing_subscriber::fmt::MakeWriter;
use tracing_subscriber::fmt::format::FmtSpan;
use tracing_subscriber::layer::SubscriberExt;
use tracing_tree::HierarchicalLayer;

struct RouterState {
    writer: Option<BufWriter<File>>,
    current_path: Option<PathBuf>,
}

#[derive(Clone)]
struct CheckingLogsRouter {
    state: Arc<Mutex<RouterState>>,
}

struct CheckingLogsWriter {
    state: Arc<Mutex<RouterState>>,
}

impl CheckingLogsRouter {
    fn new() -> CheckingLogsRouter {
        CheckingLogsRouter {
            state: Arc::new(Mutex::new(RouterState { writer: None, current_path: None })),
        }
    }

    fn set_writer(&self, writer: BufWriter<File>, path: PathBuf) -> io::Result<()> {
        let mut state = self.state.lock().unwrap();
        if let Some(w) = state.writer.as_mut() {
            w.flush()?;
        }
        state.writer = Some(writer);
        state.current_path = Some(path);
        Ok(())
    }

    fn clear(&self) -> io::Result<()> {
        let mut state = self.state.lock().unwrap();
        if let Some(w) = state.writer.as_mut() {
            w.flush()?;
        }
        state.writer = None;
        state.current_path = None;
        Ok(())
    }
}

impl Write for CheckingLogsWriter {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        let mut state = self.state.lock().unwrap();
        if let Some(w) = state.writer.as_mut() { w.write(buf) } else { Ok(buf.len()) }
    }

    fn flush(&mut self) -> io::Result<()> {
        let mut state = self.state.lock().unwrap();
        if let Some(w) = state.writer.as_mut() { w.flush() } else { Ok(()) }
    }
}

impl<'a> MakeWriter<'a> for CheckingLogsRouter {
    type Writer = CheckingLogsWriter;

    fn make_writer(&'a self) -> CheckingLogsWriter {
        CheckingLogsWriter { state: self.state.clone() }
    }
}

/// Handle for controlling per-package trace capture.
pub struct TracingHandle {
    router: CheckingLogsRouter,
    trace_dir: PathBuf,
}

impl TracingHandle {
    /// Begin capturing `checking` crate logs to a new file for the given package.
    ///
    /// Returns a guard that flushes and closes the file when dropped.
    pub fn begin_package(&self, package: &str) -> io::Result<PackageTraceGuard> {
        fs::create_dir_all(&self.trace_dir)?;

        let timestamp =
            SystemTime::now().duration_since(UNIX_EPOCH).expect("time before epoch").as_millis();

        let sanitized: String = package
            .chars()
            .map(|c| if c.is_ascii_alphanumeric() || c == '-' { c } else { '_' })
            .collect();

        let path = self.trace_dir.join(format!("{}_{}.jsonl", timestamp, sanitized));
        let file = File::create(&path)?;
        let writer = BufWriter::new(file);

        self.router.set_writer(writer, path.clone())?;

        Ok(PackageTraceGuard { router: self.router.clone(), path })
    }
}

/// Guard that flushes and closes the trace file on drop.
pub struct PackageTraceGuard {
    router: CheckingLogsRouter,
    path: PathBuf,
}

impl PackageTraceGuard {
    pub fn path(&self) -> &Path {
        &self.path
    }
}

impl Drop for PackageTraceGuard {
    fn drop(&mut self) {
        let _ = self.router.clear();
    }
}

/// Initialize global tracing with dual outputs:
/// - `compiler_compatibility` logs → stdout at `stdout_level`
/// - `checking` logs → per-package JSONL files at `checking_level`
///
/// Returns a handle for switching the file output between packages.
pub fn init_tracing(
    stdout_level: LevelFilter,
    checking_level: LevelFilter,
    trace_dir: PathBuf,
) -> TracingHandle {
    let router = CheckingLogsRouter::new();

    let stderr_filter = Targets::new()
        .with_target("compiler_compatibility", stdout_level)
        .with_default(LevelFilter::OFF);
    let stderr_layer = HierarchicalLayer::new(2)
        .with_writer(io::stderr)
        .with_targets(true)
        .with_indent_lines(true)
        .with_filter(stderr_filter);

    let file_filter =
        Targets::new().with_target("checking", checking_level).with_default(LevelFilter::OFF);
    let file_layer = tracing_subscriber::fmt::layer()
        .with_writer(router.clone())
        .json()
        .with_span_events(FmtSpan::CLOSE)
        .with_filter(file_filter);

    let subscriber = tracing_subscriber::registry().with(stderr_layer).with(file_layer);

    tracing::subscriber::set_global_default(subscriber)
        .expect("failed to set global tracing subscriber");

    TracingHandle { router, trace_dir }
}
