mod compat;
mod error;
mod layout;
mod loader;
mod report;
mod repositories;
mod resolver;
mod storage;
mod trace;
mod types;
mod unpacker;

use std::collections::BTreeMap;
use std::path::PathBuf;

use clap::Parser;
use registry::{FsRegistry, RegistryReader};
use tracing::level_filters::LevelFilter;

use crate::report::{Classification, CompatReport, PackageReport, ReportSummary};

#[derive(Parser, Debug)]
#[command(name = "compiler-compatibility")]
#[command(about = "Fetch PureScript packages for compatibility testing")]
struct Cli {
    #[arg(help = "Package names to fetch and unpack")]
    packages: Vec<String>,

    #[arg(long, help = "Check all packages in the package set")]
    all: bool,

    #[arg(long, help = "Use specific package set version (default: latest)")]
    package_set: Option<String>,

    #[arg(long, help = "List available package set versions")]
    list_sets: bool,

    #[arg(long, help = "Update local registry repos (git pull)")]
    update: bool,

    #[arg(long, default_value = "target/compiler-compatibility", help = "Output directory")]
    output: PathBuf,

    #[arg(long, default_value = "target/compiler-tracing", help = "Trace output directory")]
    trace_output: PathBuf,

    #[arg(long, help = "Disable tarball caching")]
    no_cache: bool,

    #[arg(short, long, help = "Verbose output")]
    verbose: bool,

    #[arg(long, help = "Suppress per-file diagnostic output")]
    quiet: bool,

    #[arg(long, value_name = "PATH", help = "Write JSON report to path")]
    report_json: Option<PathBuf>,

    #[arg(
        long,
        value_name = "LevelFilter",
        default_value = "off",
        help = "Log level for checking crate traces"
    )]
    log_level: LevelFilter,
}

fn main() -> error::Result<()> {
    let cli = Cli::parse();

    if cli.all && !cli.packages.is_empty() {
        return Err(error::CompatError::Other(
            "Cannot specify both --all and package names".to_string(),
        ));
    }

    let stdout_level = if cli.verbose { LevelFilter::DEBUG } else { LevelFilter::INFO };
    let trace_output = cli.trace_output.clone();
    let tracing_handle = trace::init_tracing(stdout_level, cli.log_level, trace_output);

    let layout = layout::Layout::new(&cli.output);

    repositories::ensure_repositories(&layout.registry, cli.update)?;

    let reader = FsRegistry::new(layout.registry.clone());

    if cli.list_sets {
        let sets = reader.list_package_sets()?;
        for set in sets {
            println!("{}", set);
        }
        return Ok(());
    }

    if !cli.all && cli.packages.is_empty() {
        println!(
            "No packages specified. Use --all or provide package names. Use --help for usage."
        );
        return Ok(());
    }

    let package_set = reader.read_package_set(cli.package_set.as_deref())?;
    tracing::info!(target: "compiler_compatibility", version = %package_set.version, "Using package set");

    if cli.all {
        run_all_mode(&cli, &package_set, &reader, &layout)?;
    } else {
        run_packages_mode(&cli, &package_set, &reader, &layout, &tracing_handle)?;
    }

    Ok(())
}

fn run_all_mode(
    cli: &Cli,
    package_set: &registry::PackageSet,
    reader: &impl RegistryReader,
    layout: &layout::Layout,
) -> error::Result<()> {
    let resolved = resolver::resolve_all(package_set, reader)?;
    tracing::info!(target: "compiler_compatibility", count = resolved.packages.len(), "Resolved all packages");

    // Fetch and unpack all packages
    for (name, version) in &resolved.packages {
        let metadata = reader.read_metadata(name)?;
        let published = metadata.published.get(version).ok_or_else(|| {
            error::CompatError::ManifestNotFound { name: name.clone(), version: version.clone() }
        })?;

        let tarball = storage::fetch_tarball(name, version, layout, cli.no_cache)?;
        storage::verify_tarball(&tarball, &published.hash, name, version)?;
        unpacker::unpack_tarball(&tarball, &layout.packages)?;

        tracing::debug!(target: "compiler_compatibility", name, version, "Unpacked");
    }

    tracing::info!(target: "compiler_compatibility", directory = %layout.packages.display(), "Finished unpacking");

    let all_result = compat::check_all(&layout.packages, &resolved, cli.quiet);

    if let Some(ref report_path) = cli.report_json {
        let report = build_report(&package_set.version, &all_result);
        let json = serde_json::to_string_pretty(&report)
            .map_err(|e| error::CompatError::Other(format!("Failed to serialize report: {}", e)))?;
        std::fs::write(report_path, json)?;
        tracing::info!(target: "compiler_compatibility", path = %report_path.display(), "Report written");
    }

    Ok(())
}

fn run_packages_mode(
    cli: &Cli,
    package_set: &registry::PackageSet,
    reader: &impl RegistryReader,
    layout: &layout::Layout,
    tracing_handle: &trace::TracingHandle,
) -> error::Result<()> {
    let resolved = resolver::resolve(&cli.packages, package_set, reader)?;
    tracing::info!(target: "compiler_compatibility", count = resolved.packages.len(), "Resolved packages");

    for (name, version) in &resolved.packages {
        let metadata = reader.read_metadata(name)?;
        let published = metadata.published.get(version).ok_or_else(|| {
            error::CompatError::ManifestNotFound { name: name.clone(), version: version.clone() }
        })?;

        let tarball = storage::fetch_tarball(name, version, layout, cli.no_cache)?;
        storage::verify_tarball(&tarball, &published.hash, name, version)?;
        unpacker::unpack_tarball(&tarball, &layout.packages)?;

        tracing::debug!(target: "compiler_compatibility", name, version, "Unpacked");
    }

    tracing::info!(target: "compiler_compatibility", directory = %layout.packages.display(), "Finished unpacking");

    for package in &cli.packages {
        let _span =
            tracing::info_span!(target: "compiler_compatibility", "for_each_package", package)
                .entered();

        let guard =
            tracing_handle.begin_package(package).expect("failed to start package trace capture");
        let log_file = guard.path().to_path_buf();

        let result = compat::check_package(&layout.packages, package, &resolved);

        drop(guard);

        if !cli.quiet {
            for file_result in &result.files {
                if !file_result.output.is_empty() {
                    print!("{}", file_result.output);
                }
            }
        }

        let summary = format!(
            "{}: {} errors, {} warnings",
            package, result.total_errors, result.total_warnings
        );

        if result.total_errors > 0 {
            tracing::error!(target: "compiler_compatibility", "{}", summary);
        } else if result.total_warnings > 0 {
            tracing::warn!(target: "compiler_compatibility", "{}", summary);
        } else {
            tracing::info!(target: "compiler_compatibility", "{}", summary);
        }

        tracing::debug!(target: "compiler_compatibility", path = %log_file.display(), "Trace written");
    }

    Ok(())
}

fn build_report(package_set_version: &str, all_result: &compat::AllCheckResult) -> CompatReport {
    let mut packages = BTreeMap::new();
    let mut ok = 0;
    let mut warnings_only = 0;
    let mut failed = 0;
    let mut failed_root_cause = 0;
    let mut failed_cascaded = 0;
    let mut root_causes = Vec::new();

    for (name, outcome) in &all_result.outcomes {
        if outcome.total_errors > 0 {
            failed += 1;
            if outcome.root_cause {
                failed_root_cause += 1;
                root_causes.push(name.clone());
            } else {
                failed_cascaded += 1;
            }
        } else if outcome.total_warnings > 0 {
            warnings_only += 1;
        } else {
            ok += 1;
        }

        packages.insert(
            name.clone(),
            PackageReport {
                version: outcome.version.clone(),
                topo_layer: outcome.topo_layer,
                errors: outcome.total_errors,
                warnings: outcome.total_warnings,
                classification: Classification {
                    root_cause: outcome.root_cause,
                    cascaded_from: outcome.cascaded_from.clone(),
                    cascaded_from_root_causes: outcome.cascaded_from_root_causes.clone(),
                },
            },
        );
    }

    let total = all_result.outcomes.len();

    CompatReport {
        timestamp: chrono::Utc::now().to_rfc3339(),
        git_sha: std::env::var("GITHUB_SHA").unwrap_or_else(|_| "unknown".to_string()),
        package_set: package_set_version.to_string(),
        summary: ReportSummary {
            total,
            ok,
            warnings_only,
            failed,
            failed_root_cause,
            failed_cascaded,
        },
        root_causes,
        packages,
    }
}
