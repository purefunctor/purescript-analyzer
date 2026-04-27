mod cache;
mod cli;
mod compile;
mod error;
mod report;
mod selection;
mod sources;

use std::path::Path;
use std::process::ExitCode;

use anyhow::{Context, Result};
use clap::Parser;
use git2::FetchOptions;
use git2::build::RepoBuilder;
use registry::{FsRegistry, RegistryLayout, RegistryReader};

use crate::cache::PackageCache;
use crate::cli::{Cli, Command, DEFAULT_INDEX_DIR, DEFAULT_REGISTRY_DIR};
use crate::compile::compile_sources;
use crate::report::{PackageSetReport, Report, SelectionReport};
use crate::selection::{package_map, resolve_selection};
use crate::sources::discover_sources;

fn main() -> ExitCode {
    match run() {
        Ok(has_errors) => {
            if has_errors {
                ExitCode::from(1)
            } else {
                ExitCode::SUCCESS
            }
        }
        Err(error) => {
            eprintln!("verifier: {error:#}");
            ExitCode::from(2)
        }
    }
}

fn run() -> Result<bool> {
    let cli = Cli::parse();

    match cli.command {
        Command::Verify(args) => {
            ensure_default_clone(
                &args.registry_dir,
                DEFAULT_REGISTRY_DIR,
                "https://github.com/purescript/registry.git",
            )
            .context("failed to clone default registry")?;
            ensure_default_clone(
                &args.index_dir,
                DEFAULT_INDEX_DIR,
                "https://github.com/purescript/registry-index.git",
            )
            .context("failed to clone default registry index")?;

            let layout = RegistryLayout::new(&args.registry_dir, &args.index_dir);
            let registry = FsRegistry::new(layout);
            let package_set = registry
                .read_package_set(args.package_set.as_deref())
                .context("failed to read package set")?;
            let packages = package_map(&package_set);
            let selection = resolve_selection(&registry, &packages, &args.packages, args.core)
                .context("failed to resolve package selection")?;

            let cache_dir = args.cache_dir.unwrap_or_else(|| "target/compatibility".into());
            let cache = PackageCache::new(cache_dir);

            let mut source_files = Vec::new();
            for package in &selection.packages {
                let extracted =
                    cache.ensure_package(&package.name, &package.version).with_context(|| {
                        format!("failed to prepare package {}@{}", package.name, package.version)
                    })?;
                source_files.extend(
                    discover_sources(&package.name, &package.version, &extracted).with_context(
                        || {
                            format!(
                                "failed to discover source files for {}@{}",
                                package.name, package.version
                            )
                        },
                    )?,
                );
            }
            source_files.sort_by(|a, b| {
                (&a.package, &a.version, &a.relative_path).cmp(&(
                    &b.package,
                    &b.version,
                    &b.relative_path,
                ))
            });

            let mut report = Report::new(
                PackageSetReport {
                    version: package_set.version,
                    compiler: package_set.compiler,
                    published: package_set.published,
                },
                SelectionReport {
                    mode: selection.mode,
                    requested_packages: args.packages,
                    resolved_packages: selection.packages,
                },
            );
            report.verifier_errors = selection.errors;

            let compile_report = compile_sources(&source_files)
                .context("failed to compile selected source files")?;
            report.summary.source_files = source_files.len();
            report.diagnostics = compile_report.diagnostics;
            report.verifier_errors.extend(compile_report.verifier_errors);
            report.recompute_summary();

            report.print_human();

            if let Some(path) = args.json_output {
                report.write_json(path.clone()).with_context(|| {
                    format!("failed to write JSON report to {}", path.display())
                })?;
            }

            Ok(report.has_errors())
        }
    }
}

fn ensure_default_clone(path: &Path, default_path: &str, url: &str) -> Result<()> {
    if path != Path::new(default_path) || path.exists() {
        return Ok(());
    }

    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)
            .with_context(|| format!("failed to create parent directory {}", parent.display()))?;
    }

    eprintln!("verifier: cloning {url} into {}", path.display());
    clone_shallow(url, path)?;

    Ok(())
}

fn clone_shallow(url: &str, path: &Path) -> Result<()> {
    let mut fetch_options = FetchOptions::new();
    fetch_options.depth(1);

    let mut builder = RepoBuilder::new();
    builder.fetch_options(fetch_options);
    builder
        .clone(url, path)
        .map(|_| ())
        .with_context(|| format!("failed to clone {url} into {}", path.display()))
}
