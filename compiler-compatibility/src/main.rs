mod compat;
mod error;
mod layout;
mod registry;
mod resolver;
mod storage;
mod types;
mod unpacker;

use std::path::PathBuf;

use clap::Parser;
use purescript_registry::RegistryReader;

#[derive(Parser, Debug)]
#[command(name = "compiler-compatibility")]
#[command(about = "Fetch PureScript packages for compatibility testing")]
struct Cli {
    #[arg(help = "Package names to fetch and unpack")]
    packages: Vec<String>,

    #[arg(long, help = "Use specific package set version (default: latest)")]
    package_set: Option<String>,

    #[arg(long, help = "List available package set versions")]
    list_sets: bool,

    #[arg(long, help = "Update local registry repos (git pull)")]
    update: bool,

    #[arg(long, default_value = "target/compiler-compatibility", help = "Output directory")]
    output: PathBuf,

    #[arg(long, help = "Disable tarball caching")]
    no_cache: bool,

    #[arg(short, long, help = "Verbose output")]
    verbose: bool,
}

fn main() -> error::Result<()> {
    let cli = Cli::parse();

    let filter = if cli.verbose { "debug" } else { "info" };
    tracing_subscriber::fmt().with_env_filter(filter).init();

    let layout = layout::Layout::new(&cli.output);
    let registry = registry::Registry::new(layout.clone());

    registry.ensure_repos(cli.update)?;

    if cli.list_sets {
        let sets = registry.reader().list_package_sets()?;
        for set in sets {
            println!("{}", set);
        }
        return Ok(());
    }

    if cli.packages.is_empty() {
        println!("No packages specified. Use --help for usage.");
        return Ok(());
    }

    let package_set = registry.reader().read_package_set(cli.package_set.as_deref())?;
    tracing::info!(version = %package_set.version, "Using package set");

    let resolved = resolver::resolve(&cli.packages, &package_set, registry.reader())?;
    tracing::info!(count = resolved.packages.len(), "Resolved packages");

    for (name, version) in &resolved.packages {
        let metadata = registry.reader().read_metadata(name)?;
        let published = metadata.published.get(version).ok_or_else(|| {
            error::CompatError::ManifestNotFound { name: name.clone(), version: version.clone() }
        })?;

        let tarball = storage::fetch_tarball(name, version, &layout, cli.no_cache)?;
        storage::verify_tarball(&tarball, &published.hash, name, version)?;
        unpacker::unpack_tarball(&tarball, &layout.package_dir(name, version))?;

        tracing::info!(name, version, "Unpacked");
    }

    tracing::info!(dir = %layout.packages_dir.display(), "Done");
    Ok(())
}
