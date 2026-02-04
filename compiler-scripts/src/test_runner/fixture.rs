use std::fs;
use std::path::{Path, PathBuf};

use console::style;
use heck::ToSnakeCase;

use crate::test_runner::TestCategory;

const MAIN_TEMPLATE: &str = "module Main where\n\n";

pub fn create_fixture(category: TestCategory, name: &str) -> Result<PathBuf, String> {
    let fixtures_dir = PathBuf::from(category.fixtures_subdir_fragment());
    if !fixtures_dir.is_dir() {
        return Err(format!(
            "fixtures directory '{}' does not exist",
            fixtures_dir.display()
        ));
    }

    let next_number = next_fixture_number(&fixtures_dir)?;
    let slug = slugify(name)?;
    let folder_name = format!("{:03}_{}", next_number, slug);
    let folder_path = fixtures_dir.join(&folder_name);

    if folder_path.exists() {
        return Err(format!("fixture '{}' already exists", folder_path.display()));
    }

    fs::create_dir_all(&folder_path).map_err(|err| {
        format!(
            "failed to create fixture directory '{}': {}",
            folder_path.display(),
            err
        )
    })?;

    let main_path = folder_path.join("Main.purs");
    fs::write(&main_path, MAIN_TEMPLATE)
        .map_err(|err| format!("failed to write '{}': {}", main_path.display(), err))?;

    println!(
        "{} {}",
        style("CREATED").green().bold(),
        style(main_path.display()).cyan()
    );
    println!();
    println!(
        "  {} {}",
        style("Next:").dim(),
        style(format!("just t {} {:03}", category.as_str(), next_number)).cyan()
    );

    Ok(folder_path)
}

pub struct DeleteFixtureOutcome {
    pub fixture_paths: Vec<PathBuf>,
    pub snapshot_paths: Vec<PathBuf>,
    pub confirmed: bool,
}

pub fn delete_fixture(
    category: TestCategory,
    name: &str,
    confirm: bool,
) -> Result<DeleteFixtureOutcome, String> {
    let fixtures_dir = PathBuf::from(category.fixtures_subdir_fragment());
    if !fixtures_dir.is_dir() {
        return Err(format!(
            "fixtures directory '{}' does not exist",
            fixtures_dir.display()
        ));
    }

    let fixture_paths = resolve_fixture_paths(&fixtures_dir, name)?;
    let mut snapshot_paths = Vec::new();
    for fixture_path in &fixture_paths {
        snapshot_paths.extend(find_snapshot_paths(category, fixture_path)?);
    }

    if confirm {
        for fixture_path in &fixture_paths {
            if fixture_path.exists() {
                fs::remove_dir_all(fixture_path).map_err(|err| {
                    format!(
                        "failed to delete fixture directory '{}': {}",
                        fixture_path.display(),
                        err
                    )
                })?;
            }
        }

        for snapshot_path in &snapshot_paths {
            if snapshot_path.exists() {
                fs::remove_file(snapshot_path).map_err(|err| {
                    format!(
                        "failed to delete snapshot '{}': {}",
                        snapshot_path.display(),
                        err
                    )
                })?;
            }
        }
    }

    Ok(DeleteFixtureOutcome { fixture_paths, snapshot_paths, confirmed: confirm })
}

fn next_fixture_number(fixtures_dir: &Path) -> Result<u32, String> {
    let mut max_number = 0;
    let entries = fs::read_dir(fixtures_dir)
        .map_err(|err| format!("failed to read '{}': {}", fixtures_dir.display(), err))?;

    for entry in entries {
        let entry = entry.map_err(|err| format!("failed to read entry: {}", err))?;
        let name = entry.file_name();
        let name = name.to_string_lossy();
        let Some((prefix, _)) = name.split_once('_') else {
            continue;
        };
        if prefix.len() != 3 || !prefix.chars().all(|ch| ch.is_ascii_digit()) {
            continue;
        }
        if let Ok(number) = prefix.parse::<u32>() {
            max_number = max_number.max(number);
        }
    }

    Ok(max_number + 1)
}

fn resolve_fixture_paths(fixtures_dir: &Path, name: &str) -> Result<Vec<PathBuf>, String> {
    let slug = slugify(name)?;
    let mut matches = find_matching_fixtures(fixtures_dir, &slug)?;
    if matches.is_empty() && name.chars().all(|ch| ch.is_ascii_digit()) {
        matches = find_matching_fixtures(fixtures_dir, name)?;
    }

    if matches.is_empty() {
        return Err(format!(
            "no fixture found matching '{}' in '{}'",
            name,
            fixtures_dir.display()
        ));
    }

    matches.sort();
    Ok(matches)
}

fn find_matching_fixtures(fixtures_dir: &Path, needle: &str) -> Result<Vec<PathBuf>, String> {
    let mut matches = Vec::new();
    let entries = fs::read_dir(fixtures_dir)
        .map_err(|err| format!("failed to read '{}': {}", fixtures_dir.display(), err))?;

    for entry in entries {
        let entry = entry.map_err(|err| format!("failed to read entry: {}", err))?;
        if !entry.path().is_dir() {
            continue;
        }
        let name = entry.file_name();
        let name = name.to_string_lossy();
        if name.contains(needle) {
            matches.push(entry.path());
        }
    }

    Ok(matches)
}

fn find_snapshot_paths(
    category: TestCategory,
    fixture_path: &Path,
) -> Result<Vec<PathBuf>, String> {
    let fixture_name = fixture_path
        .file_name()
        .and_then(|name| name.to_str())
        .ok_or_else(|| "fixture path is missing a valid folder name".to_string())?;
    let fixture_slug = slugify(fixture_name).unwrap_or_else(|_| fixture_name.to_string());
    let mut paths = Vec::new();

    for fragment in category.snapshot_path_fragments() {
        let base = PathBuf::from(fragment);
        if !base.exists() {
            continue;
        }
        for entry in fs::read_dir(&base)
            .map_err(|err| format!("failed to read '{}': {}", base.display(), err))?
        {
            let entry = entry.map_err(|err| format!("failed to read entry: {}", err))?;
            if !entry.path().is_file() {
                continue;
            }
            let file_name = entry.file_name();
            let file_name = file_name.to_string_lossy();
            if file_name.contains(&fixture_slug) {
                paths.push(entry.path());
            }
        }
    }

    paths.sort();
    paths.dedup();
    Ok(paths)
}

fn slugify(input: &str) -> Result<String, String> {
    if input.is_empty() {
        return Err("fixture name must not be empty".to_string());
    }
    Ok(input.to_snake_case())
}
