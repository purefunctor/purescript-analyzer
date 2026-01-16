use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};

use convert_case::{Case, Converter};
use itertools::Itertools;

fn read_dir<'output>(path: &Path) -> impl Iterator<Item = PathBuf> + use<'output> {
    fs::read_dir(path).unwrap().filter_map(|entry| Some(entry.ok()?.path())).sorted()
}

fn read_purs_files<'output>(path: &Path) -> impl Iterator<Item = PathBuf> + use<'output> {
    read_dir(path).filter(|p| p.extension().is_some_and(|ext| ext == "purs"))
}

fn main() {
    println!("cargo::rerun-if-env-changed=LSP_FIXTURES_HASH");
    println!("cargo::rerun-if-env-changed=LOWERING_FIXTURES_HASH");
    println!("cargo::rerun-if-env-changed=RESOLVING_FIXTURES_HASH");
    println!("cargo::rerun-if-env-changed=CHECKING_FIXTURES_HASH");
    generate_lsp();
    generate_lowering();
    generate_resolving();
    generate_checking();
}

fn generate_lsp() {
    let mut buffer = fs::File::create("./tests/lsp/generated.rs").unwrap();
    writeln!(
        buffer,
        r#"// Do not edit! See build.rs

#[rustfmt::skip]
fn run_test(folder: &str, file: &str) {{
    let path = std::path::Path::new("fixtures/lsp").join(folder);
    let (engine, files) = tests_integration::load_compiler(&path);
    let Some(id) = engine.module_file(file) else {{ return }};
    let report = tests_integration::generated::lsp::report(&engine, &files, id);
    let mut settings = insta::Settings::clone_current();
    settings.set_snapshot_path(std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("fixtures/lsp").join(folder));
    settings.set_prepend_module_to_snapshot(false);
    settings.bind(|| insta::assert_snapshot!(file, report));
}}"#
    )
    .unwrap();

    let converter = Converter::new().to_case(Case::Snake);
    for folder in read_dir(Path::new("./fixtures/lsp")) {
        let Some(stem) = folder.file_stem() else { continue };
        let folder_name = converter.convert(stem.to_os_string().into_string().unwrap());
        writeln!(
            buffer,
            r#"
#[rustfmt::skip] #[test] fn test_{folder_name}_main() {{ run_test("{folder_name}", "Main"); }}"#
        )
        .unwrap();
    }
}

fn generate_lowering() {
    let mut buffer = fs::File::create("./tests/lowering/generated.rs").unwrap();
    writeln!(buffer, r#"// Do not edit! See build.rs

#[rustfmt::skip]
fn run_test(folder: &str, file: &str) {{
    let path = std::path::Path::new("fixtures/lowering").join(folder);
    let (engine, _) = tests_integration::load_compiler(&path);
    let Some(id) = engine.module_file(file) else {{ return }};
    let report = tests_integration::generated::basic::report_lowered(&engine, id, file);
    let mut settings = insta::Settings::clone_current();
    settings.set_snapshot_path(std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("fixtures/lowering").join(folder));
    settings.set_prepend_module_to_snapshot(false);
    settings.bind(|| insta::assert_snapshot!(file, report));
}}"#).unwrap();

    let converter = Converter::new().to_case(Case::Snake);
    for folder in read_dir(Path::new("./fixtures/lowering")) {
        let Some(stem) = folder.file_stem() else { continue };
        let folder_name = converter.convert(stem.to_os_string().into_string().unwrap());
        for file in read_purs_files(&folder) {
            let Some(file_stem) = file.file_stem() else { continue };
            let file_name = file_stem.to_os_string().into_string().unwrap();
            let test_name = format!("{}_{}", folder_name, converter.convert(&file_name));
            writeln!(
                buffer,
                r#"
#[rustfmt::skip] #[test] fn test_{test_name}() {{ run_test("{folder_name}", "{file_name}"); }}"#
            )
            .unwrap();
        }
    }
}

fn generate_resolving() {
    let mut buffer = fs::File::create("./tests/resolving/generated.rs").unwrap();
    writeln!(buffer, r#"// Do not edit! See build.rs

#[rustfmt::skip]
fn run_test(folder: &str, file: &str) {{
    let path = std::path::Path::new("fixtures/resolving").join(folder);
    let (engine, _) = tests_integration::load_compiler(&path);
    let Some(id) = engine.module_file(file) else {{ return }};
    let report = tests_integration::generated::basic::report_resolved(&engine, id, file);
    let mut settings = insta::Settings::clone_current();
    settings.set_snapshot_path(std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("fixtures/resolving").join(folder));
    settings.set_prepend_module_to_snapshot(false);
    settings.bind(|| insta::assert_snapshot!(file, report));
}}"#).unwrap();

    let converter = Converter::new().to_case(Case::Snake);
    for folder in read_dir(Path::new("./fixtures/resolving")) {
        let Some(stem) = folder.file_stem() else { continue };
        let folder_name = converter.convert(stem.to_os_string().into_string().unwrap());
        for file in read_purs_files(&folder) {
            let Some(file_stem) = file.file_stem() else { continue };
            let file_name = file_stem.to_os_string().into_string().unwrap();
            let test_name = format!("{}_{}", folder_name, converter.convert(&file_name));
            writeln!(
                buffer,
                r#"
#[rustfmt::skip] #[test] fn test_{test_name}() {{ run_test("{folder_name}", "{file_name}"); }}"#
            )
            .unwrap();
        }
    }
}

fn generate_checking() {
    let mut buffer = fs::File::create("./tests/checking/generated.rs").unwrap();
    writeln!(buffer, r#"// Do not edit! See build.rs

#[rustfmt::skip]
fn run_test(folder: &str, file: &str) {{
    let path = std::path::Path::new("fixtures/checking").join(folder);
    let (engine, _) = tests_integration::load_compiler(&path);
    let Some(id) = engine.module_file(file) else {{ return }};
    let report = tests_integration::generated::basic::report_checked(&engine, id);
    let mut settings = insta::Settings::clone_current();
    settings.set_snapshot_path(std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("fixtures/checking").join(folder));
    settings.set_prepend_module_to_snapshot(false);
    settings.bind(|| insta::assert_snapshot!(file, report));
}}"#).unwrap();

    let converter = Converter::new().to_case(Case::Snake);
    for folder in read_dir(Path::new("./fixtures/checking")) {
        let Some(stem) = folder.file_stem() else { continue };
        let folder_name = converter.convert(stem.to_os_string().into_string().unwrap());
        // Skip the prelude folder - it's shared setup, not a test
        if folder_name == "prelude" {
            continue;
        }
        writeln!(
            buffer,
            r#"
#[rustfmt::skip] #[test] fn test_{folder_name}_main() {{ run_test("{folder_name}", "Main"); }}"#
        )
        .unwrap();
    }
}
