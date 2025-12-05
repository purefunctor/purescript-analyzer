use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};

use convert_case::{Case, Converter};
use itertools::Itertools;

fn read_dir<'output>(path: &Path) -> impl Iterator<Item = PathBuf> + use<'output> {
    fs::read_dir(path).unwrap().filter_map(|entry| Some(entry.ok()?.path())).sorted()
}

fn main() {
    println!("cargo::rerun-if-changed=fixtures/lsp");
    println!("cargo::rerun-if-changed=fixtures/lowering");
    println!("cargo::rerun-if-changed=fixtures/resolving");
    println!("cargo::rerun-if-changed=fixtures/checking");
    generate_lsp();
    generate_lowering();
    generate_resolving();
    generate_checking();
}

fn generate_lsp() {
    let lsp = Path::new("./fixtures/lsp");
    let lsp = read_dir(lsp);

    let mut buffer = fs::File::create("./tests/lsp/generated.rs").unwrap();
    writeln!(buffer, "// Do not edit! See build.rs").unwrap();

    let converter = Converter::new().to_case(Case::Snake);

    for folder in lsp {
        let Some(folder) = folder.file_stem() else {
            continue;
        };

        let folder_name = folder.to_os_string().into_string().unwrap();
        let folder_name = converter.convert(folder_name);

        writeln!(
            buffer,
            r#"
#[rustfmt::skip]
#[test]
fn test_{folder_name}_main() {{
    let (engine, files) = tests_integration::load_compiler(std::path::Path::new("fixtures/lsp/{folder_name}"));
    let Some(id) = engine.module_file("Main") else {{
        return;
    }};
    let report = tests_integration::generated::lsp::report(&engine, &files, id);
    insta::assert_snapshot!(report);
}}"#
        )
        .unwrap();
    }
}

fn generate_lowering() {
    let lowering = Path::new("./fixtures/lowering");
    let lowering = read_dir(lowering).map(|folder| {
        let files = read_dir(&folder);
        (folder, files)
    });

    let mut buffer = fs::File::create("./tests/lowering/generated.rs").unwrap();
    writeln!(buffer, "// Do not edit! See build.rs").unwrap();

    let converter = Converter::new().to_case(Case::Snake);

    for (folder, files) in lowering {
        let Some(folder) = folder.file_stem() else {
            continue;
        };

        let folder_name = folder.to_os_string().into_string().unwrap();
        let folder_name = converter.convert(folder_name);

        for file in files {
            let Some(file) = file.file_stem() else {
                continue;
            };

            let file_name = file.to_os_string().into_string().unwrap();
            let test_name = converter.convert(&file_name);

            writeln!(
                buffer,
                r#"
#[rustfmt::skip]
#[test]
fn test_{folder_name}_{test_name}() {{
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/lowering/{folder_name}"));
    let Some(id) = engine.module_file("{file_name}") else {{
        return;
    }};
    let report = tests_integration::generated::basic::report_lowered(&engine, id, "{file_name}");
    insta::assert_snapshot!(report);
}}"#
            )
            .unwrap();
        }
    }
}

fn generate_resolving() {
    let resolving = Path::new("./fixtures/resolving");
    let resolving = read_dir(resolving).map(|folder| {
        let files = read_dir(&folder);
        (folder, files)
    });

    let mut buffer = fs::File::create("./tests/resolving/generated.rs").unwrap();
    writeln!(buffer, "// Do not edit! See build.rs").unwrap();

    let converter = Converter::new().to_case(Case::Snake);

    for (folder, files) in resolving {
        let Some(folder) = folder.file_stem() else {
            continue;
        };

        let folder_name = folder.to_os_string().into_string().unwrap();
        let folder_name = converter.convert(folder_name);

        for file in files {
            let Some(file) = file.file_stem() else {
                continue;
            };

            let file_name = file.to_os_string().into_string().unwrap();
            let test_name = converter.convert(&file_name);

            writeln!(
                buffer,
                r#"
#[rustfmt::skip]
#[test]
fn test_{folder_name}_{test_name}() {{
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/resolving/{folder_name}"));
    let Some(id) = engine.module_file("{file_name}") else {{
        return;
    }};
    let report = tests_integration::generated::basic::report_resolved(&engine, id, "{file_name}");
    insta::assert_snapshot!(report);
}}"#
            )
            .unwrap();
        }
    }
}

fn generate_checking() {
    let checking = Path::new("./fixtures/checking");
    let checking = read_dir(checking);

    let mut buffer = fs::File::create("./tests/checking/generated.rs").unwrap();
    writeln!(buffer, "// Do not edit! See build.rs").unwrap();

    let converter = Converter::new().to_case(Case::Snake);

    for folder in checking {
        let Some(folder) = folder.file_stem() else {
            continue;
        };

        let folder_name = folder.to_os_string().into_string().unwrap();
        let folder_name = converter.convert(folder_name);

        writeln!(
            buffer,
            r#"
#[rustfmt::skip]
#[test]
fn test_{folder_name}_main() {{
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/checking/{folder_name}"));
    let Some(id) = engine.module_file("Main") else {{
        return;
    }};
    let report = tests_integration::generated::basic::report_checked(&engine, id);
    insta::assert_snapshot!(report);
}}"#
        )
        .unwrap();
    }
}
