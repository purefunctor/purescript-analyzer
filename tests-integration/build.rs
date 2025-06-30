use std::{
    fs,
    io::Write,
    path::{Path, PathBuf},
};

use convert_case::{Case, Converter};
use itertools::Itertools;

fn read_dir<'output>(path: &Path) -> impl Iterator<Item = PathBuf> + use<'output> {
    fs::read_dir(path).unwrap().filter_map(|entry| Some(entry.ok()?.path())).sorted()
}

fn main() {
    println!("cargo::rerun-if-changed=fixtures/deferred_resolution");
    println!("cargo::rerun-if-changed=fixtures/lsp");
    println!("cargo::rerun-if-changed=fixtures/resolving");
    generate_deferred_resolution();
    generate_lsp();
    generate_resolving();
}

fn generate_resolving() {
    let resolving = Path::new("./fixtures/resolving");
    let resolving = read_dir(resolving).map(|folder| {
        let files = read_dir(&folder);
        (folder, files)
    });

    let mut buffer = fs::File::create("./tests/resolving.rs").unwrap();
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
fn test_{}_{}() {{
    let mut compiler = tests_integration::load_compiler(std::path::Path::new("fixtures/resolving/{}"));
    let Some(id) = compiler.runtime.module_file("{}") else {{
        return;
    }};
    let resolved = compiler.runtime.resolved(id);
    let report = tests_integration::core::report_resolved("{}", &resolved);
    insta::assert_snapshot!(report);
}}"#,
                folder_name, test_name, folder_name, file_name, file_name
            )
            .unwrap();
        }
    }
}

fn generate_deferred_resolution() {
    let deferred_resolution = Path::new("./fixtures/deferred_resolution");
    let deferred_resolution = read_dir(deferred_resolution).map(|folder| {
        let files = read_dir(&folder);
        (folder, files)
    });

    let mut buffer = fs::File::create("./tests/deferred_resolution.rs").unwrap();
    writeln!(buffer, "// Do not edit! See build.rs").unwrap();

    let converter = Converter::new().to_case(Case::Snake);

    for (folder, files) in deferred_resolution {
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
fn test_{}_{}() {{
    let mut compiler = tests_integration::load_compiler(std::path::Path::new("fixtures/deferred_resolution/{}"));
    let Some(id) = compiler.runtime.module_file("{}") else {{ 
        return;
    }};
    let report = tests_integration::core::report_deferred_resolution(&mut compiler, id);
    insta::assert_snapshot!(report);
}}"#,
                folder_name, test_name, folder_name, file_name
            )
            .unwrap();
        }
    }
}

fn generate_lsp() {
    let lsp = Path::new("./fixtures/lsp");
    let lsp = read_dir(lsp).map(|folder| {
        let files = read_dir(&folder);
        (folder, files)
    });

    let mut buffer = fs::File::create("./tests/lsp.rs").unwrap();
    writeln!(buffer, "// Do not edit! See build.rs").unwrap();

    let converter = Converter::new().to_case(Case::Snake);

    for (folder, files) in lsp {
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
fn test_{}_{}() {{
    let mut compiler = tests_integration::load_compiler(std::path::Path::new("fixtures/lsp/{}"));
    let Some(id) = compiler.runtime.module_file("{}") else {{ 
        return;
    }};
    let report = tests_integration::lsp::report(&mut compiler, id);
    insta::assert_snapshot!(report);
}}"#,
                folder_name, test_name, folder_name, file_name
            )
            .unwrap();
        }
    }
}
