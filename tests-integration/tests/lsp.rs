// Do not edit! See build.rs

#[rustfmt::skip]
#[test]
fn test_001_definition_local_main() {
    let mut compiler = tests_integration::load_compiler(std::path::Path::new("fixtures/lsp/001_definition_local"));
    let Some(id) = compiler.runtime.module_file("Main") else {
        return;
    };
    let report = tests_integration::lsp::report(&mut compiler, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_002_definition_import_main() {
    let mut compiler = tests_integration::load_compiler(std::path::Path::new("fixtures/lsp/002_definition_import"));
    let Some(id) = compiler.runtime.module_file("Main") else {
        return;
    };
    let report = tests_integration::lsp::report(&mut compiler, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_003_hover_local_main() {
    let mut compiler = tests_integration::load_compiler(std::path::Path::new("fixtures/lsp/003_hover_local"));
    let Some(id) = compiler.runtime.module_file("Main") else {
        return;
    };
    let report = tests_integration::lsp::report(&mut compiler, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_004_hover_import_main() {
    let mut compiler = tests_integration::load_compiler(std::path::Path::new("fixtures/lsp/004_hover_import"));
    let Some(id) = compiler.runtime.module_file("Main") else {
        return;
    };
    let report = tests_integration::lsp::report(&mut compiler, id);
    insta::assert_snapshot!(report);
}
