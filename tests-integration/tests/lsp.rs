// Do not edit! See build.rs

#[rustfmt::skip]
#[test]
fn test_001_definition_local_main() {
    let (engine, files) = tests_integration::load_compiler(std::path::Path::new("fixtures/lsp/001_definition_local"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::lsp::report(&engine, &files, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_002_definition_import_main() {
    let (engine, files) = tests_integration::load_compiler(std::path::Path::new("fixtures/lsp/002_definition_import"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::lsp::report(&engine, &files, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_003_hover_local_main() {
    let (engine, files) = tests_integration::load_compiler(std::path::Path::new("fixtures/lsp/003_hover_local"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::lsp::report(&engine, &files, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_004_hover_import_main() {
    let (engine, files) = tests_integration::load_compiler(std::path::Path::new("fixtures/lsp/004_hover_import"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::lsp::report(&engine, &files, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_005_completion_local_main() {
    let (engine, files) = tests_integration::load_compiler(std::path::Path::new("fixtures/lsp/005_completion_local"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::lsp::report(&engine, &files, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_006_completion_partial_empty_main() {
    let (engine, files) = tests_integration::load_compiler(std::path::Path::new("fixtures/lsp/006_completion_partial_empty"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::lsp::report(&engine, &files, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_007_completion_partial_token_main() {
    let (engine, files) = tests_integration::load_compiler(std::path::Path::new("fixtures/lsp/007_completion_partial_token"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::lsp::report(&engine, &files, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_008_completion_import_main() {
    let (engine, files) = tests_integration::load_compiler(std::path::Path::new("fixtures/lsp/008_completion_import"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::lsp::report(&engine, &files, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_009_completion_suggestion_main() {
    let (engine, files) = tests_integration::load_compiler(std::path::Path::new("fixtures/lsp/009_completion_suggestion"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::lsp::report(&engine, &files, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_010_completion_qualified_suggestion_main() {
    let (engine, files) = tests_integration::load_compiler(std::path::Path::new("fixtures/lsp/010_completion_qualified_suggestion"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::lsp::report(&engine, &files, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_011_completion_module_name_main() {
    let (engine, files) = tests_integration::load_compiler(std::path::Path::new("fixtures/lsp/011_completion_module_name"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::lsp::report(&engine, &files, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_012_completion_edit_import_main() {
    let (engine, files) = tests_integration::load_compiler(std::path::Path::new("fixtures/lsp/012_completion_edit_import"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::lsp::report(&engine, &files, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_013_completion_bad_cst_main() {
    let (engine, files) = tests_integration::load_compiler(std::path::Path::new("fixtures/lsp/013_completion_bad_cst"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::lsp::report(&engine, &files, id);
    insta::assert_snapshot!(report);
}
