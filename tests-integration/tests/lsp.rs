// Do not edit! See build.rs

#[rustfmt::skip]
#[test]
fn test_001_definition_local_main() {
    let (engine, files) = tests_integration::load_compiler(std::path::Path::new("fixtures/lsp/001_definition_local"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::lsp::report(&engine, &files, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_002_definition_import_main() {
    let (engine, files) = tests_integration::load_compiler(std::path::Path::new("fixtures/lsp/002_definition_import"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::lsp::report(&engine, &files, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_003_hover_local_main() {
    let (engine, files) = tests_integration::load_compiler(std::path::Path::new("fixtures/lsp/003_hover_local"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::lsp::report(&engine, &files, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_004_hover_import_main() {
    let (engine, files) = tests_integration::load_compiler(std::path::Path::new("fixtures/lsp/004_hover_import"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::lsp::report(&engine, &files, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_005_completion_local_main() {
    let (engine, files) = tests_integration::load_compiler(std::path::Path::new("fixtures/lsp/005_completion_local"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::lsp::report(&engine, &files, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_006_completion_partial_empty_main() {
    let (engine, files) = tests_integration::load_compiler(std::path::Path::new("fixtures/lsp/006_completion_partial_empty"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::lsp::report(&engine, &files, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_007_completion_partial_token_main() {
    let (engine, files) = tests_integration::load_compiler(std::path::Path::new("fixtures/lsp/007_completion_partial_token"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::lsp::report(&engine, &files, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_008_completion_import_main() {
    let (engine, files) = tests_integration::load_compiler(std::path::Path::new("fixtures/lsp/008_completion_import"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::lsp::report(&engine, &files, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_009_completion_suggestion_main() {
    let (engine, files) = tests_integration::load_compiler(std::path::Path::new("fixtures/lsp/009_completion_suggestion"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::lsp::report(&engine, &files, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_010_completion_qualified_suggestion_main() {
    let (engine, files) = tests_integration::load_compiler(std::path::Path::new("fixtures/lsp/010_completion_qualified_suggestion"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::lsp::report(&engine, &files, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_011_completion_module_name_main() {
    let (engine, files) = tests_integration::load_compiler(std::path::Path::new("fixtures/lsp/011_completion_module_name"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::lsp::report(&engine, &files, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_012_completion_edit_import_main() {
    let (engine, files) = tests_integration::load_compiler(std::path::Path::new("fixtures/lsp/012_completion_edit_import"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::lsp::report(&engine, &files, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_013_completion_bad_cst_main() {
    let (engine, files) = tests_integration::load_compiler(std::path::Path::new("fixtures/lsp/013_completion_bad_cst"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::lsp::report(&engine, &files, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_014_prim_implicit_main() {
    let (engine, files) = tests_integration::load_compiler(std::path::Path::new("fixtures/lsp/014_prim_implicit"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::lsp::report(&engine, &files, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_015_prim_explicit_main() {
    let (engine, files) = tests_integration::load_compiler(std::path::Path::new("fixtures/lsp/015_prim_explicit"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::lsp::report(&engine, &files, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_016_prim_suggestion_main() {
    let (engine, files) = tests_integration::load_compiler(std::path::Path::new("fixtures/lsp/016_prim_suggestion"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::lsp::report(&engine, &files, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_017_unicode_operators_main() {
    let (engine, files) = tests_integration::load_compiler(std::path::Path::new("fixtures/lsp/017_unicode_operators"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::lsp::report(&engine, &files, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_018_export_suggestion_main() {
    let (engine, files) = tests_integration::load_compiler(std::path::Path::new("fixtures/lsp/018_export_suggestion"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::lsp::report(&engine, &files, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_019_documentation_string_main() {
    let (engine, files) = tests_integration::load_compiler(std::path::Path::new("fixtures/lsp/019_documentation_string"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::lsp::report(&engine, &files, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_020_references_baseline_main() {
    let (engine, files) = tests_integration::load_compiler(std::path::Path::new("fixtures/lsp/020_references_baseline"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::lsp::report(&engine, &files, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_021_references_qualified_main() {
    let (engine, files) = tests_integration::load_compiler(std::path::Path::new("fixtures/lsp/021_references_qualified"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::lsp::report(&engine, &files, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_022_references_import_main() {
    let (engine, files) = tests_integration::load_compiler(std::path::Path::new("fixtures/lsp/022_references_import"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::lsp::report(&engine, &files, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_023_references_duplicate_main() {
    let (engine, files) = tests_integration::load_compiler(std::path::Path::new("fixtures/lsp/023_references_duplicate"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::lsp::report(&engine, &files, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_024_locate_annotation_main() {
    let (engine, files) = tests_integration::load_compiler(std::path::Path::new("fixtures/lsp/024_locate_annotation"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::lsp::report(&engine, &files, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_025_completion_operators_main() {
    let (engine, files) = tests_integration::load_compiler(std::path::Path::new("fixtures/lsp/025_completion_operators"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::lsp::report(&engine, &files, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_026_completion_operator_suggestion_main() {
    let (engine, files) = tests_integration::load_compiler(std::path::Path::new("fixtures/lsp/026_completion_operator_suggestion"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::lsp::report(&engine, &files, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_027_completion_type_operators_main() {
    let (engine, files) = tests_integration::load_compiler(std::path::Path::new("fixtures/lsp/027_completion_type_operators"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::lsp::report(&engine, &files, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_028_completion_type_operator_suggestion_main() {
    let (engine, files) = tests_integration::load_compiler(std::path::Path::new("fixtures/lsp/028_completion_type_operator_suggestion"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::lsp::report(&engine, &files, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_029_locate_declaration_main() {
    let (engine, files) = tests_integration::load_compiler(std::path::Path::new("fixtures/lsp/029_locate_declaration"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::lsp::report(&engine, &files, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_030_locate_constructor_main() {
    let (engine, files) = tests_integration::load_compiler(std::path::Path::new("fixtures/lsp/030_locate_constructor"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::lsp::report(&engine, &files, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_031_locate_class_member_main() {
    let (engine, files) = tests_integration::load_compiler(std::path::Path::new("fixtures/lsp/031_locate_class_member"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::lsp::report(&engine, &files, id);
    insta::assert_snapshot!(report);
}
