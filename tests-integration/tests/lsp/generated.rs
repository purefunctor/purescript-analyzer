// Do not edit! See build.rs

fn run_test(folder: &str, file: &str) {
    let path = std::path::Path::new("fixtures/lsp").join(folder);
    let (engine, files) = tests_integration::load_compiler(&path);
    let Some(id) = engine.module_file(file) else { return };
    let report = tests_integration::generated::lsp::report(&engine, &files, id);
    let mut settings = insta::Settings::clone_current();
    settings.set_snapshot_path(std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("fixtures/lsp").join(folder));
    settings.set_prepend_module_to_snapshot(false);
    settings.bind(|| insta::assert_snapshot!(file, report));
}

#[rustfmt::skip] #[test] fn test_001_definition_local_main() { run_test("001_definition_local", "Main"); }

#[rustfmt::skip] #[test] fn test_002_definition_import_main() { run_test("002_definition_import", "Main"); }

#[rustfmt::skip] #[test] fn test_003_hover_local_main() { run_test("003_hover_local", "Main"); }

#[rustfmt::skip] #[test] fn test_004_hover_import_main() { run_test("004_hover_import", "Main"); }

#[rustfmt::skip] #[test] fn test_005_completion_local_main() { run_test("005_completion_local", "Main"); }

#[rustfmt::skip] #[test] fn test_006_completion_partial_empty_main() { run_test("006_completion_partial_empty", "Main"); }

#[rustfmt::skip] #[test] fn test_007_completion_partial_token_main() { run_test("007_completion_partial_token", "Main"); }

#[rustfmt::skip] #[test] fn test_008_completion_import_main() { run_test("008_completion_import", "Main"); }

#[rustfmt::skip] #[test] fn test_009_completion_suggestion_main() { run_test("009_completion_suggestion", "Main"); }

#[rustfmt::skip] #[test] fn test_010_completion_qualified_suggestion_main() { run_test("010_completion_qualified_suggestion", "Main"); }

#[rustfmt::skip] #[test] fn test_011_completion_module_name_main() { run_test("011_completion_module_name", "Main"); }

#[rustfmt::skip] #[test] fn test_012_completion_edit_import_main() { run_test("012_completion_edit_import", "Main"); }

#[rustfmt::skip] #[test] fn test_013_completion_bad_cst_main() { run_test("013_completion_bad_cst", "Main"); }

#[rustfmt::skip] #[test] fn test_014_prim_implicit_main() { run_test("014_prim_implicit", "Main"); }

#[rustfmt::skip] #[test] fn test_015_prim_explicit_main() { run_test("015_prim_explicit", "Main"); }

#[rustfmt::skip] #[test] fn test_016_prim_suggestion_main() { run_test("016_prim_suggestion", "Main"); }

#[rustfmt::skip] #[test] fn test_017_unicode_operators_main() { run_test("017_unicode_operators", "Main"); }

#[rustfmt::skip] #[test] fn test_018_export_suggestion_main() { run_test("018_export_suggestion", "Main"); }

#[rustfmt::skip] #[test] fn test_019_documentation_string_main() { run_test("019_documentation_string", "Main"); }

#[rustfmt::skip] #[test] fn test_020_references_baseline_main() { run_test("020_references_baseline", "Main"); }

#[rustfmt::skip] #[test] fn test_021_references_qualified_main() { run_test("021_references_qualified", "Main"); }

#[rustfmt::skip] #[test] fn test_022_references_import_main() { run_test("022_references_import", "Main"); }

#[rustfmt::skip] #[test] fn test_023_references_duplicate_main() { run_test("023_references_duplicate", "Main"); }

#[rustfmt::skip] #[test] fn test_024_locate_annotation_main() { run_test("024_locate_annotation", "Main"); }

#[rustfmt::skip] #[test] fn test_025_completion_operators_main() { run_test("025_completion_operators", "Main"); }

#[rustfmt::skip] #[test] fn test_026_completion_operator_suggestion_main() { run_test("026_completion_operator_suggestion", "Main"); }

#[rustfmt::skip] #[test] fn test_027_completion_type_operators_main() { run_test("027_completion_type_operators", "Main"); }

#[rustfmt::skip] #[test] fn test_028_completion_type_operator_suggestion_main() { run_test("028_completion_type_operator_suggestion", "Main"); }

#[rustfmt::skip] #[test] fn test_029_locate_declaration_main() { run_test("029_locate_declaration", "Main"); }

#[rustfmt::skip] #[test] fn test_030_locate_constructor_main() { run_test("030_locate_constructor", "Main"); }

#[rustfmt::skip] #[test] fn test_031_locate_class_member_main() { run_test("031_locate_class_member", "Main"); }

#[rustfmt::skip] #[test] fn test_032_completion_cache_exact_main() { run_test("032_completion_cache_exact", "Main"); }

#[rustfmt::skip] #[test] fn test_033_completion_cache_prefix_main() { run_test("033_completion_cache_prefix", "Main"); }
