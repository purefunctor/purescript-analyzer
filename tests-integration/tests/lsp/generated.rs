// Do not edit! See build.rs

#[rustfmt::skip]
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

#[rustfmt::skip] #[test] fn test_1751316180_definition_local_main() { run_test("1751316180_definition_local", "Main"); }

#[rustfmt::skip] #[test] fn test_1751358420_definition_import_main() { run_test("1751358420_definition_import", "Main"); }

#[rustfmt::skip] #[test] fn test_1751367660_hover_local_main() { run_test("1751367660_hover_local", "Main"); }

#[rustfmt::skip] #[test] fn test_1751391900_hover_import_main() { run_test("1751391900_hover_import", "Main"); }

#[rustfmt::skip] #[test] fn test_1751394000_completion_local_main() { run_test("1751394000_completion_local", "Main"); }

#[rustfmt::skip] #[test] fn test_1751395020_completion_partial_empty_main() { run_test("1751395020_completion_partial_empty", "Main"); }

#[rustfmt::skip] #[test] fn test_1751395080_completion_partial_token_main() { run_test("1751395080_completion_partial_token", "Main"); }

#[rustfmt::skip] #[test] fn test_1751469900_completion_import_main() { run_test("1751469900_completion_import", "Main"); }

#[rustfmt::skip] #[test] fn test_1751470680_completion_suggestion_main() { run_test("1751470680_completion_suggestion", "Main"); }

#[rustfmt::skip] #[test] fn test_1751470740_completion_qualified_suggestion_main() { run_test("1751470740_completion_qualified_suggestion", "Main"); }

#[rustfmt::skip] #[test] fn test_1751470800_completion_module_name_main() { run_test("1751470800_completion_module_name", "Main"); }

#[rustfmt::skip] #[test] fn test_1751558460_completion_edit_import_main() { run_test("1751558460_completion_edit_import", "Main"); }

#[rustfmt::skip] #[test] fn test_1753493760_completion_bad_cst_main() { run_test("1753493760_completion_bad_cst", "Main"); }

#[rustfmt::skip] #[test] fn test_1754895540_prim_implicit_main() { run_test("1754895540_prim_implicit", "Main"); }

#[rustfmt::skip] #[test] fn test_1754895600_prim_explicit_main() { run_test("1754895600_prim_explicit", "Main"); }

#[rustfmt::skip] #[test] fn test_1754898000_prim_suggestion_main() { run_test("1754898000_prim_suggestion", "Main"); }

#[rustfmt::skip] #[test] fn test_1755255480_unicode_operators_main() { run_test("1755255480_unicode_operators", "Main"); }

#[rustfmt::skip] #[test] fn test_1755261960_export_suggestion_main() { run_test("1755261960_export_suggestion", "Main"); }

#[rustfmt::skip] #[test] fn test_1757267400_documentation_string_main() { run_test("1757267400_documentation_string", "Main"); }

#[rustfmt::skip] #[test] fn test_1757527080_references_baseline_main() { run_test("1757527080_references_baseline", "Main"); }

#[rustfmt::skip] #[test] fn test_1757529660_references_qualified_main() { run_test("1757529660_references_qualified", "Main"); }

#[rustfmt::skip] #[test] fn test_1757532000_references_import_main() { run_test("1757532000_references_import", "Main"); }

#[rustfmt::skip] #[test] fn test_1757934780_references_duplicate_main() { run_test("1757934780_references_duplicate", "Main"); }

#[rustfmt::skip] #[test] fn test_1758124800_locate_annotation_main() { run_test("1758124800_locate_annotation", "Main"); }

#[rustfmt::skip] #[test] fn test_1758283620_completion_operators_main() { run_test("1758283620_completion_operators", "Main"); }

#[rustfmt::skip] #[test] fn test_1758283680_completion_operator_suggestion_main() { run_test("1758283680_completion_operator_suggestion", "Main"); }

#[rustfmt::skip] #[test] fn test_1758283740_completion_type_operators_main() { run_test("1758283740_completion_type_operators", "Main"); }

#[rustfmt::skip] #[test] fn test_1758283800_completion_type_operator_suggestion_main() { run_test("1758283800_completion_type_operator_suggestion", "Main"); }

#[rustfmt::skip] #[test] fn test_1758309780_locate_declaration_main() { run_test("1758309780_locate_declaration", "Main"); }

#[rustfmt::skip] #[test] fn test_1758309840_locate_constructor_main() { run_test("1758309840_locate_constructor", "Main"); }

#[rustfmt::skip] #[test] fn test_1758309900_locate_class_member_main() { run_test("1758309900_locate_class_member", "Main"); }

#[rustfmt::skip] #[test] fn test_1764148440_completion_cache_exact_main() { run_test("1764148440_completion_cache_exact", "Main"); }

#[rustfmt::skip] #[test] fn test_1764148500_completion_cache_prefix_main() { run_test("1764148500_completion_cache_prefix", "Main"); }

#[rustfmt::skip] #[test] fn test_1777394160_hover_term_literal_main() { run_test("1777394160_hover_term_literal", "Main"); }

#[rustfmt::skip] #[test] fn test_1777394220_hover_term_signature_main() { run_test("1777394220_hover_term_signature", "Main"); }

#[rustfmt::skip] #[test] fn test_1777394640_hover_term_pun_main() { run_test("1777394640_hover_term_pun", "Main"); }

#[rustfmt::skip] #[test] fn test_1777394700_hover_term_binder_main() { run_test("1777394700_hover_term_binder", "Main"); }

#[rustfmt::skip] #[test] fn test_1777476360_definition_pun_main() { run_test("1777476360_definition_pun", "Main"); }

#[rustfmt::skip] #[test] fn test_1777491240_hover_term_hole_filled_main() { run_test("1777491240_hover_term_hole_filled", "Main"); }

#[rustfmt::skip] #[test] fn test_1777491300_hover_term_let_main() { run_test("1777491300_hover_term_let", "Main"); }

#[rustfmt::skip] #[test] fn test_1777536720_hover_type_constructor_main() { run_test("1777536720_hover_type_constructor", "Main"); }

#[rustfmt::skip] #[test] fn test_1777536780_hover_type_literal_main() { run_test("1777536780_hover_type_literal", "Main"); }

#[rustfmt::skip] #[test] fn test_1777536840_hover_type_variable_main() { run_test("1777536840_hover_type_variable", "Main"); }
