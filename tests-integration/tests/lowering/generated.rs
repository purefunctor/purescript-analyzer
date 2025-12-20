// Do not edit! See build.rs

fn run_test(folder: &str, file: &str) {
    let path = std::path::Path::new("fixtures/lowering").join(folder);
    let (engine, _) = tests_integration::load_compiler(&path);
    let Some(id) = engine.module_file(file) else { return };
    let report = tests_integration::generated::basic::report_lowered(&engine, id, file);
    let mut settings = insta::Settings::clone_current();
    settings.set_snapshot_path(std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("fixtures/lowering").join(folder));
    settings.set_prepend_module_to_snapshot(false);
    settings.bind(|| insta::assert_snapshot!(file, report));
}

#[rustfmt::skip] #[test] fn test_001_ado_statement_recursion_main() { run_test("001_ado_statement_recursion", "Main"); }

#[rustfmt::skip] #[test] fn test_002_class_equation_main() { run_test("002_class_equation", "Main"); }

#[rustfmt::skip] #[test] fn test_003_data_equation_main() { run_test("003_data_equation", "Main"); }

#[rustfmt::skip] #[test] fn test_004_derive_declaration_main() { run_test("004_derive_declaration", "Main"); }

#[rustfmt::skip] #[test] fn test_005_do_statement_main() { run_test("005_do_statement", "Main"); }

#[rustfmt::skip] #[test] fn test_006_do_statement_recursion_main() { run_test("006_do_statement_recursion", "Main"); }

#[rustfmt::skip] #[test] fn test_007_instance_declaration_main() { run_test("007_instance_declaration", "Main"); }

#[rustfmt::skip] #[test] fn test_008_newtype_equation_main() { run_test("008_newtype_equation", "Main"); }

#[rustfmt::skip] #[test] fn test_009_signature_equation_main() { run_test("009_signature_equation", "Main"); }

#[rustfmt::skip] #[test] fn test_010_value_equation_main() { run_test("010_value_equation", "Main"); }

#[rustfmt::skip] #[test] fn test_011_case_after_let_main() { run_test("011_case_after_let", "Main"); }

#[rustfmt::skip] #[test] fn test_012_recursive_synonym_main() { run_test("012_recursive_synonym", "Main"); }

#[rustfmt::skip] #[test] fn test_013_ado_statement_let_main() { run_test("013_ado_statement_let", "Main"); }

#[rustfmt::skip] #[test] fn test_014_ado_statement_binder_main() { run_test("014_ado_statement_binder", "Main"); }
