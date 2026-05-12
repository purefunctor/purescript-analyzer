// Do not edit! See build.rs

#[rustfmt::skip]
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

#[rustfmt::skip] #[test] fn test_1756058760_ado_statement_recursion_main() { run_test("1756058760_ado_statement_recursion", "Main"); }

#[rustfmt::skip] #[test] fn test_1756058820_class_equation_main() { run_test("1756058820_class_equation", "Main"); }

#[rustfmt::skip] #[test] fn test_1756058880_data_equation_main() { run_test("1756058880_data_equation", "Main"); }

#[rustfmt::skip] #[test] fn test_1756058940_derive_declaration_main() { run_test("1756058940_derive_declaration", "Main"); }

#[rustfmt::skip] #[test] fn test_1756059000_do_statement_main() { run_test("1756059000_do_statement", "Main"); }

#[rustfmt::skip] #[test] fn test_1756059060_do_statement_recursion_main() { run_test("1756059060_do_statement_recursion", "Main"); }

#[rustfmt::skip] #[test] fn test_1756059120_instance_declaration_main() { run_test("1756059120_instance_declaration", "Main"); }

#[rustfmt::skip] #[test] fn test_1756059180_newtype_equation_main() { run_test("1756059180_newtype_equation", "Main"); }

#[rustfmt::skip] #[test] fn test_1756059240_signature_equation_main() { run_test("1756059240_signature_equation", "Main"); }

#[rustfmt::skip] #[test] fn test_1756059300_value_equation_main() { run_test("1756059300_value_equation", "Main"); }

#[rustfmt::skip] #[test] fn test_1764310860_case_after_let_main() { run_test("1764310860_case_after_let", "Main"); }

#[rustfmt::skip] #[test] fn test_1765566180_recursive_synonym_main() { run_test("1765566180_recursive_synonym", "Main"); }

#[rustfmt::skip] #[test] fn test_1765970040_ado_statement_let_main() { run_test("1765970040_ado_statement_let", "Main"); }

#[rustfmt::skip] #[test] fn test_1765970100_ado_statement_binder_main() { run_test("1765970100_ado_statement_binder", "Main"); }

#[rustfmt::skip] #[test] fn test_1768582140_instance_constraints_main() { run_test("1768582140_instance_constraints", "Main"); }

#[rustfmt::skip] #[test] fn test_1768582200_derive_constraints_main() { run_test("1768582200_derive_constraints", "Main"); }
