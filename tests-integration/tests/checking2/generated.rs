// Do not edit! See build.rs

#[rustfmt::skip]
fn run_test(folder: &str, file: &str) {
    let path = std::path::Path::new("fixtures/checking2").join(folder);
    let (engine, _) = tests_integration::load_compiler(&path);
    let Some(id) = engine.module_file(file) else { return };

    let level = match std::env::var("TRACE_LEVEL").as_deref() {
        Ok("debug") => tracing::Level::DEBUG,
        _ => tracing::Level::WARN,
    };

    let target_dir = env!("CARGO_TARGET_TMPDIR");
    let test_name = format!("{}_{}",  folder, file);
    let (report, trace_path) = tests_integration::trace::with_file_trace(
        level,
        target_dir,
        &test_name,
        || tests_integration::generated::basic::report_checked2(&engine, id)
    );

    println!("trace: {}", trace_path.display());

    let mut settings = insta::Settings::clone_current();
    settings.set_snapshot_path(std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("fixtures/checking2").join(folder));
    settings.set_prepend_module_to_snapshot(false);
    settings.bind(|| insta::assert_snapshot!(file, report));
}

#[rustfmt::skip] #[test] fn test_gitkeep_main() { run_test("gitkeep", "Main"); }

#[rustfmt::skip] #[test] fn test_001_foreign_check_main() { run_test("001_foreign_check", "Main"); }

#[rustfmt::skip] #[test] fn test_002_foreign_recursive_main() { run_test("002_foreign_recursive", "Main"); }

#[rustfmt::skip] #[test] fn test_003_data_check_main() { run_test("003_data_check", "Main"); }

#[rustfmt::skip] #[test] fn test_004_data_infer_main() { run_test("004_data_infer", "Main"); }

#[rustfmt::skip] #[test] fn test_005_newtype_check_main() { run_test("005_newtype_check", "Main"); }

#[rustfmt::skip] #[test] fn test_006_newtype_infer_main() { run_test("006_newtype_infer", "Main"); }

#[rustfmt::skip] #[test] fn test_007_synonym_check_main() { run_test("007_synonym_check", "Main"); }

#[rustfmt::skip] #[test] fn test_008_synonym_infer_main() { run_test("008_synonym_infer", "Main"); }

#[rustfmt::skip] #[test] fn test_009_class_check_main() { run_test("009_class_check", "Main"); }

#[rustfmt::skip] #[test] fn test_010_class_infer_main() { run_test("010_class_infer", "Main"); }

#[rustfmt::skip] #[test] fn test_011_class_superclass_main() { run_test("011_class_superclass", "Main"); }

#[rustfmt::skip] #[test] fn test_012_class_polykind_check_main() { run_test("012_class_polykind_check", "Main"); }

#[rustfmt::skip] #[test] fn test_013_class_polykind_infer_main() { run_test("013_class_polykind_infer", "Main"); }

#[rustfmt::skip] #[test] fn test_014_operator_alias_kind_main() { run_test("014_operator_alias_kind", "Main"); }

#[rustfmt::skip] #[test] fn test_015_operator_alias_invalid_kind_main() { run_test("015_operator_alias_invalid_kind", "Main"); }

#[rustfmt::skip] #[test] fn test_016_type_operator_chain_infer_main() { run_test("016_type_operator_chain_infer", "Main"); }

#[rustfmt::skip] #[test] fn test_017_type_operator_chain_check_main() { run_test("017_type_operator_chain_check", "Main"); }

#[rustfmt::skip] #[test] fn test_018_type_operator_chain_polykind_main() { run_test("018_type_operator_chain_polykind", "Main"); }

#[rustfmt::skip] #[test] fn test_019_type_operator_chain_precedence_main() { run_test("019_type_operator_chain_precedence", "Main"); }

#[rustfmt::skip] #[test] fn test_020_type_operator_chain_kind_error_main() { run_test("020_type_operator_chain_kind_error", "Main"); }

#[rustfmt::skip] #[test] fn test_021_role_inference_phantom_main() { run_test("021_role_inference_phantom", "Main"); }

#[rustfmt::skip] #[test] fn test_022_role_inference_representational_main() { run_test("022_role_inference_representational", "Main"); }

#[rustfmt::skip] #[test] fn test_023_role_inference_nominal_parametric_main() { run_test("023_role_inference_nominal_parametric", "Main"); }

#[rustfmt::skip] #[test] fn test_024_role_declaration_strengthen_main() { run_test("024_role_declaration_strengthen", "Main"); }

#[rustfmt::skip] #[test] fn test_025_role_declaration_loosen_error_main() { run_test("025_role_declaration_loosen_error", "Main"); }

#[rustfmt::skip] #[test] fn test_026_role_declaration_foreign_main() { run_test("026_role_declaration_foreign", "Main"); }

#[rustfmt::skip] #[test] fn test_027_foreign_check_main() { run_test("027_foreign_check", "Main"); }

#[rustfmt::skip] #[test] fn test_028_value_check_main() { run_test("028_value_check", "Main"); }
