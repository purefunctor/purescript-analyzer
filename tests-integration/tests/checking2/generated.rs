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
