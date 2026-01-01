// Do not edit! See build.rs

#[rustfmt::skip]
fn run_test(folder: &str, file: &str) {
    let path = std::path::Path::new("fixtures/resolving").join(folder);
    let (engine, _) = tests_integration::load_compiler(&path);
    let Some(id) = engine.module_file(file) else { return };
    let report = tests_integration::generated::basic::report_resolved(&engine, id, file);
    let mut settings = insta::Settings::clone_current();
    settings.set_snapshot_path(std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("fixtures/resolving").join(folder));
    settings.set_prepend_module_to_snapshot(false);
    settings.bind(|| insta::assert_snapshot!(file, report));
}

#[rustfmt::skip] #[test] fn test_001_local_resolution_explicit() { run_test("001_local_resolution", "Explicit"); }

#[rustfmt::skip] #[test] fn test_001_local_resolution_explicit_self() { run_test("001_local_resolution", "ExplicitSelf"); }

#[rustfmt::skip] #[test] fn test_001_local_resolution_implicit() { run_test("001_local_resolution", "Implicit"); }

#[rustfmt::skip] #[test] fn test_002_import_resolution_import_explicit() { run_test("002_import_resolution", "ImportExplicit"); }

#[rustfmt::skip] #[test] fn test_002_import_resolution_import_for_local_only() { run_test("002_import_resolution", "ImportForLocalOnly"); }

#[rustfmt::skip] #[test] fn test_002_import_resolution_import_hidden_constructor() { run_test("002_import_resolution", "ImportHiddenConstructor"); }

#[rustfmt::skip] #[test] fn test_002_import_resolution_import_qualified_explicit() { run_test("002_import_resolution", "ImportQualifiedExplicit"); }

#[rustfmt::skip] #[test] fn test_002_import_resolution_import_qualified_hiding() { run_test("002_import_resolution", "ImportQualifiedHiding"); }

#[rustfmt::skip] #[test] fn test_002_import_resolution_import_qualified_implicit() { run_test("002_import_resolution", "ImportQualifiedImplicit"); }

#[rustfmt::skip] #[test] fn test_002_import_resolution_import_unqualified_explicit() { run_test("002_import_resolution", "ImportUnqualifiedExplicit"); }

#[rustfmt::skip] #[test] fn test_002_import_resolution_import_unqualified_hiding() { run_test("002_import_resolution", "ImportUnqualifiedHiding"); }

#[rustfmt::skip] #[test] fn test_002_import_resolution_import_unqualified_implicit() { run_test("002_import_resolution", "ImportUnqualifiedImplicit"); }

#[rustfmt::skip] #[test] fn test_002_import_resolution_library() { run_test("002_import_resolution", "Library"); }

#[rustfmt::skip] #[test] fn test_002_import_resolution_library_explicit() { run_test("002_import_resolution", "LibraryExplicit"); }

#[rustfmt::skip] #[test] fn test_003_import_errors_duplicate_local() { run_test("003_import_errors", "DuplicateLocal"); }

#[rustfmt::skip] #[test] fn test_003_import_errors_duplicate_qualified_import() { run_test("003_import_errors", "DuplicateQualifiedImport"); }

#[rustfmt::skip] #[test] fn test_003_import_errors_invalid_constructor() { run_test("003_import_errors", "InvalidConstructor"); }

#[rustfmt::skip] #[test] fn test_003_import_errors_invalid_import() { run_test("003_import_errors", "InvalidImport"); }

#[rustfmt::skip] #[test] fn test_003_import_errors_library_a() { run_test("003_import_errors", "LibraryA"); }

#[rustfmt::skip] #[test] fn test_003_import_errors_library_b() { run_test("003_import_errors", "LibraryB"); }

#[rustfmt::skip] #[test] fn test_004_import_re_exported_constructor_internal() { run_test("004_import_re_exported_constructor", "Internal"); }

#[rustfmt::skip] #[test] fn test_004_import_re_exported_constructor_library() { run_test("004_import_re_exported_constructor", "Library"); }

#[rustfmt::skip] #[test] fn test_004_import_re_exported_constructor_main() { run_test("004_import_re_exported_constructor", "Main"); }
