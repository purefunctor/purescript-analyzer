// Do not edit! See build.rs

#[rustfmt::skip]
#[test]
fn test_001_local_resolution_main() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/deferred_resolution/001_local_resolution"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::core::report_deferred_resolution(&engine, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_002_import_resolution_explicit_lib() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/deferred_resolution/002_import_resolution"));
    let Some(id) = engine.module_file("ExplicitLib") else {
        return;
    };
    let report = tests_integration::core::report_deferred_resolution(&engine, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_002_import_resolution_implicit_lib() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/deferred_resolution/002_import_resolution"));
    let Some(id) = engine.module_file("ImplicitLib") else {
        return;
    };
    let report = tests_integration::core::report_deferred_resolution(&engine, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_002_import_resolution_main() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/deferred_resolution/002_import_resolution"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::core::report_deferred_resolution(&engine, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_002_import_resolution_main_qualified() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/deferred_resolution/002_import_resolution"));
    let Some(id) = engine.module_file("MainQualified") else {
        return;
    };
    let report = tests_integration::core::report_deferred_resolution(&engine, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_002_import_resolution_main_qualified_explicit() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/deferred_resolution/002_import_resolution"));
    let Some(id) = engine.module_file("MainQualifiedExplicit") else {
        return;
    };
    let report = tests_integration::core::report_deferred_resolution(&engine, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_002_import_resolution_main_qualified_explicit_merged() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/deferred_resolution/002_import_resolution"));
    let Some(id) = engine.module_file("MainQualifiedExplicitMerged") else {
        return;
    };
    let report = tests_integration::core::report_deferred_resolution(&engine, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_003_prim_resolution_implicit() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/deferred_resolution/003_prim_resolution"));
    let Some(id) = engine.module_file("Implicit") else {
        return;
    };
    let report = tests_integration::core::report_deferred_resolution(&engine, id);
    insta::assert_snapshot!(report);
}
