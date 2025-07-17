// Do not edit! See build.rs

#[rustfmt::skip]
#[test]
fn test_001_local_resolution_main() {
    let compiler = tests_integration::load_compiler(std::path::Path::new("fixtures/deferred_resolution/001_local_resolution"));
    let Some(id) = compiler.engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::core::report_deferred_resolution(&compiler, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_002_import_resolution_explicit_lib() {
    let compiler = tests_integration::load_compiler(std::path::Path::new("fixtures/deferred_resolution/002_import_resolution"));
    let Some(id) = compiler.engine.module_file("ExplicitLib") else {
        return;
    };
    let report = tests_integration::core::report_deferred_resolution(&compiler, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_002_import_resolution_implicit_lib() {
    let compiler = tests_integration::load_compiler(std::path::Path::new("fixtures/deferred_resolution/002_import_resolution"));
    let Some(id) = compiler.engine.module_file("ImplicitLib") else {
        return;
    };
    let report = tests_integration::core::report_deferred_resolution(&compiler, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_002_import_resolution_main() {
    let compiler = tests_integration::load_compiler(std::path::Path::new("fixtures/deferred_resolution/002_import_resolution"));
    let Some(id) = compiler.engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::core::report_deferred_resolution(&compiler, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_002_import_resolution_main_qualified() {
    let compiler = tests_integration::load_compiler(std::path::Path::new("fixtures/deferred_resolution/002_import_resolution"));
    let Some(id) = compiler.engine.module_file("MainQualified") else {
        return;
    };
    let report = tests_integration::core::report_deferred_resolution(&compiler, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_002_import_resolution_main_qualified_explicit() {
    let compiler = tests_integration::load_compiler(std::path::Path::new("fixtures/deferred_resolution/002_import_resolution"));
    let Some(id) = compiler.engine.module_file("MainQualifiedExplicit") else {
        return;
    };
    let report = tests_integration::core::report_deferred_resolution(&compiler, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_002_import_resolution_main_qualified_explicit_merged() {
    let compiler = tests_integration::load_compiler(std::path::Path::new("fixtures/deferred_resolution/002_import_resolution"));
    let Some(id) = compiler.engine.module_file("MainQualifiedExplicitMerged") else {
        return;
    };
    let report = tests_integration::core::report_deferred_resolution(&compiler, id);
    insta::assert_snapshot!(report);
}
