// Do not edit! See build.rs

#[rustfmt::skip]
#[test]
fn test_001_local_resolution_main() {
    let mut compiler = tests_integration::load_compiler(std::path::Path::new("fixtures/deferred_resolution/001_local_resolution"));
    let Some(id) = compiler.runtime.module_file("Main") else { 
        return;
    };
    let report = tests_integration::core::report_deferred_resolution(&mut compiler, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_002_import_resolution_explicit_lib() {
    let mut compiler = tests_integration::load_compiler(std::path::Path::new("fixtures/deferred_resolution/002_import_resolution"));
    let Some(id) = compiler.runtime.module_file("ExplicitLib") else { 
        return;
    };
    let report = tests_integration::core::report_deferred_resolution(&mut compiler, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_002_import_resolution_implicit_lib() {
    let mut compiler = tests_integration::load_compiler(std::path::Path::new("fixtures/deferred_resolution/002_import_resolution"));
    let Some(id) = compiler.runtime.module_file("ImplicitLib") else { 
        return;
    };
    let report = tests_integration::core::report_deferred_resolution(&mut compiler, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_002_import_resolution_main() {
    let mut compiler = tests_integration::load_compiler(std::path::Path::new("fixtures/deferred_resolution/002_import_resolution"));
    let Some(id) = compiler.runtime.module_file("Main") else { 
        return;
    };
    let report = tests_integration::core::report_deferred_resolution(&mut compiler, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_002_import_resolution_main_qualified() {
    let mut compiler = tests_integration::load_compiler(std::path::Path::new("fixtures/deferred_resolution/002_import_resolution"));
    let Some(id) = compiler.runtime.module_file("MainQualified") else { 
        return;
    };
    let report = tests_integration::core::report_deferred_resolution(&mut compiler, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_002_import_resolution_main_qualified_explicit() {
    let mut compiler = tests_integration::load_compiler(std::path::Path::new("fixtures/deferred_resolution/002_import_resolution"));
    let Some(id) = compiler.runtime.module_file("MainQualifiedExplicit") else { 
        return;
    };
    let report = tests_integration::core::report_deferred_resolution(&mut compiler, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_002_import_resolution_main_qualified_explicit_merged() {
    let mut compiler = tests_integration::load_compiler(std::path::Path::new("fixtures/deferred_resolution/002_import_resolution"));
    let Some(id) = compiler.runtime.module_file("MainQualifiedExplicitMerged") else { 
        return;
    };
    let report = tests_integration::core::report_deferred_resolution(&mut compiler, id);
    insta::assert_snapshot!(report);
}
