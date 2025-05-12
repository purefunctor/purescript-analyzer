// Do not edit! See build.rs

#[rustfmt::skip]
#[test]
fn test_001_local_resolution_explicit() {
    let mut compiler = tests_integration::load_compiler(std::path::Path::new("fixtures/resolving/001_local_resolution"));
    let Some(id) = compiler.runtime.module_file("Explicit") else {
        return;
    };
    let resolved = compiler.runtime.resolved(id);
    let report = tests_integration::report_resolved("Explicit", &resolved);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_001_local_resolution_explicit_self() {
    let mut compiler = tests_integration::load_compiler(std::path::Path::new("fixtures/resolving/001_local_resolution"));
    let Some(id) = compiler.runtime.module_file("ExplicitSelf") else {
        return;
    };
    let resolved = compiler.runtime.resolved(id);
    let report = tests_integration::report_resolved("ExplicitSelf", &resolved);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_001_local_resolution_implicit() {
    let mut compiler = tests_integration::load_compiler(std::path::Path::new("fixtures/resolving/001_local_resolution"));
    let Some(id) = compiler.runtime.module_file("Implicit") else {
        return;
    };
    let resolved = compiler.runtime.resolved(id);
    let report = tests_integration::report_resolved("Implicit", &resolved);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_002_import_resolution_import_explicit() {
    let mut compiler = tests_integration::load_compiler(std::path::Path::new("fixtures/resolving/002_import_resolution"));
    let Some(id) = compiler.runtime.module_file("ImportExplicit") else {
        return;
    };
    let resolved = compiler.runtime.resolved(id);
    let report = tests_integration::report_resolved("ImportExplicit", &resolved);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_002_import_resolution_import_for_local_only() {
    let mut compiler = tests_integration::load_compiler(std::path::Path::new("fixtures/resolving/002_import_resolution"));
    let Some(id) = compiler.runtime.module_file("ImportForLocalOnly") else {
        return;
    };
    let resolved = compiler.runtime.resolved(id);
    let report = tests_integration::report_resolved("ImportForLocalOnly", &resolved);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_002_import_resolution_import_hidden_constructor() {
    let mut compiler = tests_integration::load_compiler(std::path::Path::new("fixtures/resolving/002_import_resolution"));
    let Some(id) = compiler.runtime.module_file("ImportHiddenConstructor") else {
        return;
    };
    let resolved = compiler.runtime.resolved(id);
    let report = tests_integration::report_resolved("ImportHiddenConstructor", &resolved);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_002_import_resolution_import_qualified_explicit() {
    let mut compiler = tests_integration::load_compiler(std::path::Path::new("fixtures/resolving/002_import_resolution"));
    let Some(id) = compiler.runtime.module_file("ImportQualifiedExplicit") else {
        return;
    };
    let resolved = compiler.runtime.resolved(id);
    let report = tests_integration::report_resolved("ImportQualifiedExplicit", &resolved);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_002_import_resolution_import_qualified_hiding() {
    let mut compiler = tests_integration::load_compiler(std::path::Path::new("fixtures/resolving/002_import_resolution"));
    let Some(id) = compiler.runtime.module_file("ImportQualifiedHiding") else {
        return;
    };
    let resolved = compiler.runtime.resolved(id);
    let report = tests_integration::report_resolved("ImportQualifiedHiding", &resolved);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_002_import_resolution_import_qualified_implicit() {
    let mut compiler = tests_integration::load_compiler(std::path::Path::new("fixtures/resolving/002_import_resolution"));
    let Some(id) = compiler.runtime.module_file("ImportQualifiedImplicit") else {
        return;
    };
    let resolved = compiler.runtime.resolved(id);
    let report = tests_integration::report_resolved("ImportQualifiedImplicit", &resolved);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_002_import_resolution_import_unqualified_explicit() {
    let mut compiler = tests_integration::load_compiler(std::path::Path::new("fixtures/resolving/002_import_resolution"));
    let Some(id) = compiler.runtime.module_file("ImportUnqualifiedExplicit") else {
        return;
    };
    let resolved = compiler.runtime.resolved(id);
    let report = tests_integration::report_resolved("ImportUnqualifiedExplicit", &resolved);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_002_import_resolution_import_unqualified_hiding() {
    let mut compiler = tests_integration::load_compiler(std::path::Path::new("fixtures/resolving/002_import_resolution"));
    let Some(id) = compiler.runtime.module_file("ImportUnqualifiedHiding") else {
        return;
    };
    let resolved = compiler.runtime.resolved(id);
    let report = tests_integration::report_resolved("ImportUnqualifiedHiding", &resolved);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_002_import_resolution_import_unqualified_implicit() {
    let mut compiler = tests_integration::load_compiler(std::path::Path::new("fixtures/resolving/002_import_resolution"));
    let Some(id) = compiler.runtime.module_file("ImportUnqualifiedImplicit") else {
        return;
    };
    let resolved = compiler.runtime.resolved(id);
    let report = tests_integration::report_resolved("ImportUnqualifiedImplicit", &resolved);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_002_import_resolution_library() {
    let mut compiler = tests_integration::load_compiler(std::path::Path::new("fixtures/resolving/002_import_resolution"));
    let Some(id) = compiler.runtime.module_file("Library") else {
        return;
    };
    let resolved = compiler.runtime.resolved(id);
    let report = tests_integration::report_resolved("Library", &resolved);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_002_import_resolution_library_explicit() {
    let mut compiler = tests_integration::load_compiler(std::path::Path::new("fixtures/resolving/002_import_resolution"));
    let Some(id) = compiler.runtime.module_file("LibraryExplicit") else {
        return;
    };
    let resolved = compiler.runtime.resolved(id);
    let report = tests_integration::report_resolved("LibraryExplicit", &resolved);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_003_import_errors_duplicate_local() {
    let mut compiler = tests_integration::load_compiler(std::path::Path::new("fixtures/resolving/003_import_errors"));
    let Some(id) = compiler.runtime.module_file("DuplicateLocal") else {
        return;
    };
    let resolved = compiler.runtime.resolved(id);
    let report = tests_integration::report_resolved("DuplicateLocal", &resolved);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_003_import_errors_duplicate_qualified_import() {
    let mut compiler = tests_integration::load_compiler(std::path::Path::new("fixtures/resolving/003_import_errors"));
    let Some(id) = compiler.runtime.module_file("DuplicateQualifiedImport") else {
        return;
    };
    let resolved = compiler.runtime.resolved(id);
    let report = tests_integration::report_resolved("DuplicateQualifiedImport", &resolved);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_003_import_errors_invalid_constructor() {
    let mut compiler = tests_integration::load_compiler(std::path::Path::new("fixtures/resolving/003_import_errors"));
    let Some(id) = compiler.runtime.module_file("InvalidConstructor") else {
        return;
    };
    let resolved = compiler.runtime.resolved(id);
    let report = tests_integration::report_resolved("InvalidConstructor", &resolved);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_003_import_errors_invalid_import() {
    let mut compiler = tests_integration::load_compiler(std::path::Path::new("fixtures/resolving/003_import_errors"));
    let Some(id) = compiler.runtime.module_file("InvalidImport") else {
        return;
    };
    let resolved = compiler.runtime.resolved(id);
    let report = tests_integration::report_resolved("InvalidImport", &resolved);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_003_import_errors_library_a() {
    let mut compiler = tests_integration::load_compiler(std::path::Path::new("fixtures/resolving/003_import_errors"));
    let Some(id) = compiler.runtime.module_file("LibraryA") else {
        return;
    };
    let resolved = compiler.runtime.resolved(id);
    let report = tests_integration::report_resolved("LibraryA", &resolved);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_003_import_errors_library_b() {
    let mut compiler = tests_integration::load_compiler(std::path::Path::new("fixtures/resolving/003_import_errors"));
    let Some(id) = compiler.runtime.module_file("LibraryB") else {
        return;
    };
    let resolved = compiler.runtime.resolved(id);
    let report = tests_integration::report_resolved("LibraryB", &resolved);
    insta::assert_snapshot!(report);
}
