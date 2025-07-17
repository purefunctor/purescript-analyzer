// Do not edit! See build.rs

#[rustfmt::skip]
#[test]
fn test_001_local_resolution_explicit() {
    let compiler = tests_integration::load_compiler(std::path::Path::new("fixtures/resolving/001_local_resolution"));
    let Some(id) = compiler.engine.module_file("Explicit") else {
        return;
    };
    let report = tests_integration::core::report_resolved(&compiler, id, "Explicit");
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_001_local_resolution_explicit_self() {
    let compiler = tests_integration::load_compiler(std::path::Path::new("fixtures/resolving/001_local_resolution"));
    let Some(id) = compiler.engine.module_file("ExplicitSelf") else {
        return;
    };
    let report = tests_integration::core::report_resolved(&compiler, id, "ExplicitSelf");
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_001_local_resolution_implicit() {
    let compiler = tests_integration::load_compiler(std::path::Path::new("fixtures/resolving/001_local_resolution"));
    let Some(id) = compiler.engine.module_file("Implicit") else {
        return;
    };
    let report = tests_integration::core::report_resolved(&compiler, id, "Implicit");
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_002_import_resolution_import_explicit() {
    let compiler = tests_integration::load_compiler(std::path::Path::new("fixtures/resolving/002_import_resolution"));
    let Some(id) = compiler.engine.module_file("ImportExplicit") else {
        return;
    };
    let report = tests_integration::core::report_resolved(&compiler, id, "ImportExplicit");
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_002_import_resolution_import_for_local_only() {
    let compiler = tests_integration::load_compiler(std::path::Path::new("fixtures/resolving/002_import_resolution"));
    let Some(id) = compiler.engine.module_file("ImportForLocalOnly") else {
        return;
    };
    let report = tests_integration::core::report_resolved(&compiler, id, "ImportForLocalOnly");
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_002_import_resolution_import_hidden_constructor() {
    let compiler = tests_integration::load_compiler(std::path::Path::new("fixtures/resolving/002_import_resolution"));
    let Some(id) = compiler.engine.module_file("ImportHiddenConstructor") else {
        return;
    };
    let report = tests_integration::core::report_resolved(&compiler, id, "ImportHiddenConstructor");
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_002_import_resolution_import_qualified_explicit() {
    let compiler = tests_integration::load_compiler(std::path::Path::new("fixtures/resolving/002_import_resolution"));
    let Some(id) = compiler.engine.module_file("ImportQualifiedExplicit") else {
        return;
    };
    let report = tests_integration::core::report_resolved(&compiler, id, "ImportQualifiedExplicit");
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_002_import_resolution_import_qualified_hiding() {
    let compiler = tests_integration::load_compiler(std::path::Path::new("fixtures/resolving/002_import_resolution"));
    let Some(id) = compiler.engine.module_file("ImportQualifiedHiding") else {
        return;
    };
    let report = tests_integration::core::report_resolved(&compiler, id, "ImportQualifiedHiding");
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_002_import_resolution_import_qualified_implicit() {
    let compiler = tests_integration::load_compiler(std::path::Path::new("fixtures/resolving/002_import_resolution"));
    let Some(id) = compiler.engine.module_file("ImportQualifiedImplicit") else {
        return;
    };
    let report = tests_integration::core::report_resolved(&compiler, id, "ImportQualifiedImplicit");
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_002_import_resolution_import_unqualified_explicit() {
    let compiler = tests_integration::load_compiler(std::path::Path::new("fixtures/resolving/002_import_resolution"));
    let Some(id) = compiler.engine.module_file("ImportUnqualifiedExplicit") else {
        return;
    };
    let report = tests_integration::core::report_resolved(&compiler, id, "ImportUnqualifiedExplicit");
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_002_import_resolution_import_unqualified_hiding() {
    let compiler = tests_integration::load_compiler(std::path::Path::new("fixtures/resolving/002_import_resolution"));
    let Some(id) = compiler.engine.module_file("ImportUnqualifiedHiding") else {
        return;
    };
    let report = tests_integration::core::report_resolved(&compiler, id, "ImportUnqualifiedHiding");
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_002_import_resolution_import_unqualified_implicit() {
    let compiler = tests_integration::load_compiler(std::path::Path::new("fixtures/resolving/002_import_resolution"));
    let Some(id) = compiler.engine.module_file("ImportUnqualifiedImplicit") else {
        return;
    };
    let report = tests_integration::core::report_resolved(&compiler, id, "ImportUnqualifiedImplicit");
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_002_import_resolution_library() {
    let compiler = tests_integration::load_compiler(std::path::Path::new("fixtures/resolving/002_import_resolution"));
    let Some(id) = compiler.engine.module_file("Library") else {
        return;
    };
    let report = tests_integration::core::report_resolved(&compiler, id, "Library");
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_002_import_resolution_library_explicit() {
    let compiler = tests_integration::load_compiler(std::path::Path::new("fixtures/resolving/002_import_resolution"));
    let Some(id) = compiler.engine.module_file("LibraryExplicit") else {
        return;
    };
    let report = tests_integration::core::report_resolved(&compiler, id, "LibraryExplicit");
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_003_import_errors_duplicate_local() {
    let compiler = tests_integration::load_compiler(std::path::Path::new("fixtures/resolving/003_import_errors"));
    let Some(id) = compiler.engine.module_file("DuplicateLocal") else {
        return;
    };
    let report = tests_integration::core::report_resolved(&compiler, id, "DuplicateLocal");
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_003_import_errors_duplicate_qualified_import() {
    let compiler = tests_integration::load_compiler(std::path::Path::new("fixtures/resolving/003_import_errors"));
    let Some(id) = compiler.engine.module_file("DuplicateQualifiedImport") else {
        return;
    };
    let report = tests_integration::core::report_resolved(&compiler, id, "DuplicateQualifiedImport");
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_003_import_errors_invalid_constructor() {
    let compiler = tests_integration::load_compiler(std::path::Path::new("fixtures/resolving/003_import_errors"));
    let Some(id) = compiler.engine.module_file("InvalidConstructor") else {
        return;
    };
    let report = tests_integration::core::report_resolved(&compiler, id, "InvalidConstructor");
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_003_import_errors_invalid_import() {
    let compiler = tests_integration::load_compiler(std::path::Path::new("fixtures/resolving/003_import_errors"));
    let Some(id) = compiler.engine.module_file("InvalidImport") else {
        return;
    };
    let report = tests_integration::core::report_resolved(&compiler, id, "InvalidImport");
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_003_import_errors_library_a() {
    let compiler = tests_integration::load_compiler(std::path::Path::new("fixtures/resolving/003_import_errors"));
    let Some(id) = compiler.engine.module_file("LibraryA") else {
        return;
    };
    let report = tests_integration::core::report_resolved(&compiler, id, "LibraryA");
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_003_import_errors_library_b() {
    let compiler = tests_integration::load_compiler(std::path::Path::new("fixtures/resolving/003_import_errors"));
    let Some(id) = compiler.engine.module_file("LibraryB") else {
        return;
    };
    let report = tests_integration::core::report_resolved(&compiler, id, "LibraryB");
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_004_import_re_exported_constructor_internal() {
    let compiler = tests_integration::load_compiler(std::path::Path::new("fixtures/resolving/004_import_re_exported_constructor"));
    let Some(id) = compiler.engine.module_file("Internal") else {
        return;
    };
    let report = tests_integration::core::report_resolved(&compiler, id, "Internal");
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_004_import_re_exported_constructor_library() {
    let compiler = tests_integration::load_compiler(std::path::Path::new("fixtures/resolving/004_import_re_exported_constructor"));
    let Some(id) = compiler.engine.module_file("Library") else {
        return;
    };
    let report = tests_integration::core::report_resolved(&compiler, id, "Library");
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_004_import_re_exported_constructor_main() {
    let compiler = tests_integration::load_compiler(std::path::Path::new("fixtures/resolving/004_import_re_exported_constructor"));
    let Some(id) = compiler.engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::core::report_resolved(&compiler, id, "Main");
    insta::assert_snapshot!(report);
}
