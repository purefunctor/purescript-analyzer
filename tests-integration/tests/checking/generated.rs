// Do not edit! See build.rs

#[rustfmt::skip]
#[test]
fn test_001_proxy_checking_main() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/checking/001_proxy_checking"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::basic::report_checked(&engine, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_002_proxy_inference_main() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/checking/002_proxy_inference"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::basic::report_checked(&engine, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_003_data_recursive_main() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/checking/003_data_recursive"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::basic::report_checked(&engine, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_004_data_mutual_main() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/checking/004_data_mutual"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::basic::report_checked(&engine, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_005_newtype_recursive_main() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/checking/005_newtype_recursive"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::basic::report_checked(&engine, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_006_type_synonym_main() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/checking/006_type_synonym"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::basic::report_checked(&engine, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_007_foreign_poly_main() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/checking/007_foreign_poly"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::basic::report_checked(&engine, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_008_expand_simple_synonym_main() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/checking/008_expand_simple_synonym"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::basic::report_checked(&engine, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_009_expand_identity_synonym_main() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/checking/009_expand_identity_synonym"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::basic::report_checked(&engine, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_010_class_basic_main() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/checking/010_class_basic"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::basic::report_checked(&engine, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_011_class_functor_main() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/checking/011_class_functor"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::basic::report_checked(&engine, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_012_class_monad_state_main() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/checking/012_class_monad_state"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::basic::report_checked(&engine, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_013_class_phantom_main() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/checking/013_class_phantom"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::basic::report_checked(&engine, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_014_class_with_signature_main() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/checking/014_class_with_signature"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::basic::report_checked(&engine, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_015_class_superclass_main() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/checking/015_class_superclass"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::basic::report_checked(&engine, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_016_type_integer_main() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/checking/016_type_integer"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::basic::report_checked(&engine, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_017_type_string_main() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/checking/017_type_string"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::basic::report_checked(&engine, id);
    insta::assert_snapshot!(report);
}
