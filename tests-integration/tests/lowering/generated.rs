// Do not edit! See build.rs

#[rustfmt::skip]
#[test]
fn test_001_ado_statement_recursion_main() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/lowering/001_ado_statement_recursion"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::basic::report_lowered(&engine, id, "Main");
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_002_class_equation_main() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/lowering/002_class_equation"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::basic::report_lowered(&engine, id, "Main");
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_003_data_equation_main() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/lowering/003_data_equation"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::basic::report_lowered(&engine, id, "Main");
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_004_derive_declaration_main() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/lowering/004_derive_declaration"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::basic::report_lowered(&engine, id, "Main");
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_005_do_statement_main() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/lowering/005_do_statement"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::basic::report_lowered(&engine, id, "Main");
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_006_do_statement_recursion_main() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/lowering/006_do_statement_recursion"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::basic::report_lowered(&engine, id, "Main");
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_007_instance_declaration_main() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/lowering/007_instance_declaration"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::basic::report_lowered(&engine, id, "Main");
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_008_newtype_equation_main() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/lowering/008_newtype_equation"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::basic::report_lowered(&engine, id, "Main");
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_009_signature_equation_main() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/lowering/009_signature_equation"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::basic::report_lowered(&engine, id, "Main");
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_010_value_equation_main() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/lowering/010_value_equation"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::basic::report_lowered(&engine, id, "Main");
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_011_case_after_let_main() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/lowering/011_case_after_let"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::basic::report_lowered(&engine, id, "Main");
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_012_recursive_synonym_main() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/lowering/012_recursive_synonym"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::basic::report_lowered(&engine, id, "Main");
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_013_ado_statement_let_main() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/lowering/013_ado_statement_let"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::basic::report_lowered(&engine, id, "Main");
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_014_ado_statement_binder_main() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/lowering/014_ado_statement_binder"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::basic::report_lowered(&engine, id, "Main");
    insta::assert_snapshot!(report);
}
