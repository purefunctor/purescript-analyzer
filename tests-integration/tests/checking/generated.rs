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

#[rustfmt::skip]
#[test]
fn test_018_type_operator_valid_main() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/checking/018_type_operator_valid"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::basic::report_checked(&engine, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_019_type_operator_chain_main() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/checking/019_type_operator_chain"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::basic::report_checked(&engine, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_020_type_operator_chain_mixed_main() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/checking/020_type_operator_chain_mixed"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::basic::report_checked(&engine, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_021_type_operator_chain_polykinded_main() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/checking/021_type_operator_chain_polykinded"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::basic::report_checked(&engine, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_022_row_basic_main() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/checking/022_row_basic"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::basic::report_checked(&engine, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_023_row_polymorphic_main() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/checking/023_row_polymorphic"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::basic::report_checked(&engine, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_024_row_duplicates_main() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/checking/024_row_duplicates"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::basic::report_checked(&engine, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_025_row_canon_main() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/checking/025_row_canon"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::basic::report_checked(&engine, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_026_row_empty_main() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/checking/026_row_empty"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::basic::report_checked(&engine, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_027_type_constrained_main() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/checking/027_type_constrained"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::basic::report_checked(&engine, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_028_partial_synonym_lazy_main() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/checking/028_partial_synonym_lazy"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::basic::report_checked(&engine, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_029_partial_synonym_transformers_main() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/checking/029_partial_synonym_transformers"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::basic::report_checked(&engine, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_030_partial_synonym_nested_main() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/checking/030_partial_synonym_nested"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::basic::report_checked(&engine, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_031_partial_synonym_polykind_main() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/checking/031_partial_synonym_polykind"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::basic::report_checked(&engine, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_032_recursive_synonym_expansion_main() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/checking/032_recursive_synonym_expansion"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::basic::report_checked(&engine, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_033_value_recursive_signature_main() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/checking/033_value_recursive_signature"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::basic::report_checked(&engine, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_034_value_recursive_infer_main() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/checking/034_value_recursive_infer"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::basic::report_checked(&engine, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_035_value_mutual_both_signature_main() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/checking/035_value_mutual_both_signature"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::basic::report_checked(&engine, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_036_value_mutual_one_signature_main() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/checking/036_value_mutual_one_signature"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::basic::report_checked(&engine, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_037_value_mutual_no_signature_main() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/checking/037_value_mutual_no_signature"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::basic::report_checked(&engine, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_038_value_mutual_polytype_main() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/checking/038_value_mutual_polytype"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::basic::report_checked(&engine, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_039_value_mutual_multiple_type_variables_main() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/checking/039_value_mutual_multiple_type_variables"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::basic::report_checked(&engine, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_040_pattern_guard_main() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/checking/040_pattern_guard"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::basic::report_checked(&engine, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_041_where_expression_main() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/checking/041_where_expression"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::basic::report_checked(&engine, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_042_where_polymorphic_main() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/checking/042_where_polymorphic"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::basic::report_checked(&engine, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_043_binder_named_main() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/checking/043_binder_named"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::basic::report_checked(&engine, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_044_binder_constructor_main() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/checking/044_binder_constructor"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::basic::report_checked(&engine, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_045_do_discard_main() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/checking/045_do_discard"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::basic::report_checked(&engine, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_046_do_bind_main() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/checking/046_do_bind"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::basic::report_checked(&engine, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_047_do_non_monadic_main() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/checking/047_do_non_monadic"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::basic::report_checked(&engine, id);
    insta::assert_snapshot!(report);
}

#[rustfmt::skip]
#[test]
fn test_048_do_non_monadic_discard_main() {
    let (engine, _) = tests_integration::load_compiler(std::path::Path::new("fixtures/checking/048_do_non_monadic_discard"));
    let Some(id) = engine.module_file("Main") else {
        return;
    };
    let report = tests_integration::generated::basic::report_checked(&engine, id);
    insta::assert_snapshot!(report);
}
