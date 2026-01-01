// Do not edit! See build.rs

#[rustfmt::skip]
fn run_test(folder: &str, file: &str) {
    let path = std::path::Path::new("fixtures/checking").join(folder);
    let (engine, _) = tests_integration::load_compiler(&path);
    let Some(id) = engine.module_file(file) else { return };
    let report = tests_integration::generated::basic::report_checked(&engine, id);
    let mut settings = insta::Settings::clone_current();
    settings.set_snapshot_path(std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("fixtures/checking").join(folder));
    settings.set_prepend_module_to_snapshot(false);
    settings.bind(|| insta::assert_snapshot!(file, report));
}

#[rustfmt::skip] #[test] fn test_001_proxy_checking_main() { run_test("001_proxy_checking", "Main"); }

#[rustfmt::skip] #[test] fn test_002_proxy_inference_main() { run_test("002_proxy_inference", "Main"); }

#[rustfmt::skip] #[test] fn test_003_data_recursive_main() { run_test("003_data_recursive", "Main"); }

#[rustfmt::skip] #[test] fn test_004_data_mutual_main() { run_test("004_data_mutual", "Main"); }

#[rustfmt::skip] #[test] fn test_005_newtype_recursive_main() { run_test("005_newtype_recursive", "Main"); }

#[rustfmt::skip] #[test] fn test_006_type_synonym_main() { run_test("006_type_synonym", "Main"); }

#[rustfmt::skip] #[test] fn test_007_foreign_poly_main() { run_test("007_foreign_poly", "Main"); }

#[rustfmt::skip] #[test] fn test_008_expand_simple_synonym_main() { run_test("008_expand_simple_synonym", "Main"); }

#[rustfmt::skip] #[test] fn test_009_expand_identity_synonym_main() { run_test("009_expand_identity_synonym", "Main"); }

#[rustfmt::skip] #[test] fn test_010_class_basic_main() { run_test("010_class_basic", "Main"); }

#[rustfmt::skip] #[test] fn test_011_class_functor_main() { run_test("011_class_functor", "Main"); }

#[rustfmt::skip] #[test] fn test_012_class_monad_state_main() { run_test("012_class_monad_state", "Main"); }

#[rustfmt::skip] #[test] fn test_013_class_phantom_main() { run_test("013_class_phantom", "Main"); }

#[rustfmt::skip] #[test] fn test_014_class_with_signature_main() { run_test("014_class_with_signature", "Main"); }

#[rustfmt::skip] #[test] fn test_015_class_superclass_main() { run_test("015_class_superclass", "Main"); }

#[rustfmt::skip] #[test] fn test_016_type_integer_main() { run_test("016_type_integer", "Main"); }

#[rustfmt::skip] #[test] fn test_017_type_string_main() { run_test("017_type_string", "Main"); }

#[rustfmt::skip] #[test] fn test_018_type_operator_valid_main() { run_test("018_type_operator_valid", "Main"); }

#[rustfmt::skip] #[test] fn test_019_type_operator_chain_main() { run_test("019_type_operator_chain", "Main"); }

#[rustfmt::skip] #[test] fn test_020_type_operator_chain_mixed_main() { run_test("020_type_operator_chain_mixed", "Main"); }

#[rustfmt::skip] #[test] fn test_021_type_operator_chain_polykinded_main() { run_test("021_type_operator_chain_polykinded", "Main"); }

#[rustfmt::skip] #[test] fn test_022_row_basic_main() { run_test("022_row_basic", "Main"); }

#[rustfmt::skip] #[test] fn test_023_row_polymorphic_main() { run_test("023_row_polymorphic", "Main"); }

#[rustfmt::skip] #[test] fn test_024_row_duplicates_main() { run_test("024_row_duplicates", "Main"); }

#[rustfmt::skip] #[test] fn test_025_row_canon_main() { run_test("025_row_canon", "Main"); }

#[rustfmt::skip] #[test] fn test_026_row_empty_main() { run_test("026_row_empty", "Main"); }

#[rustfmt::skip] #[test] fn test_027_type_constrained_main() { run_test("027_type_constrained", "Main"); }

#[rustfmt::skip] #[test] fn test_028_partial_synonym_lazy_main() { run_test("028_partial_synonym_lazy", "Main"); }

#[rustfmt::skip] #[test] fn test_029_partial_synonym_transformers_main() { run_test("029_partial_synonym_transformers", "Main"); }

#[rustfmt::skip] #[test] fn test_030_partial_synonym_nested_main() { run_test("030_partial_synonym_nested", "Main"); }

#[rustfmt::skip] #[test] fn test_031_partial_synonym_polykind_main() { run_test("031_partial_synonym_polykind", "Main"); }

#[rustfmt::skip] #[test] fn test_032_recursive_synonym_expansion_main() { run_test("032_recursive_synonym_expansion", "Main"); }

#[rustfmt::skip] #[test] fn test_033_value_recursive_signature_main() { run_test("033_value_recursive_signature", "Main"); }

#[rustfmt::skip] #[test] fn test_034_value_recursive_infer_main() { run_test("034_value_recursive_infer", "Main"); }

#[rustfmt::skip] #[test] fn test_035_value_mutual_both_signature_main() { run_test("035_value_mutual_both_signature", "Main"); }

#[rustfmt::skip] #[test] fn test_036_value_mutual_one_signature_main() { run_test("036_value_mutual_one_signature", "Main"); }

#[rustfmt::skip] #[test] fn test_037_value_mutual_no_signature_main() { run_test("037_value_mutual_no_signature", "Main"); }

#[rustfmt::skip] #[test] fn test_038_value_mutual_polytype_main() { run_test("038_value_mutual_polytype", "Main"); }

#[rustfmt::skip] #[test] fn test_039_value_mutual_multiple_type_variables_main() { run_test("039_value_mutual_multiple_type_variables", "Main"); }

#[rustfmt::skip] #[test] fn test_040_pattern_guard_main() { run_test("040_pattern_guard", "Main"); }

#[rustfmt::skip] #[test] fn test_041_where_expression_main() { run_test("041_where_expression", "Main"); }

#[rustfmt::skip] #[test] fn test_042_where_polymorphic_main() { run_test("042_where_polymorphic", "Main"); }

#[rustfmt::skip] #[test] fn test_043_binder_named_main() { run_test("043_binder_named", "Main"); }

#[rustfmt::skip] #[test] fn test_044_binder_constructor_main() { run_test("044_binder_constructor", "Main"); }

#[rustfmt::skip] #[test] fn test_045_do_discard_main() { run_test("045_do_discard", "Main"); }

#[rustfmt::skip] #[test] fn test_046_do_bind_main() { run_test("046_do_bind", "Main"); }

#[rustfmt::skip] #[test] fn test_047_do_non_monadic_main() { run_test("047_do_non_monadic", "Main"); }

#[rustfmt::skip] #[test] fn test_048_do_non_monadic_discard_main() { run_test("048_do_non_monadic_discard", "Main"); }

#[rustfmt::skip] #[test] fn test_049_ado_discard_main() { run_test("049_ado_discard", "Main"); }

#[rustfmt::skip] #[test] fn test_050_ado_bind_main() { run_test("050_ado_bind", "Main"); }

#[rustfmt::skip] #[test] fn test_051_ado_non_applicative_main() { run_test("051_ado_non_applicative", "Main"); }

#[rustfmt::skip] #[test] fn test_052_ado_non_applicative_discard_main() { run_test("052_ado_non_applicative_discard", "Main"); }

#[rustfmt::skip] #[test] fn test_053_do_polymorphic_main() { run_test("053_do_polymorphic", "Main"); }

#[rustfmt::skip] #[test] fn test_054_ado_polymorphic_main() { run_test("054_ado_polymorphic", "Main"); }

#[rustfmt::skip] #[test] fn test_055_array_main() { run_test("055_array", "Main"); }

#[rustfmt::skip] #[test] fn test_056_record_main() { run_test("056_record", "Main"); }

#[rustfmt::skip] #[test] fn test_057_term_operator_chain_main() { run_test("057_term_operator_chain", "Main"); }

#[rustfmt::skip] #[test] fn test_058_binder_operator_chain_main() { run_test("058_binder_operator_chain", "Main"); }

#[rustfmt::skip] #[test] fn test_059_term_infix_chain_main() { run_test("059_term_infix_chain", "Main"); }

#[rustfmt::skip] #[test] fn test_060_array_binder_main() { run_test("060_array_binder", "Main"); }

#[rustfmt::skip] #[test] fn test_061_record_binder_main() { run_test("061_record_binder", "Main"); }

#[rustfmt::skip] #[test] fn test_062_case_of_main() { run_test("062_case_of", "Main"); }

#[rustfmt::skip] #[test] fn test_063_negate_main() { run_test("063_negate", "Main"); }

#[rustfmt::skip] #[test] fn test_064_negate_local_main() { run_test("064_negate_local", "Main"); }

#[rustfmt::skip] #[test] fn test_065_do_collector_main() { run_test("065_do_collector", "Main"); }

#[rustfmt::skip] #[test] fn test_066_ado_collector_main() { run_test("066_ado_collector", "Main"); }

#[rustfmt::skip] #[test] fn test_067_ado_let_main() { run_test("067_ado_let", "Main"); }

#[rustfmt::skip] #[test] fn test_068_expression_sections_main() { run_test("068_expression_sections", "Main"); }

#[rustfmt::skip] #[test] fn test_069_expression_sections_inference_main() { run_test("069_expression_sections_inference", "Main"); }

#[rustfmt::skip] #[test] fn test_070_record_access_sections_main() { run_test("070_record_access_sections", "Main"); }

#[rustfmt::skip] #[test] fn test_071_record_update_sections_main() { run_test("071_record_update_sections", "Main"); }

#[rustfmt::skip] #[test] fn test_072_inspect_synonym_forall_main() { run_test("072_inspect_synonym_forall", "Main"); }

#[rustfmt::skip] #[test] fn test_073_inspect_synonym_return_main() { run_test("073_inspect_synonym_return", "Main"); }

#[rustfmt::skip] #[test] fn test_074_inspect_nested_forall_main() { run_test("074_inspect_nested_forall", "Main"); }

#[rustfmt::skip] #[test] fn test_075_inspect_edge_cases_main() { run_test("075_inspect_edge_cases", "Main"); }

#[rustfmt::skip] #[test] fn test_076_inspect_constraints_main() { run_test("076_inspect_constraints", "Main"); }

#[rustfmt::skip] #[test] fn test_077_inspect_arity_main() { run_test("077_inspect_arity", "Main"); }

#[rustfmt::skip] #[test] fn test_078_inspect_arity_invalid_main() { run_test("078_inspect_arity_invalid", "Main"); }

#[rustfmt::skip] #[test] fn test_079_let_recursive_main() { run_test("079_let_recursive", "Main"); }

#[rustfmt::skip] #[test] fn test_080_let_recursive_errors_main() { run_test("080_let_recursive_errors", "Main"); }

#[rustfmt::skip] #[test] fn test_081_prim_rowlist_main() { run_test("081_prim_rowlist", "Main"); }

#[rustfmt::skip] #[test] fn test_082_prim_function_main() { run_test("082_prim_function", "Main"); }

#[rustfmt::skip] #[test] fn test_083_instance_basic_main() { run_test("083_instance_basic", "Main"); }

#[rustfmt::skip] #[test] fn test_084_instance_eq_main() { run_test("084_instance_eq", "Main"); }

#[rustfmt::skip] #[test] fn test_085_instance_functional_dependency_main() { run_test("085_instance_functional_dependency", "Main"); }

#[rustfmt::skip] #[test] fn test_086_instance_functional_dependency_transitive_main() { run_test("086_instance_functional_dependency_transitive", "Main"); }

#[rustfmt::skip] #[test] fn test_087_instance_functional_dependency_multiple_main() { run_test("087_instance_functional_dependency_multiple", "Main"); }

#[rustfmt::skip] #[test] fn test_088_given_constraint_matching_main() { run_test("088_given_constraint_matching", "Main"); }

#[rustfmt::skip] #[test] fn test_089_no_instance_found_main() { run_test("089_no_instance_found", "Main"); }

#[rustfmt::skip] #[test] fn test_090_instance_improve_main() { run_test("090_instance_improve", "Main"); }

#[rustfmt::skip] #[test] fn test_091_superclass_elaboration_main() { run_test("091_superclass_elaboration", "Main"); }

#[rustfmt::skip] #[test] fn test_092_ambiguous_constraint_main() { run_test("092_ambiguous_constraint", "Main"); }

#[rustfmt::skip] #[test] fn test_093_constraint_generalization_main() { run_test("093_constraint_generalization", "Main"); }

#[rustfmt::skip] #[test] fn test_094_let_binding_constraint_error_main() { run_test("094_let_binding_constraint_error", "Main"); }

#[rustfmt::skip] #[test] fn test_095_given_constraint_arityless_main() { run_test("095_given_constraint_arityless", "Main"); }

#[rustfmt::skip] #[test] fn test_096_given_functional_dependency_main() { run_test("096_given_functional_dependency", "Main"); }

#[rustfmt::skip] #[test] fn test_097_instance_chains_main() { run_test("097_instance_chains", "Main"); }

#[rustfmt::skip] #[test] fn test_098_fundep_propagation_main() { run_test("098_fundep_propagation", "Main"); }

#[rustfmt::skip] #[test] fn test_099_builtin_int_main() { run_test("099_builtin_int", "Main"); }

#[rustfmt::skip] #[test] fn test_100_builtin_given_main() { run_test("100_builtin_given", "Main"); }

#[rustfmt::skip] #[test] fn test_101_builtin_symbol_main() { run_test("101_builtin_symbol", "Main"); }

#[rustfmt::skip] #[test] fn test_102_builtin_row_main() { run_test("102_builtin_row", "Main"); }

#[rustfmt::skip] #[test] fn test_103_do_row_collector_main() { run_test("103_do_row_collector", "Main"); }
