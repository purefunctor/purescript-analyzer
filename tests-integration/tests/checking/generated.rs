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

#[rustfmt::skip] #[test] fn test_104_incomplete_signature_main() { run_test("104_incomplete_signature", "Main"); }

#[rustfmt::skip] #[test] fn test_105_incomplete_type_signature_main() { run_test("105_incomplete_type_signature", "Main"); }

#[rustfmt::skip] #[test] fn test_106_row_union_invalid_discharged_main() { run_test("106_row_union_invalid_discharged", "Main"); }

#[rustfmt::skip] #[test] fn test_107_symbol_append_invalid_discharged_main() { run_test("107_symbol_append_invalid_discharged", "Main"); }

#[rustfmt::skip] #[test] fn test_108_symbol_cons_invalid_discharged_main() { run_test("108_symbol_cons_invalid_discharged", "Main"); }

#[rustfmt::skip] #[test] fn test_109_row_cons_invalid_discharged_main() { run_test("109_row_cons_invalid_discharged", "Main"); }

#[rustfmt::skip] #[test] fn test_110_row_lacks_invalid_no_instance_main() { run_test("110_row_lacks_invalid_no_instance", "Main"); }

#[rustfmt::skip] #[test] fn test_111_int_add_invalid_no_instance_main() { run_test("111_int_add_invalid_no_instance", "Main"); }

#[rustfmt::skip] #[test] fn test_112_int_mul_invalid_no_instance_main() { run_test("112_int_mul_invalid_no_instance", "Main"); }

#[rustfmt::skip] #[test] fn test_113_int_compare_invalid_no_instance_main() { run_test("113_int_compare_invalid_no_instance", "Main"); }

#[rustfmt::skip] #[test] fn test_114_int_tostring_invalid_no_instance_main() { run_test("114_int_tostring_invalid_no_instance", "Main"); }

#[rustfmt::skip] #[test] fn test_115_empty_do_block_main() { run_test("115_empty_do_block", "Main"); }

#[rustfmt::skip] #[test] fn test_116_empty_ado_block_main() { run_test("116_empty_ado_block", "Main"); }

#[rustfmt::skip] #[test] fn test_117_do_ado_constrained_main() { run_test("117_do_ado_constrained", "Main"); }

#[rustfmt::skip] #[test] fn test_118_instance_member_type_match_main() { run_test("118_instance_member_type_match", "Main"); }

#[rustfmt::skip] #[test] fn test_119_instance_member_type_mismatch_main() { run_test("119_instance_member_type_mismatch", "Main"); }

#[rustfmt::skip] #[test] fn test_120_class_explicit_kind_variable_main() { run_test("120_class_explicit_kind_variable", "Main"); }

#[rustfmt::skip] #[test] fn test_121_instance_member_inner_forall_main() { run_test("121_instance_member_inner_forall", "Main"); }

#[rustfmt::skip] #[test] fn test_122_instance_member_inner_forall_constraint_main() { run_test("122_instance_member_inner_forall_constraint", "Main"); }

#[rustfmt::skip] #[test] fn test_123_incomplete_instance_head_main() { run_test("123_incomplete_instance_head", "Main"); }

#[rustfmt::skip] #[test] fn test_124_instance_member_missing_constraint_main() { run_test("124_instance_member_missing_constraint", "Main"); }

#[rustfmt::skip] #[test] fn test_125_instance_member_overly_general_main() { run_test("125_instance_member_overly_general", "Main"); }

#[rustfmt::skip] #[test] fn test_126_instance_phantom_main() { run_test("126_instance_phantom", "Main"); }

#[rustfmt::skip] #[test] fn test_127_derive_eq_simple_main() { run_test("127_derive_eq_simple", "Main"); }

#[rustfmt::skip] #[test] fn test_128_type_operator_mutual_main() { run_test("128_type_operator_mutual", "Main"); }

#[rustfmt::skip] #[test] fn test_129_derive_eq_with_fields_main() { run_test("129_derive_eq_with_fields", "Main"); }

#[rustfmt::skip] #[test] fn test_130_derive_eq_parameterized_main() { run_test("130_derive_eq_parameterized", "Main"); }

#[rustfmt::skip] #[test] fn test_131_derive_eq_missing_instance_main() { run_test("131_derive_eq_missing_instance", "Main"); }

#[rustfmt::skip] #[test] fn test_132_derive_eq_1_higher_kinded_main() { run_test("132_derive_eq_1_higher_kinded", "Main"); }

#[rustfmt::skip] #[test] fn test_133_derive_eq_partial_main() { run_test("133_derive_eq_partial", "Main"); }

#[rustfmt::skip] #[test] fn test_134_derive_ord_simple_main() { run_test("134_derive_ord_simple", "Main"); }

#[rustfmt::skip] #[test] fn test_135_derive_ord_1_higher_kinded_main() { run_test("135_derive_ord_1_higher_kinded", "Main"); }

#[rustfmt::skip] #[test] fn test_136_derive_nested_higher_kinded_main() { run_test("136_derive_nested_higher_kinded", "Main"); }

#[rustfmt::skip] #[test] fn test_137_derive_newtype_simple_main() { run_test("137_derive_newtype_simple", "Main"); }

#[rustfmt::skip] #[test] fn test_138_derive_newtype_parameterized_main() { run_test("138_derive_newtype_parameterized", "Main"); }

#[rustfmt::skip] #[test] fn test_139_derive_newtype_with_given_main() { run_test("139_derive_newtype_with_given", "Main"); }

#[rustfmt::skip] #[test] fn test_140_derive_newtype_recursive_main() { run_test("140_derive_newtype_recursive", "Main"); }

#[rustfmt::skip] #[test] fn test_141_derive_newtype_phantom_main() { run_test("141_derive_newtype_phantom", "Main"); }

#[rustfmt::skip] #[test] fn test_142_derive_newtype_not_newtype_main() { run_test("142_derive_newtype_not_newtype", "Main"); }

#[rustfmt::skip] #[test] fn test_143_derive_newtype_missing_instance_main() { run_test("143_derive_newtype_missing_instance", "Main"); }

#[rustfmt::skip] #[test] fn test_144_derive_newtype_missing_given_main() { run_test("144_derive_newtype_missing_given", "Main"); }

#[rustfmt::skip] #[test] fn test_145_derive_newtype_multi_param_main() { run_test("145_derive_newtype_multi_param", "Main"); }

#[rustfmt::skip] #[test] fn test_146_derive_functor_simple_main() { run_test("146_derive_functor_simple", "Main"); }

#[rustfmt::skip] #[test] fn test_147_derive_functor_higher_kinded_main() { run_test("147_derive_functor_higher_kinded", "Main"); }

#[rustfmt::skip] #[test] fn test_148_derive_functor_contravariant_error_main() { run_test("148_derive_functor_contravariant_error", "Main"); }

#[rustfmt::skip] #[test] fn test_149_derive_bifunctor_simple_main() { run_test("149_derive_bifunctor_simple", "Main"); }

#[rustfmt::skip] #[test] fn test_150_derive_bifunctor_higher_kinded_main() { run_test("150_derive_bifunctor_higher_kinded", "Main"); }

#[rustfmt::skip] #[test] fn test_151_derive_bifunctor_missing_functor_main() { run_test("151_derive_bifunctor_missing_functor", "Main"); }

#[rustfmt::skip] #[test] fn test_152_derive_contravariant_simple_main() { run_test("152_derive_contravariant_simple", "Main"); }

#[rustfmt::skip] #[test] fn test_153_derive_contravariant_error_main() { run_test("153_derive_contravariant_error", "Main"); }

#[rustfmt::skip] #[test] fn test_154_derive_profunctor_simple_main() { run_test("154_derive_profunctor_simple", "Main"); }

#[rustfmt::skip] #[test] fn test_155_derive_profunctor_error_main() { run_test("155_derive_profunctor_error", "Main"); }

#[rustfmt::skip] #[test] fn test_156_derive_bifunctor_insufficient_params_main() { run_test("156_derive_bifunctor_insufficient_params", "Main"); }

#[rustfmt::skip] #[test] fn test_157_derive_functor_insufficient_params_main() { run_test("157_derive_functor_insufficient_params", "Main"); }

#[rustfmt::skip] #[test] fn test_158_derive_foldable_simple_main() { run_test("158_derive_foldable_simple", "Main"); }

#[rustfmt::skip] #[test] fn test_159_derive_foldable_higher_kinded_main() { run_test("159_derive_foldable_higher_kinded", "Main"); }

#[rustfmt::skip] #[test] fn test_160_derive_bifoldable_simple_main() { run_test("160_derive_bifoldable_simple", "Main"); }

#[rustfmt::skip] #[test] fn test_161_derive_bifoldable_higher_kinded_main() { run_test("161_derive_bifoldable_higher_kinded", "Main"); }

#[rustfmt::skip] #[test] fn test_162_derive_traversable_simple_main() { run_test("162_derive_traversable_simple", "Main"); }

#[rustfmt::skip] #[test] fn test_163_derive_traversable_higher_kinded_main() { run_test("163_derive_traversable_higher_kinded", "Main"); }

#[rustfmt::skip] #[test] fn test_164_derive_bitraversable_simple_main() { run_test("164_derive_bitraversable_simple", "Main"); }

#[rustfmt::skip] #[test] fn test_165_derive_bitraversable_higher_kinded_main() { run_test("165_derive_bitraversable_higher_kinded", "Main"); }

#[rustfmt::skip] #[test] fn test_166_derive_traversable_missing_superclass_main() { run_test("166_derive_traversable_missing_superclass", "Main"); }

#[rustfmt::skip] #[test] fn test_167_derive_eq_1_main() { run_test("167_derive_eq_1", "Main"); }

#[rustfmt::skip] #[test] fn test_168_derive_ord_1_main() { run_test("168_derive_ord_1", "Main"); }

#[rustfmt::skip] #[test] fn test_169_derive_newtype_class_simple_main() { run_test("169_derive_newtype_class_simple", "Main"); }

#[rustfmt::skip] #[test] fn test_170_derive_newtype_class_parameterized_main() { run_test("170_derive_newtype_class_parameterized", "Main"); }

#[rustfmt::skip] #[test] fn test_171_derive_newtype_class_not_newtype_main() { run_test("171_derive_newtype_class_not_newtype", "Main"); }

#[rustfmt::skip] #[test] fn test_172_derive_generic_simple_main() { run_test("172_derive_generic_simple", "Main"); }

#[rustfmt::skip] #[test] fn test_173_derive_newtype_class_coercible_main() { run_test("173_derive_newtype_class_coercible", "Main"); }

#[rustfmt::skip] #[test] fn test_174_role_inference_phantom_main() { run_test("174_role_inference_phantom", "Main"); }

#[rustfmt::skip] #[test] fn test_175_role_inference_representational_main() { run_test("175_role_inference_representational", "Main"); }

#[rustfmt::skip] #[test] fn test_176_role_inference_nominal_constraint_main() { run_test("176_role_inference_nominal_constraint", "Main"); }

#[rustfmt::skip] #[test] fn test_177_role_inference_nominal_parametric_main() { run_test("177_role_inference_nominal_parametric", "Main"); }

#[rustfmt::skip] #[test] fn test_178_role_inference_nested_main() { run_test("178_role_inference_nested", "Main"); }

#[rustfmt::skip] #[test] fn test_179_role_inference_recursive_main() { run_test("179_role_inference_recursive", "Main"); }

#[rustfmt::skip] #[test] fn test_180_role_declaration_strengthen_main() { run_test("180_role_declaration_strengthen", "Main"); }

#[rustfmt::skip] #[test] fn test_181_role_declaration_loosen_error_main() { run_test("181_role_declaration_loosen_error", "Main"); }

#[rustfmt::skip] #[test] fn test_182_role_declaration_foreign_main() { run_test("182_role_declaration_foreign", "Main"); }

#[rustfmt::skip] #[test] fn test_183_coercible_reflexivity_main() { run_test("183_coercible_reflexivity", "Main"); }

#[rustfmt::skip] #[test] fn test_184_coercible_newtype_wrap_main() { run_test("184_coercible_newtype_wrap", "Main"); }

#[rustfmt::skip] #[test] fn test_185_coercible_phantom_main() { run_test("185_coercible_phantom", "Main"); }

#[rustfmt::skip] #[test] fn test_186_coercible_representational_main() { run_test("186_coercible_representational", "Main"); }

#[rustfmt::skip] #[test] fn test_187_coercible_array_main() { run_test("187_coercible_array", "Main"); }

#[rustfmt::skip] #[test] fn test_188_coercible_record_main() { run_test("188_coercible_record", "Main"); }

#[rustfmt::skip] #[test] fn test_189_coercible_different_heads_error_main() { run_test("189_coercible_different_heads_error", "Main"); }

#[rustfmt::skip] #[test] fn test_190_coercible_nominal_main() { run_test("190_coercible_nominal", "Main"); }

#[rustfmt::skip] #[test] fn test_191_coercible_newtype_hidden_main() { run_test("191_coercible_newtype_hidden", "Main"); }

#[rustfmt::skip] #[test] fn test_192_coercible_newtype_qualified_main() { run_test("192_coercible_newtype_qualified", "Main"); }

#[rustfmt::skip] #[test] fn test_193_coercible_newtype_open_hidden_main() { run_test("193_coercible_newtype_open_hidden", "Main"); }

#[rustfmt::skip] #[test] fn test_194_coercible_transitivity_main() { run_test("194_coercible_transitivity", "Main"); }

#[rustfmt::skip] #[test] fn test_195_coercible_nested_records_main() { run_test("195_coercible_nested_records", "Main"); }

#[rustfmt::skip] #[test] fn test_196_coercible_higher_kinded_main() { run_test("196_coercible_higher_kinded", "Main"); }

#[rustfmt::skip] #[test] fn test_197_coercible_higher_kinded_error_main() { run_test("197_coercible_higher_kinded_error", "Main"); }

#[rustfmt::skip] #[test] fn test_198_coercible_higher_kinded_multi_main() { run_test("198_coercible_higher_kinded_multi", "Main"); }

#[rustfmt::skip] #[test] fn test_199_coercible_higher_kinded_polykinded_main() { run_test("199_coercible_higher_kinded_polykinded", "Main"); }

#[rustfmt::skip] #[test] fn test_200_int_compare_transitive_main() { run_test("200_int_compare_transitive", "Main"); }

#[rustfmt::skip] #[test] fn test_201_int_compare_concrete_main() { run_test("201_int_compare_concrete", "Main"); }

#[rustfmt::skip] #[test] fn test_202_int_compare_invalid_main() { run_test("202_int_compare_invalid", "Main"); }

#[rustfmt::skip] #[test] fn test_203_is_symbol_main() { run_test("203_is_symbol", "Main"); }

#[rustfmt::skip] #[test] fn test_204_reflectable_main() { run_test("204_reflectable", "Main"); }

#[rustfmt::skip] #[test] fn test_205_builtin_warn_main() { run_test("205_builtin_warn", "Main"); }

#[rustfmt::skip] #[test] fn test_206_builtin_fail_main() { run_test("206_builtin_fail", "Main"); }
