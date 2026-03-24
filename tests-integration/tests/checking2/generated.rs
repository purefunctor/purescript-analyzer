// Do not edit! See build.rs

#[rustfmt::skip]
fn run_test(folder: &str, file: &str) {
    let path = std::path::Path::new("fixtures/checking2").join(folder);
    let (engine, _) = tests_integration::load_compiler(&path);
    let Some(id) = engine.module_file(file) else { return };

    let level = match std::env::var("TRACE_LEVEL").as_deref() {
        Ok("debug") => tracing::Level::DEBUG,
        _ => tracing::Level::WARN,
    };

    let target_dir = env!("CARGO_TARGET_TMPDIR");
    let test_name = format!("{}_{}",  folder, file);
    let (report, trace_path) = tests_integration::trace::with_file_trace(
        level,
        target_dir,
        &test_name,
        || tests_integration::generated::basic::report_checked2(&engine, id)
    );

    println!("trace: {}", trace_path.display());

    let mut settings = insta::Settings::clone_current();
    settings.set_snapshot_path(std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("fixtures/checking2").join(folder));
    settings.set_prepend_module_to_snapshot(false);
    settings.bind(|| insta::assert_snapshot!(file, report));
}

#[rustfmt::skip] #[test] fn test_001_foreign_check_main() { run_test("001_foreign_check", "Main"); }

#[rustfmt::skip] #[test] fn test_002_foreign_recursive_main() { run_test("002_foreign_recursive", "Main"); }

#[rustfmt::skip] #[test] fn test_003_data_check_main() { run_test("003_data_check", "Main"); }

#[rustfmt::skip] #[test] fn test_004_data_infer_main() { run_test("004_data_infer", "Main"); }

#[rustfmt::skip] #[test] fn test_005_newtype_check_main() { run_test("005_newtype_check", "Main"); }

#[rustfmt::skip] #[test] fn test_006_newtype_infer_main() { run_test("006_newtype_infer", "Main"); }

#[rustfmt::skip] #[test] fn test_007_synonym_check_main() { run_test("007_synonym_check", "Main"); }

#[rustfmt::skip] #[test] fn test_008_synonym_infer_main() { run_test("008_synonym_infer", "Main"); }

#[rustfmt::skip] #[test] fn test_009_class_check_main() { run_test("009_class_check", "Main"); }

#[rustfmt::skip] #[test] fn test_010_class_infer_main() { run_test("010_class_infer", "Main"); }

#[rustfmt::skip] #[test] fn test_011_class_superclass_main() { run_test("011_class_superclass", "Main"); }

#[rustfmt::skip] #[test] fn test_012_class_polykind_check_main() { run_test("012_class_polykind_check", "Main"); }

#[rustfmt::skip] #[test] fn test_013_class_polykind_infer_main() { run_test("013_class_polykind_infer", "Main"); }

#[rustfmt::skip] #[test] fn test_014_operator_alias_kind_main() { run_test("014_operator_alias_kind", "Main"); }

#[rustfmt::skip] #[test] fn test_015_operator_alias_invalid_kind_main() { run_test("015_operator_alias_invalid_kind", "Main"); }

#[rustfmt::skip] #[test] fn test_016_type_operator_chain_infer_main() { run_test("016_type_operator_chain_infer", "Main"); }

#[rustfmt::skip] #[test] fn test_017_type_operator_chain_check_main() { run_test("017_type_operator_chain_check", "Main"); }

#[rustfmt::skip] #[test] fn test_018_type_operator_chain_polykind_main() { run_test("018_type_operator_chain_polykind", "Main"); }

#[rustfmt::skip] #[test] fn test_019_type_operator_chain_precedence_main() { run_test("019_type_operator_chain_precedence", "Main"); }

#[rustfmt::skip] #[test] fn test_020_type_operator_chain_kind_error_main() { run_test("020_type_operator_chain_kind_error", "Main"); }

#[rustfmt::skip] #[test] fn test_021_role_inference_phantom_main() { run_test("021_role_inference_phantom", "Main"); }

#[rustfmt::skip] #[test] fn test_022_role_inference_representational_main() { run_test("022_role_inference_representational", "Main"); }

#[rustfmt::skip] #[test] fn test_023_role_inference_nominal_parametric_main() { run_test("023_role_inference_nominal_parametric", "Main"); }

#[rustfmt::skip] #[test] fn test_024_role_declaration_strengthen_main() { run_test("024_role_declaration_strengthen", "Main"); }

#[rustfmt::skip] #[test] fn test_025_role_declaration_loosen_error_main() { run_test("025_role_declaration_loosen_error", "Main"); }

#[rustfmt::skip] #[test] fn test_026_role_declaration_foreign_main() { run_test("026_role_declaration_foreign", "Main"); }

#[rustfmt::skip] #[test] fn test_027_foreign_check_main() { run_test("027_foreign_check", "Main"); }

#[rustfmt::skip] #[test] fn test_028_value_check_main() { run_test("028_value_check", "Main"); }

#[rustfmt::skip] #[test] fn test_029_operator_check_main() { run_test("029_operator_check", "Main"); }

#[rustfmt::skip] #[test] fn test_030_exhaustive_case_infer_main() { run_test("030_exhaustive_case_infer", "Main"); }

#[rustfmt::skip] #[test] fn test_031_exhaustive_case_redundant_main() { run_test("031_exhaustive_case_redundant", "Main"); }

#[rustfmt::skip] #[test] fn test_032_exhaustive_equation_signature_main() { run_test("032_exhaustive_equation_signature", "Main"); }

#[rustfmt::skip] #[test] fn test_033_exhaustive_guards_otherwise_main() { run_test("033_exhaustive_guards_otherwise", "Main"); }

#[rustfmt::skip] #[test] fn test_034_exhaustive_let_pattern_main() { run_test("034_exhaustive_let_pattern", "Main"); }

#[rustfmt::skip] #[test] fn test_035_exhaustive_operator_constructor_main() { run_test("035_exhaustive_operator_constructor", "Main"); }

#[rustfmt::skip] #[test] fn test_036_synonym_partial_defer_main() { run_test("036_synonym_partial_defer", "Main"); }

#[rustfmt::skip] #[test] fn test_037_value_recursive_check_main() { run_test("037_value_recursive_check", "Main"); }

#[rustfmt::skip] #[test] fn test_038_value_recursive_infer_main() { run_test("038_value_recursive_infer", "Main"); }

#[rustfmt::skip] #[test] fn test_039_value_mutual_check_main() { run_test("039_value_mutual_check", "Main"); }

#[rustfmt::skip] #[test] fn test_040_value_mutual_infer_main() { run_test("040_value_mutual_infer", "Main"); }

#[rustfmt::skip] #[test] fn test_041_data_mutual_check_main() { run_test("041_data_mutual_check", "Main"); }

#[rustfmt::skip] #[test] fn test_042_data_mutual_infer_main() { run_test("042_data_mutual_infer", "Main"); }

#[rustfmt::skip] #[test] fn test_043_instance_check_main() { run_test("043_instance_check", "Main"); }

#[rustfmt::skip] #[test] fn test_044_instance_constraint_solving_main() { run_test("044_instance_constraint_solving", "Main"); }

#[rustfmt::skip] #[test] fn test_045_instance_functional_dependency_main() { run_test("045_instance_functional_dependency", "Main"); }

#[rustfmt::skip] #[test] fn test_046_instance_given_constraint_main() { run_test("046_instance_given_constraint", "Main"); }

#[rustfmt::skip] #[test] fn test_047_instance_constraint_generalization_main() { run_test("047_instance_constraint_generalization", "Main"); }

#[rustfmt::skip] #[test] fn test_048_instance_superclass_elaboration_main() { run_test("048_instance_superclass_elaboration", "Main"); }

#[rustfmt::skip] #[test] fn test_049_given_constraint_check_main() { run_test("049_given_constraint_check", "Main"); }

#[rustfmt::skip] #[test] fn test_050_given_constraint_let_check_main() { run_test("050_given_constraint_let_check", "Main"); }

#[rustfmt::skip] #[test] fn test_051_given_constraint_operator_check_main() { run_test("051_given_constraint_operator_check", "Main"); }

#[rustfmt::skip] #[test] fn test_052_prim_int_main() { run_test("052_prim_int", "Main"); }

#[rustfmt::skip] #[test] fn test_053_prim_int_compare_transitive_main() { run_test("053_prim_int_compare_transitive", "Main"); }

#[rustfmt::skip] #[test] fn test_054_prim_symbol_main() { run_test("054_prim_symbol", "Main"); }

#[rustfmt::skip] #[test] fn test_055_prim_solver_apart_main() { run_test("055_prim_solver_apart", "Main"); }

#[rustfmt::skip] #[test] fn test_056_prim_row_list_main() { run_test("056_prim_row_list", "Main"); }

#[rustfmt::skip] #[test] fn test_057_prim_row_main() { run_test("057_prim_row", "Main"); }

#[rustfmt::skip] #[test] fn test_058_prim_row_apart_main() { run_test("058_prim_row_apart", "Main"); }

#[rustfmt::skip] #[test] fn test_059_prim_row_open_main() { run_test("059_prim_row_open", "Main"); }

#[rustfmt::skip] #[test] fn test_060_prim_row_generalization_main() { run_test("060_prim_row_generalization", "Main"); }

#[rustfmt::skip] #[test] fn test_061_prim_row_nub_left_bias_main() { run_test("061_prim_row_nub_left_bias", "Main"); }

#[rustfmt::skip] #[test] fn test_062_prim_row_record_main() { run_test("062_prim_row_record", "Main"); }

#[rustfmt::skip] #[test] fn test_063_prim_reflectable_main() { run_test("063_prim_reflectable", "Main"); }

#[rustfmt::skip] #[test] fn test_064_prim_type_error_warn_main() { run_test("064_prim_type_error_warn", "Main"); }

#[rustfmt::skip] #[test] fn test_065_prim_type_error_fail_main() { run_test("065_prim_type_error_fail", "Main"); }

#[rustfmt::skip] #[test] fn test_066_compiler_solved_superclass_given_main() { run_test("066_compiler_solved_superclass_given", "Main"); }

#[rustfmt::skip] #[test] fn test_067_compiler_solved_superclass_minimisation_main() { run_test("067_compiler_solved_superclass_minimisation", "Main"); }

#[rustfmt::skip] #[test] fn test_068_prim_coercible_reflexivity_main() { run_test("068_prim_coercible_reflexivity", "Main"); }

#[rustfmt::skip] #[test] fn test_069_prim_coercible_newtype_main() { run_test("069_prim_coercible_newtype", "Main"); }

#[rustfmt::skip] #[test] fn test_070_prim_coercible_roles_main() { run_test("070_prim_coercible_roles", "Main"); }

#[rustfmt::skip] #[test] fn test_071_prim_coercible_apart_main() { run_test("071_prim_coercible_apart", "Main"); }

#[rustfmt::skip] #[test] fn test_072_prim_coercible_hidden_constructor_main() { run_test("072_prim_coercible_hidden_constructor", "Main"); }

#[rustfmt::skip] #[test] fn test_073_prim_coercible_higher_kinded_main() { run_test("073_prim_coercible_higher_kinded", "Main"); }

#[rustfmt::skip] #[test] fn test_074_prim_coercible_transitivity_main() { run_test("074_prim_coercible_transitivity", "Main"); }

#[rustfmt::skip] #[test] fn test_075_prim_coercible_given_symmetry_main() { run_test("075_prim_coercible_given_symmetry", "Main"); }

#[rustfmt::skip] #[test] fn test_076_instance_member_functor_main() { run_test("076_instance_member_functor", "Main"); }

#[rustfmt::skip] #[test] fn test_077_instance_member_signature_head_variable_main() { run_test("077_instance_member_signature_head_variable", "Main"); }

#[rustfmt::skip] #[test] fn test_078_instance_member_missing_constraint_main() { run_test("078_instance_member_missing_constraint", "Main"); }

#[rustfmt::skip] #[test] fn test_079_instance_member_signature_mismatch_main() { run_test("079_instance_member_signature_mismatch", "Main"); }

#[rustfmt::skip] #[test] fn test_080_instance_member_higher_kinded_main() { run_test("080_instance_member_higher_kinded", "Main"); }

#[rustfmt::skip] #[test] fn test_081_instance_member_too_many_binders_main() { run_test("081_instance_member_too_many_binders", "Main"); }

#[rustfmt::skip] #[test] fn test_082_derive_pipeline_smoke_main() { run_test("082_derive_pipeline_smoke", "Main"); }

#[rustfmt::skip] #[test] fn test_083_derive_eq_simple_main() { run_test("083_derive_eq_simple", "Main"); }

#[rustfmt::skip] #[test] fn test_084_derive_eq_parameterized_main() { run_test("084_derive_eq_parameterized", "Main"); }

#[rustfmt::skip] #[test] fn test_085_derive_eq_missing_instance_main() { run_test("085_derive_eq_missing_instance", "Main"); }

#[rustfmt::skip] #[test] fn test_086_derive_ord_simple_main() { run_test("086_derive_ord_simple", "Main"); }

#[rustfmt::skip] #[test] fn test_087_derive_ord_1_higher_kinded_main() { run_test("087_derive_ord_1_higher_kinded", "Main"); }

#[rustfmt::skip] #[test] fn test_088_derive_eq_1_main() { run_test("088_derive_eq_1", "Main"); }

#[rustfmt::skip] #[test] fn test_089_derive_ord_1_main() { run_test("089_derive_ord_1", "Main"); }

#[rustfmt::skip] #[test] fn test_090_derive_functor_simple_main() { run_test("090_derive_functor_simple", "Main"); }

#[rustfmt::skip] #[test] fn test_091_derive_functor_contravariant_error_main() { run_test("091_derive_functor_contravariant_error", "Main"); }

#[rustfmt::skip] #[test] fn test_092_derive_functor_insufficient_params_main() { run_test("092_derive_functor_insufficient_params", "Main"); }

#[rustfmt::skip] #[test] fn test_093_derive_bifunctor_simple_main() { run_test("093_derive_bifunctor_simple", "Main"); }

#[rustfmt::skip] #[test] fn test_094_derive_bifunctor_missing_functor_main() { run_test("094_derive_bifunctor_missing_functor", "Main"); }

#[rustfmt::skip] #[test] fn test_095_derive_bifunctor_insufficient_params_main() { run_test("095_derive_bifunctor_insufficient_params", "Main"); }

#[rustfmt::skip] #[test] fn test_096_derive_contravariant_simple_main() { run_test("096_derive_contravariant_simple", "Main"); }

#[rustfmt::skip] #[test] fn test_097_derive_contravariant_error_main() { run_test("097_derive_contravariant_error", "Main"); }

#[rustfmt::skip] #[test] fn test_098_derive_profunctor_simple_main() { run_test("098_derive_profunctor_simple", "Main"); }

#[rustfmt::skip] #[test] fn test_099_derive_profunctor_error_main() { run_test("099_derive_profunctor_error", "Main"); }

#[rustfmt::skip] #[test] fn test_100_derive_foldable_simple_main() { run_test("100_derive_foldable_simple", "Main"); }

#[rustfmt::skip] #[test] fn test_101_derive_foldable_higher_kinded_main() { run_test("101_derive_foldable_higher_kinded", "Main"); }

#[rustfmt::skip] #[test] fn test_102_derive_bifoldable_simple_main() { run_test("102_derive_bifoldable_simple", "Main"); }

#[rustfmt::skip] #[test] fn test_103_derive_bifoldable_higher_kinded_main() { run_test("103_derive_bifoldable_higher_kinded", "Main"); }

#[rustfmt::skip] #[test] fn test_104_derive_traversable_simple_main() { run_test("104_derive_traversable_simple", "Main"); }

#[rustfmt::skip] #[test] fn test_105_derive_traversable_higher_kinded_main() { run_test("105_derive_traversable_higher_kinded", "Main"); }

#[rustfmt::skip] #[test] fn test_106_derive_traversable_missing_superclass_main() { run_test("106_derive_traversable_missing_superclass", "Main"); }

#[rustfmt::skip] #[test] fn test_107_derive_bitraversable_simple_main() { run_test("107_derive_bitraversable_simple", "Main"); }

#[rustfmt::skip] #[test] fn test_108_derive_bitraversable_higher_kinded_main() { run_test("108_derive_bitraversable_higher_kinded", "Main"); }

#[rustfmt::skip] #[test] fn test_109_derive_newtype_simple_main() { run_test("109_derive_newtype_simple", "Main"); }

#[rustfmt::skip] #[test] fn test_110_derive_newtype_parameterized_main() { run_test("110_derive_newtype_parameterized", "Main"); }

#[rustfmt::skip] #[test] fn test_111_derive_newtype_not_newtype_main() { run_test("111_derive_newtype_not_newtype", "Main"); }

#[rustfmt::skip] #[test] fn test_112_derive_newtype_class_simple_main() { run_test("112_derive_newtype_class_simple", "Main"); }

#[rustfmt::skip] #[test] fn test_113_derive_newtype_class_parameterized_main() { run_test("113_derive_newtype_class_parameterized", "Main"); }

#[rustfmt::skip] #[test] fn test_114_derive_newtype_class_not_newtype_main() { run_test("114_derive_newtype_class_not_newtype", "Main"); }

#[rustfmt::skip] #[test] fn test_115_derive_generic_simple_main() { run_test("115_derive_generic_simple", "Main"); }

#[rustfmt::skip] #[test] fn test_116_derive_eq_mutual_visibility_same_module_main() { run_test("116_derive_eq_mutual_visibility_same_module", "Main"); }

#[rustfmt::skip] #[test] fn test_117_derive_newtype_class_coercible_main() { run_test("117_derive_newtype_class_coercible", "Main"); }

#[rustfmt::skip] #[test] fn test_118_derive_newtype_with_given_main() { run_test("118_derive_newtype_with_given", "Main"); }

#[rustfmt::skip] #[test] fn test_119_derive_newtype_missing_instance_main() { run_test("119_derive_newtype_missing_instance", "Main"); }

#[rustfmt::skip] #[test] fn test_120_derive_newtype_missing_given_main() { run_test("120_derive_newtype_missing_given", "Main"); }

#[rustfmt::skip] #[test] fn test_121_derive_newtype_multi_param_main() { run_test("121_derive_newtype_multi_param", "Main"); }

#[rustfmt::skip] #[test] fn test_122_derive_newtype_higher_kinded_main() { run_test("122_derive_newtype_higher_kinded", "Main"); }

#[rustfmt::skip] #[test] fn test_123_derive_newtype_function_main() { run_test("123_derive_newtype_function", "Main"); }

#[rustfmt::skip] #[test] fn test_124_derive_newtype_synonym_inner_main() { run_test("124_derive_newtype_synonym_inner", "Main"); }

#[rustfmt::skip] #[test] fn test_125_derive_eq_1_higher_kinded_main() { run_test("125_derive_eq_1_higher_kinded", "Main"); }

#[rustfmt::skip] #[test] fn test_126_derive_eq_partial_main() { run_test("126_derive_eq_partial", "Main"); }

#[rustfmt::skip] #[test] fn test_127_derive_eq_ord_nested_higher_kinded_main() { run_test("127_derive_eq_ord_nested_higher_kinded", "Main"); }

#[rustfmt::skip] #[test] fn test_128_derive_functor_higher_kinded_main() { run_test("128_derive_functor_higher_kinded", "Main"); }

#[rustfmt::skip] #[test] fn test_129_derive_bifunctor_higher_kinded_main() { run_test("129_derive_bifunctor_higher_kinded", "Main"); }

#[rustfmt::skip] #[test] fn test_130_givens_retained_main() { run_test("130_givens_retained", "Main"); }

#[rustfmt::skip] #[test] fn test_131_givens_scoped_main() { run_test("131_givens_scoped", "Main"); }

#[rustfmt::skip] #[test] fn test_132_let_constraint_scoping_main() { run_test("132_let_constraint_scoping", "Main"); }

#[rustfmt::skip] #[test] fn test_133_row_open_union_main() { run_test("133_row_open_union", "Main"); }

#[rustfmt::skip] #[test] fn test_134_row_open_cons_main() { run_test("134_row_open_cons", "Main"); }

#[rustfmt::skip] #[test] fn test_135_row_open_lacks_main() { run_test("135_row_open_lacks", "Main"); }

#[rustfmt::skip] #[test] fn test_136_row_open_record_main() { run_test("136_row_open_record", "Main"); }

#[rustfmt::skip] #[test] fn test_137_type_operator_synonym_expansion_main() { run_test("137_type_operator_synonym_expansion", "Main"); }

#[rustfmt::skip] #[test] fn test_138_synonym_kind_application_main() { run_test("138_synonym_kind_application", "Main"); }

#[rustfmt::skip] #[test] fn test_139_synonym_operator_alias_main() { run_test("139_synonym_operator_alias", "Main"); }

#[rustfmt::skip] #[test] fn test_140_const_equation_forms_main() { run_test("140_const_equation_forms", "Main"); }

#[rustfmt::skip] #[test] fn test_141_const_forms_synonym_arrow_main() { run_test("141_const_forms_synonym_arrow", "Main"); }

#[rustfmt::skip] #[test] fn test_142_const_forms_synonym_forall_main() { run_test("142_const_forms_synonym_forall", "Main"); }

#[rustfmt::skip] #[test] fn test_143_const_forms_fn_alias_main() { run_test("143_const_forms_fn_alias", "Main"); }

#[rustfmt::skip] #[test] fn test_144_signature_synonym_data_equation_main() { run_test("144_signature_synonym_data_equation", "Main"); }

#[rustfmt::skip] #[test] fn test_145_signature_synonym_class_equation_main() { run_test("145_signature_synonym_class_equation", "Main"); }

#[rustfmt::skip] #[test] fn test_146_signature_synonym_type_equation_main() { run_test("146_signature_synonym_type_equation", "Main"); }

#[rustfmt::skip] #[test] fn test_147_synonym_oversaturation_equations_main() { run_test("147_synonym_oversaturation_equations", "Main"); }

#[rustfmt::skip] #[test] fn test_148_class_member_prenex_main() { run_test("148_class_member_prenex", "Main"); }

#[rustfmt::skip] #[test] fn test_149_synonym_oversaturation_kind_main() { run_test("149_synonym_oversaturation_kind", "Main"); }

#[rustfmt::skip] #[test] fn test_150_derive_generic_insufficient_params_main() { run_test("150_derive_generic_insufficient_params", "Main"); }

#[rustfmt::skip] #[test] fn test_151_derive_generic_not_constructor_main() { run_test("151_derive_generic_not_constructor", "Main"); }

#[rustfmt::skip] #[test] fn test_152_derive_generic_missing_rep_main() { run_test("152_derive_generic_missing_rep", "Main"); }

#[rustfmt::skip] #[test] fn test_153_do_discard_main() { run_test("153_do_discard", "Main"); }

#[rustfmt::skip] #[test] fn test_154_do_bind_main() { run_test("154_do_bind", "Main"); }

#[rustfmt::skip] #[test] fn test_155_ado_discard_main() { run_test("155_ado_discard", "Main"); }

#[rustfmt::skip] #[test] fn test_156_ado_bind_main() { run_test("156_ado_bind", "Main"); }

#[rustfmt::skip] #[test] fn test_157_do_polymorphic_main() { run_test("157_do_polymorphic", "Main"); }

#[rustfmt::skip] #[test] fn test_158_ado_polymorphic_main() { run_test("158_ado_polymorphic", "Main"); }

#[rustfmt::skip] #[test] fn test_159_do_empty_block_main() { run_test("159_do_empty_block", "Main"); }

#[rustfmt::skip] #[test] fn test_160_ado_empty_block_main() { run_test("160_ado_empty_block", "Main"); }

#[rustfmt::skip] #[test] fn test_161_do_ado_constrained_main() { run_test("161_do_ado_constrained", "Main"); }

#[rustfmt::skip] #[test] fn test_162_do_let_premature_solve_main() { run_test("162_do_let_premature_solve", "Main"); }

#[rustfmt::skip] #[test] fn test_163_do_let_annotation_solve_main() { run_test("163_do_let_annotation_solve", "Main"); }

#[rustfmt::skip] #[test] fn test_164_ado_let_premature_solve_main() { run_test("164_ado_let_premature_solve", "Main"); }

#[rustfmt::skip] #[test] fn test_165_ado_let_annotation_solve_main() { run_test("165_ado_let_annotation_solve", "Main"); }

#[rustfmt::skip] #[test] fn test_166_do_bind_only_main() { run_test("166_do_bind_only", "Main"); }

#[rustfmt::skip] #[test] fn test_167_do_final_bind_main() { run_test("167_do_final_bind", "Main"); }

#[rustfmt::skip] #[test] fn test_168_do_final_let_main() { run_test("168_do_final_let", "Main"); }

#[rustfmt::skip] #[test] fn test_169_exhaustive_multiple_main() { run_test("169_exhaustive_multiple", "Main"); }

#[rustfmt::skip] #[test] fn test_170_exhaustive_tuple_main() { run_test("170_exhaustive_tuple", "Main"); }

#[rustfmt::skip] #[test] fn test_171_exhaustive_equation_redundant_main() { run_test("171_exhaustive_equation_redundant", "Main"); }

#[rustfmt::skip] #[test] fn test_172_exhaustive_equation_guarded_main() { run_test("172_exhaustive_equation_guarded", "Main"); }

#[rustfmt::skip] #[test] fn test_173_exhaustive_let_equation_main() { run_test("173_exhaustive_let_equation", "Main"); }

#[rustfmt::skip] #[test] fn test_174_exhaustive_instance_equation_main() { run_test("174_exhaustive_instance_equation", "Main"); }

#[rustfmt::skip] #[test] fn test_175_exhaustive_record_main() { run_test("175_exhaustive_record", "Main"); }

#[rustfmt::skip] #[test] fn test_176_exhaustive_array_main() { run_test("176_exhaustive_array", "Main"); }

#[rustfmt::skip] #[test] fn test_177_exhaustive_guards_otherwise_true_main() { run_test("177_exhaustive_guards_otherwise_true", "Main"); }

#[rustfmt::skip] #[test] fn test_178_application_infix_chain_main() { run_test("178_application_infix_chain", "Main"); }

#[rustfmt::skip] #[test] fn test_179_application_function_subtype_main() { run_test("179_application_function_subtype", "Main"); }

#[rustfmt::skip] #[test] fn test_180_application_function_decomposition_main() { run_test("180_application_function_decomposition", "Main"); }

#[rustfmt::skip] #[test] fn test_181_type_application_invalid_basic_main() { run_test("181_type_application_invalid_basic", "Main"); }

#[rustfmt::skip] #[test] fn test_182_type_application_invalid_too_many_main() { run_test("182_type_application_invalid_too_many", "Main"); }

#[rustfmt::skip] #[test] fn test_183_record_expression_exact_main() { run_test("183_record_expression_exact", "Main"); }

#[rustfmt::skip] #[test] fn test_184_record_expression_missing_field_main() { run_test("184_record_expression_missing_field", "Main"); }

#[rustfmt::skip] #[test] fn test_185_record_expression_additional_field_main() { run_test("185_record_expression_additional_field", "Main"); }

#[rustfmt::skip] #[test] fn test_186_record_expression_missing_and_additional_main() { run_test("186_record_expression_missing_and_additional", "Main"); }

#[rustfmt::skip] #[test] fn test_187_record_expression_nested_additional_main() { run_test("187_record_expression_nested_additional", "Main"); }

#[rustfmt::skip] #[test] fn test_188_record_access_sections_main() { run_test("188_record_access_sections", "Main"); }

#[rustfmt::skip] #[test] fn test_189_record_update_sections_main() { run_test("189_record_update_sections", "Main"); }

#[rustfmt::skip] #[test] fn test_190_record_binder_shrinking_main() { run_test("190_record_binder_shrinking", "Main"); }

#[rustfmt::skip] #[test] fn test_191_record_binder_additional_property_main() { run_test("191_record_binder_additional_property", "Main"); }

#[rustfmt::skip] #[test] fn test_192_record_binder_additional_property_nested_main() { run_test("192_record_binder_additional_property_nested", "Main"); }

#[rustfmt::skip] #[test] fn test_193_givens_matching_main() { run_test("193_givens_matching", "Main"); }

#[rustfmt::skip] #[test] fn test_194_givens_functional_dependency_main() { run_test("194_givens_functional_dependency", "Main"); }

#[rustfmt::skip] #[test] fn test_195_givens_superclass_where_binding_main() { run_test("195_givens_superclass_where_binding", "Main"); }

#[rustfmt::skip] #[test] fn test_196_instance_record_matching_main() { run_test("196_instance_record_matching", "Main"); }

#[rustfmt::skip] #[test] fn test_197_instance_record_open_row_main() { run_test("197_instance_record_open_row", "Main"); }

#[rustfmt::skip] #[test] fn test_198_instance_head_invalid_row_main() { run_test("198_instance_head_invalid_row", "Main"); }

#[rustfmt::skip] #[test] fn test_199_instance_kind_application_matching_main() { run_test("199_instance_kind_application_matching", "Main"); }

#[rustfmt::skip] #[test] fn test_200_instance_bound_variable_unification_main() { run_test("200_instance_bound_variable_unification", "Main"); }

#[rustfmt::skip] #[test] fn test_201_synonym_function_result_kind_main() { run_test("201_synonym_function_result_kind", "Main"); }

#[rustfmt::skip] #[test] fn test_202_synonym_forall_expansion_main() { run_test("202_synonym_forall_expansion", "Main"); }

#[rustfmt::skip] #[test] fn test_203_binder_instantiation_main() { run_test("203_binder_instantiation", "Main"); }

#[rustfmt::skip] #[test] fn test_204_coercible_phantom_main() { run_test("204_coercible_phantom", "Main"); }

#[rustfmt::skip] #[test] fn test_205_coercible_representational_main() { run_test("205_coercible_representational", "Main"); }

#[rustfmt::skip] #[test] fn test_206_coercible_array_main() { run_test("206_coercible_array", "Main"); }

#[rustfmt::skip] #[test] fn test_207_coercible_record_main() { run_test("207_coercible_record", "Main"); }

#[rustfmt::skip] #[test] fn test_208_coercible_transitivity_main() { run_test("208_coercible_transitivity", "Main"); }

#[rustfmt::skip] #[test] fn test_209_coercible_different_heads_error_main() { run_test("209_coercible_different_heads_error", "Main"); }

#[rustfmt::skip] #[test] fn test_210_coercible_nominal_main() { run_test("210_coercible_nominal", "Main"); }

#[rustfmt::skip] #[test] fn test_211_coercible_newtype_hidden_main() { run_test("211_coercible_newtype_hidden", "Main"); }

#[rustfmt::skip] #[test] fn test_212_coercible_newtype_qualified_main() { run_test("212_coercible_newtype_qualified", "Main"); }

#[rustfmt::skip] #[test] fn test_213_coercible_newtype_open_hidden_main() { run_test("213_coercible_newtype_open_hidden", "Main"); }

#[rustfmt::skip] #[test] fn test_214_coercible_nested_records_main() { run_test("214_coercible_nested_records", "Main"); }

#[rustfmt::skip] #[test] fn test_215_coercible_higher_kinded_error_main() { run_test("215_coercible_higher_kinded_error", "Main"); }

#[rustfmt::skip] #[test] fn test_216_coercible_higher_kinded_multi_main() { run_test("216_coercible_higher_kinded_multi", "Main"); }

#[rustfmt::skip] #[test] fn test_217_coercible_higher_kinded_polykinded_main() { run_test("217_coercible_higher_kinded_polykinded", "Main"); }

#[rustfmt::skip] #[test] fn test_218_coercible_function_decomposition_main() { run_test("218_coercible_function_decomposition", "Main"); }

#[rustfmt::skip] #[test] fn test_219_derive_newtype_not_constructor_main() { run_test("219_derive_newtype_not_constructor", "Main"); }

#[rustfmt::skip] #[test] fn test_220_derive_newtype_class_not_constructor_main() { run_test("220_derive_newtype_class_not_constructor", "Main"); }

#[rustfmt::skip] #[test] fn test_222_derive_newtype_not_local_main() { run_test("222_derive_newtype_not_local", "Main"); }

#[rustfmt::skip] #[test] fn test_223_derive_newtype_class_not_local_main() { run_test("223_derive_newtype_class_not_local", "Main"); }

#[rustfmt::skip] #[test] fn test_224_derive_newtype_insufficient_params_main() { run_test("224_derive_newtype_insufficient_params", "Main"); }

#[rustfmt::skip] #[test] fn test_225_derive_newtype_class_insufficient_params_main() { run_test("225_derive_newtype_class_insufficient_params", "Main"); }

#[rustfmt::skip] #[test] fn test_226_equation_synonym_expansion_main() { run_test("226_equation_synonym_expansion", "Main"); }

#[rustfmt::skip] #[test] fn test_227_derive_newtype_invalid_skolem_layout_main() { run_test("227_derive_newtype_invalid_skolem_layout", "Main"); }

#[rustfmt::skip] #[test] fn test_228_instance_implicit_variable_freshening_main() { run_test("228_instance_implicit_variable_freshening", "Main"); }

#[rustfmt::skip] #[test] fn test_229_type_operator_synonym_partial_arguments_main() { run_test("229_type_operator_synonym_partial_arguments", "Main"); }

#[rustfmt::skip] #[test] fn test_230_type_synonym_higher_order_main() { run_test("230_type_synonym_higher_order", "Main"); }

#[rustfmt::skip] #[test] fn test_231_type_synonym_higher_order_operator_main() { run_test("231_type_synonym_higher_order_operator", "Main"); }

#[rustfmt::skip] #[test] fn test_232_category_identity_operator_alias_main() { run_test("232_category_identity_operator_alias", "Main"); }

#[rustfmt::skip] #[test] fn test_233_operator_alias_kind_application_main() { run_test("233_operator_alias_kind_application", "Main"); }

#[rustfmt::skip] #[test] fn test_234_instance_function_unification_main() { run_test("234_instance_function_unification", "Main"); }

#[rustfmt::skip] #[test] fn test_235_derive_eq_ord_1_kind_arguments_main() { run_test("235_derive_eq_ord_1_kind_arguments", "Main"); }

#[rustfmt::skip] #[test] fn test_236_exhaustive_missing_patterns_main() { run_test("236_exhaustive_missing_patterns", "Main"); }

#[rustfmt::skip] #[test] fn test_237_exhaustive_unsafe_partial_main() { run_test("237_exhaustive_unsafe_partial", "Main"); }

#[rustfmt::skip] #[test] fn test_238_visible_type_application_value_main() { run_test("238_visible_type_application_value", "Main"); }

#[rustfmt::skip] #[test] fn test_239_visible_type_application_data_newtype_main() { run_test("239_visible_type_application_data_newtype", "Main"); }

#[rustfmt::skip] #[test] fn test_240_visible_type_application_class_main() { run_test("240_visible_type_application_class", "Main"); }

#[rustfmt::skip] #[test] fn test_241_visible_type_application_tuple_main() { run_test("241_visible_type_application_tuple", "Main"); }

#[rustfmt::skip] #[test] fn test_242_visible_type_application_either_main() { run_test("242_visible_type_application_either", "Main"); }

#[rustfmt::skip] #[test] fn test_243_visible_type_application_error_main() { run_test("243_visible_type_application_error", "Main"); }

#[rustfmt::skip] #[test] fn test_244_record_quoted_label_main() { run_test("244_record_quoted_label", "Main"); }

#[rustfmt::skip] #[test] fn test_245_record_field_instantiation_main() { run_test("245_record_field_instantiation", "Main"); }

#[rustfmt::skip] #[test] fn test_246_data_argument_kind_applications_main() { run_test("246_data_argument_kind_applications", "Main"); }

#[rustfmt::skip] #[test] fn test_247_instance_kind_substitution_main() { run_test("247_instance_kind_substitution", "Main"); }

#[rustfmt::skip] #[test] fn test_248_prim_row_cons_tail_synonym_main() { run_test("248_prim_row_cons_tail_synonym", "Main"); }

#[rustfmt::skip] #[test] fn test_249_prim_int_given_improvement_main() { run_test("249_prim_int_given_improvement", "Main"); }

#[rustfmt::skip] #[test] fn test_250_given_deep_name_improvement_main() { run_test("250_given_deep_name_improvement", "Main"); }

#[rustfmt::skip] #[test] fn test_251_prim_row_nub_union_class_constraint_main() { run_test("251_prim_row_nub_union_class_constraint", "Main"); }

#[rustfmt::skip] #[test] fn test_252_forall_row_field_main() { run_test("252_forall_row_field", "Main"); }

#[rustfmt::skip] #[test] fn test_253_kind_applied_reference_double_application_main() { run_test("253_kind_applied_reference_double_application", "Main"); }

#[rustfmt::skip] #[test] fn test_254_given_synonym_expansion_main() { run_test("254_given_synonym_expansion", "Main"); }

#[rustfmt::skip] #[test] fn test_255_section_rule_progress_main() { run_test("255_section_rule_progress", "Main"); }

#[rustfmt::skip] #[test] fn test_256_record_field_polymorphic_main() { run_test("256_record_field_polymorphic", "Main"); }

#[rustfmt::skip] #[test] fn test_257_operator_result_ordering_main() { run_test("257_operator_result_ordering", "Main"); }

#[rustfmt::skip] #[test] fn test_258_self_recursive_through_operator_main() { run_test("258_self_recursive_through_operator", "Main"); }

#[rustfmt::skip] #[test] fn test_259_hidden_forall_scope_check_main() { run_test("259_hidden_forall_scope_check", "Main"); }
