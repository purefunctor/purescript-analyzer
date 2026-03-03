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

#[rustfmt::skip] #[test] fn test_gitkeep_main() { run_test("gitkeep", "Main"); }

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
