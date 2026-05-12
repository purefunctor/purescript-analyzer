// Do not edit! See build.rs

#[rustfmt::skip]
fn run_test(folder: &str, file: &str) {
    let path = std::path::Path::new("fixtures/checking").join(folder);
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
        || tests_integration::generated::basic::report_checked(&engine, id)
    );

    println!("trace: {}", trace_path.display());

    let mut settings = insta::Settings::clone_current();
    settings.set_snapshot_path(std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("fixtures/checking").join(folder));
    settings.set_prepend_module_to_snapshot(false);
    settings.bind(|| insta::assert_snapshot!(file, report));
}

#[rustfmt::skip] #[test] fn test_1771860360_foreign_check_main() { run_test("1771860360_foreign_check", "Main"); }

#[rustfmt::skip] #[test] fn test_1771860420_foreign_recursive_main() { run_test("1771860420_foreign_recursive", "Main"); }

#[rustfmt::skip] #[test] fn test_1771864260_newtype_check_main() { run_test("1771864260_newtype_check", "Main"); }

#[rustfmt::skip] #[test] fn test_1771864320_newtype_infer_main() { run_test("1771864320_newtype_infer", "Main"); }

#[rustfmt::skip] #[test] fn test_1771864380_data_check_main() { run_test("1771864380_data_check", "Main"); }

#[rustfmt::skip] #[test] fn test_1771864440_data_infer_main() { run_test("1771864440_data_infer", "Main"); }

#[rustfmt::skip] #[test] fn test_1771869960_synonym_check_main() { run_test("1771869960_synonym_check", "Main"); }

#[rustfmt::skip] #[test] fn test_1771918140_synonym_infer_main() { run_test("1771918140_synonym_infer", "Main"); }

#[rustfmt::skip] #[test] fn test_1771923300_class_check_main() { run_test("1771923300_class_check", "Main"); }

#[rustfmt::skip] #[test] fn test_1771923360_class_infer_main() { run_test("1771923360_class_infer", "Main"); }

#[rustfmt::skip] #[test] fn test_1771923420_class_superclass_main() { run_test("1771923420_class_superclass", "Main"); }

#[rustfmt::skip] #[test] fn test_1771923480_class_polykind_check_main() { run_test("1771923480_class_polykind_check", "Main"); }

#[rustfmt::skip] #[test] fn test_1771923540_class_polykind_infer_main() { run_test("1771923540_class_polykind_infer", "Main"); }

#[rustfmt::skip] #[test] fn test_1771935300_operator_alias_kind_main() { run_test("1771935300_operator_alias_kind", "Main"); }

#[rustfmt::skip] #[test] fn test_1771935360_operator_alias_invalid_kind_main() { run_test("1771935360_operator_alias_invalid_kind", "Main"); }

#[rustfmt::skip] #[test] fn test_1772011200_type_operator_chain_infer_main() { run_test("1772011200_type_operator_chain_infer", "Main"); }

#[rustfmt::skip] #[test] fn test_1772011260_type_operator_chain_check_main() { run_test("1772011260_type_operator_chain_check", "Main"); }

#[rustfmt::skip] #[test] fn test_1772011320_type_operator_chain_polykind_main() { run_test("1772011320_type_operator_chain_polykind", "Main"); }

#[rustfmt::skip] #[test] fn test_1772011380_type_operator_chain_precedence_main() { run_test("1772011380_type_operator_chain_precedence", "Main"); }

#[rustfmt::skip] #[test] fn test_1772011440_type_operator_chain_kind_error_main() { run_test("1772011440_type_operator_chain_kind_error", "Main"); }

#[rustfmt::skip] #[test] fn test_1772011500_operator_check_main() { run_test("1772011500_operator_check", "Main"); }

#[rustfmt::skip] #[test] fn test_1772011560_role_inference_phantom_main() { run_test("1772011560_role_inference_phantom", "Main"); }

#[rustfmt::skip] #[test] fn test_1772011620_role_inference_representational_main() { run_test("1772011620_role_inference_representational", "Main"); }

#[rustfmt::skip] #[test] fn test_1772011680_role_inference_nominal_parametric_main() { run_test("1772011680_role_inference_nominal_parametric", "Main"); }

#[rustfmt::skip] #[test] fn test_1772011740_role_declaration_strengthen_main() { run_test("1772011740_role_declaration_strengthen", "Main"); }

#[rustfmt::skip] #[test] fn test_1772011800_role_declaration_loosen_error_main() { run_test("1772011800_role_declaration_loosen_error", "Main"); }

#[rustfmt::skip] #[test] fn test_1772011860_role_declaration_foreign_main() { run_test("1772011860_role_declaration_foreign", "Main"); }

#[rustfmt::skip] #[test] fn test_1772011920_foreign_check_main() { run_test("1772011920_foreign_check", "Main"); }

#[rustfmt::skip] #[test] fn test_1772011980_value_check_main() { run_test("1772011980_value_check", "Main"); }

#[rustfmt::skip] #[test] fn test_1772045820_exhaustive_case_infer_main() { run_test("1772045820_exhaustive_case_infer", "Main"); }

#[rustfmt::skip] #[test] fn test_1772045880_exhaustive_case_redundant_main() { run_test("1772045880_exhaustive_case_redundant", "Main"); }

#[rustfmt::skip] #[test] fn test_1772045940_exhaustive_equation_signature_main() { run_test("1772045940_exhaustive_equation_signature", "Main"); }

#[rustfmt::skip] #[test] fn test_1772046000_exhaustive_guards_otherwise_main() { run_test("1772046000_exhaustive_guards_otherwise", "Main"); }

#[rustfmt::skip] #[test] fn test_1772046060_exhaustive_let_pattern_main() { run_test("1772046060_exhaustive_let_pattern", "Main"); }

#[rustfmt::skip] #[test] fn test_1772046120_exhaustive_operator_constructor_main() { run_test("1772046120_exhaustive_operator_constructor", "Main"); }

#[rustfmt::skip] #[test] fn test_1772088540_synonym_partial_defer_main() { run_test("1772088540_synonym_partial_defer", "Main"); }

#[rustfmt::skip] #[test] fn test_1772108280_value_recursive_check_main() { run_test("1772108280_value_recursive_check", "Main"); }

#[rustfmt::skip] #[test] fn test_1772108340_value_recursive_infer_main() { run_test("1772108340_value_recursive_infer", "Main"); }

#[rustfmt::skip] #[test] fn test_1772108400_value_mutual_check_main() { run_test("1772108400_value_mutual_check", "Main"); }

#[rustfmt::skip] #[test] fn test_1772108460_value_mutual_infer_main() { run_test("1772108460_value_mutual_infer", "Main"); }

#[rustfmt::skip] #[test] fn test_1772108520_data_mutual_check_main() { run_test("1772108520_data_mutual_check", "Main"); }

#[rustfmt::skip] #[test] fn test_1772108580_data_mutual_infer_main() { run_test("1772108580_data_mutual_infer", "Main"); }

#[rustfmt::skip] #[test] fn test_1772140680_instance_check_main() { run_test("1772140680_instance_check", "Main"); }

#[rustfmt::skip] #[test] fn test_1772301720_instance_constraint_solving_main() { run_test("1772301720_instance_constraint_solving", "Main"); }

#[rustfmt::skip] #[test] fn test_1772301780_instance_functional_dependency_main() { run_test("1772301780_instance_functional_dependency", "Main"); }

#[rustfmt::skip] #[test] fn test_1772301840_instance_given_constraint_main() { run_test("1772301840_instance_given_constraint", "Main"); }

#[rustfmt::skip] #[test] fn test_1772301900_instance_constraint_generalization_main() { run_test("1772301900_instance_constraint_generalization", "Main"); }

#[rustfmt::skip] #[test] fn test_1772301960_instance_superclass_elaboration_main() { run_test("1772301960_instance_superclass_elaboration", "Main"); }

#[rustfmt::skip] #[test] fn test_1772304600_given_constraint_check_main() { run_test("1772304600_given_constraint_check", "Main"); }

#[rustfmt::skip] #[test] fn test_1772304660_given_constraint_let_check_main() { run_test("1772304660_given_constraint_let_check", "Main"); }

#[rustfmt::skip] #[test] fn test_1772304720_given_constraint_operator_check_main() { run_test("1772304720_given_constraint_operator_check", "Main"); }

#[rustfmt::skip] #[test] fn test_1772440020_prim_int_main() { run_test("1772440020_prim_int", "Main"); }

#[rustfmt::skip] #[test] fn test_1772440080_prim_int_compare_transitive_main() { run_test("1772440080_prim_int_compare_transitive", "Main"); }

#[rustfmt::skip] #[test] fn test_1772440140_prim_symbol_main() { run_test("1772440140_prim_symbol", "Main"); }

#[rustfmt::skip] #[test] fn test_1772440200_prim_solver_apart_main() { run_test("1772440200_prim_solver_apart", "Main"); }

#[rustfmt::skip] #[test] fn test_1772440260_prim_row_list_main() { run_test("1772440260_prim_row_list", "Main"); }

#[rustfmt::skip] #[test] fn test_1772440320_prim_row_main() { run_test("1772440320_prim_row", "Main"); }

#[rustfmt::skip] #[test] fn test_1772440380_prim_row_apart_main() { run_test("1772440380_prim_row_apart", "Main"); }

#[rustfmt::skip] #[test] fn test_1772440440_prim_row_open_main() { run_test("1772440440_prim_row_open", "Main"); }

#[rustfmt::skip] #[test] fn test_1772440500_prim_row_generalization_main() { run_test("1772440500_prim_row_generalization", "Main"); }

#[rustfmt::skip] #[test] fn test_1772440560_prim_row_nub_left_bias_main() { run_test("1772440560_prim_row_nub_left_bias", "Main"); }

#[rustfmt::skip] #[test] fn test_1772440620_prim_row_record_main() { run_test("1772440620_prim_row_record", "Main"); }

#[rustfmt::skip] #[test] fn test_1772442480_compiler_solved_superclass_given_main() { run_test("1772442480_compiler_solved_superclass_given", "Main"); }

#[rustfmt::skip] #[test] fn test_1772442540_compiler_solved_superclass_minimisation_main() { run_test("1772442540_compiler_solved_superclass_minimisation", "Main"); }

#[rustfmt::skip] #[test] fn test_1772442600_prim_reflectable_main() { run_test("1772442600_prim_reflectable", "Main"); }

#[rustfmt::skip] #[test] fn test_1772442660_prim_type_error_warn_main() { run_test("1772442660_prim_type_error_warn", "Main"); }

#[rustfmt::skip] #[test] fn test_1772442720_prim_type_error_fail_main() { run_test("1772442720_prim_type_error_fail", "Main"); }

#[rustfmt::skip] #[test] fn test_1772475240_prim_coercible_given_symmetry_main() { run_test("1772475240_prim_coercible_given_symmetry", "Main"); }

#[rustfmt::skip] #[test] fn test_1772475300_prim_coercible_reflexivity_main() { run_test("1772475300_prim_coercible_reflexivity", "Main"); }

#[rustfmt::skip] #[test] fn test_1772475360_prim_coercible_newtype_main() { run_test("1772475360_prim_coercible_newtype", "Main"); }

#[rustfmt::skip] #[test] fn test_1772475420_prim_coercible_roles_main() { run_test("1772475420_prim_coercible_roles", "Main"); }

#[rustfmt::skip] #[test] fn test_1772475480_prim_coercible_apart_main() { run_test("1772475480_prim_coercible_apart", "Main"); }

#[rustfmt::skip] #[test] fn test_1772475540_prim_coercible_hidden_constructor_main() { run_test("1772475540_prim_coercible_hidden_constructor", "Main"); }

#[rustfmt::skip] #[test] fn test_1772475600_prim_coercible_higher_kinded_main() { run_test("1772475600_prim_coercible_higher_kinded", "Main"); }

#[rustfmt::skip] #[test] fn test_1772475660_prim_coercible_transitivity_main() { run_test("1772475660_prim_coercible_transitivity", "Main"); }

#[rustfmt::skip] #[test] fn test_1772475720_instance_member_functor_main() { run_test("1772475720_instance_member_functor", "Main"); }

#[rustfmt::skip] #[test] fn test_1772475780_instance_member_signature_head_variable_main() { run_test("1772475780_instance_member_signature_head_variable", "Main"); }

#[rustfmt::skip] #[test] fn test_1772475840_instance_member_missing_constraint_main() { run_test("1772475840_instance_member_missing_constraint", "Main"); }

#[rustfmt::skip] #[test] fn test_1772475900_instance_member_signature_mismatch_main() { run_test("1772475900_instance_member_signature_mismatch", "Main"); }

#[rustfmt::skip] #[test] fn test_1772475960_instance_member_higher_kinded_main() { run_test("1772475960_instance_member_higher_kinded", "Main"); }

#[rustfmt::skip] #[test] fn test_1772476020_instance_member_too_many_binders_main() { run_test("1772476020_instance_member_too_many_binders", "Main"); }

#[rustfmt::skip] #[test] fn test_1772564520_derive_pipeline_smoke_main() { run_test("1772564520_derive_pipeline_smoke", "Main"); }

#[rustfmt::skip] #[test] fn test_1772564580_derive_ord_simple_main() { run_test("1772564580_derive_ord_simple", "Main"); }

#[rustfmt::skip] #[test] fn test_1772564640_derive_ord_1_higher_kinded_main() { run_test("1772564640_derive_ord_1_higher_kinded", "Main"); }

#[rustfmt::skip] #[test] fn test_1772564700_derive_eq_simple_main() { run_test("1772564700_derive_eq_simple", "Main"); }

#[rustfmt::skip] #[test] fn test_1772564760_derive_eq_parameterized_main() { run_test("1772564760_derive_eq_parameterized", "Main"); }

#[rustfmt::skip] #[test] fn test_1772564820_derive_eq_missing_instance_main() { run_test("1772564820_derive_eq_missing_instance", "Main"); }

#[rustfmt::skip] #[test] fn test_1772564880_derive_eq_1_main() { run_test("1772564880_derive_eq_1", "Main"); }

#[rustfmt::skip] #[test] fn test_1772564940_derive_ord_1_main() { run_test("1772564940_derive_ord_1", "Main"); }

#[rustfmt::skip] #[test] fn test_1772565000_derive_functor_simple_main() { run_test("1772565000_derive_functor_simple", "Main"); }

#[rustfmt::skip] #[test] fn test_1772565060_derive_functor_contravariant_error_main() { run_test("1772565060_derive_functor_contravariant_error", "Main"); }

#[rustfmt::skip] #[test] fn test_1772565120_derive_functor_insufficient_params_main() { run_test("1772565120_derive_functor_insufficient_params", "Main"); }

#[rustfmt::skip] #[test] fn test_1772565180_derive_bifunctor_simple_main() { run_test("1772565180_derive_bifunctor_simple", "Main"); }

#[rustfmt::skip] #[test] fn test_1772565240_derive_bifunctor_missing_functor_main() { run_test("1772565240_derive_bifunctor_missing_functor", "Main"); }

#[rustfmt::skip] #[test] fn test_1772565300_derive_bifunctor_insufficient_params_main() { run_test("1772565300_derive_bifunctor_insufficient_params", "Main"); }

#[rustfmt::skip] #[test] fn test_1772565360_derive_traversable_simple_main() { run_test("1772565360_derive_traversable_simple", "Main"); }

#[rustfmt::skip] #[test] fn test_1772565420_derive_traversable_higher_kinded_main() { run_test("1772565420_derive_traversable_higher_kinded", "Main"); }

#[rustfmt::skip] #[test] fn test_1772565480_derive_traversable_missing_superclass_main() { run_test("1772565480_derive_traversable_missing_superclass", "Main"); }

#[rustfmt::skip] #[test] fn test_1772565540_derive_newtype_simple_main() { run_test("1772565540_derive_newtype_simple", "Main"); }

#[rustfmt::skip] #[test] fn test_1772565600_derive_newtype_parameterized_main() { run_test("1772565600_derive_newtype_parameterized", "Main"); }

#[rustfmt::skip] #[test] fn test_1772565660_derive_newtype_not_newtype_main() { run_test("1772565660_derive_newtype_not_newtype", "Main"); }

#[rustfmt::skip] #[test] fn test_1772565720_derive_profunctor_simple_main() { run_test("1772565720_derive_profunctor_simple", "Main"); }

#[rustfmt::skip] #[test] fn test_1772565780_derive_profunctor_error_main() { run_test("1772565780_derive_profunctor_error", "Main"); }

#[rustfmt::skip] #[test] fn test_1772565840_derive_contravariant_simple_main() { run_test("1772565840_derive_contravariant_simple", "Main"); }

#[rustfmt::skip] #[test] fn test_1772565900_derive_contravariant_error_main() { run_test("1772565900_derive_contravariant_error", "Main"); }

#[rustfmt::skip] #[test] fn test_1772565960_derive_foldable_simple_main() { run_test("1772565960_derive_foldable_simple", "Main"); }

#[rustfmt::skip] #[test] fn test_1772566020_derive_foldable_higher_kinded_main() { run_test("1772566020_derive_foldable_higher_kinded", "Main"); }

#[rustfmt::skip] #[test] fn test_1772566080_derive_bitraversable_simple_main() { run_test("1772566080_derive_bitraversable_simple", "Main"); }

#[rustfmt::skip] #[test] fn test_1772566140_derive_bitraversable_higher_kinded_main() { run_test("1772566140_derive_bitraversable_higher_kinded", "Main"); }

#[rustfmt::skip] #[test] fn test_1772566200_derive_bifoldable_simple_main() { run_test("1772566200_derive_bifoldable_simple", "Main"); }

#[rustfmt::skip] #[test] fn test_1772566260_derive_bifoldable_higher_kinded_main() { run_test("1772566260_derive_bifoldable_higher_kinded", "Main"); }

#[rustfmt::skip] #[test] fn test_1772566320_derive_newtype_class_simple_main() { run_test("1772566320_derive_newtype_class_simple", "Main"); }

#[rustfmt::skip] #[test] fn test_1772566380_derive_newtype_class_parameterized_main() { run_test("1772566380_derive_newtype_class_parameterized", "Main"); }

#[rustfmt::skip] #[test] fn test_1772566440_derive_newtype_class_not_newtype_main() { run_test("1772566440_derive_newtype_class_not_newtype", "Main"); }

#[rustfmt::skip] #[test] fn test_1772566500_derive_generic_simple_main() { run_test("1772566500_derive_generic_simple", "Main"); }

#[rustfmt::skip] #[test] fn test_1772566560_row_open_union_main() { run_test("1772566560_row_open_union", "Main"); }

#[rustfmt::skip] #[test] fn test_1772566620_row_open_cons_main() { run_test("1772566620_row_open_cons", "Main"); }

#[rustfmt::skip] #[test] fn test_1772566680_row_open_lacks_main() { run_test("1772566680_row_open_lacks", "Main"); }

#[rustfmt::skip] #[test] fn test_1772566740_row_open_record_main() { run_test("1772566740_row_open_record", "Main"); }

#[rustfmt::skip] #[test] fn test_1772566800_derive_eq_mutual_visibility_same_module_main() { run_test("1772566800_derive_eq_mutual_visibility_same_module", "Main"); }

#[rustfmt::skip] #[test] fn test_1772566860_derive_newtype_class_coercible_main() { run_test("1772566860_derive_newtype_class_coercible", "Main"); }

#[rustfmt::skip] #[test] fn test_1772566920_derive_newtype_with_given_main() { run_test("1772566920_derive_newtype_with_given", "Main"); }

#[rustfmt::skip] #[test] fn test_1772566980_derive_newtype_missing_instance_main() { run_test("1772566980_derive_newtype_missing_instance", "Main"); }

#[rustfmt::skip] #[test] fn test_1772567040_derive_newtype_missing_given_main() { run_test("1772567040_derive_newtype_missing_given", "Main"); }

#[rustfmt::skip] #[test] fn test_1772567100_derive_newtype_multi_param_main() { run_test("1772567100_derive_newtype_multi_param", "Main"); }

#[rustfmt::skip] #[test] fn test_1772567160_derive_newtype_higher_kinded_main() { run_test("1772567160_derive_newtype_higher_kinded", "Main"); }

#[rustfmt::skip] #[test] fn test_1772567220_derive_newtype_function_main() { run_test("1772567220_derive_newtype_function", "Main"); }

#[rustfmt::skip] #[test] fn test_1772567280_derive_newtype_synonym_inner_main() { run_test("1772567280_derive_newtype_synonym_inner", "Main"); }

#[rustfmt::skip] #[test] fn test_1772567340_derive_eq_1_higher_kinded_main() { run_test("1772567340_derive_eq_1_higher_kinded", "Main"); }

#[rustfmt::skip] #[test] fn test_1772567400_derive_eq_partial_main() { run_test("1772567400_derive_eq_partial", "Main"); }

#[rustfmt::skip] #[test] fn test_1772567460_derive_eq_ord_nested_higher_kinded_main() { run_test("1772567460_derive_eq_ord_nested_higher_kinded", "Main"); }

#[rustfmt::skip] #[test] fn test_1772567520_derive_functor_higher_kinded_main() { run_test("1772567520_derive_functor_higher_kinded", "Main"); }

#[rustfmt::skip] #[test] fn test_1772567580_derive_bifunctor_higher_kinded_main() { run_test("1772567580_derive_bifunctor_higher_kinded", "Main"); }

#[rustfmt::skip] #[test] fn test_1772567640_givens_retained_main() { run_test("1772567640_givens_retained", "Main"); }

#[rustfmt::skip] #[test] fn test_1772567700_givens_scoped_main() { run_test("1772567700_givens_scoped", "Main"); }

#[rustfmt::skip] #[test] fn test_1772567760_let_constraint_scoping_main() { run_test("1772567760_let_constraint_scoping", "Main"); }

#[rustfmt::skip] #[test] fn test_1772633520_type_operator_synonym_expansion_main() { run_test("1772633520_type_operator_synonym_expansion", "Main"); }

#[rustfmt::skip] #[test] fn test_1772649720_synonym_kind_application_main() { run_test("1772649720_synonym_kind_application", "Main"); }

#[rustfmt::skip] #[test] fn test_1772649780_synonym_operator_alias_main() { run_test("1772649780_synonym_operator_alias", "Main"); }

#[rustfmt::skip] #[test] fn test_1772716080_const_equation_forms_main() { run_test("1772716080_const_equation_forms", "Main"); }

#[rustfmt::skip] #[test] fn test_1772716140_const_forms_synonym_arrow_main() { run_test("1772716140_const_forms_synonym_arrow", "Main"); }

#[rustfmt::skip] #[test] fn test_1772716200_const_forms_synonym_forall_main() { run_test("1772716200_const_forms_synonym_forall", "Main"); }

#[rustfmt::skip] #[test] fn test_1772716260_const_forms_fn_alias_main() { run_test("1772716260_const_forms_fn_alias", "Main"); }

#[rustfmt::skip] #[test] fn test_1772723460_signature_synonym_data_equation_main() { run_test("1772723460_signature_synonym_data_equation", "Main"); }

#[rustfmt::skip] #[test] fn test_1772723520_signature_synonym_class_equation_main() { run_test("1772723520_signature_synonym_class_equation", "Main"); }

#[rustfmt::skip] #[test] fn test_1772723580_signature_synonym_type_equation_main() { run_test("1772723580_signature_synonym_type_equation", "Main"); }

#[rustfmt::skip] #[test] fn test_1772723640_synonym_oversaturation_equations_main() { run_test("1772723640_synonym_oversaturation_equations", "Main"); }

#[rustfmt::skip] #[test] fn test_1772743200_class_member_prenex_main() { run_test("1772743200_class_member_prenex", "Main"); }

#[rustfmt::skip] #[test] fn test_1772814960_synonym_oversaturation_kind_main() { run_test("1772814960_synonym_oversaturation_kind", "Main"); }

#[rustfmt::skip] #[test] fn test_1772815740_derive_generic_insufficient_params_main() { run_test("1772815740_derive_generic_insufficient_params", "Main"); }

#[rustfmt::skip] #[test] fn test_1772815800_derive_generic_not_constructor_main() { run_test("1772815800_derive_generic_not_constructor", "Main"); }

#[rustfmt::skip] #[test] fn test_1772815860_derive_generic_missing_rep_main() { run_test("1772815860_derive_generic_missing_rep", "Main"); }

#[rustfmt::skip] #[test] fn test_1772817840_do_discard_main() { run_test("1772817840_do_discard", "Main"); }

#[rustfmt::skip] #[test] fn test_1772817900_do_bind_main() { run_test("1772817900_do_bind", "Main"); }

#[rustfmt::skip] #[test] fn test_1772817960_ado_discard_main() { run_test("1772817960_ado_discard", "Main"); }

#[rustfmt::skip] #[test] fn test_1772818020_ado_bind_main() { run_test("1772818020_ado_bind", "Main"); }

#[rustfmt::skip] #[test] fn test_1772818080_do_polymorphic_main() { run_test("1772818080_do_polymorphic", "Main"); }

#[rustfmt::skip] #[test] fn test_1772818140_ado_polymorphic_main() { run_test("1772818140_ado_polymorphic", "Main"); }

#[rustfmt::skip] #[test] fn test_1772818200_do_empty_block_main() { run_test("1772818200_do_empty_block", "Main"); }

#[rustfmt::skip] #[test] fn test_1772818260_ado_empty_block_main() { run_test("1772818260_ado_empty_block", "Main"); }

#[rustfmt::skip] #[test] fn test_1772818320_do_ado_constrained_main() { run_test("1772818320_do_ado_constrained", "Main"); }

#[rustfmt::skip] #[test] fn test_1772818380_do_let_premature_solve_main() { run_test("1772818380_do_let_premature_solve", "Main"); }

#[rustfmt::skip] #[test] fn test_1772818440_do_let_annotation_solve_main() { run_test("1772818440_do_let_annotation_solve", "Main"); }

#[rustfmt::skip] #[test] fn test_1772818500_ado_let_premature_solve_main() { run_test("1772818500_ado_let_premature_solve", "Main"); }

#[rustfmt::skip] #[test] fn test_1772818560_ado_let_annotation_solve_main() { run_test("1772818560_ado_let_annotation_solve", "Main"); }

#[rustfmt::skip] #[test] fn test_1772818620_do_bind_only_main() { run_test("1772818620_do_bind_only", "Main"); }

#[rustfmt::skip] #[test] fn test_1772818680_do_final_bind_main() { run_test("1772818680_do_final_bind", "Main"); }

#[rustfmt::skip] #[test] fn test_1772818740_do_final_let_main() { run_test("1772818740_do_final_let", "Main"); }

#[rustfmt::skip] #[test] fn test_1772818800_exhaustive_multiple_main() { run_test("1772818800_exhaustive_multiple", "Main"); }

#[rustfmt::skip] #[test] fn test_1772818860_exhaustive_tuple_main() { run_test("1772818860_exhaustive_tuple", "Main"); }

#[rustfmt::skip] #[test] fn test_1772818920_exhaustive_equation_redundant_main() { run_test("1772818920_exhaustive_equation_redundant", "Main"); }

#[rustfmt::skip] #[test] fn test_1772818980_exhaustive_equation_guarded_main() { run_test("1772818980_exhaustive_equation_guarded", "Main"); }

#[rustfmt::skip] #[test] fn test_1772819040_exhaustive_let_equation_main() { run_test("1772819040_exhaustive_let_equation", "Main"); }

#[rustfmt::skip] #[test] fn test_1772819100_exhaustive_instance_equation_main() { run_test("1772819100_exhaustive_instance_equation", "Main"); }

#[rustfmt::skip] #[test] fn test_1772819160_exhaustive_guards_otherwise_true_main() { run_test("1772819160_exhaustive_guards_otherwise_true", "Main"); }

#[rustfmt::skip] #[test] fn test_1772822940_givens_matching_main() { run_test("1772822940_givens_matching", "Main"); }

#[rustfmt::skip] #[test] fn test_1772823000_givens_functional_dependency_main() { run_test("1772823000_givens_functional_dependency", "Main"); }

#[rustfmt::skip] #[test] fn test_1772823060_givens_superclass_where_binding_main() { run_test("1772823060_givens_superclass_where_binding", "Main"); }

#[rustfmt::skip] #[test] fn test_1772823120_instance_record_matching_main() { run_test("1772823120_instance_record_matching", "Main"); }

#[rustfmt::skip] #[test] fn test_1772823180_instance_record_open_row_main() { run_test("1772823180_instance_record_open_row", "Main"); }

#[rustfmt::skip] #[test] fn test_1772823240_instance_head_invalid_row_main() { run_test("1772823240_instance_head_invalid_row", "Main"); }

#[rustfmt::skip] #[test] fn test_1772823300_instance_kind_application_matching_main() { run_test("1772823300_instance_kind_application_matching", "Main"); }

#[rustfmt::skip] #[test] fn test_1772823360_application_infix_chain_main() { run_test("1772823360_application_infix_chain", "Main"); }

#[rustfmt::skip] #[test] fn test_1772823420_application_function_subtype_main() { run_test("1772823420_application_function_subtype", "Main"); }

#[rustfmt::skip] #[test] fn test_1772823480_application_function_decomposition_main() { run_test("1772823480_application_function_decomposition", "Main"); }

#[rustfmt::skip] #[test] fn test_1772823540_type_application_invalid_basic_main() { run_test("1772823540_type_application_invalid_basic", "Main"); }

#[rustfmt::skip] #[test] fn test_1772823600_type_application_invalid_too_many_main() { run_test("1772823600_type_application_invalid_too_many", "Main"); }

#[rustfmt::skip] #[test] fn test_1772823660_record_expression_exact_main() { run_test("1772823660_record_expression_exact", "Main"); }

#[rustfmt::skip] #[test] fn test_1772823720_record_expression_missing_field_main() { run_test("1772823720_record_expression_missing_field", "Main"); }

#[rustfmt::skip] #[test] fn test_1772823780_record_expression_additional_field_main() { run_test("1772823780_record_expression_additional_field", "Main"); }

#[rustfmt::skip] #[test] fn test_1772823840_record_expression_missing_and_additional_main() { run_test("1772823840_record_expression_missing_and_additional", "Main"); }

#[rustfmt::skip] #[test] fn test_1772823900_record_expression_nested_additional_main() { run_test("1772823900_record_expression_nested_additional", "Main"); }

#[rustfmt::skip] #[test] fn test_1772823960_record_access_sections_main() { run_test("1772823960_record_access_sections", "Main"); }

#[rustfmt::skip] #[test] fn test_1772824020_record_update_sections_main() { run_test("1772824020_record_update_sections", "Main"); }

#[rustfmt::skip] #[test] fn test_1772824080_record_binder_shrinking_main() { run_test("1772824080_record_binder_shrinking", "Main"); }

#[rustfmt::skip] #[test] fn test_1772824140_record_binder_additional_property_main() { run_test("1772824140_record_binder_additional_property", "Main"); }

#[rustfmt::skip] #[test] fn test_1772824200_record_binder_additional_property_nested_main() { run_test("1772824200_record_binder_additional_property_nested", "Main"); }

#[rustfmt::skip] #[test] fn test_1772824260_synonym_function_result_kind_main() { run_test("1772824260_synonym_function_result_kind", "Main"); }

#[rustfmt::skip] #[test] fn test_1772824320_synonym_forall_expansion_main() { run_test("1772824320_synonym_forall_expansion", "Main"); }

#[rustfmt::skip] #[test] fn test_1772824380_binder_instantiation_main() { run_test("1772824380_binder_instantiation", "Main"); }

#[rustfmt::skip] #[test] fn test_1772826900_coercible_phantom_main() { run_test("1772826900_coercible_phantom", "Main"); }

#[rustfmt::skip] #[test] fn test_1772826960_coercible_representational_main() { run_test("1772826960_coercible_representational", "Main"); }

#[rustfmt::skip] #[test] fn test_1772827020_coercible_array_main() { run_test("1772827020_coercible_array", "Main"); }

#[rustfmt::skip] #[test] fn test_1772827080_coercible_record_main() { run_test("1772827080_coercible_record", "Main"); }

#[rustfmt::skip] #[test] fn test_1772827140_coercible_transitivity_main() { run_test("1772827140_coercible_transitivity", "Main"); }

#[rustfmt::skip] #[test] fn test_1772827200_coercible_different_heads_error_main() { run_test("1772827200_coercible_different_heads_error", "Main"); }

#[rustfmt::skip] #[test] fn test_1772827260_coercible_nominal_main() { run_test("1772827260_coercible_nominal", "Main"); }

#[rustfmt::skip] #[test] fn test_1772827320_coercible_newtype_hidden_main() { run_test("1772827320_coercible_newtype_hidden", "Main"); }

#[rustfmt::skip] #[test] fn test_1772827380_coercible_newtype_qualified_main() { run_test("1772827380_coercible_newtype_qualified", "Main"); }

#[rustfmt::skip] #[test] fn test_1772827440_coercible_newtype_open_hidden_main() { run_test("1772827440_coercible_newtype_open_hidden", "Main"); }

#[rustfmt::skip] #[test] fn test_1772827500_coercible_nested_records_main() { run_test("1772827500_coercible_nested_records", "Main"); }

#[rustfmt::skip] #[test] fn test_1772827560_coercible_higher_kinded_error_main() { run_test("1772827560_coercible_higher_kinded_error", "Main"); }

#[rustfmt::skip] #[test] fn test_1772827620_coercible_higher_kinded_multi_main() { run_test("1772827620_coercible_higher_kinded_multi", "Main"); }

#[rustfmt::skip] #[test] fn test_1772827680_coercible_higher_kinded_polykinded_main() { run_test("1772827680_coercible_higher_kinded_polykinded", "Main"); }

#[rustfmt::skip] #[test] fn test_1772827740_coercible_function_decomposition_main() { run_test("1772827740_coercible_function_decomposition", "Main"); }

#[rustfmt::skip] #[test] fn test_1772866020_derive_newtype_not_constructor_main() { run_test("1772866020_derive_newtype_not_constructor", "Main"); }

#[rustfmt::skip] #[test] fn test_1772866080_derive_newtype_class_not_constructor_main() { run_test("1772866080_derive_newtype_class_not_constructor", "Main"); }

#[rustfmt::skip] #[test] fn test_1772866140_derive_newtype_not_local_main() { run_test("1772866140_derive_newtype_not_local", "Main"); }

#[rustfmt::skip] #[test] fn test_1772866200_derive_newtype_class_not_local_main() { run_test("1772866200_derive_newtype_class_not_local", "Main"); }

#[rustfmt::skip] #[test] fn test_1772866260_derive_newtype_insufficient_params_main() { run_test("1772866260_derive_newtype_insufficient_params", "Main"); }

#[rustfmt::skip] #[test] fn test_1772866320_derive_newtype_class_insufficient_params_main() { run_test("1772866320_derive_newtype_class_insufficient_params", "Main"); }

#[rustfmt::skip] #[test] fn test_1773005580_equation_synonym_expansion_main() { run_test("1773005580_equation_synonym_expansion", "Main"); }

#[rustfmt::skip] #[test] fn test_1773152280_derive_newtype_invalid_skolem_layout_main() { run_test("1773152280_derive_newtype_invalid_skolem_layout", "Main"); }

#[rustfmt::skip] #[test] fn test_1773174840_instance_implicit_variable_freshening_main() { run_test("1773174840_instance_implicit_variable_freshening", "Main"); }

#[rustfmt::skip] #[test] fn test_1773326220_type_operator_synonym_partial_arguments_main() { run_test("1773326220_type_operator_synonym_partial_arguments", "Main"); }

#[rustfmt::skip] #[test] fn test_1773330180_type_synonym_higher_order_main() { run_test("1773330180_type_synonym_higher_order", "Main"); }

#[rustfmt::skip] #[test] fn test_1773330240_type_synonym_higher_order_operator_main() { run_test("1773330240_type_synonym_higher_order_operator", "Main"); }

#[rustfmt::skip] #[test] fn test_1773478800_category_identity_operator_alias_main() { run_test("1773478800_category_identity_operator_alias", "Main"); }

#[rustfmt::skip] #[test] fn test_1773490920_operator_alias_kind_application_main() { run_test("1773490920_operator_alias_kind_application", "Main"); }

#[rustfmt::skip] #[test] fn test_1773491520_instance_function_unification_main() { run_test("1773491520_instance_function_unification", "Main"); }

#[rustfmt::skip] #[test] fn test_1773501960_derive_eq_ord_1_kind_arguments_main() { run_test("1773501960_derive_eq_ord_1_kind_arguments", "Main"); }

#[rustfmt::skip] #[test] fn test_1773504480_exhaustive_record_main() { run_test("1773504480_exhaustive_record", "Main"); }

#[rustfmt::skip] #[test] fn test_1773504540_exhaustive_array_main() { run_test("1773504540_exhaustive_array", "Main"); }

#[rustfmt::skip] #[test] fn test_1773504600_exhaustive_missing_patterns_main() { run_test("1773504600_exhaustive_missing_patterns", "Main"); }

#[rustfmt::skip] #[test] fn test_1773504660_exhaustive_unsafe_partial_main() { run_test("1773504660_exhaustive_unsafe_partial", "Main"); }

#[rustfmt::skip] #[test] fn test_1773712800_visible_type_application_value_main() { run_test("1773712800_visible_type_application_value", "Main"); }

#[rustfmt::skip] #[test] fn test_1773712860_visible_type_application_data_newtype_main() { run_test("1773712860_visible_type_application_data_newtype", "Main"); }

#[rustfmt::skip] #[test] fn test_1773712920_visible_type_application_class_main() { run_test("1773712920_visible_type_application_class", "Main"); }

#[rustfmt::skip] #[test] fn test_1773712980_visible_type_application_tuple_main() { run_test("1773712980_visible_type_application_tuple", "Main"); }

#[rustfmt::skip] #[test] fn test_1773713040_visible_type_application_either_main() { run_test("1773713040_visible_type_application_either", "Main"); }

#[rustfmt::skip] #[test] fn test_1773713100_visible_type_application_error_main() { run_test("1773713100_visible_type_application_error", "Main"); }

#[rustfmt::skip] #[test] fn test_1773745500_record_quoted_label_main() { run_test("1773745500_record_quoted_label", "Main"); }

#[rustfmt::skip] #[test] fn test_1773773820_record_field_instantiation_main() { run_test("1773773820_record_field_instantiation", "Main"); }

#[rustfmt::skip] #[test] fn test_1773823020_data_argument_kind_applications_main() { run_test("1773823020_data_argument_kind_applications", "Main"); }

#[rustfmt::skip] #[test] fn test_1773841860_instance_kind_substitution_main() { run_test("1773841860_instance_kind_substitution", "Main"); }

#[rustfmt::skip] #[test] fn test_1773852780_prim_row_cons_tail_synonym_main() { run_test("1773852780_prim_row_cons_tail_synonym", "Main"); }

#[rustfmt::skip] #[test] fn test_1774024080_prim_int_given_improvement_main() { run_test("1774024080_prim_int_given_improvement", "Main"); }

#[rustfmt::skip] #[test] fn test_1774024140_given_deep_name_improvement_main() { run_test("1774024140_given_deep_name_improvement", "Main"); }

#[rustfmt::skip] #[test] fn test_1774024440_prim_row_nub_union_class_constraint_main() { run_test("1774024440_prim_row_nub_union_class_constraint", "Main"); }

#[rustfmt::skip] #[test] fn test_1774448100_self_recursive_through_operator_main() { run_test("1774448100_self_recursive_through_operator", "Main"); }

#[rustfmt::skip] #[test] fn test_1774448160_kind_applied_reference_double_application_main() { run_test("1774448160_kind_applied_reference_double_application", "Main"); }

#[rustfmt::skip] #[test] fn test_1774448220_section_rule_progress_main() { run_test("1774448220_section_rule_progress", "Main"); }

#[rustfmt::skip] #[test] fn test_1774448280_given_synonym_expansion_main() { run_test("1774448280_given_synonym_expansion", "Main"); }

#[rustfmt::skip] #[test] fn test_1774448340_operator_result_ordering_main() { run_test("1774448340_operator_result_ordering", "Main"); }

#[rustfmt::skip] #[test] fn test_1774448400_record_field_polymorphic_main() { run_test("1774448400_record_field_polymorphic", "Main"); }

#[rustfmt::skip] #[test] fn test_1774448460_hidden_forall_scope_check_main() { run_test("1774448460_hidden_forall_scope_check", "Main"); }

#[rustfmt::skip] #[test] fn test_1774448520_hidden_forall_compose_application_subtype_main() { run_test("1774448520_hidden_forall_compose_application_subtype", "Main"); }

#[rustfmt::skip] #[test] fn test_1774448580_forall_row_field_main() { run_test("1774448580_forall_row_field", "Main"); }

#[rustfmt::skip] #[test] fn test_1774553700_instance_chain_stuck_matching_main() { run_test("1774553700_instance_chain_stuck_matching", "Main"); }

#[rustfmt::skip] #[test] fn test_1774667040_given_stuck_match_collapse_main() { run_test("1774667040_given_stuck_match_collapse", "Main"); }

#[rustfmt::skip] #[test] fn test_1774799580_let_binding_ambiguous_constraint_main() { run_test("1774799580_let_binding_ambiguous_constraint", "Main"); }

#[rustfmt::skip] #[test] fn test_1777091040_row_union_open_duplicates_main() { run_test("1777091040_row_union_open_duplicates", "Main"); }

#[rustfmt::skip] #[test] fn test_1777091100_row_union_documentation_main() { run_test("1777091100_row_union_documentation", "Main"); }

#[rustfmt::skip] #[test] fn test_1777130640_instance_given_is_not_blocking_main() { run_test("1777130640_instance_given_is_not_blocking", "Main"); }

#[rustfmt::skip] #[test] fn test_1777201800_row_tail_main() { run_test("1777201800_row_tail", "Main"); }

#[rustfmt::skip] #[test] fn test_1777201860_row_tail_record_main() { run_test("1777201860_row_tail_record", "Main"); }

#[rustfmt::skip] #[test] fn test_1777287480_guarded_conditionals_checking_main() { run_test("1777287480_guarded_conditionals_checking", "Main"); }

#[rustfmt::skip] #[test] fn test_1777298160_instance_context_fail_deferred_main() { run_test("1777298160_instance_context_fail_deferred", "Main"); }

#[rustfmt::skip] #[test] fn test_1777298340_top_level_fail_eager_main() { run_test("1777298340_top_level_fail_eager", "Main"); }

#[rustfmt::skip] #[test] fn test_1777303980_let_retain_polymorphism_main() { run_test("1777303980_let_retain_polymorphism", "Main"); }

#[rustfmt::skip] #[test] fn test_1777463820_stress_test_row_main() { run_test("1777463820_stress_test_row", "Main"); }

#[rustfmt::skip] #[test] fn test_1777489980_expression_hole_main() { run_test("1777489980_expression_hole", "Main"); }

#[rustfmt::skip] #[test] fn test_1777626000_indexed_do_discard_continuation_main() { run_test("1777626000_indexed_do_discard_continuation", "Main"); }

#[rustfmt::skip] #[test] fn test_1777708740_row_tails_higher_kinded_main() { run_test("1777708740_row_tails_higher_kinded", "Main"); }

#[rustfmt::skip] #[test] fn test_1777708800_cross_module_instance_lookup_main() { run_test("1777708800_cross_module_instance_lookup", "Main"); }

#[rustfmt::skip] #[test] fn test_1777708860_local_instance_conflict_main() { run_test("1777708860_local_instance_conflict", "Main"); }

#[rustfmt::skip] #[test] fn test_1777710900_prim_int_compare_concrete_seed_main() { run_test("1777710900_prim_int_compare_concrete_seed", "Main"); }

#[rustfmt::skip] #[test] fn test_1777725840_derive_newtype_recursive_record_main() { run_test("1777725840_derive_newtype_recursive_record", "Main"); }

#[rustfmt::skip] #[test] fn test_1777825440_irrefutable_variable_pattern_main() { run_test("1777825440_irrefutable_variable_pattern", "Main"); }

#[rustfmt::skip] #[test] fn test_1777825620_instance_chain_skolem_main() { run_test("1777825620_instance_chain_skolem", "Main"); }

#[rustfmt::skip] #[test] fn test_1777829280_lacks_empty_closed_row_skolem_main() { run_test("1777829280_lacks_empty_closed_row_skolem", "Main"); }

#[rustfmt::skip] #[test] fn test_1777834140_coercible_via_newtype_superclass_main() { run_test("1777834140_coercible_via_newtype_superclass", "Main"); }

#[rustfmt::skip] #[test] fn test_1777993860_type_application_instantiation_main() { run_test("1777993860_type_application_instantiation", "Main"); }

#[rustfmt::skip] #[test] fn test_1778008860_kind_variable_visibility_main() { run_test("1778008860_kind_variable_visibility", "Main"); }

#[rustfmt::skip] #[test] fn test_1778010840_wildcard_arity_main() { run_test("1778010840_wildcard_arity", "Main"); }

#[rustfmt::skip] #[test] fn test_1778051400_record_pun_constrained_field_main() { run_test("1778051400_record_pun_constrained_field", "Main"); }

#[rustfmt::skip] #[test] fn test_1778051460_bare_row_tail_syntax_main() { run_test("1778051460_bare_row_tail_syntax", "Main"); }
