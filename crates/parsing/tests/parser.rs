macro_rules! parser_tests {
    ($($name:ident => $source:expr),+ $(,)?) => {
        $(
            #[test]
            fn $name() {
                let lexed = lexing::lex($source);
                let tokens = lexing::layout(&lexed);
                let (node, errors) = parsing::parse(&lexed, &tokens);
                insta::assert_debug_snapshot!((node, errors));
            }
        )+
    };
}

parser_tests!(
    comment_pre_post => include_str!("parser/CommentPrePost.purs"),
    lexer_error_inserted => include_str!("parser/LexerErrorInserted.purs"),
    module_export => include_str!("parser/ModuleExport.purs"),
    module_export_class => include_str!("parser/ModuleExportClass.purs"),
    module_export_class_error_end => include_str!("parser/ModuleExportClassErrorEnd.purs"),
    module_export_comma_error => include_str!("parser/ModuleExportCommaError.purs"),
    module_export_data => include_str!("parser/ModuleExportData.purs"),
    module_export_data_comma_error => include_str!("parser/ModuleExportDataCommaError.purs"),
    module_export_data_item_after_double_period => include_str!("parser/ModuleExportDataItemAfterDoublePeriod.purs"),
    module_export_data_item_before_double_period => include_str!("parser/ModuleExportDataItemBeforeDoublePeriod.purs"),
    module_export_data_item_invalid => include_str!("parser/ModuleExportDataItemInvalid.purs"),
    module_export_empty => include_str!("parser/ModuleExportEmpty.purs"),
    module_export_invalid => include_str!("parser/ModuleExportInvalid.purs"),
    module_export_type_operator => include_str!("parser/ModuleExportTypeOperator.purs"),
    module_export_type_operator_error => include_str!("parser/ModuleExportTypeOperatorError.purs"),
    module_export_type_operator_error_end => include_str!("parser/ModuleExportTypeOperatorErrorEnd.purs"),
    module_header => include_str!("parser/ModuleHeader.purs"),
    module_header_prefixed => include_str!("parser/ModuleHeaderPrefixed.purs"),
);
