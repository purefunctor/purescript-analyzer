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
    module_export => include_str!("parser/ModuleExport.purs"),
    module_export_comma_error => include_str!("parser/ModuleExportCommaError.purs"),
    module_export_empty => include_str!("parser/ModuleExportEmpty.purs"),
    module_export_invalid => include_str!("parser/ModuleExportInvalid.purs"),
    module_header => include_str!("parser/ModuleHeader.purs"),
    module_header_prefixed => include_str!("parser/ModuleHeaderPrefixed.purs"),
);
