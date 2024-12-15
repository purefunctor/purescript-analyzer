macro_rules! parser_tests {
    ($($name:ident => $source:expr),+ $(,)?) => {
        $(
            #[test]
            fn $name() {
                let lexed = lexing::lex($source);
                let tokens = lexing::layout(&lexed);
                let (node, _) = parsing::parse(&lexed, &tokens);
                insta::assert_debug_snapshot!(node);
            }
        )+
    };
}

parser_tests!(
    module_header => include_str!("parser/ModuleHeader.purs"),
    module_header_prefixed => include_str!("parser/ModuleHeaderPrefixed.purs"),
);
