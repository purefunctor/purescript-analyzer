use test_each_file::test_each_file;

test_each_file! { in "./crates/parsing/tests/parser" => |content: &str| {
    let lexed = lexing::lex(content);
    let tokens = lexing::layout(&lexed);
    let (node, errors) = parsing::parse(&lexed, &tokens);
    insta::assert_debug_snapshot!((node, errors));
}}
