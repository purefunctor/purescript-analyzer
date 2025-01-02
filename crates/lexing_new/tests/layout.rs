use lexing_new::Lexed;
use syntax::SyntaxKind;
use test_each_file::test_each_file;

fn print_tokens(lexed: &Lexed, tokens: &[SyntaxKind]) -> String {
    let mut buffer = String::new();
    let mut index = 0;

    for token in tokens {
        match token {
            SyntaxKind::LAYOUT_START => {
                buffer.push('{');
            }
            SyntaxKind::LAYOUT_SEPARATOR => {
                buffer.push(';');
            }
            SyntaxKind::LAYOUT_END => {
                buffer.push('}');
            }
            i => {
                if let Some(annotation) = lexed.annotation(index) {
                    buffer.push_str(annotation);
                }
                if let Some(qualifier) = lexed.qualifier(index) {
                    buffer.push_str(qualifier);
                }
                if matches!(i, SyntaxKind::END_OF_FILE) {
                    buffer.push_str("<eof>");
                } else {
                    buffer.push_str(lexed.text(index));
                }
                index += 1;
            }
        }
    }
    buffer
}

test_each_file! { in "./crates/lexing_new/tests/layout" => |content: &str| {
    let lexed = lexing_new::lex(content);
    let tokens = lexing_new::layout(&lexed);
    insta::assert_snapshot!(print_tokens(&lexed, &tokens));
}}
