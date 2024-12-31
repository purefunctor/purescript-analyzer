use lexing::Lexed;
use syntax::SyntaxKind;

fn print_tokens(lexed: &Lexed, tokens: &[SyntaxKind]) -> String {
    let mut buffer = String::new();
    let mut index = 0;
    let mut whitespace = vec![];

    for token in tokens {
        while lexed.kind(index).is_whitespace_or_comment() {
            whitespace.push(index);
            index += 1;
        }
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
            SyntaxKind::END_OF_FILE => {
                whitespace.drain(..).for_each(|index| {
                    buffer.push_str(lexed.text(index));
                });
                buffer.push_str("<eof>");
            }
            _ => {
                whitespace.drain(..).for_each(|index| {
                    buffer.push_str(lexed.text(index));
                });
                buffer.push_str(lexed.text(index));
                index += 1;
            }
        }
    }
    buffer
}

macro_rules! layout_tests {
    ($($name:ident => $source:expr),+ $(,)?) => {
        $(
            #[test]
            fn $name() {
                let lexed = lexing::lex($source);
                let tokens = lexing::layout(&lexed);
                let source = print_tokens(&lexed, &tokens);
                insta::assert_snapshot!(source);
            }
        )+
    };
}

layout_tests!(
    ado_in => include_str!("layout/AdoIn.purs"),
    backtick_operator => include_str!("layout/BacktickOperator.purs"),
    case_guards => include_str!("layout/CaseGuards.purs"),
    case_where => include_str!("layout/CaseWhere.purs"),
    class_head => include_str!("layout/ClassHead.purs"),
    commas => include_str!("layout/Commas.purs"),
    delimiter => include_str!("layout/Delimiter.purs"),
    do_let => include_str!("layout/DoLet.purs"),
    do_operator => include_str!("layout/DoOperator.purs"),
    do_where => include_str!("layout/DoWhere.purs"),
    if_then_else_do => include_str!("layout/IfThenElseDo.purs"),
    instance_chain_else => include_str!("layout/InstanceChainElse.purs"),
    instance_chain_else_newline => include_str!("layout/InstanceChainElseNewline.purs"),
    int_type => include_str!("layout/IntType.purs"),
    let_guards => include_str!("layout/LetGuards.purs"),
    // shebang => include_str!("layout/Shebang.purs"),
);
