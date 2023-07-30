use std::fmt::Write;

use parsing::{
    grammar::qualified_name,
    lexer::lex,
    parser::{Event, Parser},
};

fn expect_parse<F>(name: &str, source: &str, rule: F)
where
    F: Fn(&mut Parser),
{
    let lexed = lex(source);
    let input = lexed.as_input();
    let mut parser = Parser::new(input);
    rule(&mut parser);

    let mut result = String::new();
    writeln!(result, "Input: {source}").unwrap();

    let mut indentation = 0;
    for actual in parser.as_output() {
        match actual {
            Event::Start { .. } => {
                writeln!(result, "{:indentation$}{:?}", "", actual, indentation = indentation)
                    .unwrap();
                indentation += 2
            }
            Event::Finish => {
                indentation -= 2;
                writeln!(result, "{:indentation$}{:?}", "", actual, indentation = indentation)
                    .unwrap();
            }
            _ => {
                writeln!(result, "{:indentation$}{:?}", "", actual, indentation = indentation)
                    .unwrap();
            }
        }
    }

    insta::assert_snapshot!(name, result);
}

#[test]
fn test_qualified_constructor() {
    expect_parse("qualified-constructor", "Hello.World $", qualified_name);
}

#[test]
fn test_qualified_variable() {
    expect_parse("qualified-variable", "Hello.World.hello", qualified_name);
}

#[test]
fn test_qualified_variable_as() {
    expect_parse("qualified-variable-as", "Hello.World.as", qualified_name);
}
