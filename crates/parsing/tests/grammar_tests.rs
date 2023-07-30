use std::fmt::Write;

use parsing::{
    grammar::{expression_atom, qualified_name},
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
    expect_parse("qualified-constructor", "Hello.World", qualified_name);
}

#[test]
fn test_qualified_variable() {
    expect_parse("qualified-variable", "Hello.World.hello", qualified_name);
}

#[test]
fn test_qualified_variable_as() {
    expect_parse("qualified-variable-as", "Hello.World.as", qualified_name);
}

#[test]
fn test_qualified_name_error() {
    expect_parse("qualified-error", "Hello.World.(import", qualified_name);
}

#[test]
fn test_qualified_name_plural() {
    expect_parse("qualified-plural", "(hello) world", |parser| {
        qualified_name(parser);
        qualified_name(parser);
    })
}

#[test]
fn test_expression_atom_literal_int() {
    expect_parse("literal-int", "1", expression_atom);
}
