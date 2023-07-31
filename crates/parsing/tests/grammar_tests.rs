use std::fmt::Write;

use parsing::{
    grammar::{expression_1, expression_2, expression_atom, qualified_name},
    lexer::lex,
    parser::{Event, Parser},
};
use syntax::SyntaxKind;

fn expect_parse<F, T>(name: &str, source: &str, rule: F)
where
    F: Fn(&mut Parser) -> T,
{
    let lexed = lex(source);
    let input = lexed.as_input();
    let mut parser = Parser::new(input);
    let _ = rule(&mut parser);

    let mut result = String::new();
    writeln!(result, "Input: {source}").unwrap();

    let mut indentation = 0;
    for actual in parser.as_output() {
        match actual {
            Event::Start { kind } => {
                writeln!(result, "{:indentation$}{:?}", "", actual, indentation = indentation)
                    .unwrap();
                if let SyntaxKind::Sentinel = kind {
                    continue;
                }
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

#[test]
fn test_expression_operator_chain() {
    expect_parse("operator-chain", "1 + 2 + 3", expression_1);
}

#[test]
fn test_expression_operator_chain_empty() {
    expect_parse("operator-chain-empty", "1", expression_1);
}

#[test]
fn test_expression_function_application() {
    expect_parse("application-expression", "1 2", expression_2);
}

#[test]
fn test_expression_function_application_empty_spine() {
    expect_parse("application-expression-empty-spine", "1", expression_2);
}
