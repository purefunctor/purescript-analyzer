use std::fmt::Write;

use insta::glob;
use parsing::{
    grammar::{expression, ty},
    layout::LayoutKind,
    lexer::lex,
    parser::{Event, Parser},
};
use syntax::SyntaxKind;

fn expect_parse<F, T>(source: &str, rule: F)
where
    F: Fn(&mut Parser) -> T,
{
    let lexed = lex(source);
    let input = lexed.as_input();
    let mut parser = Parser::new(input);
    parser.layout_start(LayoutKind::Module);
    let _ = rule(&mut parser);

    let mut result = String::new();
    writeln!(result, "Input: {source}").unwrap();

    let mut indentation = 0;
    for actual in parser.as_output() {
        if let Event::Start { kind: SyntaxKind::Sentinel } = actual {
            continue;
        }
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

    insta::assert_snapshot!(result);
}

#[test]
fn test_expression() {
    glob!("inputs/passing/expression", "*.input", |path| {
        let source = std::fs::read_to_string(path).unwrap();
        expect_parse(&source, expression);
    })
}

#[test]
fn test_expression_failing() {
    glob!("inputs/failing/expression", "*.input", |path| {
        let source = std::fs::read_to_string(path).unwrap();
        expect_parse(&source, expression);
    })
}

#[test]
fn test_type() {
    glob!("inputs/passing/type", "*.input", |path| {
        let source = std::fs::read_to_string(path).unwrap();
        expect_parse(&source, ty);
    });
}

// #[test]
// fn test_binder() {
//     expect_parse("typed-binder", "1 :: Type", binder);
//     expect_parse("negative-binder", "-1", binder);
//     expect_parse("integer-binder", "1", binder);
//     expect_parse("number-binder", "1.0", binder);
//     expect_parse("string-binder", "\"hi!\"", binder);
//     expect_parse("char-binder", "'a'", binder);
//     expect_parse("true-binder", "true", binder);
//     expect_parse("false-binder", "false", binder);
//     expect_parse("wildcard-binder", "_", binder);
//     expect_parse("parenthesized-binder-negative", "(-1)", binder);
//     expect_parse("parenthesized-binder-literal", "(1.0)", binder);
//     expect_parse("parenthesized-binder-nested", "(((1.0)))", binder);
//     expect_parse("variable-binder", "f", binder);
//     expect_parse("constructor-zero-arity", "Nil", binder);
//     expect_parse("qualified-constructor-zero-arity", "Data.List.Nil", binder);
//     expect_parse("constructor-n-arity", "Cons 1", binder);
//     expect_parse("qualified-constructor-n-arity", "Data.List.Cons 1", binder);
//     expect_parse("complex-binder", "Tuple (Cons 0 Nil) (Cons 1 Nil)", binder);
// }
