use std::fmt::Write;

use insta::glob;
use lexing::{layout, lex};
use parsing::{
    grammar::{expression, pattern, ty},
    parser::{Event, Parser},
};
use syntax::SyntaxKind;

fn expect_parse<F, T>(source: &str, rule: F)
where
    F: Fn(&mut Parser) -> T,
{
    let lexed = lex(source);
    let input = layout(&lexed);
    let mut parser = Parser::new(&input);
    let _ = rule(&mut parser);

    let mut result = String::new();
    writeln!(result, "Input: {source}").unwrap();

    let mut indentation = 0;
    for actual in parser.finalize() {
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

#[test]
fn test_pattern() {
    glob!("inputs/passing/pattern", "*.input", |path| {
        let source = std::fs::read_to_string(path).unwrap();
        expect_parse(&source, pattern);
    });
}

// #[test]
// fn test_declaration() {
//     glob!("inputs/passing/declaration", "*.input", |path| {
//         let source = std::fs::read_to_string(path).unwrap();
//         expect_parse(&source, declaration);
//     });
// }
