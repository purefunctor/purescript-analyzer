use std::fmt::Write;

use insta::glob;
use lexing::{layout, lex};
use syntax::SyntaxKind;

use crate::{parser::Event, Parser};

fn expect_parse<T>(source: &str, rule: impl Fn(&mut Parser) -> T) {
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
fn test_module() {
    glob!("inputs/module", "*.purs", |path| {
        let source = std::fs::read_to_string(path).unwrap();
        expect_parse(&source, super::module);
    });
}
