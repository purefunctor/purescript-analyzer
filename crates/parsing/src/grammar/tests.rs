use std::{fmt::Write, fs, path::PathBuf};

use lexing::{layout, lex};
use syntax::SyntaxKind;

use crate::{
    grammar::{rules::expr_0, rules::module, rules::pat_0, rules::type_0},
    parser::{Event, Parser},
};

fn expect_parse<F, T>(path: PathBuf, rule: F)
where
    F: Fn(&mut Parser) -> T,
{
    let source = fs::read_to_string(&path).unwrap();
    let name = path.file_stem().unwrap().to_str().unwrap();
    let lexed = lex(&source);
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

    insta::assert_snapshot!(name, result);
}

#[test_each::path(glob = "crates/parsing/src/grammar/inputs/passing/expression/*.input")]
fn test_expression(path: PathBuf) {
    expect_parse(path, expr_0);
}

#[test_each::path(glob = "crates/parsing/src/grammar/inputs/failing/expression/*.input")]
fn test_expression_failing(path: PathBuf) {
    expect_parse(path, expr_0);
}

#[test_each::path(glob = "crates/parsing/src/grammar/inputs/passing/type/*.input")]
fn test_type(path: PathBuf) {
    expect_parse(path, type_0);
}

#[test_each::path(glob = "crates/parsing/src/grammar/inputs/passing/pattern/*.input")]
fn test_pattern(path: PathBuf) {
    expect_parse(path, pat_0);
}

#[test_each::path(glob = "crates/parsing/src/grammar/inputs/passing/file/*.input")]
fn test_file(path: PathBuf) {
    expect_parse(path, module);
}
