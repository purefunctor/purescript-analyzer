use std::{collections::HashMap, fs, path::PathBuf};

use parsing::ParseError;
use tests_package_set::all_source_files;

type ParseErrorPerFile = HashMap<PathBuf, Vec<ParseError>>;

#[test]
fn test_parse_package_set() {
    let mut all_errors = ParseErrorPerFile::default();

    for file in all_source_files() {
        let Ok(source) = fs::read_to_string(&file) else {
            continue;
        };

        let lexed = lexing::lex(&source);
        let tokens = lexing::layout(&lexed);

        let (_, errors) = parsing::parse(&lexed, &tokens);
        if !errors.is_empty() {
            all_errors.insert(file, errors);
        }
    }

    assert!(all_errors.is_empty(), "{:#?}", all_errors);
}
