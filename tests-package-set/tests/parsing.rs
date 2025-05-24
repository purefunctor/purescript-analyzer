use std::{collections::HashMap, fs, path::PathBuf, sync::Arc, time::Instant};

use files::Files;
use parsing::ParseError;
use tests_package_set::all_source_files;

type ParseErrorPerFile = HashMap<PathBuf, Arc<[ParseError]>>;

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

#[test]
fn test_parallel_parse_package_set() {
    use building::parallel_runtime::*;
    use rayon::prelude::*;

    let mut files = Files::default();
    let runtime = SequentialRuntime::default();

    let mut source = vec![];
    for path in all_source_files() {
        let content = std::fs::read_to_string(&path).unwrap();
        let path = format!("file://{}", path.to_str().unwrap());

        let id = files.insert(path, content);
        let content = files.content(id);

        runtime.set_content(id, content);
        source.push(id);
    }

    let start = Instant::now();
    runtime.upgraded(|runtime| {
        source.par_iter().for_each(|&id| {
            runtime.parsed(id);
        });
    });
    println!("Parsing {:?}", start.elapsed());

    let start = Instant::now();
    runtime.upgraded(|runtime| {
        source.par_iter().for_each(|&id| {
            runtime.indexed(id);
        });
    });
    println!("Indexing {:?}", start.elapsed());

    let start = Instant::now();
    runtime.upgraded(|runtime| {
        source.par_iter().for_each(|&id| {
            runtime.resolved(id);
        });
    });
    println!("Resolving {:?}", start.elapsed());

    let start = Instant::now();
    runtime.upgraded(|runtime| {
        source.par_iter().for_each(|&id| {
            rayon::join(|| runtime.lowered(id), || runtime.lowered(id));
        });
    });
    println!("Lowering {:?}", start.elapsed());
}
