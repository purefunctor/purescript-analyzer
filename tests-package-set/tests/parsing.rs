use std::{collections::HashMap, fs, path::PathBuf, time::Instant};

use building::prim;
use files::Files;
use tests_package_set::all_source_files;

type ErrorPerFile<E> = HashMap<PathBuf, E>;

#[test]
fn test_parse_package_set() {
    let mut all_errors = ErrorPerFile::default();

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

    assert!(all_errors.is_empty(), "{all_errors:#?}");
}

#[test]
fn test_index_package_set() {
    let mut all_errors = ErrorPerFile::default();

    for file in all_source_files() {
        let Ok(source) = fs::read_to_string(&file) else {
            continue;
        };

        let lexed = lexing::lex(&source);
        let tokens = lexing::layout(&lexed);

        let (parsed, _) = parsing::parse(&lexed, &tokens);
        let indexed = indexing::index_module(&parsed.cst());

        if !indexed.errors.is_empty() {
            all_errors.insert(file, indexed.errors);
        }
    }

    assert!(all_errors.is_empty(), "{all_errors:#?}");
}

#[test]
fn test_parallel_parse_package_set() {
    use building::QueryEngine;
    use rayon::prelude::*;

    let mut engine = QueryEngine::default();
    let mut files = Files::default();
    prim::configure(&mut engine, &mut files);

    let mut source = vec![];
    for path in all_source_files() {
        let content = std::fs::read_to_string(&path).unwrap();
        let url = url::Url::from_file_path(path).unwrap();
        let uri = url.to_string();

        let id = files.insert(uri, content);
        let content = files.content(id);

        engine.set_content(id, content);
        source.push(id);
    }

    let start = Instant::now();
    source.par_iter().for_each(|&id| {
        let engine = engine.snapshot();
        let parsed = engine.parsed(id);
        assert!(parsed.is_ok());
    });
    let parsing = start.elapsed();
    println!("Parsing {parsing:?}");

    let names = source.iter().filter_map(|&id| {
        let (parsed, _) = engine.parsed(id).ok()?;
        let module_name = parsed.module_name()?;
        Some((module_name, id))
    });

    for (name, file) in names {
        engine.set_module_file(&name, file);
    }

    let start = Instant::now();
    source.par_iter().for_each(|&id| {
        let engine = engine.snapshot();
        let indexed = engine.indexed(id);
        assert!(indexed.is_ok());
    });
    let indexing = start.elapsed();
    println!("Indexing {indexing:?}");

    let start = Instant::now();
    source.par_iter().for_each(|&id| {
        let engine = engine.snapshot();
        let resolved = engine.resolved(id);
        assert!(resolved.is_ok());
    });
    let resolving = start.elapsed();
    println!("Resolving {resolving:?}");

    let start = Instant::now();
    source.par_iter().for_each(|&id| {
        let engine = engine.snapshot();
        let lowered = engine.lowered(id);
        assert!(lowered.is_ok());
    });
    let lowering = start.elapsed();
    println!("Lowering {lowering:?}");

    println!("Total {:?}", parsing + indexing + resolving + lowering);
}
