use indexing::{index_module, Index, IndexError, Relational, IndexingSource};
use rowan::ast::AstNode;
use std::fmt::Write;
use syntax::cst;
use test_each_file::test_each_file;

fn index_source(source: &str) -> (cst::Module, Index, Relational, IndexingSource, Vec<IndexError>) {
    let lexed = lexing::lex(source);
    let tokens = lexing::layout(&lexed);

    let (module, _) = parsing::parse(&lexed, &tokens);
    let module = cst::Module::cast(module).unwrap();

    let (index, relational, source, errors) = index_module(&module);
    (module, index, relational, source, errors)
}

test_each_file! { in "./crates/indexing/tests/indexing" => |source: &str| {
    let (_, index, relational, source, errors) = index_source(source);

    let mut snapshot = String::new();
    writeln!(snapshot, "{:#?}", index).unwrap();
    writeln!(snapshot, "{:#?}", relational).unwrap();
    writeln!(snapshot, "{:#?}", source).unwrap();
    writeln!(snapshot, "{:#?}", errors).unwrap();
    insta::assert_snapshot!(snapshot);
}}
