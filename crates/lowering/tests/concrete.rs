use indexing::IndexingResult;
use lowering::LoweringResult;
use rowan::ast::AstNode;
use syntax::cst;
use test_each_file::test_each_file;

fn lower_source(source: &str) -> (IndexingResult, LoweringResult) {
    let lexed = lexing::lex(source);
    let tokens = lexing::layout(&lexed);

    let (node, _) = parsing::parse(&lexed, &tokens);
    let module = cst::Module::cast(node).unwrap();

    let (index, _) = indexing::index(&module);
    let lower = lowering::lower(&module, &index);

    (index, lower)
}

test_each_file! { in "./crates/lowering/tests/concrete" => |source: &str| {
    let (_, lower) = lower_source(source);
    match source.lines().next() {
        Some(annotation) if annotation.starts_with("-- @lowering_map") => {
            insta::assert_debug_snapshot!(&lower.lowering_map)
        }
        _ => {
            insta::assert_debug_snapshot!(&lower.source_map);
        }
    }
}}
