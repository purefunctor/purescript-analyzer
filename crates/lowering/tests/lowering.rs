use indexing::FullModuleIndex;
use lowering::{Graph, Intermediate, LoweringSource};
use rowan::ast::AstNode;
use syntax::cst;

fn index_source(source: &str) -> (cst::Module, Intermediate, LoweringSource, Graph) {
    let lexed = lexing::lex(source);
    let tokens = lexing::layout(&lexed);

    let (module, _) = parsing::parse(&lexed, &tokens);
    let module = cst::Module::cast(module).unwrap();

    let FullModuleIndex { index, relational, source, .. } = indexing::index_module(&module);
    let (ir, source, graph) = lowering::lower_module(&module, &index, &relational, &source);

    (module, ir, source, graph)
}

#[test]
fn wtf() {
    let (_, ir, source, graph) = index_source(
        r#"
module Main where

foreign import unit :: Unit
"#,
    );
    dbg!((ir, source, graph));
}
