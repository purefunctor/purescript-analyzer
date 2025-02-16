use lowering::{Intermediate, LoweringSource};
use rowan::ast::AstNode;
use syntax::cst;

fn index_source(source: &str) -> (cst::Module, Intermediate, LoweringSource) {
    let lexed = lexing::lex(source);
    let tokens = lexing::layout(&lexed);

    let (module, _) = parsing::parse(&lexed, &tokens);
    let module = cst::Module::cast(module).unwrap();

    let (index, relational, source, _) = indexing::index_module(&module);
    let (ir, source) = lowering::lower_module(&module, &index, &relational, &source);

    (module, ir, source)
}

#[test]
fn wtf() {
    let (_, ir, source) = index_source(
        r#"
module Main where

const :: forall a. forall b. a -> b -> a
const a _ = a
"#,
    );

    dbg!((ir, source));
}
