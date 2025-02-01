use rowan::ast::AstNode;
use syntax::cst;

fn lower_declaration<'s>(source: impl AsRef<[&'s str]>) {
    let source = format!("module Main where\n\n{}", source.as_ref().join("\n"));

    let lexed = lexing::lex(&source);
    let tokens = lexing::layout(&lexed);

    let (node, _) = parsing::parse(&lexed, &tokens);
    let module = cst::Module::cast(node).unwrap();
    let (index, _) = indexing::index(&module);

    lowering::lower(&module, &index);
}

#[test]
fn lower_integer() {
    lower_declaration(&[
        "id :: forall a. a -> a",
        "id a = a",
    ]);
}
