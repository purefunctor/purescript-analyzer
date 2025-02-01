use indexing::IndexingResult;
use lowering::LoweringResult;
use rowan::ast::AstNode;
use syntax::cst;

fn lower_declaration<'s>(source: impl AsRef<[&'s str]>) -> (IndexingResult, LoweringResult) {
    let source = format!("module Main where\n\n{}", source.as_ref().join("\n"));

    let lexed = lexing::lex(&source);
    let tokens = lexing::layout(&lexed);

    let (node, _) = parsing::parse(&lexed, &tokens);
    let module = cst::Module::cast(node).unwrap();

    let (index, _) = indexing::index(&module);
    let lower = lowering::lower(&module, &index);

    (index, lower)
}

#[test]
fn lower_value_equation() {
    let (index, lower) = lower_declaration(&["id :: forall a. a -> a", "id a = a", "id b = b"]);

    let (id, _, _) = index.nominal.lookup_expr_item("id").unwrap();
    let id = lower.lowering_map.expr_item.get(&id).unwrap();
 
    insta::assert_debug_snapshot!(id);
}

#[test]
fn lower_value_equation_partial() {
    let (index, lower) = lower_declaration(&["id :: forall a. a -> a", "id a = ;", "id b = b"]);

    let (id, _, _) = index.nominal.lookup_expr_item("id").unwrap();
    let id = lower.lowering_map.expr_item.get(&id).unwrap();
 
    insta::assert_debug_snapshot!(id);
}
