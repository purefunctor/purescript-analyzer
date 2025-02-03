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
    let (index, lower) = lower_declaration(["id :: forall a. a -> a", "id a = a", "id b = b"]);

    let (id, _, _) = index.nominal.lookup_expr_item("id").unwrap();
    let id = lower.lowering_map.expr_item.get(&id).unwrap();

    insta::assert_debug_snapshot!(id);
}

#[test]
fn lower_value_equation_partial() {
    let (index, lower) = lower_declaration(["id :: forall a. a -> a", "id a = ;", "id b = b"]);

    let (id, _, _) = index.nominal.lookup_expr_item("id").unwrap();
    let id = lower.lowering_map.expr_item.get(&id).unwrap();

    insta::assert_debug_snapshot!(id);
}

#[test]
fn lower_expression_typed() {
    let (_, lower) = lower_declaration(["four = 4 :: Int", "five = 5 ::"]);
    insta::assert_debug_snapshot!(&lower.source_map);
}

#[test]
fn lower_expression_operator_chain() {
    let (_, lower) = lower_declaration(["plus = 1 + 2 + 3 + 4 + 5"]);
    insta::assert_debug_snapshot!(&lower.source_map);
}

#[test]
fn lower_expression_infix_chain() {
    let (_, lower) = lower_declaration(["chain = a `for` b `for` c"]);
    insta::assert_debug_snapshot!(&lower.source_map);
}

#[test]
fn lower_expression_negate() {
    let (_, lower) = lower_declaration(["negate = -1"]);
    insta::assert_debug_snapshot!(&lower.source_map);
}

#[test]
fn lower_expression_application_chain() {
    let (_, lower) = lower_declaration(["negate = f @a @b a b"]);
    insta::assert_debug_snapshot!(&lower.source_map);
}

#[test]
fn lower_expression_if_then_else() {
    let (_, lower) = lower_declaration(["ifThenElse = if a then b else c"]);
    insta::assert_debug_snapshot!(&lower.source_map);
}

#[test]
fn lower_expression_let_in() {
    let (_, lower) = lower_declaration([
        "letIn =",
        "  let",
        "    life :: Int",
        "    life = 42",
        "",
        "    42 = value",
        "      where",
        "      value = 42",
        "",
        "  in",
        "    life",
    ]);
    insta::assert_debug_snapshot!(&lower.source_map);
}

#[test]
fn lower_expression_lambda() {
    let (_, lower) = lower_declaration(["id = \\a -> a"]);
    insta::assert_debug_snapshot!(&lower.source_map);
}

#[test]
fn lower_expression_conditionals() {
    let (_, lower) = lower_declaration([
        "id a",
        "  | b <- c",
        "  , d <- e",
        "  , f <- g = 0",
        "  | true = 1",
        "  | false = 1",
    ]);
    insta::assert_debug_snapshot!(&lower.lowering_map.expr_item);
}

#[test]
fn lower_expression_case_of() {
    let (_, lower) = lower_declaration([
        "id = case _ of",
        "  a | b <- c",
        "    , d <- e",
        "    , f <- g -> 0",
        "    | true -> 1",
        "    | false -> 1",
    ]);
    insta::assert_debug_snapshot!(&lower.source_map);
}

#[test]
fn lower_expression_do() {
    let (_, lower) = lower_declaration([
        "main = Hooks.do",
        "  pure unit",
        "  _ <- pure unit",
        "  let",
        "    a = pure b",
        "  pure unit",
    ]);
    insta::assert_debug_snapshot!(&lower.source_map);
}

#[test]
fn lower_expression_ado() {
    let (_, lower) = lower_declaration([
        "main = Hooks.ado",
        "  _ <- action",
        "  let",
        "    id a = a",
        "  _ <- action",
        "  in result",
    ]);
    insta::assert_debug_snapshot!(&lower.source_map);
}

#[test]
fn lower_expression_name() {
    let (_, lower) = lower_declaration(["id = F.Id F.id F.(+)"]);
    insta::assert_debug_snapshot!(&lower.source_map);
}
