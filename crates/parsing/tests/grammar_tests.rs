use std::fmt::Write;

use parsing::{
    grammar::{binder, expression, type_0},
    layout::LayoutKind,
    lexer::lex,
    parser::{Event, Parser},
};
use syntax::SyntaxKind;

fn expect_parse<F, T>(name: &str, source: &str, rule: F)
where
    F: Fn(&mut Parser) -> T,
{
    let lexed = lex(source);
    let input = lexed.as_input();
    let mut parser = Parser::new(input);
    parser.layout_start(LayoutKind::Module);
    let _ = rule(&mut parser);

    let mut result = String::new();
    writeln!(result, "Input: {source}").unwrap();

    let mut indentation = 0;
    for actual in parser.as_output() {
        if let Event::Start { kind: SyntaxKind::Sentinel } = actual {
            continue;
        }
        match actual {
            Event::Start { .. } => {
                writeln!(result, "{:indentation$}{:?}", "", actual, indentation = indentation)
                    .unwrap();
                indentation += 2
            }
            Event::Finish => {
                indentation -= 2;
                writeln!(result, "{:indentation$}{:?}", "", actual, indentation = indentation)
                    .unwrap();
            }
            _ => {
                writeln!(result, "{:indentation$}{:?}", "", actual, indentation = indentation)
                    .unwrap();
            }
        }
    }

    insta::assert_snapshot!(name, result);
}

#[test]
fn test_expression() {
    expect_parse("literal-int", "1", expression);
    expect_parse("operator-chain", "1 + 2 * 3", expression);
    expect_parse("application-expression", "1 2", expression);
    expect_parse("typed-expression", "1 :: Int", expression);
    expect_parse("parenthesized-expression", "(1)", expression);
    expect_parse("parenthesized-application", "(1 2)", expression);
    expect_parse("parenthesized-operator-application", "(1 2) + (3 4)", expression);
    expect_parse("qualified-lower-name", "Hello.as", expression);
    expect_parse("qualified-upper-name", "Hello.As", expression);
    expect_parse("qualified-operator-name", "Hello.(+)", expression);
    expect_parse("qualified-application", "Hello.as Hello.we + Hello.go", expression);
    expect_parse("type-application", "f @a @b 1 2", expression);
    expect_parse("unqualified-operator-name", "(+) 1 2", expression);
    expect_parse("tick-expression", "a `plus` b `plus` c", expression);
    expect_parse("tick-expression-operator-name", "a `(+)` b", expression);
    expect_parse("tick-expression-multiple", "a `plus plus` b", expression);
    expect_parse("negate-expression", "-1", expression);
    expect_parse("multiple-negate-expression", "- -1", expression);
    expect_parse("minus-is-operator", "1 - 2", expression);
    expect_parse("minus-is-operator-name", "(-)", expression);
    expect_parse("if-then-else-expression", "if f x then g + y else h `finally` z", expression);
    expect_parse("block-if-then-else", "f if g then h else i", expression);
    expect_parse("optional-qualified-prefix", "a", expression);

    let source = r"
do
  f x
  g y";
    expect_parse("do-expression", source, expression);

    let source = r"
run do
  f x
  g y";
    expect_parse("block-do-expression", source, expression);

    let source = r"
Hello.do
  f x
  g y";
    expect_parse("qualified-do-expression", source, expression);

    let source = r"
do
  let
    a :: Int
    a = 0

    f x = x
    g (Tuple y z) = y + z

    Tuple a b = Tuple 0 1

  pure hello
    ";
    expect_parse("do-expression-various", source, expression)
}

#[test]
fn test_expression_error() {
    expect_parse("qualified-operator-name-not-operator", "Hello.(as)", expression);
    expect_parse("qualified-operator-name-unfinished", "Hello.(+ 1", expression);
}

#[test]
fn test_type() {
    expect_parse("kinded-type", "Type :: Type", type_0);
    expect_parse("forall-type-plain", "forall a. Type", type_0);
    expect_parse("forall-type-plain-kinded", "forall (a :: Type). Type", type_0);
    expect_parse("forall-type-visible", "forall @a. Type", type_0);
    expect_parse("forall-type-visible-kinded", "forall (@a :: Type). Type", type_0);
    expect_parse("forall-type-mixed", "forall a (b :: C) @d (@e :: F). Type", type_0);
}

#[test]
fn test_binder() {
    expect_parse("typed-binder", "1 :: Type", binder);
    expect_parse("negative-binder", "-1", binder);
    expect_parse("integer-binder", "1", binder);
    expect_parse("number-binder", "1.0", binder);
    expect_parse("string-binder", "\"hi!\"", binder);
    expect_parse("char-binder", "'a'", binder);
    expect_parse("true-binder", "true", binder);
    expect_parse("false-binder", "false", binder);
    expect_parse("wildcard-binder", "_", binder);
    expect_parse("parenthesized-binder-negative", "(-1)", binder);
    expect_parse("parenthesized-binder-literal", "(1.0)", binder);
    expect_parse("parenthesized-binder-nested", "(((1.0)))", binder);
    expect_parse("variable-binder", "f", binder);
    expect_parse("constructor-zero-arity", "Nil", binder);
    expect_parse("qualified-constructor-zero-arity", "Data.List.Nil", binder);
    expect_parse("constructor-n-arity", "Cons 1", binder);
    expect_parse("qualified-constructor-n-arity", "Data.List.Cons 1", binder);
    expect_parse("complex-binder", "Tuple (Cons 0 Nil) (Cons 1 Nil)", binder);
}
