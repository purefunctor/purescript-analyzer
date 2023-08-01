use std::fmt::Write;

use parsing::{
    grammar::{expression, type_0},
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
    expect_parse("qualified-do-expression", "Hello.do", expression);
    expect_parse("qualified-ado-expression", "Hello.ado", expression);
    expect_parse("block-qualified-do-expression", "run Hello.do", expression);
    expect_parse("block qualified-ado-expression", "run Hello.ado", expression);
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
