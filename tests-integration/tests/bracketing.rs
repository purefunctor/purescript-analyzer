use analyzer::{QueryEngine, prim};
use files::Files;

#[test]
fn test_rebracketing() {
    let mut engine = QueryEngine::default();
    let mut files = Files::default();
    prim::configure(&mut engine, &mut files);

    let id = files.insert(
        "Main.purs",
        r#"
module Main where

eq a a = false
add a a = 0
mult a a = 0
apply f a = f a
map f a = f a

infix 4 eq as ==
infixl 5 add as +
infixl 6 mult as *
infixr 0 apply as $
infixl 4 map as <$>

test = 1 + 2 * 3 + 4
"#,
    );
    let content = files.content(id);

    engine.set_content(id, content);

    let lowered = engine.lowered(id).unwrap();
    let result = sugar::bracketed(&engine, &lowered);

    println!("{result:#?}");
}
