use analyzer::{QueryEngine, prim};
use files::Files;

#[test]
fn test_basic_cycle() {
    let mut engine = QueryEngine::default();
    let mut files = Files::default();
    prim::configure(&mut engine, &mut files);

    let id = files.insert(
        "Main.purs",
        r#"
module Main where

type T = 123
t = 123

single a = single a

double1 a = double2 a
double2 a = double1 a

triple1 a = triple2 a
triple2 a = triple3 a
triple3 a = triple1 a

newtype Mu f a = Mu (f (Mu f a))

data Double1 = Double1 Double2
data Double2 = Double2 Double1

newtype Triple1 a = Triple1 Triple2
newtype Triple2 a = Triple2 Triple3
type Triple3 a = Triple1 a
"#,
    );
    let content = files.content(id);

    engine.set_content(id, content);

    let lowered = engine.lowered(id).unwrap();

    let terms = &lowered.term_scc;
    let types = &lowered.type_scc;

    insta::assert_debug_snapshot!((terms, types));
}

#[test]
fn test_operator_cycle() {
    let mut engine = QueryEngine::default();
    let mut files = Files::default();
    prim::configure(&mut engine, &mut files);

    let id = files.insert(
        "Main.purs",
        r#"
module Main where

add a b = a + b

infix 5 add as +

type Add a b = a + b

infix 5 type Add as +
"#,
    );
    let content = files.content(id);

    engine.set_content(id, content);

    let lowered = engine.lowered(id).unwrap();

    let terms = &lowered.term_scc;
    let types = &lowered.type_scc;

    insta::assert_debug_snapshot!((terms, types));
}

#[test]
fn test_non_cycle_ordering() {{
    let mut engine = QueryEngine::default();
    let mut files = Files::default();
    prim::configure(&mut engine, &mut files);

    let id = files.insert(
        "Main.purs",
        r#"
module Main where

a _ = b 0
b _ = c 0
c _ = 0
"#,
    );
    let content = files.content(id);

    engine.set_content(id, content);

    let lowered = engine.lowered(id).unwrap();

    let terms = &lowered.term_scc;
    let types = &lowered.type_scc;

    insta::assert_debug_snapshot!((terms, types));
}
}
