use std::fmt::Write;
use std::num::NonZeroU32;

use analyzer::{QueryEngine, prim};
use checking::algorithm::state::{CheckContext, CheckState, UnificationState};
use checking::algorithm::{quantify, unification};
use checking::core::{ForallBinder, Type, TypeId, Variable, debruijn, pretty};
use files::{FileId, Files};
use indexing::{TermItem, TypeItem};
use lowering::TypeVariableBindingId;

struct ContextState<'r> {
    context: CheckContext<'r, QueryEngine>,
    state: CheckState,
}

impl<'a> ContextState<'a> {
    fn new(engine: &'a QueryEngine, id: FileId) -> ContextState<'a> {
        let mut state = CheckState::default();
        let context = CheckContext::new(engine, &mut state, id).unwrap();
        ContextState { state, context }
    }
}

trait CheckStateExt {
    fn bound_variable(&mut self, index: u32) -> TypeId;

    fn function(&mut self, argument: TypeId, result: TypeId) -> TypeId;
}

impl CheckStateExt for CheckState {
    fn bound_variable(&mut self, index: u32) -> TypeId {
        let var = Variable::Bound(debruijn::Index(index));
        self.storage.intern(Type::Variable(var))
    }

    fn function(&mut self, argument: TypeId, result: TypeId) -> TypeId {
        self.storage.intern(Type::Function(argument, result))
    }
}

fn empty_engine() -> (QueryEngine, FileId) {
    let mut engine = QueryEngine::default();
    let mut files = Files::default();
    prim::configure(&mut engine, &mut files);

    let id = files.insert("Main.purs", "module Main where\n\n");
    let content = files.content(id);
    engine.set_content(id, content);

    (engine, id)
}

fn print_terms_types(engine: QueryEngine, id: FileId) -> String {
    let indexed = engine.indexed(id).unwrap();
    let checked = engine.checked(id).unwrap();

    let mut snapshot = String::default();

    writeln!(snapshot, "Terms").unwrap();
    for (id, TermItem { name, .. }) in indexed.items.iter_terms() {
        let Some(n) = name else { continue };
        let Some(t) = checked.lookup_term(id) else { continue };
        let t = pretty::print_global(&engine, t);
        writeln!(snapshot, "{n} :: {t}").unwrap();
    }

    writeln!(snapshot, "\nTypes").unwrap();
    for (id, TypeItem { name, .. }) in indexed.items.iter_types() {
        let Some(n) = name else { continue };
        let Some(t) = checked.lookup_type(id) else { continue };
        let t = pretty::print_global(&engine, t);
        writeln!(snapshot, "{n} :: {t}").unwrap();
    }

    snapshot
}

const FAKE_NONZERO_1: NonZeroU32 = NonZeroU32::new(1).unwrap();
const FAKE_NONZERO_2: NonZeroU32 = NonZeroU32::new(2).unwrap();

#[test]
fn test_solve_simple() {
    let (engine, id) = empty_engine();
    let ContextState { ref context, ref mut state } = ContextState::new(&engine, id);

    // [a :: Int, b :: String]
    state.bind_forall(TypeVariableBindingId::new(FAKE_NONZERO_1), context.prim.int);
    state.bind_forall(TypeVariableBindingId::new(FAKE_NONZERO_2), context.prim.string);

    let unification = state.fresh_unification_type(context);
    let Type::Unification(unification_id) = state.storage[unification] else {
        unreachable!("invariant violated");
    };

    unification::solve(state, context, unification_id, context.prim.symbol).unwrap();

    let entry = *state.unification.get(unification_id);
    let UnificationState::Solved(solution) = entry.state else {
        unreachable!("invariant violated");
    };

    let solution = pretty::print_local(state, context, solution);
    let kind = pretty::print_local(state, context, entry.kind);

    let snapshot = format!("{solution} :: {kind}");
    insta::assert_snapshot!(snapshot);
}

#[test]
fn test_solve_bound() {
    let (engine, id) = empty_engine();
    let ContextState { ref context, ref mut state } = ContextState::new(&engine, id);

    // [a :: Int, b :: String]
    state.bind_forall(TypeVariableBindingId::new(FAKE_NONZERO_1), context.prim.int);
    state.bind_forall(TypeVariableBindingId::new(FAKE_NONZERO_2), context.prim.string);

    let unification = state.fresh_unification_type(context);
    let Type::Unification(unification_id) = state.storage[unification] else {
        unreachable!("invariant violated");
    };

    let bound_b = state.bound_variable(0);
    let bound_a = state.bound_variable(1);
    let b_to_a = state.function(bound_b, bound_a);

    unification::solve(state, context, unification_id, b_to_a).unwrap();

    let entry = *state.unification.get(unification_id);
    let UnificationState::Solved(solution) = entry.state else {
        unreachable!("invariant violated");
    };

    let solution = pretty::print_local(state, context, solution);
    let kind = pretty::print_local(state, context, entry.kind);

    let snapshot = format!("{solution} :: {kind}");
    insta::assert_snapshot!(snapshot);
}

#[test]
fn test_solve_invalid() {
    let (engine, id) = empty_engine();
    let ContextState { ref context, ref mut state } = ContextState::new(&engine, id);

    // [a :: Int]
    state.bind_forall(TypeVariableBindingId::new(FAKE_NONZERO_1), context.prim.int);

    let unification = state.fresh_unification_type(context);
    let Type::Unification(unification_id) = state.storage[unification] else {
        unreachable!("invariant violated");
    };

    // [a :: Int, b :: String]
    let level = state.bind_forall(TypeVariableBindingId::new(FAKE_NONZERO_2), context.prim.string);

    let bound_b = state.bound_variable(0);
    let bound_a = state.bound_variable(1);
    let b_to_a = state.function(bound_b, bound_a);

    state.unbind(level);

    let solve_result = unification::solve(state, context, unification_id, b_to_a);
    assert!(solve_result.is_none());
}

#[test]
fn test_solve_promotion() {
    let (engine, id) = empty_engine();
    let ContextState { ref context, ref mut state } = ContextState::new(&engine, id);

    // [a :: Int]
    state.bind_forall(TypeVariableBindingId::new(FAKE_NONZERO_1), context.prim.int);

    let unification_a = state.fresh_unification_type(context);
    let Type::Unification(unification_id) = state.storage[unification_a] else {
        unreachable!("invariant violated");
    };

    // [a :: Int, b :: String]
    state.bind_forall(TypeVariableBindingId::new(FAKE_NONZERO_2), context.prim.string);

    let unification_a_b = state.fresh_unification_type(context);
    unification::solve(state, context, unification_id, unification_a_b).unwrap();

    let mut snapshot = String::default();

    let entries: Vec<_> = state.unification.iter().copied().collect();
    for (index, entry) in entries.iter().enumerate() {
        let UnificationState::Solved(solution) = entry.state else { continue };
        let domain = entry.domain;
        let solution = pretty::print_local(state, context, solution);
        writeln!(snapshot, "?{index}[{domain}] := {solution}").unwrap();
    }

    insta::assert_snapshot!(snapshot);
}

#[test]
fn test_quantify_simple() {
    let (engine, id) = empty_engine();
    let ContextState { ref context, ref mut state } = ContextState::new(&engine, id);

    let unification_a = state.fresh_unification_type(context);
    let unification_b = state.fresh_unification_type(context);

    let function = state.storage.intern(Type::Function(unification_a, unification_b));
    let quantified = quantify::quantify(state, function).unwrap();

    let mut snapshot = String::default();

    let quantified = pretty::print_local(state, context, quantified);
    writeln!(snapshot, "{quantified}").unwrap();

    insta::assert_snapshot!(snapshot)
}

#[test]
fn test_quantify_polykind() {
    let (engine, id) = empty_engine();
    let ContextState { ref context, ref mut state } = ContextState::new(&engine, id);

    let unification = state.fresh_unification(context);
    let quantified = quantify::quantify(state, unification).unwrap();

    let mut snapshot = String::default();

    let quantified = pretty::print_local(state, context, quantified);
    writeln!(snapshot, "{quantified}").unwrap();

    insta::assert_snapshot!(snapshot)
}

#[test]
fn test_quantify_ordering() {
    let (engine, id) = empty_engine();
    let ContextState { ref context, ref mut state } = ContextState::new(&engine, id);

    state.bind_forall(TypeVariableBindingId::new(FAKE_NONZERO_1), context.prim.t);
    let unification_a = state.fresh_unification_type(context);

    state.bind_forall(TypeVariableBindingId::new(FAKE_NONZERO_2), context.prim.t);
    let unification_b = state.fresh_unification_type(context);

    let function = state.storage.intern(Type::Function(unification_b, unification_a));
    let quantified = quantify::quantify(state, function).unwrap();

    let mut snapshot = String::default();

    let quantified = pretty::print_local(state, context, quantified);
    writeln!(snapshot, "{quantified}").unwrap();

    insta::assert_snapshot!(snapshot)
}

#[test]
fn test_quantify_scoped() {
    let (engine, id) = empty_engine();
    let ContextState { ref context, ref mut state } = ContextState::new(&engine, id);

    let unification_0 = state.fresh_unification_type(context);
    let unification_1 = state.fresh_unification_kinded(unification_0);
    let unification_2 = state.fresh_unification_kinded(unification_1);

    let quantified = quantify::quantify(state, unification_2).unwrap();

    let mut snapshot = String::default();

    let quantified = pretty::print_local(state, context, quantified);
    writeln!(snapshot, "{quantified}").unwrap();

    insta::assert_snapshot!(snapshot)
}

#[test]
fn test_quantify_multiple_scoped() {
    let (engine, id) = empty_engine();
    let ContextState { ref context, ref mut state } = ContextState::new(&engine, id);

    let unification_0 = state.fresh_unification_type(context);
    let unification_1 = state.fresh_unification_kinded(unification_0);
    let unification_2 = state.fresh_unification_kinded(unification_1);

    let unification_3 = state.fresh_unification_type(context);
    let unification_4 = state.fresh_unification_kinded(unification_3);
    let unification_5 = state.fresh_unification_kinded(unification_4);

    let function = state.storage.intern(Type::Function(unification_2, unification_5));
    let quantified = quantify::quantify(state, function).unwrap();

    let mut snapshot = String::default();

    let quantified = pretty::print_local(state, context, quantified);
    writeln!(snapshot, "{quantified}").unwrap();

    insta::assert_snapshot!(snapshot)
}

fn make_forall_a_to_a(context: &CheckContext<QueryEngine>, state: &mut CheckState) -> TypeId {
    let fake_id = TypeVariableBindingId::new(FAKE_NONZERO_1);

    let level = state.bind_forall(fake_id, context.prim.t);

    let bound_a = state.bound_variable(0);
    let a_to_a = state.function(bound_a, bound_a);

    let binder = ForallBinder { visible: false, name: "a".into(), level, kind: context.prim.t };
    let forall_a_to_a = state.storage.intern(Type::Forall(binder, a_to_a));

    state.unbind(level);

    forall_a_to_a
}

#[test]
fn test_subsumes_forall_left_pass() {
    let (engine, id) = empty_engine();
    let ContextState { ref context, ref mut state } = ContextState::new(&engine, id);

    // Given ∀a. (a -> a)
    let forall_a_to_a = make_forall_a_to_a(context, state);

    // ∀a. (a -> a) should subsume (Int -> Int)
    let int_to_int = state.function(context.prim.int, context.prim.int);
    let result = unification::subsumes(state, context, forall_a_to_a, int_to_int);
    assert!(result, "∀a. (a -> a) should subsume (Int -> Int)");
}

#[test]
fn test_subsumes_forall_left_fail() {
    let (engine, id) = empty_engine();
    let ContextState { ref context, ref mut state } = ContextState::new(&engine, id);

    // Given ∀a. (a -> a)
    let forall_a_to_a = make_forall_a_to_a(context, state);

    // ∀a. (a -> a) should NOT subsume (Int -> String)
    let int_to_string = state.function(context.prim.int, context.prim.string);
    let result = unification::subsumes(state, context, forall_a_to_a, int_to_string);
    assert!(!result, "∀a. (a -> a) should not subsume (Int -> String)");
}

#[test]
fn test_subsumes_forall_right_fail() {
    let (engine, id) = empty_engine();
    let ContextState { ref context, ref mut state } = ContextState::new(&engine, id);

    // Create ∀a. a -> a
    let forall_a_to_a = make_forall_a_to_a(context, state);

    // (Int -> Int) should NOT subsume ∀a. (a -> a)
    let int_to_int = state.function(context.prim.int, context.prim.int);
    let result = unification::subsumes(state, context, int_to_int, forall_a_to_a);
    assert!(!result, "(Int -> Int) should not subsume ∀a. (a -> a)");
}

#[test]
fn test_subsumes_nested_forall() {
    let (engine, id) = empty_engine();
    let ContextState { ref context, ref mut state } = ContextState::new(&engine, id);

    // Create ∀a. ∀b. (a -> b -> a)
    let level_a = state.bind_forall(TypeVariableBindingId::new(FAKE_NONZERO_1), context.prim.t);
    let level_b = state.bind_forall(TypeVariableBindingId::new(FAKE_NONZERO_2), context.prim.t);

    let bound_a = state.bound_variable(1);
    let bound_b = state.bound_variable(0);
    let b_to_a = state.function(bound_b, bound_a);
    let a_to_b_to_a = state.function(bound_a, b_to_a);

    let forall_b = state.storage.intern(Type::Forall(
        ForallBinder { visible: false, name: "b".into(), level: level_b, kind: context.prim.t },
        a_to_b_to_a,
    ));
    state.unbind(level_b);

    let forall_a_b = state.storage.intern(Type::Forall(
        ForallBinder { visible: false, name: "a".into(), level: level_a, kind: context.prim.t },
        forall_b,
    ));
    state.unbind(level_a);

    // ∀a. ∀b. (a -> b -> a) should subsume (Int -> String -> Int)
    let string_to_int = state.function(context.prim.string, context.prim.int);
    let int_to_string_to_int = state.function(context.prim.int, string_to_int);

    let result = unification::subsumes(state, context, forall_a_b, int_to_string_to_int);
    assert!(result, "∀a. ∀b. (a -> b -> a) should subsume (Int -> String -> Int)");
}

#[test]
fn test_manual() {
    let (engine, id) = empty_engine();
    engine.set_content(
        id,
        r#"
module Main where

data Proxy :: forall k. k -> Type
data Proxy a = Proxy
"#,
    );

    let indexed = engine.indexed(id).unwrap();
    let checked = engine.checked(id).unwrap();

    for (id, TermItem { name, .. }) in indexed.items.iter_terms() {
        let Some(n) = name else { continue };
        let Some(t) = checked.lookup_term(id) else { continue };
        let t = pretty::print_global(&engine, t);
        eprintln!("{n} :: {t}")
    }

    for (id, TypeItem { name, .. }) in indexed.items.iter_types() {
        let Some(n) = name else { continue };
        let Some(t) = checked.lookup_type(id) else { continue };
        let t = pretty::print_global(&engine, t);
        eprintln!("{n} :: {t}")
    }
}

#[test]
fn test_manual_2() {
    let (engine, id) = empty_engine();
    engine.set_content(
        id,
        r#"
module Main where

data Maybe :: Type -> Type
data Maybe (a :: Type) = Just a | Nothing
"#,
    );

    let indexed = engine.indexed(id).unwrap();
    let checked = engine.checked(id).unwrap();

    for (id, TermItem { name, .. }) in indexed.items.iter_terms() {
        let Some(n) = name else { continue };
        let Some(t) = checked.lookup_term(id) else { continue };
        let t = pretty::print_global(&engine, t);
        eprintln!("{n} :: {t}")
    }

    for (id, TypeItem { name, .. }) in indexed.items.iter_types() {
        let Some(n) = name else { continue };
        let Some(t) = checked.lookup_type(id) else { continue };
        let t = pretty::print_global(&engine, t);
        eprintln!("{n} :: {t}")
    }
}

#[test]
fn test_manual_3() {
    let (engine, id) = empty_engine();
    engine.set_content(
        id,
        r#"
module Main where

data Proxy a = Proxy
"#,
    );

    let indexed = engine.indexed(id).unwrap();
    let checked = engine.checked(id).unwrap();

    for (id, TermItem { name, .. }) in indexed.items.iter_terms() {
        let Some(n) = name else { continue };
        let Some(t) = checked.lookup_term(id) else { continue };
        let t = pretty::print_global(&engine, t);
        eprintln!("{n} :: {t}")
    }

    for (id, TypeItem { name, .. }) in indexed.items.iter_types() {
        let Some(n) = name else { continue };
        let Some(t) = checked.lookup_type(id) else { continue };
        let t = pretty::print_global(&engine, t);
        eprintln!("{n} :: {t}")
    }
}

#[test]
fn test_data_recursive() {
    let (engine, id) = empty_engine();
    engine.set_content(
        id,
        r#"
module Main where

data List a = Nil | Cons a (List a)
"#,
    );

    let snapshot = print_terms_types(engine, id);
    insta::assert_snapshot!(snapshot);
}

#[test]
fn test_data_mutual() {
    let (engine, id) = empty_engine();
    engine.set_content(
        id,
        r#"
module Main where

data Tree a = Tree a (Forest a)
data Forest a = Nil | Cons (Tree a) (Forest a)
"#,
    );

    let snapshot = print_terms_types(engine, id);
    insta::assert_snapshot!(snapshot);
}

#[test]
fn test_newtype_recursive() {
    let (engine, id) = empty_engine();
    engine.set_content(
        id,
        r#"
module Main where

newtype Mu f = In (f (Mu f))
"#,
    );

    let snapshot = print_terms_types(engine, id);
    insta::assert_snapshot!(snapshot);
}

#[test]
fn test_data_arity_fail() {
    {
        let (engine, id) = empty_engine();
        engine.set_content(
            id,
            r#"
module Main where

data Maybe :: Type
data Maybe a = Just a | Nothing
"#,
        );

        let checked = engine.checked(id).unwrap();
        insta::assert_debug_snapshot!(checked.errors);
    }
}

#[test]
fn test_unification_fail() {
    {
        let (engine, id) = empty_engine();
        engine.set_content(
            id,
            r#"
module Main where

data Maybe (a :: Int) = Just a | Nothing
"#,
        );

        let checked = engine.checked(id).unwrap();
        insta::assert_debug_snapshot!(checked.errors);
    }
}
