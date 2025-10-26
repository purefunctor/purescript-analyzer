use std::{fmt::Write, num::NonZeroU32};

use analyzer::{QueryEngine, prim};
use checking::{
    check::{
        CheckContext, CheckState, quantify,
        unification::{self, UnificationState},
    },
    core::{Type, TypeId, Variable, debruijn, pretty},
};
use files::{FileId, Files};
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

#[test]
fn test_manual() {
    let (engine, id) = empty_engine();

    engine.set_content(id, "module Main where\n\nforeign import data T :: Proxy 123");

    let resolved = engine.resolved(id).unwrap();
    let checked = engine.checked(id).unwrap();

    let (_, id) = resolved.locals.lookup_type("T").unwrap();
    let id = checked.lookup_type(id).unwrap();

    eprintln!("{}", pretty::print_global(&engine, id));
}
