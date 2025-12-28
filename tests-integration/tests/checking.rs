#[path = "checking/generated.rs"]
mod generated;

use std::fmt::Write;
use std::num::NonZeroU32;

use analyzer::{QueryEngine, prim};
use checking::algorithm::state::{CheckContext, CheckState, UnificationState};
use checking::algorithm::{quantify, unification};
use checking::core::{ForallBinder, RowField, RowType, Type, TypeId, Variable, debruijn, pretty};
use files::{FileId, Files};
use itertools::Itertools;
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
        let var = Variable::Bound(debruijn::Level(index));
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
    state.type_scope.bind_forall(TypeVariableBindingId::new(FAKE_NONZERO_1), context.prim.int);
    state.type_scope.bind_forall(TypeVariableBindingId::new(FAKE_NONZERO_2), context.prim.string);

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
    state.type_scope.bind_forall(TypeVariableBindingId::new(FAKE_NONZERO_1), context.prim.int);
    state.type_scope.bind_forall(TypeVariableBindingId::new(FAKE_NONZERO_2), context.prim.string);

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
    state.type_scope.bind_forall(TypeVariableBindingId::new(FAKE_NONZERO_1), context.prim.int);

    let unification = state.fresh_unification_type(context);
    let Type::Unification(unification_id) = state.storage[unification] else {
        unreachable!("invariant violated");
    };

    // [a :: Int, b :: String]
    let level = state.type_scope.bind_forall(TypeVariableBindingId::new(FAKE_NONZERO_2), context.prim.string);

    let bound_b = state.bound_variable(0);
    let bound_a = state.bound_variable(1);
    let b_to_a = state.function(bound_b, bound_a);

    state.type_scope.unbind(level);

    let solve_result = unification::solve(state, context, unification_id, b_to_a).unwrap();
    assert!(solve_result.is_none());
}

#[test]
fn test_solve_promotion() {
    let (engine, id) = empty_engine();
    let ContextState { ref context, ref mut state } = ContextState::new(&engine, id);

    // [a :: Int]
    state.type_scope.bind_forall(TypeVariableBindingId::new(FAKE_NONZERO_1), context.prim.int);

    let unification_a = state.fresh_unification_type(context);
    let Type::Unification(unification_id) = state.storage[unification_a] else {
        unreachable!("invariant violated");
    };

    // [a :: Int, b :: String]
    state.type_scope.bind_forall(TypeVariableBindingId::new(FAKE_NONZERO_2), context.prim.string);

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
    let (quantified, _) = quantify::quantify(state, function).unwrap();

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
    let (quantified, _) = quantify::quantify(state, unification).unwrap();

    let mut snapshot = String::default();

    let quantified = pretty::print_local(state, context, quantified);
    writeln!(snapshot, "{quantified}").unwrap();

    insta::assert_snapshot!(snapshot)
}

#[test]
fn test_quantify_ordering() {
    let (engine, id) = empty_engine();
    let ContextState { ref context, ref mut state } = ContextState::new(&engine, id);

    state.type_scope.bind_forall(TypeVariableBindingId::new(FAKE_NONZERO_1), context.prim.t);
    let unification_a = state.fresh_unification_type(context);

    state.type_scope.bind_forall(TypeVariableBindingId::new(FAKE_NONZERO_2), context.prim.t);
    let unification_b = state.fresh_unification_type(context);

    let function = state.storage.intern(Type::Function(unification_b, unification_a));
    let (quantified, _) = quantify::quantify(state, function).unwrap();

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

    let (quantified, _) = quantify::quantify(state, unification_2).unwrap();

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
    let (quantified, _) = quantify::quantify(state, function).unwrap();

    let mut snapshot = String::default();

    let quantified = pretty::print_local(state, context, quantified);
    writeln!(snapshot, "{quantified}").unwrap();

    insta::assert_snapshot!(snapshot)
}

fn make_forall_a_to_a(context: &CheckContext<QueryEngine>, state: &mut CheckState) -> TypeId {
    let fake_id = TypeVariableBindingId::new(FAKE_NONZERO_1);

    let level = state.type_scope.bind_forall(fake_id, context.prim.t);

    let bound_a = state.bound_variable(0);
    let a_to_a = state.function(bound_a, bound_a);

    let binder = ForallBinder { visible: false, name: "a".into(), level, kind: context.prim.t };
    let forall_a_to_a = state.storage.intern(Type::Forall(binder, a_to_a));

    state.type_scope.unbind(level);

    forall_a_to_a
}

#[test]
fn test_subtype_poly_of_mono_pass() {
    let (engine, id) = empty_engine();
    let ContextState { ref context, ref mut state } = ContextState::new(&engine, id);

    // Given ∀a. (a -> a)
    let forall_a_to_a = make_forall_a_to_a(context, state);

    // ∀a. (a -> a) <: (Int -> Int) should pass (LHS forall gets instantiated)
    let int_to_int = state.function(context.prim.int, context.prim.int);
    let result = unification::subtype(state, context, forall_a_to_a, int_to_int).unwrap();
    assert!(result, "∀a. (a -> a) <: (Int -> Int) should pass");
}

#[test]
fn test_subtype_poly_of_mono_fail() {
    let (engine, id) = empty_engine();
    let ContextState { ref context, ref mut state } = ContextState::new(&engine, id);

    // Given ∀a. (a -> a)
    let forall_a_to_a = make_forall_a_to_a(context, state);

    // ∀a. (a -> a) <: (Int -> String) should fail
    let int_to_string = state.function(context.prim.int, context.prim.string);
    let result = unification::subtype(state, context, forall_a_to_a, int_to_string).unwrap();
    assert!(!result, "∀a. (a -> a) <: (Int -> String) should fail");
}

#[test]
fn test_subtype_mono_of_poly_fail() {
    let (engine, id) = empty_engine();
    let ContextState { ref context, ref mut state } = ContextState::new(&engine, id);

    // Create ∀a. a -> a
    let forall_a_to_a = make_forall_a_to_a(context, state);

    // (Int -> Int) <: ∀a. (a -> a) should fail (RHS forall gets skolemized)
    let int_to_int = state.function(context.prim.int, context.prim.int);
    let result = unification::subtype(state, context, int_to_int, forall_a_to_a).unwrap();
    assert!(!result, "(Int -> Int) <: ∀a. (a -> a) should fail");
}

#[test]
fn test_subtype_nested_forall() {
    let (engine, id) = empty_engine();
    let ContextState { ref context, ref mut state } = ContextState::new(&engine, id);

    // Create ∀a. ∀b. (a -> b -> a)
    let level_a = state.type_scope.bind_forall(TypeVariableBindingId::new(FAKE_NONZERO_1), context.prim.t);
    let level_b = state.type_scope.bind_forall(TypeVariableBindingId::new(FAKE_NONZERO_2), context.prim.t);

    let bound_a = state.bound_variable(1);
    let bound_b = state.bound_variable(0);
    let b_to_a = state.function(bound_b, bound_a);
    let a_to_b_to_a = state.function(bound_a, b_to_a);

    let forall_b = state.storage.intern(Type::Forall(
        ForallBinder { visible: false, name: "b".into(), level: level_b, kind: context.prim.t },
        a_to_b_to_a,
    ));
    state.type_scope.unbind(level_b);

    let forall_a_b = state.storage.intern(Type::Forall(
        ForallBinder { visible: false, name: "a".into(), level: level_a, kind: context.prim.t },
        forall_b,
    ));
    state.type_scope.unbind(level_a);

    // ∀a. ∀b. (a -> b -> a) <: (Int -> String -> Int) should pass (LHS foralls get instantiated)
    let string_to_int = state.function(context.prim.string, context.prim.int);
    let int_to_string_to_int = state.function(context.prim.int, string_to_int);

    let result = unification::subtype(state, context, forall_a_b, int_to_string_to_int).unwrap();
    assert!(result, "∀a. ∀b. (a -> b -> a) <: (Int -> String -> Int) should pass");
}

// Error tests

#[test]
fn test_data_arity_fail() {
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

#[test]
fn test_unification_fail() {
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

#[test]
fn test_partial_synonym() {
    let (engine, id) = empty_engine();
    engine.set_content(
        id,
        r#"
module Main where

type Identity a = a

type Test = Identity
"#,
    );

    let checked = engine.checked(id).unwrap();
    insta::assert_debug_snapshot!(checked.errors);
}

#[test]
fn test_invalid_type_operator_unary() {
    let (engine, id) = empty_engine();
    engine.set_content(
        id,
        r#"
module Main where

type Unary a = a

infix 5 type Unary as !
"#,
    );

    let checked = engine.checked(id).unwrap();
    insta::assert_debug_snapshot!(checked.errors);
}

#[test]
fn test_invalid_type_operator_ternary() {
    let (engine, id) = empty_engine();
    engine.set_content(
        id,
        r#"
module Main where

type Ternary a b c = a

infix 5 type Ternary as ?
"#,
    );

    let checked = engine.checked(id).unwrap();
    insta::assert_debug_snapshot!(checked.errors);
}

#[test]
fn test_invalid_type_operator_nullary() {
    let (engine, id) = empty_engine();
    engine.set_content(
        id,
        r#"
module Main where

type Nullary = Int

infix 5 type Nullary as @
"#,
    );

    let checked = engine.checked(id).unwrap();
    insta::assert_debug_snapshot!(checked.errors);
}

// Constrained type tests

#[test]
fn test_constrained_invalid_constraint() {
    let (engine, id) = empty_engine();
    engine.set_content(
        id,
        r#"
module Main where

data NotAClass

type Bad = NotAClass => Int
"#,
    );

    let checked = engine.checked(id).unwrap();
    insta::assert_debug_snapshot!(checked.errors);
}

#[test]
fn test_constrained_invalid_type() {
    let (engine, id) = empty_engine();
    engine.set_content(
        id,
        r#"
module Main where

class Show a where
  show :: a -> String

type Bad = Show Int => 123
"#,
    );

    let checked = engine.checked(id).unwrap();
    insta::assert_debug_snapshot!(checked.errors);
}

// Row unification tests

fn row_field(label: &str, id: TypeId) -> RowField {
    RowField { label: label.into(), id }
}

fn format_row(
    state: &mut CheckState,
    context: &CheckContext<QueryEngine>,
    row: &RowType,
) -> String {
    let fields = row.fields.iter().map(|f| {
        let ty = pretty::print_local(state, context, f.id);
        format!("{}: {ty}", f.label)
    });

    let fields = fields.collect_vec();
    let tail = row.tail.map(|tail| pretty::print_local(state, context, tail));

    match tail {
        Some(t) if fields.is_empty() => format!("({t})"),
        Some(t) => format!("({} | {t})", fields.join(", ")),
        None => format!("({})", fields.join(", ")),
    }
}

fn format_fields(
    state: &mut CheckState,
    context: &CheckContext<QueryEngine>,
    fields: &[RowField],
) -> String {
    let inner = fields.iter().map(|RowField { label, id }| {
        let t = pretty::print_local(state, context, *id);
        format!("{label} :: {t}")
    });
    let inner = inner.collect_vec();
    format!("[{}]", inner.join(", "))
}

fn format_partition_result(
    state: &mut CheckState,
    context: &CheckContext<QueryEngine>,
    row1: &RowType,
    row2: &RowType,
    extras_left: &[RowField],
    extras_right: &[RowField],
    ok: bool,
) -> String {
    let mut buffer = String::new();

    writeln!(buffer, "row1: {}", format_row(state, context, row1)).unwrap();
    writeln!(buffer, "row2: {}", format_row(state, context, row2)).unwrap();
    writeln!(buffer, "ok: {ok}").unwrap();
    writeln!(buffer, "left: {}", format_fields(state, context, extras_left)).unwrap();
    writeln!(buffer, "right: {}", format_fields(state, context, extras_right)).unwrap();

    buffer
}

#[test]
fn test_unify_rows_identical() {
    let (engine, id) = empty_engine();
    let ContextState { ref context, ref mut state } = ContextState::new(&engine, id);

    let row1 = RowType::closed(vec![
        row_field("a", context.prim.int),
        row_field("b", context.prim.string),
    ]);
    let row2 = RowType::closed(vec![
        row_field("a", context.prim.int),
        row_field("b", context.prim.string),
    ]);

    let t1 = state.storage.intern(Type::Row(row1));
    let t2 = state.storage.intern(Type::Row(row2));

    assert!(unification::unify(state, context, t1, t2).unwrap());
}

#[test]
fn test_unify_rows_different_types() {
    let (engine, id) = empty_engine();
    let ContextState { ref context, ref mut state } = ContextState::new(&engine, id);

    let row1 = RowType::closed(vec![row_field("a", context.prim.int)]);
    let row2 = RowType::closed(vec![row_field("a", context.prim.string)]);

    let t1 = state.storage.intern(Type::Row(row1));
    let t2 = state.storage.intern(Type::Row(row2));

    assert!(!unification::unify(state, context, t1, t2).unwrap());
}

#[test]
fn test_unify_rows_extras_left() {
    let (engine, id) = empty_engine();
    let ContextState { ref context, ref mut state } = ContextState::new(&engine, id);

    let row1 = RowType::closed(vec![
        row_field("a", context.prim.int),
        row_field("b", context.prim.string),
    ]);
    let row2 = RowType::closed(vec![row_field("a", context.prim.int)]);

    let t1 = state.storage.intern(Type::Row(row1));
    let t2 = state.storage.intern(Type::Row(row2));

    assert!(!unification::unify(state, context, t1, t2).unwrap());
}

#[test]
fn test_unify_rows_extras_right() {
    let (engine, id) = empty_engine();
    let ContextState { ref context, ref mut state } = ContextState::new(&engine, id);

    let row1 = RowType::closed(vec![row_field("a", context.prim.int)]);
    let row2 = RowType::closed(vec![
        row_field("a", context.prim.int),
        row_field("b", context.prim.string),
    ]);

    let t1 = state.storage.intern(Type::Row(row1));
    let t2 = state.storage.intern(Type::Row(row2));

    assert!(!unification::unify(state, context, t1, t2).unwrap());
}

#[test]
fn test_unify_rows_duplicate_labels() {
    let (engine, id) = empty_engine();
    let ContextState { ref context, ref mut state } = ContextState::new(&engine, id);

    let row1 = RowType::closed(vec![
        row_field("a", context.prim.int),
        row_field("a", context.prim.string),
    ]);
    let row2 = RowType::closed(vec![
        row_field("a", context.prim.int),
        row_field("a", context.prim.string),
    ]);

    let t1 = state.storage.intern(Type::Row(row1));
    let t2 = state.storage.intern(Type::Row(row2));

    assert!(unification::unify(state, context, t1, t2).unwrap());
}

#[test]
fn test_unify_rows_duplicate_labels_mismatch() {
    let (engine, id) = empty_engine();
    let ContextState { ref context, ref mut state } = ContextState::new(&engine, id);

    let row1 =
        RowType::closed(vec![row_field("a", context.prim.int), row_field("a", context.prim.int)]);
    let row2 = RowType::closed(vec![
        row_field("a", context.prim.int),
        row_field("a", context.prim.string),
    ]);

    let t1 = state.storage.intern(Type::Row(row1));
    let t2 = state.storage.intern(Type::Row(row2));

    assert!(!unification::unify(state, context, t1, t2).unwrap());
}

#[test]
fn test_unify_rows_duplicate_labels_extra() {
    let (engine, id) = empty_engine();
    let ContextState { ref context, ref mut state } = ContextState::new(&engine, id);

    let row1 = RowType::closed(vec![
        row_field("a", context.prim.int),
        row_field("a", context.prim.string),
        row_field("a", context.prim.number),
    ]);
    let row2 = RowType::closed(vec![
        row_field("a", context.prim.int),
        row_field("a", context.prim.string),
    ]);

    let t1 = state.storage.intern(Type::Row(row1));
    let t2 = state.storage.intern(Type::Row(row2));

    assert!(!unification::unify(state, context, t1, t2).unwrap());
}

#[test]
fn test_unify_rows_open_closed() {
    let (engine, id) = empty_engine();
    let ContextState { ref context, ref mut state } = ContextState::new(&engine, id);

    let unification = state.fresh_unification(context);
    let row1 = RowType::from_unsorted(vec![row_field("a", context.prim.int)], Some(unification));
    let row2 = RowType::closed(vec![
        row_field("a", context.prim.int),
        row_field("b", context.prim.string),
    ]);

    let t1 = state.storage.intern(Type::Row(row1));
    let t2 = state.storage.intern(Type::Row(row2));

    assert!(unification::unify(state, context, t1, t2).unwrap());
}

#[test]
fn test_unify_rows_open_open() {
    let (engine, id) = empty_engine();
    let ContextState { ref context, ref mut state } = ContextState::new(&engine, id);

    let u1 = state.fresh_unification(context);
    let u2 = state.fresh_unification(context);

    let row1 = RowType::from_unsorted(vec![row_field("a", context.prim.int)], Some(u1));
    let row2 = RowType::from_unsorted(vec![row_field("b", context.prim.string)], Some(u2));

    let t1 = state.storage.intern(Type::Row(row1));
    let t2 = state.storage.intern(Type::Row(row2));

    assert!(unification::unify(state, context, t1, t2).unwrap());
}

#[test]
fn test_unify_rows_interleaved_labels() {
    let (engine, id) = empty_engine();
    let ContextState { ref context, ref mut state } = ContextState::new(&engine, id);

    let row1 = RowType::closed(vec![
        row_field("a", context.prim.int),
        row_field("c", context.prim.int),
        row_field("e", context.prim.int),
    ]);
    let row2 = RowType::closed(vec![
        row_field("b", context.prim.string),
        row_field("d", context.prim.string),
    ]);

    let t1 = state.storage.intern(Type::Row(row1));
    let t2 = state.storage.intern(Type::Row(row2));

    assert!(!unification::unify(state, context, t1, t2).unwrap());
}

// Partition snapshot tests

#[test]
fn test_partition_identical() {
    let (engine, id) = empty_engine();
    let ContextState { ref context, ref mut state } = ContextState::new(&engine, id);

    let row1 = RowType::closed(vec![
        row_field("a", context.prim.int),
        row_field("b", context.prim.string),
    ]);
    let row2 = RowType::closed(vec![
        row_field("a", context.prim.int),
        row_field("b", context.prim.string),
    ]);

    let (extras_left, extras_right, ok) =
        unification::partition_row_fields(state, context, &row1, &row2).unwrap();
    let snapshot =
        format_partition_result(state, context, &row1, &row2, &extras_left, &extras_right, ok);
    insta::assert_snapshot!(snapshot);
}

#[test]
fn test_partition_extras_left() {
    let (engine, id) = empty_engine();
    let ContextState { ref context, ref mut state } = ContextState::new(&engine, id);

    let row1 = RowType::closed(vec![
        row_field("a", context.prim.int),
        row_field("b", context.prim.string),
        row_field("c", context.prim.number),
    ]);
    let row2 = RowType::closed(vec![row_field("a", context.prim.int)]);

    let (extras_left, extras_right, ok) =
        unification::partition_row_fields(state, context, &row1, &row2).unwrap();
    let snapshot =
        format_partition_result(state, context, &row1, &row2, &extras_left, &extras_right, ok);
    insta::assert_snapshot!(snapshot);
}

#[test]
fn test_partition_extras_right() {
    let (engine, id) = empty_engine();
    let ContextState { ref context, ref mut state } = ContextState::new(&engine, id);

    let row1 = RowType::closed(vec![row_field("a", context.prim.int)]);
    let row2 = RowType::closed(vec![
        row_field("a", context.prim.int),
        row_field("b", context.prim.string),
        row_field("c", context.prim.number),
    ]);

    let (extras_left, extras_right, ok) =
        unification::partition_row_fields(state, context, &row1, &row2).unwrap();
    let snapshot =
        format_partition_result(state, context, &row1, &row2, &extras_left, &extras_right, ok);
    insta::assert_snapshot!(snapshot);
}

#[test]
fn test_partition_interleaved() {
    let (engine, id) = empty_engine();
    let ContextState { ref context, ref mut state } = ContextState::new(&engine, id);

    let row1 = RowType::closed(vec![
        row_field("a", context.prim.int),
        row_field("c", context.prim.int),
        row_field("e", context.prim.int),
    ]);
    let row2 = RowType::closed(vec![
        row_field("b", context.prim.string),
        row_field("d", context.prim.string),
    ]);

    let (extras_left, extras_right, ok) =
        unification::partition_row_fields(state, context, &row1, &row2).unwrap();
    let snapshot =
        format_partition_result(state, context, &row1, &row2, &extras_left, &extras_right, ok);
    insta::assert_snapshot!(snapshot);
}

#[test]
fn test_partition_duplicate_labels_equal() {
    let (engine, id) = empty_engine();
    let ContextState { ref context, ref mut state } = ContextState::new(&engine, id);

    let row1 = RowType::closed(vec![
        row_field("a", context.prim.int),
        row_field("a", context.prim.string),
    ]);
    let row2 = RowType::closed(vec![
        row_field("a", context.prim.int),
        row_field("a", context.prim.string),
    ]);

    let (extras_left, extras_right, ok) =
        unification::partition_row_fields(state, context, &row1, &row2).unwrap();
    let snapshot =
        format_partition_result(state, context, &row1, &row2, &extras_left, &extras_right, ok);
    insta::assert_snapshot!(snapshot);
}

#[test]
fn test_partition_duplicate_labels_extra_left() {
    let (engine, id) = empty_engine();
    let ContextState { ref context, ref mut state } = ContextState::new(&engine, id);

    let row1 = RowType::closed(vec![
        row_field("a", context.prim.int),
        row_field("a", context.prim.string),
        row_field("a", context.prim.number),
    ]);
    let row2 = RowType::closed(vec![
        row_field("a", context.prim.int),
        row_field("a", context.prim.string),
    ]);

    let (extras_left, extras_right, ok) =
        unification::partition_row_fields(state, context, &row1, &row2).unwrap();
    let snapshot =
        format_partition_result(state, context, &row1, &row2, &extras_left, &extras_right, ok);
    insta::assert_snapshot!(snapshot);
}

#[test]
fn test_partition_duplicate_labels_extra_right() {
    let (engine, id) = empty_engine();
    let ContextState { ref context, ref mut state } = ContextState::new(&engine, id);

    let row1 = RowType::closed(vec![row_field("a", context.prim.int)]);
    let row2 = RowType::closed(vec![
        row_field("a", context.prim.int),
        row_field("a", context.prim.string),
        row_field("a", context.prim.number),
    ]);

    let (extras_left, extras_right, ok) =
        unification::partition_row_fields(state, context, &row1, &row2).unwrap();
    let snapshot =
        format_partition_result(state, context, &row1, &row2, &extras_left, &extras_right, ok);
    insta::assert_snapshot!(snapshot);
}

#[test]
fn test_partition_unify_failure() {
    let (engine, id) = empty_engine();
    let ContextState { ref context, ref mut state } = ContextState::new(&engine, id);

    let row1 =
        RowType::closed(vec![row_field("a", context.prim.int), row_field("b", context.prim.int)]);
    let row2 = RowType::closed(vec![
        row_field("a", context.prim.string),
        row_field("b", context.prim.int),
    ]);

    let (extras_left, extras_right, ok) =
        unification::partition_row_fields(state, context, &row1, &row2).unwrap();
    let snapshot =
        format_partition_result(state, context, &row1, &row2, &extras_left, &extras_right, ok);
    insta::assert_snapshot!(snapshot);
}

#[test]
fn test_partition_duplicate_labels_positional_mismatch() {
    let (engine, id) = empty_engine();
    let ContextState { ref context, ref mut state } = ContextState::new(&engine, id);

    let row1 = RowType::closed(vec![
        row_field("a", context.prim.int),
        row_field("a", context.prim.string),
    ]);
    let row2 = RowType::closed(vec![
        row_field("a", context.prim.string),
        row_field("a", context.prim.int),
    ]);

    let (extras_left, extras_right, ok) =
        unification::partition_row_fields(state, context, &row1, &row2).unwrap();
    let snapshot =
        format_partition_result(state, context, &row1, &row2, &extras_left, &extras_right, ok);
    insta::assert_snapshot!(snapshot);
}

#[test]
fn test_recursive_synonym_expansion_errors() {
    let mut engine = QueryEngine::default();
    let mut files = Files::default();
    prim::configure(&mut engine, &mut files);

    let id = files.insert(
        "Main.purs",
        r#"
module Main where

type F = G
type G = F

type H = H

testF :: F -> F
testF x = x

testH :: H -> H
testH x = x
"#,
    );
    let content = files.content(id);
    engine.set_content(id, content);

    let checked = engine.checked(id).unwrap();
    insta::assert_debug_snapshot!(checked.errors);
}
