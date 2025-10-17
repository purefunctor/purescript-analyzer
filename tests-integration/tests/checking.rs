use std::{fmt::Write, num::NonZeroU32, sync::Arc};

use analyzer::{QueryEngine, prim};
use checking::{
    check::{CheckContext, CheckState, PrimCore, unification::UnificationState},
    core::{Type, TypeId, TypeStorage, Variable, debruijn, pretty},
};
use files::{FileId, Files};
use interner::Interner;
use lowering::TypeVariableBindingId;

#[derive(Debug)]
struct InlineStorage {
    inner: Interner<checking::core::Type>,
}

impl Default for InlineStorage {
    fn default() -> Self {
        let inner = Interner::default();
        InlineStorage { inner }
    }
}

impl TypeStorage for InlineStorage {
    fn intern(&mut self, t: checking::core::Type) -> checking::core::TypeId {
        self.inner.intern(t)
    }

    fn index(&self, id: checking::core::TypeId) -> &checking::core::Type {
        &self.inner[id]
    }
}

struct TestEnv<'r> {
    engine: &'r QueryEngine,
    storage: InlineStorage,
    indexed: Arc<indexing::IndexedModule>,
    lowered: Arc<lowering::LoweredModule>,
    prim_indexed: Arc<indexing::IndexedModule>,
}

struct TestHandle<'r, 'w, S>
where
    S: TypeStorage,
{
    engine: &'r QueryEngine,
    state: &'w mut CheckState<'w, S>,
    context: &'r CheckContext<'r>,
}

impl<'r> TestEnv<'r> {
    fn new(engine: &'r QueryEngine, id: files::FileId) -> Self {
        let indexed = engine.indexed(id).unwrap();
        let lowered = engine.lowered(id).unwrap();
        let prim_indexed = {
            let prim_id = engine.prim_id();
            engine.indexed(prim_id).unwrap()
        };

        let storage = InlineStorage::default();

        Self { engine, storage, indexed, lowered, prim_indexed }
    }

    fn with_handle<F, R>(&mut self, f: F) -> R
    where
        F: FnOnce(TestHandle<InlineStorage>) -> R,
    {
        let mut state = CheckState::new(&mut self.storage);

        let prim = PrimCore::collect(self.engine, &mut state).unwrap();
        let context = CheckContext::new(prim, &self.indexed, &self.lowered, &self.prim_indexed);

        f(TestHandle { engine: self.engine, state: &mut state, context: &context })
    }
}

trait CheckStateExt {
    fn bound_variable(&mut self, index: u32) -> TypeId;

    fn function(&mut self, argument: TypeId, result: TypeId) -> TypeId;
}

impl<S: TypeStorage> CheckStateExt for CheckState<'_, S> {
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
    let mut env = TestEnv::new(&engine, id);

    env.with_handle(|TestHandle { engine, state, context }| {
        // [a :: Int, b :: String]
        state.bind_forall(TypeVariableBindingId::new(FAKE_NONZERO_1), context.prim.int);
        state.bind_forall(TypeVariableBindingId::new(FAKE_NONZERO_2), context.prim.string);
        let codomain = state.bound.size();

        let unification = state.fresh_unification_type(context);
        let Type::Unification(unification_id) = *state.storage.index(unification) else {
            unreachable!("invariant violated");
        };

        state.solve(codomain, unification_id, context.prim.symbol).unwrap();

        let entry = state.unification.get(unification_id);
        let UnificationState::Solved(solution) = entry.state else {
            unreachable!("invariant violated");
        };

        let solution = pretty::print(engine, state, solution);
        let kind = pretty::print(engine, state, entry.kind);

        let snapshot = format!("{solution} :: {kind}");
        insta::assert_snapshot!(snapshot);
    });
}

#[test]
fn test_solve_bound() {
    let (engine, id) = empty_engine();
    let mut env = TestEnv::new(&engine, id);

    env.with_handle(|TestHandle { engine, state, context }| {
        // [a :: Int, b :: String]
        state.bind_forall(TypeVariableBindingId::new(FAKE_NONZERO_1), context.prim.int);
        state.bind_forall(TypeVariableBindingId::new(FAKE_NONZERO_2), context.prim.string);
        let codomain = state.bound.size();

        let unification = state.fresh_unification_type(context);
        let Type::Unification(unification_id) = *state.storage.index(unification) else {
            unreachable!("invariant violated");
        };

        let bound_b = state.bound_variable(0);
        let bound_a = state.bound_variable(1);
        let b_to_a = state.function(bound_b, bound_a);

        state.solve(codomain, unification_id, b_to_a).unwrap();

        let entry = state.unification.get(unification_id);
        let UnificationState::Solved(solution) = entry.state else {
            unreachable!("invariant violated");
        };

        let solution = pretty::print(engine, state, solution);
        let kind = pretty::print(engine, state, entry.kind);

        let snapshot = format!("{solution} :: {kind}");
        insta::assert_snapshot!(snapshot);
    });
}

#[test]
fn test_solve_invalid() {
    let (engine, id) = empty_engine();
    let mut env = TestEnv::new(&engine, id);

    env.with_handle(|TestHandle { state, context, .. }| {
        // [a :: Int]
        state.bind_forall(TypeVariableBindingId::new(FAKE_NONZERO_1), context.prim.int);
        let codomain = state.bound.size();

        let unification = state.fresh_unification_type(context);
        let Type::Unification(unification_id) = *state.storage.index(unification) else {
            unreachable!("invariant violated");
        };

        // [a :: Int, b :: String]
        state.bind_forall(TypeVariableBindingId::new(FAKE_NONZERO_2), context.prim.string);

        let bound_b = state.bound_variable(0);
        let bound_a = state.bound_variable(1);
        let b_to_a = state.function(bound_b, bound_a);

        let solve_result = state.solve(codomain, unification_id, b_to_a);
        assert!(solve_result.is_none());
    })
}

#[test]
fn test_solve_promotion() {
    let (engine, id) = empty_engine();
    let mut env = TestEnv::new(&engine, id);

    env.with_handle(|TestHandle { engine, state, context }| {
        // [a :: Int]
        state.bind_forall(TypeVariableBindingId::new(FAKE_NONZERO_1), context.prim.int);

        let unification_a = state.fresh_unification_type(context);
        let Type::Unification(unification_id) = *state.storage.index(unification_a) else {
            unreachable!("invariant violated");
        };

        // [a :: Int, b :: String]
        state.bind_forall(TypeVariableBindingId::new(FAKE_NONZERO_2), context.prim.string);
        let codomain = state.bound.size();

        let unification_a_b = state.fresh_unification_type(context);
        state.solve(codomain, unification_id, unification_a_b).unwrap();

        let mut snapshot = String::default();

        for (index, entry) in state.unification.iter().enumerate() {
            let UnificationState::Solved(solution) = entry.state else { continue };
            let domain = entry.domain;
            let solution = pretty::print(engine, state, solution);
            writeln!(snapshot, "?{index}[{domain}] := {solution}").unwrap();
        }

        insta::assert_snapshot!(snapshot);
    });
}
