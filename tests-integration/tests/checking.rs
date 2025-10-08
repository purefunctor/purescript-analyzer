use std::{num::NonZeroU32, sync::Arc};

use analyzer::{QueryEngine, prim};
use checking::{
    check::{CheckContext, CheckState, PrimCore, unification::UnificationEntry},
    core::{Type, TypeId, TypeStorage, Variable, debruijn, pretty},
};
use files::{FileId, Files};
use interner::Interner;
use itertools::Itertools;
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

    fn with_state<F, R>(&mut self, f: F) -> R
    where
        F: FnOnce(&mut CheckState<InlineStorage>, &CheckContext) -> R,
    {
        let mut state = CheckState::new(&mut self.storage);

        let prim = PrimCore::collect(self.engine, &mut state).unwrap();
        let context = CheckContext::new(prim, &self.indexed, &self.lowered, &self.prim_indexed);

        f(&mut state, &context)
    }

    fn pretty(&mut self, id: TypeId) -> String {
        self.with_state(|state, _| pretty::print(self.engine, state, id))
    }
}

trait CheckStateExt {
    fn bound_variable(&mut self, index: u32) -> TypeId;

    fn free_variable(&mut self, name: &str) -> TypeId;

    fn lambda(&mut self, body: TypeId) -> TypeId;

    fn application(&mut self, function: TypeId, argument: TypeId) -> TypeId;

    fn function(&mut self, arg: TypeId, result: TypeId) -> TypeId;

    fn unification(&mut self, id: u32, spine: &[TypeId]) -> TypeId;

    fn pruning(&mut self, id: u32, pruning: &[bool]) -> TypeId;

    fn unknown(&mut self) -> TypeId;
}

impl<S: TypeStorage> CheckStateExt for CheckState<'_, S> {
    fn bound_variable(&mut self, index: u32) -> TypeId {
        let var = Variable::Bound(debruijn::Index(index));
        self.storage.intern(Type::Variable(var))
    }

    fn free_variable(&mut self, name: &str) -> TypeId {
        let var = Variable::Free(name.into());
        self.storage.intern(Type::Variable(var))
    }

    fn lambda(&mut self, body: TypeId) -> TypeId {
        self.storage.intern(Type::Lambda(body))
    }

    fn application(&mut self, function: TypeId, argument: TypeId) -> TypeId {
        self.storage.intern(Type::Application(function, argument))
    }

    fn function(&mut self, arg: TypeId, result: TypeId) -> TypeId {
        self.storage.intern(Type::Function(arg, result))
    }

    fn unification(&mut self, id: u32, spine: &[TypeId]) -> TypeId {
        self.storage.intern(Type::Unification(id, Arc::from(spine)))
    }

    fn pruning(&mut self, id: u32, pruning: &[bool]) -> TypeId {
        self.storage.intern(Type::Pruning(id, Arc::from(pruning)))
    }

    fn unknown(&mut self) -> TypeId {
        self.storage.intern(Type::Unknown)
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

#[test]
fn test_beta_reduction_id() {
    let (engine, id) = empty_engine();
    let mut env = TestEnv::new(&engine, id);

    let (application, normalized) = env.with_state(|state, _| {
        let rigid = state.free_variable("rigid");
        let body = state.bound_variable(0);
        let lambda = state.lambda(body);
        let application = state.application(lambda, rigid);

        let normalized = state.normalize(application);
        (application, normalized)
    });

    let snapshot = format!("{} ~> {}", env.pretty(application), env.pretty(normalized));
    insta::assert_snapshot!(snapshot);
}

#[test]
fn test_beta_reduction_const_a() {
    let (engine, id) = empty_engine();
    let mut env = TestEnv::new(&engine, id);

    let (application, normalized) = env.with_state(|state, _| {
        let rigid_a = state.free_variable("rigid-a");
        let rigid_b = state.free_variable("rigid-b");

        let body = state.bound_variable(1);
        let lambda_b = state.lambda(body);
        let lambda_a = state.lambda(lambda_b);

        let application = state.application(lambda_a, rigid_a);
        let application = state.application(application, rigid_b);

        let normalized = state.normalize(application);
        (application, normalized)
    });

    let snapshot = format!("{} ~> {}", env.pretty(application), env.pretty(normalized));
    insta::assert_snapshot!(snapshot);
}

#[test]
fn test_beta_reduction_const_b() {
    let (engine, id) = empty_engine();
    let mut env = TestEnv::new(&engine, id);

    let (application, normalized) = env.with_state(|state, _| {
        let rigid_a = state.free_variable("rigid-a");
        let rigid_b = state.free_variable("rigid-b");

        let body = state.bound_variable(0);
        let lambda_b = state.lambda(body);
        let lambda_a = state.lambda(lambda_b);

        let application = state.application(lambda_a, rigid_a);
        let application = state.application(application, rigid_b);

        let normalized = state.normalize(application);
        (application, normalized)
    });

    let snapshot = format!("{} ~> {}", env.pretty(application), env.pretty(normalized));
    insta::assert_snapshot!(snapshot);
}

const FAKE_NONZERO_1: NonZeroU32 = NonZeroU32::new(1).unwrap();
const FAKE_NONZERO_2: NonZeroU32 = NonZeroU32::new(2).unwrap();

#[test]
fn test_fresh_unification_normalized() {
    let (engine, id) = empty_engine();
    let mut env = TestEnv::new(&engine, id);

    let (unification, normalized, kind) = env.with_state(|state, context| {
        // [a :: Int, b :: Int]
        state.bind_forall(TypeVariableBindingId::new(FAKE_NONZERO_1), context.prim.int);
        state.bind_forall(TypeVariableBindingId::new(FAKE_NONZERO_2), context.prim.int);

        let unification = state.fresh_unification(context);
        let normalized = state.normalize(unification);

        let Type::Pruning(u, _) = state.storage.index(unification) else {
            unreachable!("invariant violated");
        };

        let entry = state.unification.get(*u);
        (unification, normalized, entry.kind)
    });

    let snapshot = format!(
        "{} ~> {} :: {}",
        env.pretty(unification),
        env.pretty(normalized),
        env.pretty(kind)
    );

    insta::assert_snapshot!(snapshot);
}

#[test]
fn test_fresh_unification_inversion() {
    let (engine, id) = empty_engine();
    let mut env = TestEnv::new(&engine, id);

    let (proper, nonlinear) = env.with_state(|state, context| {
        // [a :: Int, b :: Int]
        state.bind_forall(TypeVariableBindingId::new(FAKE_NONZERO_1), context.prim.int);
        state.bind_forall(TypeVariableBindingId::new(FAKE_NONZERO_2), context.prim.int);
        let codomain = state.bound.level();

        let unification = state.fresh_unification(context);
        let normalized = state.normalize(unification);

        let Type::Unification(_, spine) = state.storage.index(normalized) else {
            unreachable!("invariant violated");
        };

        let nonlinear_spine = spine.iter().chain(spine.iter()).copied().collect_vec();

        let proper_inversion = state.invert_spine(codomain, spine);
        let nonlinear_inversion = state.invert_spine(codomain, &nonlinear_spine);

        (proper_inversion, nonlinear_inversion)
    });

    let snapshot = format!("{proper:?}\n{nonlinear:?}");
    insta::assert_snapshot!(snapshot);
}
