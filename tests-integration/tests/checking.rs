use std::sync::Arc;

use analyzer::{QueryEngine, prim};
use checking::{
    check::{CheckContext, CheckState, PrimCore},
    core::{Type, TypeId, TypeStorage, Variable, debruijn, pretty},
};
use files::{FileId, Files};
use interner::Interner;

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

    // Type construction helpers
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

    fn pretty(&mut self, id: TypeId) -> String {
        self.with_state(|state, _| pretty::print(self.engine, state, id))
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

    let rigid = env.free_variable("rigid");
    let body = env.bound_variable(0);
    let lambda = env.lambda(body);
    let application = env.application(lambda, rigid);

    let normalized = env.with_state(|state, _| state.normalize(application));
    let snapshot = format!("{} ~> {}", env.pretty(application), env.pretty(normalized));

    insta::assert_snapshot!(snapshot);
}

#[test]
fn test_beta_reduction_const_a() {
    let (engine, id) = empty_engine();
    let mut env = TestEnv::new(&engine, id);

    let rigid_a = env.free_variable("rigid-a");
    let rigid_b = env.free_variable("rigid-b");

    let body = env.bound_variable(1);
    let lambda_b = env.lambda(body);
    let lambda_a = env.lambda(lambda_b);

    let application = env.application(lambda_a, rigid_a);
    let application = env.application(application, rigid_b);

    let normalized = env.with_state(|state, _| state.normalize(application));
    let snapshot = format!("{} ~> {}", env.pretty(application), env.pretty(normalized));

    insta::assert_snapshot!(snapshot);
}

#[test]
fn test_beta_reduction_const_b() {
    let (engine, id) = empty_engine();
    let mut env = TestEnv::new(&engine, id);

    let rigid_a = env.free_variable("rigid-a");
    let rigid_b = env.free_variable("rigid-b");

    let body = env.bound_variable(1);
    let lambda_b = env.lambda(body);
    let lambda_a = env.lambda(lambda_b);

    let application = env.application(lambda_a, rigid_a);
    let application = env.application(application, rigid_b);

    let normalized = env.with_state(|state, _| state.normalize(application));
    let snapshot = format!("{} ~> {}", env.pretty(application), env.pretty(normalized));

    insta::assert_snapshot!(snapshot);
}
