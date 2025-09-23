//! Implements the core build system for the query-based compiler
//!
//! Our implementation is inspired by the verifying step traces described in
//! the [Build systems à la carte: Theory and practice] paper. However, the
//! implementation has two key differences: we only retain the latest step
//! trace for any given query; and more significantly, we use structural
//! equality instead of hashing to compare cached and fresh values.
//!
//! Unlike traditional phase-based compilation, query-based compilers are
//! designed to have its intermediate states be observed directly using a
//! convenient API.
//!
//! The build system is designed to be pure and hermetic—the current state of
//! the workspace e.g. file contents are stored in-memory to make dependency
//! tracking easier to manage.
//!
//! Our implementation also borrows a few techniques used by [salsa] such as
//! using global query lock for ordering query reads and input writes, and
//! future-promise-based work deduplication. These techniques enable parallel
//! computation with cancellation and work deduplication!
//!
//! [Build systems à la carte: Theory and practice]: https://www.cambridge.org/core/journals/journal-of-functional-programming/article/build-systems-a-la-carte-theory-and-practice/097CE52C750E69BD16B78C318754C7A4
//! [salsa]: https://github.com/salsa-rs/salsa

mod graph;
mod promise;

use std::{
    cell::RefCell,
    collections::hash_map::Entry,
    sync::{
        Arc,
        atomic::{AtomicBool, AtomicU32, AtomicUsize, Ordering},
    },
};

use building_types::{ModuleNameId, ModuleNameInterner, QueryError, QueryKey, QueryResult};
use files::FileId;
use graph::SnapshotGraph;
use indexing::IndexedModule;
use lock_api::{RawRwLock, RawRwLockRecursive};
use lowering::LoweredModule;
use parking_lot::{Mutex, RwLock, RwLockUpgradableReadGuard};
use parsing::FullParsedModule;
use promise::{Future, Promise};
use resolving::ResolvedModule;
use rustc_hash::{FxHashMap, FxHashSet};
use stabilizing::StabilizedModule;
use thread_local::ThreadLocal;

#[derive(Debug, Clone, Copy)]
struct Trace {
    /// Timestamp of when the query was last called.
    built: usize,
    /// Timestamp of when the query was last recomputed.
    changed: usize,
}

#[derive(Debug, Default)]
enum DerivedState<T> {
    #[default]
    NotComputed,
    InProgress {
        id: SnapshotId,
        promises: Mutex<Vec<Promise<T>>>,
    },
    Computed {
        computed: T,
        trace: Trace,
        dependencies: Arc<[QueryKey]>,
    },
}

impl<T> DerivedState<T> {
    fn in_progress(id: SnapshotId) -> DerivedState<T> {
        DerivedState::InProgress { id, promises: Mutex::default() }
    }
}

#[derive(Debug)]
struct InputState<T> {
    value: T,
    changed: usize,
}

#[derive(Default)]
struct InputStorage {
    content: FxHashMap<FileId, InputState<Arc<str>>>,
    module: FxHashMap<ModuleNameId, InputState<FileId>>,
}

#[derive(Default)]
struct DerivedStorage {
    parsed: FxHashMap<FileId, DerivedState<FullParsedModule>>,
    stabilized: FxHashMap<FileId, DerivedState<Arc<StabilizedModule>>>,
    indexed: FxHashMap<FileId, DerivedState<Arc<IndexedModule>>>,
    lowered: FxHashMap<FileId, DerivedState<Arc<LoweredModule>>>,
    resolved: FxHashMap<FileId, DerivedState<Arc<ResolvedModule>>>,
}

#[derive(Default)]
struct InternedStorage {
    module: ModuleNameInterner,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct SnapshotId(u32);

#[derive(Default)]
struct GlobalState {
    /// An atomic token that determines if query execution had been cancelled.
    cancelled: AtomicBool,
    /// A global read-write lock for enforcing the order of reads and writes.
    query_lock: RwLock<()>,
    /// A counter that tracks the current revision of the query engine.
    revision: AtomicUsize,
    /// A counter that tracks the next [`SnapshotId`],
    snapshot: AtomicU32,
    /// A graph that tracks dependencies between [`SnapshotId`]
    graph: Mutex<SnapshotGraph>,
}

impl GlobalState {
    fn next_snapshot(&self) -> SnapshotId {
        SnapshotId(self.snapshot.fetch_add(1, Ordering::Relaxed))
    }
}

#[derive(Default)]
struct LocalState {
    inner: ThreadLocal<RefCell<LocalStateInner>>,
}

impl LocalState {
    fn with_current<T>(&self, current: QueryKey, f: impl FnOnce() -> T) -> T {
        let inner = self.inner.get_or_default();
        {
            let mut setup = inner.borrow_mut();
            setup.stack.push(current);
        }
        let result = f();
        {
            let mut cleanup = inner.borrow_mut();
            cleanup.stack.pop();
            cleanup.in_progress.remove(&current);
            cleanup.dependencies.remove(&current);
        }
        result
    }

    fn with_dependency(&self, dependency: QueryKey) {
        let mut inner = self.inner.get_or_default().borrow_mut();
        if let Some(&current) = inner.stack.last() {
            inner.dependencies.entry(current).or_default().insert(dependency);
        }
    }

    fn dependencies(&self, key: QueryKey) -> Arc<[QueryKey]> {
        let inner = &self.inner.get_or_default().borrow();
        inner
            .dependencies
            .get(&key)
            .map(|dependencies| dependencies.iter().copied())
            .unwrap_or_default()
            .collect()
    }

    fn stack(&self) -> Arc<[QueryKey]> {
        let inner = self.inner.get_or_default().borrow();
        inner.stack.as_slice().into()
    }

    fn add_in_progress(&self, key: QueryKey) {
        let mut inner = self.inner.get_or_default().borrow_mut();
        inner.in_progress.insert(key);
    }

    fn is_in_progress(&self, key: QueryKey) -> bool {
        let inner = self.inner.get_or_default().borrow();
        inner.in_progress.contains(&key)
    }
}

#[derive(Debug, Default)]
struct LocalStateInner {
    stack: Vec<QueryKey>,
    in_progress: FxHashSet<QueryKey>,
    dependencies: FxHashMap<QueryKey, FxHashSet<QueryKey>>,
}

/// Custom guard that acquires a read lock from the [`GlobalState::query_lock`]
/// and releases it when dropped, effectively tying it to the lifetime of the
/// [`QueryControl`] it belongs to.
struct QueryControlGuard {
    global: Arc<GlobalState>,
}

impl QueryControlGuard {
    fn new(global: &Arc<GlobalState>) -> QueryControlGuard {
        // SAFETY: QueryControlGuard::drop
        unsafe { global.query_lock.raw().lock_shared_recursive() };
        QueryControlGuard { global: Arc::clone(global) }
    }
}

impl Drop for QueryControlGuard {
    fn drop(&mut self) {
        // SAFETY: QueryControlGuard::new
        unsafe { self.global.query_lock.raw().unlock_shared() }
    }
}

struct QueryControl {
    _guard: Option<QueryControlGuard>,
    id: SnapshotId,
    local: Arc<LocalState>,
    global: Arc<GlobalState>,
}

impl QueryControl {
    fn snapshot(&self) -> QueryControl {
        let _guard = Some(QueryControlGuard::new(&self.global));
        let local = Arc::new(LocalState::default());
        let global = Arc::clone(&self.global);
        let id = global.next_snapshot();
        QueryControl { _guard, id, local, global }
    }
}

impl Default for QueryControl {
    fn default() -> QueryControl {
        let _guard = None;
        let local = Arc::new(LocalState::default());
        let global = Arc::new(GlobalState::default());
        let id = global.next_snapshot();
        QueryControl { _guard, id, local, global }
    }
}

#[derive(Default)]
struct QueryStorage {
    input: InputStorage,
    derived: DerivedStorage,
    interned: InternedStorage,
}

#[derive(Default)]
pub struct QueryEngine {
    storage: Arc<RwLock<QueryStorage>>,
    control: QueryControl,
}

impl QueryEngine {
    /// Creates a snapshot of the [`QueryEngine`].
    ///
    /// Snapshots are read locks over the [`QueryEngine`] that must
    /// be sent across threads to perform query execution.
    ///
    /// As with read locks, keeping snapshots alive indefinitely is
    /// a logic error and will cause a deadlock on mutation or on a
    /// [cancellation request].
    ///
    /// [cancellation request]: QueryEngine::request_cancel
    pub fn snapshot(&self) -> QueryEngine {
        let storage = self.storage.clone();
        let control = self.control.snapshot();
        QueryEngine { storage, control }
    }

    /// Creates a cancellation request for queries.
    ///
    /// Query cancellation is cooperative. A cancellation flag is read
    /// at some point during query execution. This function also waits
    /// for all snapshots to be dropped, as in the expected consequence
    /// of cancelling all queries running across all threads.
    pub fn request_cancel(&self) {
        self.control.global.cancelled.store(true, Ordering::Relaxed);
        let _query_lock = self.control.global.query_lock.write();
        self.control.global.cancelled.store(false, Ordering::Relaxed);
    }
}

impl QueryEngine {
    fn query<K, V, GetFn, GetMutFn, ComputeFn>(
        &self,
        key: QueryKey,
        get: GetFn,
        get_mut: GetMutFn,
        compute: ComputeFn,
    ) -> QueryResult<V>
    where
        GetFn: Fn(&QueryStorage) -> Option<&DerivedState<V>>,
        GetMutFn: Fn(&mut QueryStorage) -> Entry<K, DerivedState<V>>,
        ComputeFn: Fn(&QueryEngine) -> QueryResult<V>,
        V: Eq + Clone,
    {
        self.control.local.with_dependency(key);
        self.control.local.with_current(key, || {
            // If query execution fails at any given point, clean up the state.
            self.query_core(key, &get, &get_mut, &compute).inspect_err(|_| {
                if self.control.local.is_in_progress(key) {
                    let mut storage = self.storage.write();
                    if let Entry::Occupied(o) = get_mut(&mut storage) {
                        if let DerivedState::InProgress { id, promises } = o.remove() {
                            let mut graph = self.control.global.graph.lock();
                            drop(promises);
                            drop(storage);
                            graph.remove_edge(id);
                        } else {
                            unreachable!("invariant violated: expected InProgress");
                        }
                    }
                }
            })
        })
    }

    /// Fulfills the promises of an [`DerivedState::InProgress`] query and
    /// replaces it with a [`DerivedState::Computed`] result in the store.
    fn fulfill_and_store<K, V, GetMutFn>(
        &self,
        get_mut: &GetMutFn,
        computed: V,
        trace: Trace,
        dependencies: Arc<[QueryKey]>,
    ) where
        GetMutFn: Fn(&mut QueryStorage) -> Entry<K, DerivedState<V>>,
        V: Clone,
    {
        let mut storage = self.storage.write();
        if let Entry::Occupied(o) = get_mut(&mut storage) {
            if let DerivedState::InProgress { id, promises } = o.remove() {
                let mut graph = self.control.global.graph.lock();
                let promises = promises.into_inner();
                promises.into_iter().for_each(|promise| {
                    let computed = V::clone(&computed);
                    promise.fulfill(computed);
                });
                graph.remove_edge(id);
            } else {
                unreachable!("invariant violated: expected InProgress");
            }
        }

        let state = DerivedState::Computed { computed, trace, dependencies };
        get_mut(&mut storage).insert_entry(state);
    }

    fn compute_core<K, V, GetMutFn, ComputeFn>(
        &self,
        get_mut: &GetMutFn,
        compute: &ComputeFn,
        key: QueryKey,
        revision: usize,
        previous: Option<(V, Trace)>,
    ) -> QueryResult<V>
    where
        GetMutFn: Fn(&mut QueryStorage) -> Entry<K, DerivedState<V>>,
        ComputeFn: Fn(&QueryEngine) -> QueryResult<V>,
        V: Eq + Clone,
    {
        if self.control.global.cancelled.load(Ordering::Relaxed) {
            return Err(QueryError::Cancelled);
        }

        let computed = compute(self)?;

        // If the computed result is equal to the cached one, the changed
        // timestamp does not need to be updated. Likewise, we also insert
        // the previous value back into the cache. The latter is a niche,
        // but useful optimisation for when V = Arc<T>, since it enables
        // pointer equality.
        match previous {
            Some((previous, trace)) if computed == previous => {
                let trace = Trace { built: revision, changed: trace.changed };
                let dependencies = self.control.local.dependencies(key);
                self.fulfill_and_store(get_mut, V::clone(&previous), trace, dependencies);
                Ok(previous)
            }
            _ => {
                let trace = Trace { built: revision, changed: revision };
                let dependencies = self.control.local.dependencies(key);
                self.fulfill_and_store(get_mut, V::clone(&computed), trace, dependencies);
                Ok(computed)
            }
        }
    }

    /// Verifies the given dependencies by executing them, returning the
    /// timestamp of the most latest change.
    fn verify_core(&self, dependencies: &[QueryKey]) -> QueryResult<usize> {
        let mut latest = 0;

        macro_rules! input_changed {
            ($field:ident, $key:expr) => {{
                let storage = self.storage.read();
                if let Some(InputState { changed, .. }) = storage.input.$field.get($key) {
                    latest = latest.max(*changed);
                }
            }};
        }

        macro_rules! derived_changed {
            ($field:ident, $key:expr) => {{
                self.$field(*$key)?;
                let storage = self.storage.read();
                if let Some(DerivedState::Computed { trace, .. }) = storage.derived.$field.get($key)
                {
                    latest = latest.max(trace.changed);
                }
            }};
        }

        for dependency in dependencies {
            match dependency {
                QueryKey::Content(k) => input_changed!(content, k),
                QueryKey::Module(k) => input_changed!(module, k),
                QueryKey::Parsed(k) => derived_changed!(parsed, k),
                QueryKey::Stabilized(k) => derived_changed!(stabilized, k),
                QueryKey::Indexed(k) => derived_changed!(indexed, k),
                QueryKey::Lowered(k) => derived_changed!(lowered, k),
                QueryKey::Resolved(k) => derived_changed!(resolved, k),
            }
        }

        Ok(latest)
    }

    fn create_future<T>(
        &self,
        to_id: SnapshotId,
        promises: &Mutex<Vec<Promise<T>>>,
    ) -> QueryResult<Future<T>> {
        {
            let mut graph = self.control.global.graph.lock();
            let stack = self.control.local.stack();
            if !graph.add_edge(self.control.id, to_id) {
                return Err(QueryError::Cycle { stack });
            }
        }

        let (future, promise) = Future::new();
        promises.lock().push(promise);
        Ok(future)
    }

    fn query_core<K, V, GetFn, GetMutFn, ComputeFn>(
        &self,
        key: QueryKey,
        get: &GetFn,
        get_mut: &GetMutFn,
        compute: &ComputeFn,
    ) -> QueryResult<V>
    where
        GetFn: Fn(&QueryStorage) -> Option<&DerivedState<V>>,
        GetMutFn: Fn(&mut QueryStorage) -> Entry<K, DerivedState<V>>,
        ComputeFn: Fn(&QueryEngine) -> QueryResult<V>,
        V: Eq + Clone,
    {
        if self.control.global.cancelled.load(Ordering::Relaxed) {
            return Err(QueryError::Cancelled);
        }

        let revision = self.control.global.revision.load(Ordering::Relaxed);

        // Certain query states can be checked with only a read lock, and this
        // is an extremely useful optimisation because it allows threads to
        // skip their turn on acquiring an upgradable read lock.
        //
        // For computed queries, we can skip dependency verification if the
        // cached value was built during the current revision.
        //
        // For in-progress queries, we can simply push to the internally mutable
        // vector of promises and then wait on the future.
        {
            let storage = self.storage.read();
            match get(&storage).unwrap_or(&DerivedState::NotComputed) {
                DerivedState::Computed { computed, trace, .. } => {
                    if trace.built == revision {
                        return Ok(V::clone(computed));
                    }
                }
                DerivedState::InProgress { id, promises } => {
                    let future = self.create_future(*id, promises)?;

                    // Remember that Future::wait blocks the current thread!
                    drop(storage);

                    return future.wait().ok_or(QueryError::Cancelled);
                }
                _ => (),
            }
        }

        // Otherwise, we will have to perform computation or cache verification.
        // Instead of a write lock, we use an upgradable read lock for two reasons:
        // we want to ensure that only a single thread can observe the NotComputed
        // state for any given query while allowing read locks to be acquired for
        // the optimisation above.
        {
            let storage = self.storage.upgradable_read();
            match get(&storage).unwrap_or(&DerivedState::NotComputed) {
                DerivedState::NotComputed => {
                    // At the end of this block, threads waiting to acquire the
                    // upgradable read lock should read that the query is InProgress.
                    {
                        let mut storage = RwLockUpgradableReadGuard::upgrade(storage);
                        get_mut(&mut storage)
                            .insert_entry(DerivedState::in_progress(self.control.id));
                        self.control.local.add_in_progress(key);
                    }

                    self.compute_core(get_mut, compute, key, revision, None)
                }
                DerivedState::InProgress { id, promises } => {
                    let future = self.create_future(*id, promises)?;

                    // Remember that Future::wait blocks the current thread!
                    drop(storage);

                    future.wait().ok_or(QueryError::Cancelled)
                }
                DerivedState::Computed { computed, trace, dependencies } => {
                    let computed = V::clone(computed);
                    let trace = *trace;
                    let dependencies = Arc::clone(dependencies);

                    // If the cached value was built during the current revision
                    // we can skip dependency verification entirely. This is also
                    // checked at the start of the query_core with a read lock.
                    if trace.built == revision {
                        return Ok(computed);
                    }

                    // Same as NotComputed, see comment above.
                    {
                        let mut storage = RwLockUpgradableReadGuard::upgrade(storage);
                        get_mut(&mut storage)
                            .insert_entry(DerivedState::in_progress(self.control.id));
                        self.control.local.add_in_progress(key);
                    }

                    let latest = self.verify_core(&dependencies)?;

                    // If the cached value was built more recently the the
                    // latest change, we can update its built timestamp to
                    // the current revision. This allows the query to hit
                    // the fastest path if it's called in the same revision.
                    if trace.built >= latest {
                        let trace = Trace { built: revision, ..trace };
                        self.fulfill_and_store(get_mut, V::clone(&computed), trace, dependencies);
                        return Ok(computed);
                    }

                    self.compute_core(get_mut, compute, key, revision, Some((computed, trace)))
                }
            }
        }
    }

    fn set_input<K, V, F>(&self, f: F, v: V)
    where
        F: FnOnce(&mut QueryStorage) -> Entry<K, InputState<V>>,
    {
        self.control.global.cancelled.store(true, Ordering::Relaxed);
        let _query_lock = self.control.global.query_lock.write();

        let changed = self.control.global.revision.fetch_add(1, Ordering::Relaxed);
        let state = InputState { value: v, changed: changed + 1 };

        let mut storage = self.storage.write();
        f(&mut storage).insert_entry(state);

        self.control.global.cancelled.store(false, Ordering::Relaxed);
    }

    fn get_input<V, F>(&self, k: QueryKey, f: F) -> Option<V>
    where
        F: FnOnce(&QueryStorage) -> Option<&InputState<V>>,
        V: Clone,
    {
        self.control.local.with_dependency(k);
        let storage = self.storage.read();
        f(&storage).map(|state| V::clone(&state.value))
    }
}

impl QueryEngine {
    pub fn set_content(&self, id: FileId, content: impl Into<Arc<str>>) {
        self.set_input(|storage| storage.input.content.entry(id), content.into());
    }

    pub fn content(&self, id: FileId) -> Arc<str> {
        self.get_input(QueryKey::Content(id), |storage| storage.input.content.get(&id))
            .unwrap_or_else(|| {
                panic!("invariant violated: set_content({id:?}, ..)");
            })
    }

    pub fn set_module_file(&self, name: &str, file_id: FileId) {
        let id = {
            let mut storage = self.storage.write();
            storage.interned.module.intern(name)
        };
        self.set_input(|storage| storage.input.module.entry(id), file_id);
    }

    pub fn module_file(&self, name: &str) -> Option<FileId> {
        let id = {
            let storage = self.storage.read();
            storage.interned.module.lookup(name)?
        };
        self.get_input(QueryKey::Module(id), |storage| storage.input.module.get(&id))
    }

    pub fn parsed(&self, id: FileId) -> QueryResult<FullParsedModule> {
        self.query(
            QueryKey::Parsed(id),
            |storage| storage.derived.parsed.get(&id),
            |storage| storage.derived.parsed.entry(id),
            |this| {
                let content = this.content(id);

                let lexed = lexing::lex(&content);
                let tokens = lexing::layout(&lexed);
                let parsed = parsing::parse(&lexed, &tokens);

                Ok(parsed)
            },
        )
    }

    pub fn stabilized(&self, id: FileId) -> QueryResult<Arc<StabilizedModule>> {
        self.query(
            QueryKey::Stabilized(id),
            |storage| storage.derived.stabilized.get(&id),
            |storage| storage.derived.stabilized.entry(id),
            |this| {
                let (parsed, _) = this.parsed(id)?;
                let node = parsed.syntax_node();
                Ok(Arc::new(stabilizing::stabilize(&node)))
            },
        )
    }

    pub fn indexed(&self, id: FileId) -> QueryResult<Arc<IndexedModule>> {
        self.query(
            QueryKey::Indexed(id),
            |storage| storage.derived.indexed.get(&id),
            |storage| storage.derived.indexed.entry(id),
            |this| {
                let (parsed, _) = this.parsed(id)?;
                let stabilized = this.stabilized(id)?;

                let module = parsed.cst();
                let indexed = indexing::index_module(&module, &stabilized);

                Ok(Arc::new(indexed))
            },
        )
    }

    pub fn lowered(&self, id: FileId) -> QueryResult<Arc<LoweredModule>> {
        self.query(
            QueryKey::Lowered(id),
            |storage| storage.derived.lowered.get(&id),
            |storage| storage.derived.lowered.entry(id),
            |this| {
                let (parsed, _) = this.parsed(id)?;

                let prim = {
                    let prim_id = this.prim_id();
                    this.resolved(prim_id)?
                };

                let stabilized = this.stabilized(id)?;
                let indexed = this.indexed(id)?;
                let resolved = this.resolved(id)?;

                let module = parsed.cst();
                let lowered =
                    lowering::lower_module(&module, &prim, &stabilized, &indexed, &resolved);

                Ok(Arc::new(lowered))
            },
        )
    }

    pub fn resolved(&self, id: FileId) -> QueryResult<Arc<ResolvedModule>> {
        self.query(
            QueryKey::Resolved(id),
            |storage| storage.derived.resolved.get(&id),
            |storage| storage.derived.resolved.entry(id),
            |this| {
                let resolved = resolving::resolve_module(this, id)?;
                Ok(Arc::new(resolved))
            },
        )
    }
}

impl QueryEngine {
    pub fn prim_id(&self) -> FileId {
        self.module_file("Prim").expect("invariant violated: prim::configure")
    }
}

impl resolving::External for QueryEngine {
    fn indexed(&self, id: FileId) -> QueryResult<Arc<IndexedModule>> {
        QueryEngine::indexed(self, id)
    }

    fn resolved(&self, id: FileId) -> QueryResult<Arc<ResolvedModule>> {
        QueryEngine::resolved(self, id)
    }

    fn module_file(&self, name: &str) -> Option<FileId> {
        QueryEngine::module_file(self, name)
    }
}

impl sugar::External for QueryEngine {
    fn indexed(&self, id: FileId) -> QueryResult<Arc<IndexedModule>> {
        QueryEngine::indexed(self, id)
    }

    fn resolved(&self, id: FileId) -> QueryResult<Arc<ResolvedModule>> {
        QueryEngine::resolved(self, id)
    }

    fn lowered(&self, id: FileId) -> QueryResult<Arc<LoweredModule>> {
        QueryEngine::lowered(self, id)
    }

    fn prim_id(&self) -> FileId {
        QueryEngine::prim_id(self)
    }
}

#[cfg(test)]
mod tests {
    use std::{
        fmt::Debug,
        sync::{Arc, atomic::Ordering},
    };

    use building_types::{QueryError, QueryResult};
    use files::{FileId, Files};
    use la_arena::RawIdx;
    use resolving::ResolvedModule;

    use crate::prim;

    use super::{DerivedState, QueryEngine, QueryKey};

    #[derive(Debug)]
    struct Trace<'a> {
        built: usize,
        changed: usize,
        dependencies: &'a [QueryKey],
    }

    struct ShowTrace<'a, T>(&'a DerivedState<T>);

    impl<'a, T> Debug for ShowTrace<'a, T> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match &self.0 {
                DerivedState::NotComputed => write!(f, "NotComputed"),
                DerivedState::InProgress { .. } => write!(f, "InProgress {{ .. }}"),
                DerivedState::Computed { trace, dependencies, .. } => f
                    .debug_struct("Trace")
                    .field("built", &trace.built)
                    .field("changed", &trace.changed)
                    .field("dependencies", dependencies)
                    .finish(),
            }
        }
    }

    impl<'a, 'b, T> PartialEq<Trace<'b>> for ShowTrace<'a, T> {
        fn eq(&self, other: &Trace<'b>) -> bool {
            match self.0 {
                DerivedState::NotComputed => false,
                DerivedState::InProgress { .. } => false,
                DerivedState::Computed { trace, dependencies, .. } => {
                    trace.built == other.built
                        && trace.changed == other.changed
                        && dependencies.as_ref() == other.dependencies
                }
            }
        }
    }

    #[test]
    fn test_pointer_equality() {
        let mut engine = QueryEngine::default();
        let mut files = Files::default();
        prim::configure(&mut engine, &mut files);

        let id = files.insert("./src/Main.purs", "module Main where\n\nlife = 42");
        let content = files.content(id);

        engine.set_content(id, content);
        let index_a = engine.indexed(id).unwrap();
        let index_b = engine.indexed(id).unwrap();
        assert!(Arc::ptr_eq(&index_a, &index_b));

        let id = files.insert("./src/Main.purs", "module Main where\n\nlife = 42\n\n");
        let content = files.content(id);

        engine.set_content(id, content);
        let index_a = engine.indexed(id).unwrap();
        let index_b = engine.indexed(id).unwrap();
        assert!(Arc::ptr_eq(&index_a, &index_b));
    }

    #[test]
    fn test_verifying_step_traces() {
        let mut engine = QueryEngine::default();
        let mut files = Files::default();
        prim::configure(&mut engine, &mut files);

        macro_rules! assert_trace {
            ($storage:expr, $field:ident($id:expr) => $trace:expr) => {
                assert_eq!(ShowTrace($storage.derived.$field.get(&$id).unwrap()), $trace);
            };
        }

        let id = files.insert("./src/Main.purs", "module Main where\n\nlife = 42");
        let content = files.content(id);

        engine.set_content(id, content);
        let indexed_a = engine.indexed(id).unwrap();
        let lowered_a = engine.lowered(id).unwrap();
        let resolved_a = engine.resolved(id).unwrap();

        {
            let storage = engine.storage.read();
            assert_trace!(storage, parsed(id) => Trace {
                built: 19,
                changed: 19,
                dependencies: &[QueryKey::Content(id)]
            });
            assert_trace!(storage, indexed(id) => Trace {
                built: 19,
                changed: 19,
                dependencies: &[QueryKey::Parsed(id), QueryKey::Stabilized(id)]
            });
            assert_trace!(storage, resolved(id) => Trace {
                built: 19,
                changed: 19,
                dependencies: &[QueryKey::Indexed(id)]
            });
        }

        let id = files.insert("./src/Main.purs", "module Main where\n\n\n\nlife = 42");
        let content = files.content(id);

        engine.set_content(id, content);
        let indexed_b = engine.indexed(id).unwrap();
        let lowered_b = engine.lowered(id).unwrap();
        let resolved_b = engine.resolved(id).unwrap();

        {
            let storage = engine.storage.read();
            assert_trace!(storage, parsed(id) => Trace {
                built: 20,
                changed: 20,
                dependencies: &[QueryKey::Content(id)]
            });
            assert_trace!(storage, indexed(id) => Trace {
                built: 20,
                changed: 19,
                dependencies: &[QueryKey::Parsed(id), QueryKey::Stabilized(id)]
            });
            assert_trace!(storage, resolved(id) => Trace {
                built: 20,
                changed: 19,
                dependencies: &[QueryKey::Indexed(id)]
            });
        }

        let id = files.insert("./src/Main.purs", "module Main where\n\n\n\nlife = 42\n\n");
        let content = files.content(id);

        engine.set_content(id, content);
        let indexed_c = engine.indexed(id).unwrap();
        let lowered_c = engine.lowered(id).unwrap();
        let resolved_c = engine.resolved(id).unwrap();

        {
            let storage = engine.storage.read();
            assert_trace!(storage, parsed(id) => Trace {
                built: 21,
                changed: 21,
                dependencies: &[QueryKey::Content(id)]
            });
            assert_trace!(storage, indexed(id) => Trace {
                built: 21,
                changed: 19,
                dependencies: &[QueryKey::Parsed(id), QueryKey::Stabilized(id)]
            });
            assert_trace!(storage, resolved(id) => Trace {
                built: 21,
                changed: 19,
                dependencies: &[QueryKey::Indexed(id)]
            });
        }

        assert!(Arc::ptr_eq(&indexed_a, &indexed_b));
        assert!(Arc::ptr_eq(&indexed_b, &indexed_c));

        assert!(Arc::ptr_eq(&lowered_a, &lowered_b));
        assert!(Arc::ptr_eq(&lowered_b, &lowered_c));

        assert!(Arc::ptr_eq(&resolved_a, &resolved_b));
        assert!(Arc::ptr_eq(&resolved_b, &resolved_c));
    }

    #[test]
    fn test_local_state_cleanup() {
        let mut engine = QueryEngine::default();
        let mut files = Files::default();
        prim::configure(&mut engine, &mut files);

        let id = files.insert("./src/Main.purs", "module Main where\n\n\n\nlife = 42");
        let content = files.content(id);

        engine.set_content(id, content);
        let key = QueryKey::Parsed(id);

        let indexed_a = engine.indexed(id).unwrap();
        assert!(!engine.control.local.is_in_progress(key));

        let indexed_b = engine.indexed(id).unwrap();
        assert!(!engine.control.local.is_in_progress(key));

        assert_eq!(indexed_a, indexed_b);
    }

    #[test]
    fn test_cancellation_cleanup() {
        let mut engine = QueryEngine::default();
        let mut files = Files::default();
        prim::configure(&mut engine, &mut files);

        let id = files.insert("./src/Main.purs", "module Main where\n\n\n\nlife = 42");
        let key = QueryKey::Indexed(id);

        // Simulate the current thread starting a computation.
        {
            let mut storage = engine.storage.write();
            storage.derived.indexed.insert(id, DerivedState::in_progress(engine.control.id));
            engine.control.local.add_in_progress(key);
        }

        // Finally, enable cancellation and run the query on this thread.
        engine.control.global.cancelled.store(true, Ordering::Relaxed);
        let result = engine.query(
            key,
            |storage| storage.derived.indexed.get(&id),
            |storage| storage.derived.indexed.entry(id),
            |_| unreachable!("impossible."),
        );

        assert_eq!(result, Err(QueryError::Cancelled));

        // Observe that the storage has been edited.
        {
            let storage = engine.storage.read();
            assert!(!storage.derived.indexed.contains_key(&id));
        }
    }

    #[test]
    fn test_cancellation_no_cleanup() {
        let mut engine = QueryEngine::default();
        let mut files = Files::default();
        prim::configure(&mut engine, &mut files);

        let id = files.insert("./src/Main.purs", "module Main where\n\n\n\nlife = 42");
        let key = QueryKey::Indexed(id);

        // Simulate the current thread starting a computation.
        {
            let mut storage = engine.storage.write();
            storage.derived.indexed.insert(id, DerivedState::in_progress(engine.control.id));
            engine.control.local.add_in_progress(key);
        }

        // Finally, enable cancellation and run the query on another thread.
        engine.control.global.cancelled.store(true, Ordering::Relaxed);
        let result = std::thread::scope(|scope| {
            let runtime = engine.snapshot();
            let thread = scope.spawn(move || {
                runtime.query(
                    key,
                    |storage| storage.derived.indexed.get(&id),
                    |storage| storage.derived.indexed.entry(id),
                    |_| unreachable!("impossible."),
                )
            });
            thread.join().unwrap()
        });

        assert_eq!(result, Err(QueryError::Cancelled));

        // Observe that the storage is not edited.
        {
            let storage = engine.storage.read();
            assert!(storage.derived.indexed.contains_key(&id));
        }

        let result = engine.query(
            key,
            |storage| storage.derived.indexed.get(&id),
            |storage| storage.derived.indexed.entry(id),
            |_| unreachable!("impossible."),
        );

        assert_eq!(result, Err(QueryError::Cancelled));

        // Finally, observe that the storage is edited.
        {
            let storage = engine.storage.read();
            assert!(!storage.derived.indexed.contains_key(&id));
        }
    }

    #[test]
    fn test_cycle_detection() {
        const ID: FileId = FileId::from_raw(RawIdx::from_u32(0));
        const KEY: QueryKey = QueryKey::Resolved(ID);

        fn fake_query_a(engine: &QueryEngine) -> QueryResult<Arc<ResolvedModule>> {
            engine.query(
                QueryKey::Resolved(ID),
                |storage| storage.derived.resolved.get(&ID),
                |storage| storage.derived.resolved.entry(ID),
                fake_query_a,
            )
        }

        let engine = QueryEngine::default();
        let result = fake_query_a(&engine);
        assert_eq!(result, Err(QueryError::Cycle { stack: [KEY, KEY].into() }));
    }

    #[test]
    fn test_cycle_recovery() {
        const ID: FileId = FileId::from_raw(RawIdx::from_u32(0));

        fn fake_query_a(engine: &QueryEngine) -> QueryResult<Arc<ResolvedModule>> {
            engine.query(
                QueryKey::Resolved(ID),
                |storage| storage.derived.resolved.get(&ID),
                |storage| storage.derived.resolved.entry(ID),
                |engine| fake_query_a(engine).map_err(|_| QueryError::Cancelled),
            )
        }

        let engine = QueryEngine::default();
        let result = fake_query_a(&engine);
        assert!(matches!(result, Err(QueryError::Cancelled)));
    }

    #[test]
    fn test_snapshot_cycle_detection() {
        const ID_A: FileId = FileId::from_raw(RawIdx::from_u32(0));
        const ID_B: FileId = FileId::from_raw(RawIdx::from_u32(1));

        fn fake_query_a(engine: &QueryEngine) -> QueryResult<Arc<ResolvedModule>> {
            engine.query(
                QueryKey::Resolved(ID_A),
                |storage| storage.derived.resolved.get(&ID_A),
                |storage| storage.derived.resolved.entry(ID_A),
                fake_query_b,
            )
        }

        fn fake_query_b(engine: &QueryEngine) -> QueryResult<Arc<ResolvedModule>> {
            engine.query(
                QueryKey::Resolved(ID_B),
                |storage| storage.derived.resolved.get(&ID_B),
                |storage| storage.derived.resolved.entry(ID_B),
                fake_query_a,
            )
        }

        let engine = QueryEngine::default();

        let snapshot = engine.snapshot();
        let thread = std::thread::spawn(move || fake_query_b(&snapshot));

        let result_a = fake_query_a(&engine);
        let result_b = thread.join().unwrap();

        assert!(result_a.is_err());
        assert!(result_b.is_err());

        // Either result can return `Cancelled`, but at least one of should be `Cycle`
        assert!(
            [result_a, result_b]
                .iter()
                .any(|result| matches!(result, Err(QueryError::Cycle { .. })))
        );
    }

    #[test]
    fn test_snapshot_cycle_recovery() {
        const ID_A: FileId = FileId::from_raw(RawIdx::from_u32(0));
        const ID_B: FileId = FileId::from_raw(RawIdx::from_u32(1));

        fn fake_query_a(engine: &QueryEngine) -> QueryResult<Arc<ResolvedModule>> {
            engine.query(
                QueryKey::Resolved(ID_A),
                |storage| storage.derived.resolved.get(&ID_A),
                |storage| storage.derived.resolved.entry(ID_A),
                |engine| fake_query_b(engine).map_err(|_| QueryError::Cancelled),
            )
        }

        fn fake_query_b(engine: &QueryEngine) -> QueryResult<Arc<ResolvedModule>> {
            engine.query(
                QueryKey::Resolved(ID_B),
                |storage| storage.derived.resolved.get(&ID_B),
                |storage| storage.derived.resolved.entry(ID_B),
                |engine| fake_query_a(engine).map_err(|_| QueryError::Cancelled),
            )
        }

        let engine = QueryEngine::default();

        let snapshot = engine.snapshot();
        let thread = std::thread::spawn(move || fake_query_b(&snapshot));

        let result_a = fake_query_a(&engine);
        let result_b = thread.join().unwrap();

        assert!(matches!(result_a, Err(QueryError::Cancelled)));
        assert!(matches!(result_b, Err(QueryError::Cancelled)));
    }

    #[test]
    fn test_resolving_cycle() {
        let mut engine = QueryEngine::default();
        let mut files = Files::default();
        prim::configure(&mut engine, &mut files);

        let main = files.insert("Main.purs", "module Main where\n\nimport Lib (b)\n\na = 123");
        let library = files.insert("Lib.purs", "module Lib where\n\nimport Main (a)\n\nb = 123");

        engine.set_content(main, files.content(main));
        engine.set_content(library, files.content(library));
        engine.set_module_file("Main", main);
        engine.set_module_file("Lib", library);

        let result_a = engine.resolved(main);
        assert_eq!(
            result_a,
            Err(QueryError::Cycle {
                stack: [
                    QueryKey::Resolved(main),
                    QueryKey::Resolved(library),
                    QueryKey::Resolved(main)
                ]
                .into()
            })
        );

        let result_b = engine.resolved(library);
        assert_eq!(
            result_b,
            Err(QueryError::Cycle {
                stack: [
                    QueryKey::Resolved(library),
                    QueryKey::Resolved(main),
                    QueryKey::Resolved(library)
                ]
                .into()
            })
        );
    }
}
