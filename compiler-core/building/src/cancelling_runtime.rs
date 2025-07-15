mod promise;

use std::{
    cell::RefCell,
    collections::hash_map::Entry,
    sync::{
        atomic::{AtomicBool, AtomicUsize, Ordering},
        Arc,
    },
};

use building_types::{QueryError, QueryResult};
use lock_api::{RawRwLock, RawRwLockRecursive};
use parking_lot::{Mutex, RwLock, RwLockUpgradableReadGuard};
use promise::{Future, Promise};
use rustc_hash::{FxHashMap, FxHashSet};
use thread_local::ThreadLocal;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum QueryKey {
    Content(usize),
    Lines(usize),
    Analyse(usize),
}

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
        promises: Mutex<Vec<Promise<T>>>,
    },
    Computed {
        computed: T,
        trace: Trace,
        dependencies: Arc<[QueryKey]>,
    },
}

impl<T> DerivedState<T> {
    fn in_progress() -> DerivedState<T> {
        DerivedState::InProgress { promises: Mutex::default() }
    }
}

#[derive(Debug)]
struct InputState<T> {
    value: T,
    changed: usize,
}

#[derive(Default)]
struct InputStorage {
    content: FxHashMap<usize, InputState<Arc<str>>>,
}

#[derive(Default)]
struct DerivedStorage {
    lines: FxHashMap<usize, DerivedState<usize>>,
    analyse: FxHashMap<usize, DerivedState<usize>>,
}

#[derive(Default)]
struct GlobalState {
    /// An atomic token that determines if query execution had been cancelled.
    cancelled: AtomicBool,
    /// A global read-write lock for enforcing the order of reads and writes.
    query_lock: RwLock<()>,
    /// A counter that tracks the current revision of the query engine.
    revision: AtomicUsize,
}

#[derive(Default)]
struct LocalState {
    inner: ThreadLocal<RefCell<LocalStateInner>>,
}

impl LocalState {
    fn with_current<T>(&self, current: QueryKey, f: impl FnOnce() -> T) -> T {
        let inner = self.inner.get_or_default();
        let previous = inner.borrow_mut().current.replace(current);
        let result = f();
        inner.borrow_mut().current = previous;
        result
    }

    fn with_dependency(&self, dependency: QueryKey) {
        let mut inner = self.inner.get_or_default().borrow_mut();
        if let Some(current) = inner.current {
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
}

#[derive(Debug, Default)]
struct LocalStateInner {
    current: Option<QueryKey>,
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

#[derive(Default)]
struct QueryControl {
    guard: Option<QueryControlGuard>,
    local: Arc<LocalState>,
    global: Arc<GlobalState>,
}

impl QueryControl {
    fn snapshot(&self) -> QueryControl {
        let guard = Some(QueryControlGuard::new(&self.global));
        let local = Arc::new(LocalState::default());
        let global = Arc::clone(&self.global);
        QueryControl { guard, local, global }
    }
}

#[derive(Default)]
struct QueryStorage {
    input: InputStorage,
    derived: DerivedStorage,
}

#[derive(Default)]
pub struct QueryEngine {
    storage: Arc<RwLock<QueryStorage>>,
    control: QueryControl,
}

impl QueryEngine {
    pub fn snapshot(&self) -> QueryEngine {
        let storage = self.storage.clone();
        let control = self.control.snapshot();
        QueryEngine { storage, control }
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
            self.query_core(key, &get, &get_mut, &compute).map_err(
                |QueryError::Cancelled { cleanup }| {
                    if cleanup {
                        let mut storage = self.storage.write();
                        if let Entry::Occupied(o) = get_mut(&mut storage) {
                            if let DerivedState::InProgress { promises } = o.remove() {
                                drop(promises);
                                drop(storage);
                            } else {
                                unreachable!("invariant violated: expected InProgress");
                            }
                        }
                    }
                    // Dependent queries must perform cleanup regardless of
                    // whether cleanup was executed for this query. For instance:
                    //
                    // Thread 2: B
                    // Thread 1: A -> B
                    //
                    // Once cancelled, Thread 2 executes cleanup for query B.
                    // The cancellation propagates to Thread 1, where cleanup
                    // is skipped for query B, but executed for query A.
                    QueryError::Cancelled { cleanup: true }
                },
            )
        })
    }

    /// Fulfills the promises of an [`DerivedState::InProgress`] query and
    /// replaces it with a [`DerivedState::Computed`] result in the store.
    fn fulfill_and_store<K, V, GetMutFn>(
        &self,
        get_mut: &GetMutFn,
        key: QueryKey,
        computed: V,
        trace: Trace,
    ) where
        GetMutFn: Fn(&mut QueryStorage) -> Entry<K, DerivedState<V>>,
        V: Clone,
    {
        let mut storage = self.storage.write();
        if let Entry::Occupied(o) = get_mut(&mut storage) {
            if let DerivedState::InProgress { promises } = o.remove() {
                let promises = promises.into_inner();
                promises.into_iter().for_each(|promise| {
                    let computed = V::clone(&computed);
                    promise.fulfill(computed);
                });
            } else {
                unreachable!("invariant violated: expected InProgress");
            }
        }

        let dependencies = self.control.local.dependencies(key);
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
            return Err(QueryError::Cancelled { cleanup: true });
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
                self.fulfill_and_store(get_mut, key, V::clone(&previous), trace);
                Ok(previous)
            }
            _ => {
                let trace = Trace { built: revision, changed: revision };
                self.fulfill_and_store(get_mut, key, V::clone(&computed), trace);
                Ok(computed)
            }
        }
    }

    /// Verifies the given dependencies by executing them, returning the
    /// timestamp of the most latest change.
    fn verify_core(&self, dependencies: &[QueryKey]) -> Result<usize, QueryError> {
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
                QueryKey::Lines(k) => derived_changed!(lines, k),
                QueryKey::Analyse(k) => derived_changed!(analyse, k),
            }
        }

        Ok(latest)
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
                DerivedState::InProgress { promises } => {
                    let (future, promise) = Future::new();
                    promises.lock().push(promise);

                    // Remember that Future::wait blocks the current thread!
                    drop(storage);

                    return future.wait().ok_or(QueryError::Cancelled { cleanup: false });
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
                        get_mut(&mut storage).insert_entry(DerivedState::in_progress());
                    }

                    self.compute_core(get_mut, compute, key, revision, None)
                }
                DerivedState::InProgress { promises } => {
                    let (future, promise) = Future::new();
                    promises.lock().push(promise);

                    // Remember that Future::wait blocks the current thread!
                    drop(storage);

                    future.wait().ok_or(QueryError::Cancelled { cleanup: false })
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
                        get_mut(&mut storage).insert_entry(DerivedState::in_progress());
                    }

                    let latest = self.verify_core(&dependencies)?;

                    // If the cached value was built more recently the the
                    // latest change, we can update its built timestamp to
                    // the current revision. This allows the query to hit
                    // the fastest path if it's called in the same revision.
                    if trace.built >= latest {
                        let trace = Trace { built: revision, ..trace };
                        self.fulfill_and_store(get_mut, key, V::clone(&computed), trace);
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

        let changed = self.control.global.revision.fetch_add(1, Ordering::AcqRel);
        let state = InputState { value: v, changed: changed + 1 };

        let mut storage = self.storage.write();
        f(&mut storage).insert_entry(state);

        self.control.global.cancelled.store(false, Ordering::Relaxed);
    }

    fn get_input<V, F>(&self, k: QueryKey, f: F) -> V
    where
        F: FnOnce(&QueryStorage) -> Option<&InputState<V>>,
        V: Clone,
    {
        self.control.local.with_dependency(k);
        let storage = self.storage.read();
        let state = f(&storage).unwrap_or_else(|| {
            panic!("invariant violated: missing input {:?}", k);
        });
        V::clone(&state.value)
    }
}

impl QueryEngine {
    pub fn set_content(&self, k: usize, v: impl Into<Arc<str>>) {
        self.set_input(|storage| storage.input.content.entry(k), v.into());
    }

    pub fn content(&self, k: usize) -> Arc<str> {
        self.get_input(QueryKey::Content(k), |storage| storage.input.content.get(&k))
    }

    pub fn lines(&self, k: usize) -> Result<usize, QueryError> {
        self.query(
            QueryKey::Lines(k),
            |storage| storage.derived.lines.get(&k),
            |storage| storage.derived.lines.entry(k),
            |this| {
                let content = this.content(k);
                Ok(content.lines().count())
            },
        )
    }

    pub fn analyse(&self, k: usize) -> Result<usize, QueryError> {
        self.query(
            QueryKey::Analyse(k),
            |storage| storage.derived.analyse.get(&k),
            |storage| storage.derived.analyse.entry(k),
            |this| {
                let lines = this.lines(k)?;
                Ok(lines * 2)
            },
        )
    }
}

#[cfg(test)]
mod tests {
    use std::{sync::Arc, time::Duration};

    use super::{DerivedState, GlobalState, QueryControlGuard, QueryEngine, Trace};

    #[test]
    fn query_control_guard_holds() {
        let global = Arc::new(GlobalState::default());
        let guard = QueryControlGuard::new(&global);
        assert!(global.query_lock.try_write().is_none());
        drop(guard);
    }

    #[test]
    fn query_control_guard_drops() {
        let global = Arc::new(GlobalState::default());
        let guard = QueryControlGuard::new(&global);
        drop(guard);
        assert!(global.query_lock.try_write().is_some());
    }

    #[test]
    fn query_engine() {
        let engine = QueryEngine::default();
        engine.set_content(0, "abc\ndef");
        assert_eq!(engine.lines(0), Ok(2));
    }

    #[test]
    fn query_engine_parallel() {
        let engine = QueryEngine::default();
        engine.set_content(0, "abc\ndef");

        let mut threads = vec![];
        for _ in 0..1024 {
            let engine = engine.snapshot();
            threads.push(std::thread::spawn(move || {
                assert_eq!(engine.lines(0), Ok(2));
            }))
        }

        for thread in threads {
            assert!(thread.join().is_ok());
        }
    }

    #[test]
    fn query_engine_validate() {
        let engine = QueryEngine::default();

        engine.set_content(0, "abc\ndef");
        assert_eq!(engine.lines(0), Ok(2));
        engine.set_content(0, "abc\ndef\nghi");

        let mut threads = vec![];
        for _ in 0..1024 {
            let engine = engine.snapshot();
            threads.push(std::thread::spawn(move || {
                assert_eq!(engine.lines(0), Ok(3));
            }))
        }

        for thread in threads {
            assert!(thread.join().is_ok());
        }
    }

    #[test]
    fn query_engine_stress() {
        let global_engine = Arc::new(QueryEngine::default());
        global_engine.set_content(0, "abc\ndef");

        let mut threads = vec![];

        // NOTE: snapshot is only used for threads that need to acquire
        // read-only access for the current revision. We should enforce
        // this using a type state pattern in the future.
        let engine = global_engine.clone();
        let _infinite = std::thread::spawn(move || loop {
            // This test is intentionally flaky to simulate unpredictability
            // in real-world use cases. It just needs to be random enough that
            // it triggers both cases. However, this also makes coverage very
            // volatile so ideally it should be replaced by more thorough test
            // cases that make use of Barrier.
            std::thread::sleep(Duration::from_nanos(10));
            engine.set_content(0, "ghi\njkl");
        });

        for _ in 0..1024 {
            let engine = global_engine.clone();
            threads.push(std::thread::spawn(move || {
                let engine = engine.snapshot();
                let lines = engine.lines(0);
                let success = lines.is_ok();
                let failure = lines.is_err();
                assert!(success || failure);
            }));
        }

        for thread in threads {
            assert!(thread.join().is_ok());
        }
    }

    #[test]
    fn query_engine_deep() {
        let engine = QueryEngine::default();

        engine.set_content(0, "abc\ndef");
        assert_eq!(engine.analyse(0), Ok(4));

        {
            let storage = engine.storage.read();
            assert!(matches!(
                storage.derived.lines.get(&0),
                Some(DerivedState::Computed {
                    computed: 2,
                    trace: Trace { built: 1, changed: 1 },
                    dependencies: _
                })
            ));
            assert!(matches!(
                storage.derived.analyse.get(&0),
                Some(DerivedState::Computed {
                    computed: 4,
                    trace: Trace { built: 1, changed: 1 },
                    dependencies: _,
                })
            ));
        }

        engine.set_content(0, "def\nghi");
        assert_eq!(engine.analyse(0), Ok(4));

        {
            let storage = engine.storage.read();
            assert!(matches!(
                storage.derived.lines.get(&0),
                Some(DerivedState::Computed {
                    computed: 2,
                    trace: Trace { built: 2, changed: 1 },
                    dependencies: _
                })
            ));
            assert!(matches!(
                storage.derived.analyse.get(&0),
                Some(DerivedState::Computed {
                    computed: 4,
                    trace: Trace { built: 2, changed: 1 },
                    dependencies: _
                })
            ));
        }
    }
}
