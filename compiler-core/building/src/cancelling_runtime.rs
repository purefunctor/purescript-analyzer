mod promise;

use std::{
    collections::hash_map::Entry,
    sync::{
        atomic::{AtomicBool, AtomicUsize, Ordering},
        Arc,
    },
};

use lock_api::{RawRwLock, RawRwLockRecursive};
use parking_lot::{Mutex, RwLock, RwLockUpgradableReadGuard};
use promise::{Future, Promise};
use rustc_hash::FxHashMap;

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
    },
}

impl<T> DerivedState<T> {
    fn in_progress() -> DerivedState<T> {
        DerivedState::InProgress { promises: Mutex::default() }
    }
}

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
struct LocalState {}

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

#[derive(Debug, PartialEq, Eq)]
pub enum QueryError {
    Cancelled { cleanup: bool },
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
        get: GetFn,
        get_mut: GetMutFn,
        compute: ComputeFn,
        dependencies: &[QueryKey],
    ) -> Result<V, QueryError>
    where
        GetFn: Fn(&QueryStorage) -> Option<&DerivedState<V>>,
        GetMutFn: Fn(&mut QueryStorage) -> Entry<K, DerivedState<V>>,
        ComputeFn: Fn(&QueryEngine) -> Result<V, QueryError>,
        V: Eq + Clone,
    {
        // If query execution fails at any given point, clean up the state.
        self.query_core(&get, &get_mut, &compute, dependencies).inspect_err(
            |QueryError::Cancelled { cleanup }| {
                if *cleanup {
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
            },
        )
    }

    fn compute_core<K, V, GetMutFn, ComputeFn>(
        &self,
        get_mut: &GetMutFn,
        compute: &ComputeFn,
        revision: usize,
        previous: Option<(V, Trace)>,
    ) -> Result<V, QueryError>
    where
        GetMutFn: Fn(&mut QueryStorage) -> Entry<K, DerivedState<V>>,
        ComputeFn: Fn(&QueryEngine) -> Result<V, QueryError>,
        V: Eq + Clone,
    {
        if self.control.global.cancelled.load(Ordering::Relaxed) {
            return Err(QueryError::Cancelled { cleanup: true });
        }

        let computed = compute(self)?;

        // If the current computed result is equal to the previous one, we
        // don't need to update the changed timestamp of the query. We also
        // insert the previous value back into the cache. This is a niche
        // but useful optimisation for when V = Arc<T> as it means pointer
        // equality is checked first before structural equality.
        let state = match previous {
            Some((previous, trace)) if computed == previous => DerivedState::Computed {
                computed: previous,
                trace: Trace { built: revision, changed: trace.changed },
            },
            _ => DerivedState::Computed {
                computed: V::clone(&computed),
                trace: Trace { built: revision, changed: revision },
            },
        };

        // Invariant Check!
        //
        // If we reach this code path and observe that the query state was
        // updated somewhere else, we crash the entire program. This is
        // dependent on the invariant that only one thread can acquire an
        // upgradable read lock at any given time.
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
            get_mut(&mut storage).insert_entry(state);
        }

        Ok(computed)
    }

    /// Verify the given dependencies by executing them,
    /// returning the timestamp of the most latest change.
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
        get: &GetFn,
        get_mut: &GetMutFn,
        compute: &ComputeFn,
        dependencies: &[QueryKey],
    ) -> Result<V, QueryError>
    where
        GetFn: Fn(&QueryStorage) -> Option<&DerivedState<V>>,
        GetMutFn: Fn(&mut QueryStorage) -> Entry<K, DerivedState<V>>,
        ComputeFn: Fn(&QueryEngine) -> Result<V, QueryError>,
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
                DerivedState::Computed { computed, trace } => {
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
                    // upgradable read lock should read that the query is in progress.
                    {
                        let mut storage = RwLockUpgradableReadGuard::upgrade(storage);
                        get_mut(&mut storage).insert_entry(DerivedState::in_progress());
                    }

                    self.compute_core(get_mut, compute, revision, None)
                }
                DerivedState::InProgress { promises } => {
                    let (future, promise) = Future::new();
                    promises.lock().push(promise);

                    // Remember that Future::wait blocks the current thread!
                    drop(storage);

                    future.wait().ok_or(QueryError::Cancelled { cleanup: false })
                }
                DerivedState::Computed { computed, trace } => {
                    let computed = V::clone(computed);
                    let trace = *trace;

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

                    let latest = self.verify_core(dependencies)?;

                    // If the cached value was built more recently the the
                    // latest change, we can update its built timestamp to
                    // the current revision. This allows the query to hit
                    // the fastest path if it's called in the same revision.
                    if trace.built >= latest {
                        let mut storage = self.storage.write();
                        if let Entry::Occupied(o) = get_mut(&mut storage) {
                            if let DerivedState::Computed { trace, .. } = o.into_mut() {
                                trace.built = self.control.global.revision.load(Ordering::Relaxed);
                            }
                        }
                        return Ok(computed);
                    }

                    self.compute_core(get_mut, compute, revision, Some((computed, trace)))
                }
            }
        }
    }
}

impl QueryEngine {
    pub fn set_content(&self, k: usize, v: impl Into<Arc<str>>) {
        self.control.global.cancelled.store(true, Ordering::Relaxed);
        let _query_lock = self.control.global.query_lock.write();

        let changed = self.control.global.revision.fetch_add(1, Ordering::Relaxed);
        let input = InputState { value: v.into(), changed: changed + 1 };
        self.storage.write().input.content.insert(k, input);

        self.control.global.cancelled.store(false, Ordering::Relaxed);
    }

    pub fn content(&self, k: usize) -> Arc<str> {
        if let Some(InputState { value, .. }) = self.storage.read().input.content.get(&k) {
            Arc::clone(value)
        } else {
            unreachable!("invariant violated: invalid {}", k);
        }
    }

    pub fn lines(&self, k: usize) -> Result<usize, QueryError> {
        self.query(
            |storage| storage.derived.lines.get(&k),
            |storage| storage.derived.lines.entry(k),
            |this| {
                let content = this.content(k);
                Ok(content.lines().count())
            },
            &[QueryKey::Content(k)],
        )
    }

    pub fn analyse(&self, _k: usize) -> Result<usize, QueryError> {
        Err(QueryError::Cancelled { cleanup: false })
    }
}

#[cfg(test)]
mod tests {
    use std::{sync::Arc, time::Duration};

    use super::{GlobalState, QueryControlGuard, QueryEngine};

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
            let engine = global_engine.snapshot();
            threads.push(std::thread::spawn(move || {
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
}
