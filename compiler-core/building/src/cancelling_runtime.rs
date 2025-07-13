// Requirements:
//
// 1. Derived queries can be ran in parallel
// 2. Derived queries can be cancelled
// 3. Derived queries need to be deduplicated
// 4. Input queries cancel in-flight derived queries
//
// Todo:
//
// 1. Implement a future/promise mechanism for deduplication
// 2. Implement runtime forking
// 3. Implement a signal and handling for cancellation.
//    For this, we'll probably use an atomic boolean that's
//    checked periodically.
//
// Some important concepts to grok
// 1. In salsa, runtimes are a bundle an ID, local state,
//    shared state, and a read lock.
// 2. When a runtime is forked, it increments the ID,
//    creates a new local state, clones the shared state,
//    and acquires a read lock.
// 3. When the runtime is dropped, make sure that the read
//    lock is dropped as well.
// 4. The local state just tracks which queries are
//    currently active using a stack
// 5. The shared state tracks a few more things, in particular,
//    it holds the counter for the next ID, the query lock
//    from which the runtime's read lock is derived from,
//    and the cancellation flag which coordinates cancellation
//    globally across all forked runtimes
// 6. In salsa, the runtime lives alongside the query storage,
//    which is different from our current formulation.
// 7. Then, salsa defines the database which holds the storage,
//    and exposes the functions that actually interact with
//    the runtime and storage mechanisms.
//
// Good, we've successfully implemented a future/promise mechanism
// This would allow us to create a mapping from keys to states
//
// The current implementation below demonstrates a fibonacci
// implementation with capabilities for cancellation using
// a result-based approach.
//
// Needs to be implemented:
// 1. Revision and dependency tracking
// 2. Cancellation after input writes
// 3. Dependency verification
//
// On revision on dependency tracking, much remains the same:
// updating inputs creates a new revision. Ideally we can store
// the revision as an atomic value, with storage backed by a
// read-write lock or a mutex. To prevent queries from accessing
// stale versions of inputs, a "query lock" is needed to act as
// a way to make read/writes more consistent.
//
// Cancellation can be implemented as an atomic boolean, which
// propagates through all queries across all threads. Special
// care needs to be taken to make sure that promises for ongoing
// threads are dropped once a computation fails to resolve.
//
// Finally, there's dependency verification—we take the revisions
// and dependencies tracked for any given query, then check if
// its dependencies need updating or if the cached value can be
// returned successfully.
//
// In both the blocking and parallel runtimes, we've designed
// traces to be stored homogeneously through the query keys.
// I think historically it was implemented this way to align
// more with the original paper's description of the implementation,
// but since it only stores the latest trace then storing said
// trace information alongside the actual values is perfectly
// acceptable.
//
// One complication that arises with this approach is that
// it combines storage for memoized values (i.e. the actual hashmaps)
// and storage for the synchronization mechanisms. This makes the
// future goal of cross-process incrementality a little bit more
// difficult. In terms of the actual code, I don't think much needs
// to be changed.
//
// We could simply panic if serialization is attempted for in-progress
// queries, or simply ignore them from being serialized.
//
// There's 3 states for a derived query to be in:
//
// enum QueryState<T> {
//   NotComputed,
//   InProgress {
//     promises: Mutex<Vec<Promise<T>>>,
//   },
//   Computed {
//     computed: T,
//     trace: Trace,
//   },
// }
//
// NotComputed/Computed are trivial—InProgress is a little more interesting.
// The idea with InProgress is to keep track of an internally mutable vector
// of promises that need to be fulfilled by the thread tasked with computing
// the query i.e. the first thread to read NotComputed
//
// Any thread that encounters an InProgress query would create a new future/
// promise pair, push the newly created promise, and block on the future.
//
// Note that when writing lookup functions for the query storage, we must avoid
// Option<QueryState<T>>, in favor of defaulting to NotComputed.
//
// In the parallel runtime, thread-local state is maintained, specifically for
// tracking dependencies between queries. We set the expectation that queries
// execute their dependencies on the same thread, which would make significantly
// easier to perform dependency tracking.
//
// Going back to the global query_lock approach, an important strategy that
// salsa employs is only acquiring a singular read lock from the query_lock
// per "snapshot" of the runtime, rather than on every query! It uses the raw
// API for parking_lot to implement a custom RAII structure that releases
// the lock when the snapshot is dropped.

mod promise;

use std::{
    sync::{
        atomic::{AtomicBool, AtomicUsize, Ordering},
        Arc,
    },
    time::Duration,
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

#[derive(Debug)]
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
struct QueryEngine {
    storage: Arc<RwLock<QueryStorage>>,
    control: QueryControl,
}

impl QueryEngine {
    fn snapshot(&self) -> QueryEngine {
        let storage = self.storage.clone();
        let control = self.control.snapshot();
        QueryEngine { storage, control }
    }
}

impl QueryEngine {
    fn set_content(&self, k: usize, v: impl Into<Arc<str>>) {
        self.control.global.cancelled.store(true, Ordering::Relaxed);
        let _query_lock = self.control.global.query_lock.write();

        let changed = self.control.global.revision.fetch_add(1, Ordering::Relaxed);
        let input = InputState { value: v.into(), changed: changed + 1 };
        self.storage.write().input.content.insert(k, input);

        self.control.global.cancelled.store(false, Ordering::Relaxed);
    }

    fn content(&self, k: usize) -> Arc<str> {
        if let Some(InputState { value, .. }) = self.storage.read().input.content.get(&k) {
            Arc::clone(value)
        } else {
            unreachable!("invariant violated: invalid {}", k);
        }
    }

    fn lines(&self, k: usize) -> usize {
        if self.control.global.cancelled.load(Ordering::Relaxed) {
            return usize::MAX;
        }

        let storage = self.storage.read();
        match storage.derived.lines.get(&k).unwrap_or(&DerivedState::NotComputed) {
            DerivedState::NotComputed => {
                drop(storage);

                {
                    let mut storage = self.storage.write();
                    storage.derived.lines.insert(k, DerivedState::in_progress());
                }

                let content = self.content(k);
                let computed = content.lines().count();

                {
                    let mut storage = self.storage.write();
                    if let Some(DerivedState::InProgress { promises }) =
                        storage.derived.lines.remove(&k)
                    {
                        let promises = promises.into_inner();
                        promises.into_iter().for_each(|promise| promise.fulfill(computed));
                    } else {
                        unreachable!("invariant violated: expected InProgress");
                    }

                    let revision = self.control.global.revision.load(Ordering::Relaxed);
                    let trace = Trace { built: revision, changed: revision };
                    storage.derived.lines.insert(k, DerivedState::Computed { computed, trace });
                }

                computed
            }
            DerivedState::InProgress { promises } => {
                let (future, promise) = Future::new();
                promises.lock().push(promise);

                drop(storage);
                future.wait().unwrap_or(usize::MAX)
            }
            DerivedState::Computed { computed, trace } => {
                // If this query was built during the current revision i.e.
                // we've already called this query, we can skip revalidation.
                let revision = self.control.global.revision.load(Ordering::Relaxed);
                if trace.built == revision {
                    return *computed;
                }

                drop(storage);

                // If multiple threads were to end up needing to revalidate
                // the current query, we make sure to use upgradable_read
                // such that only a single thread can proceed with actually
                // computing the query.
                let storage = self.storage.upgradable_read();
                match storage.derived.lines.get(&k).unwrap_or(&DerivedState::NotComputed) {
                    DerivedState::NotComputed => {
                        unreachable!("invariant violated: invalid revalidation state");
                    }
                    DerivedState::InProgress { promises } => {
                        let (future, promise) = Future::new();
                        promises.lock().push(promise);

                        drop(storage);
                        future.wait().unwrap_or(usize::MAX)
                    }
                    DerivedState::Computed { .. } => {
                        // TODO: Our current formulation of step traces in
                        // the single-thread runtime involves revalidation
                        // of dependencies by calling them. However, this
                        // risks a deadlock since the dependencies themselves
                        // might need to acquire the same upgradable_read
                        // lock that we're currently holding. We could release
                        // this lock temporarily, but releasing said lock
                        // will signal other threads to proceed, which will
                        // cause duplicated work.
                        //
                        // We should be able to alleviate this by making
                        // the current query InProgress before dropping
                        // the lock, ensuring that other threads only
                        // have to wait for the result.
                        {
                            let mut storage = RwLockUpgradableReadGuard::upgrade(storage);
                            storage.derived.lines.insert(k, DerivedState::in_progress());
                        }

                        let content = self.content(k);
                        let computed = content.lines().count();

                        {
                            let mut storage = self.storage.write();
                            if let Some(DerivedState::InProgress { promises }) =
                                storage.derived.lines.remove(&k)
                            {
                                let promises = promises.into_inner();
                                promises.into_iter().for_each(|promise| promise.fulfill(computed));
                            } else {
                                unreachable!("invariant violated: expected InProgress");
                            }

                            let revision = self.control.global.revision.load(Ordering::Relaxed);
                            let trace = Trace { built: revision, changed: revision };
                            storage
                                .derived
                                .lines
                                .insert(k, DerivedState::Computed { computed, trace });
                        }

                        computed
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

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
        assert_eq!(engine.lines(0), 2);
    }

    #[test]
    fn query_engine_parallel() {
        let engine = QueryEngine::default();
        engine.set_content(0, "abc\ndef");

        let mut threads = vec![];
        for _ in 0..1024 {
            let engine = engine.snapshot();
            threads.push(std::thread::spawn(move || {
                assert_eq!(engine.lines(0), 2);
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
        assert_eq!(engine.lines(0), 2);
        engine.set_content(0, "abc\ndef\nghi");

        let mut threads = vec![];
        for _ in 0..1024 {
            let engine = engine.snapshot();
            threads.push(std::thread::spawn(move || {
                assert_eq!(engine.lines(0), 3);
            }))
        }

        for thread in threads {
            assert!(thread.join().is_ok());
        }
    }
}
