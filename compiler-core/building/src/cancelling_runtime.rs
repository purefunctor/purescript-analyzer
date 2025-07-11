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

use std::sync::{
    atomic::{AtomicBool, Ordering},
    Arc,
};

use lock_api::{RawRwLock, RawRwLockRecursive};
use parking_lot::{Mutex, RwLock};
use promise::Promise;
use rustc_hash::FxHashMap;

enum QueryKey {
    Content(usize),
    Lines(usize),
    Analyse(usize),
}

enum QueryState<T> {
    NotComputed,
    InProgress { promises: Mutex<Vec<Promise<T>>> },
    Computed { computed: T },
}

struct InputStorage {
    content: FxHashMap<usize, Arc<str>>,
}

struct QueryStorage {
    lines: FxHashMap<usize, QueryState<usize>>,
    analyse: FxHashMap<usize, QueryState<usize>>,
}

#[derive(Default)]
struct GlobalState {
    /// An atomic token that determines if query execution had been cancelled.
    cancelled: AtomicBool,
    /// A global read-write lock for enforcing the order of reads and writes.
    query_lock: RwLock<()>,
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

struct QueryControl {
    guard: Option<QueryControlGuard>,
    local: Arc<LocalState>,
    global: Arc<GlobalState>,
}

impl QueryControl {
    fn new() -> QueryControl {
        let guard = None;
        let local = Arc::new(LocalState::default());
        let global = Arc::new(GlobalState::default());
        QueryControl { guard, local, global }
    }

    fn snapshot(&self) -> QueryControl {
        let guard = Some(QueryControlGuard::new(&self.global));
        let local = Arc::new(LocalState::default());
        let global = Arc::clone(&self.global);
        QueryControl { guard, local, global }
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use super::{GlobalState, QueryControlGuard};

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
}
