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

use std::sync::{
    atomic::{AtomicBool, Ordering},
    Arc,
};

use parking_lot::{Mutex, RwLock};
use promise::Promise;
use rustc_hash::FxHashMap;

mod promise;

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

struct Runtime {
    cancelled: AtomicBool,
    global_lock: RwLock<()>,
    input_storage: Arc<RwLock<InputStorage>>,
    query_storage: Arc<RwLock<QueryStorage>>,
}

impl Runtime {
    fn set_content(&mut self, k: usize, v: impl Into<Arc<str>>) {
        // Using an AtomicBool seems like a good idea at first, but then how do
        // we revert it back to false while also making sure that _all_ derived
        // queries have finished execution? I suppose that if a query were to
        // acquire the `global_lock` under a read first and foremost, then it
        // would present some guarantee that it would read `cancelled` as true,
        // thus failing immediately. I suppose the `Ordering` that we provide to
        // the atomic store/load operations would have an effect as well.
        //
        // The alternative would be to take advantage of revision numbers,
        // where we can store the current revision and the pending revision,
        // the latter of which we increment during input setting.
        //
        // Queries can then compare the current revision and pending revision
        // from time to time to determine if they should be cancelled.
        //
        // Going back to the AtomicBool approach, if a thread is able to acquire
        // a read lock on the global lock before a thread was able to acquire a
        // write lock, there's a chance that the value of `cancelled` that it
        // would read is false, thus allowing it to continue execution. In the
        // instance that the thread needed to compute dependencies, those dependencies
        // would be able to acquire a recursive read lock on the global lock,
        // but at this point `cancelled` might have been already set to true!
        // Thus, an Err is immediately returned which propagates up to the top-level
        // query. Finally, the read lock is released, freeing up the write lock.
        // Consider that the write lock guarantees no other threads can execute
        // queries: any thread spawned during the lifetime of the write lock does
        // not to be cancelled, and the fact that we were able to acquire a write
        // lock in the first place tells us that there are no other threads holding
        // a read lock.
        self.cancelled.store(true, Ordering::SeqCst);
        let _global_lock = self.global_lock.write();
        self.cancelled.store(false, Ordering::SeqCst);
        self.input_storage.write().content.insert(k, v.into());
    }
}

// use std::sync::atomic::{AtomicBool, Ordering};
// use std::sync::Arc;
//
// use parking_lot::Mutex;
// use promise::{Future, Promise};
// use rustc_hash::FxHashMap;
//
// #[derive(Debug)]
// enum State {
//     InProgress(Mutex<Vec<Promise<usize>>>),
//     Memoized(usize),
// }
//
// fn fibonacci_with_hook<F>(
//     state: Arc<Mutex<FxHashMap<usize, State>>>,
//     cancelled: Arc<AtomicBool>,
//     n: usize,
//     hook: F,
// ) -> Option<usize>
// where
//     F: Fn(usize) + Clone,
// {
//     // Call the hook with current n
//     hook(n);
//
//     // Check cancellation at the start
//     if cancelled.load(Ordering::Relaxed) {
//         return None;
//     }
//
//     if n <= 1 {
//         return Some(n);
//     }
//
//     // Check if already computed or in progress
//     {
//         let mut guard = state.lock();
//         match guard.get(&n) {
//             Some(State::Memoized(value)) => {
//                 let value = *value;
//                 return Some(value);
//             }
//             Some(State::InProgress(promises)) => {
//                 // Another thread is computing, create a future and add our promise
//                 let (future, promise) = Future::new();
//                 promises.lock().push(promise);
//
//                 // This is important because `wait` blocks execution for the current
//                 // thread and would hold the guard indefinitely. The computing thread
//                 // needs to be able to acquire the `state` to read the promises.
//                 drop(guard);
//                 return future.wait();
//             }
//             None => {
//                 guard.insert(n, State::InProgress(Mutex::new(Vec::new())));
//             }
//         }
//     };
//
//     // If the computation falls through, make sure that the cancellation is
//     // propagated across the waiting futures by dropping the promises that
//     // have been collected so far.
//     let Some(n_1) = fibonacci_with_hook(state.clone(), cancelled.clone(), n - 1, hook.clone())
//     else {
//         state.lock().remove(&n);
//         return None;
//     };
//     let Some(n_2) = fibonacci_with_hook(state.clone(), cancelled.clone(), n - 2, hook.clone())
//     else {
//         state.lock().remove(&n);
//         return None;
//     };
//     let result = n_1 + n_2;
//
//     // Fulfill all waiting promises and memoize
//     {
//         let mut guard = state.lock();
//         if let Some(State::InProgress(promises)) = guard.remove(&n) {
//             let promises = promises.into_inner();
//             promises.into_iter().for_each(|promise| promise.fulfill(result));
//             guard.insert(n, State::Memoized(result));
//         } else {
//             unreachable!("invariant violated: expected InProgress");
//         }
//     };
//
//     Some(result)
// }
//
// fn fibonacci(
//     state: Arc<Mutex<FxHashMap<usize, State>>>,
//     cancelled: Arc<AtomicBool>,
//     n: usize,
// ) -> Option<usize> {
//     fibonacci_with_hook(state, cancelled, n, |_| {})
// }
//
// #[test]
// fn test_fibonacci() {
//     let state1 = Arc::new(Mutex::new(FxHashMap::default()));
//     let cancelled = Arc::new(AtomicBool::new(false));
//
//     let mut threads = vec![];
//     for _ in 0..10_000 {
//         let state2 = state1.clone();
//         let cancelled2 = cancelled.clone();
//         let a = std::thread::spawn(move || fibonacci(state2, cancelled2, 100));
//         threads.push(a);
//     }
//
//     threads.into_iter().for_each(|thread| {
//         thread.join().unwrap();
//     });
// }
//
// #[test]
// fn test_fibonacci_cancellation() {
//     let state = Arc::new(Mutex::new(FxHashMap::default()));
//     let cancelled = Arc::new(AtomicBool::new(false));
//
//     // Pre-cancel before starting computation
//     cancelled.store(true, Ordering::Relaxed);
//
//     // Should return None immediately due to cancellation check
//     let result = fibonacci(state.clone(), cancelled.clone(), 100);
//     assert_eq!(result, None);
// }
//
// #[test]
// fn test_fibonacci_cancellation_propagation() {
//     let state = Arc::new(Mutex::new(FxHashMap::default()));
//     let cancelled = Arc::new(AtomicBool::new(false));
//
//     // Compute a value first
//     let result = fibonacci(state.clone(), cancelled.clone(), 10);
//     assert_eq!(result, Some(55));
//
//     // Now cancel and try to compute again
//     cancelled.store(true, Ordering::Relaxed);
//
//     // Should return None for new computations
//     let result = fibonacci(state.clone(), cancelled.clone(), 15);
//     assert_eq!(result, None);
//
//     // Should also return None for previously computed values
//     let result = fibonacci(state.clone(), cancelled.clone(), 10);
//     assert_eq!(result, None);
// }
//
// #[test]
// fn test_fibonacci_cancellation_mid_computation() {
//     use std::sync::{Arc, Barrier};
//
//     let state = Arc::new(Mutex::new(FxHashMap::default()));
//     let cancelled = Arc::new(AtomicBool::new(false));
//
//     // Use a barrier to synchronize between threads
//     let barrier = Arc::new(Barrier::new(2));
//
//     // Start computing in a separate thread with a synchronization hook
//     let state1 = state.clone();
//     let cancelled1 = cancelled.clone();
//     let barrier1 = barrier.clone();
//     let handle1 = std::thread::spawn(move || {
//         fibonacci_with_hook(state1, cancelled1, 5, |n| {
//             // Signal when we reach n=15 (middle of computation)
//             if n == 2 {
//                 // First barrier: signal we've reached the target
//                 barrier1.wait();
//                 // Second barrier: wait for cancellation to be set
//                 barrier1.wait();
//             }
//         })
//     });
//
//     let state2 = state.clone();
//     let cancelled2 = cancelled.clone();
//     let barrier2 = barrier.clone();
//     let handle2 = std::thread::spawn(move || {
//         fibonacci_with_hook(state2, cancelled2, 5, |n| {
//             // Signal when we reach n=15 (middle of computation)
//             if n == 2 {
//                 // First barrier: signal we've reached the target
//                 barrier2.wait();
//                 // Second barrier: wait for cancellation to be set
//                 barrier2.wait();
//             }
//         })
//     });
//
//     // Wait for the computation to reach the synchronization point
//     barrier.wait();
//
//     // Now we know the computation is at n=15, set cancellation
//     cancelled.store(true, Ordering::Relaxed);
//
//     // Release the computation thread to continue
//     barrier.wait();
//
//     // Should return None due to cancellation
//     let result1 = handle1.join().unwrap();
//     assert_eq!(result1, None);
//
//     let result2 = handle2.join().unwrap();
//     assert_eq!(result2, None);
// }
