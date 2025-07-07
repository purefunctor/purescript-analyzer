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

mod promise;

use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

use parking_lot::Mutex;
use promise::{Future, Promise};
use rustc_hash::FxHashMap;

#[derive(Debug)]
enum State {
    InProgress(Mutex<Vec<Promise<usize>>>),
    Memoized(usize),
}

fn fibonacci_with_hook<F>(
    state: Arc<Mutex<FxHashMap<usize, State>>>,
    cancelled: Arc<AtomicBool>,
    n: usize,
    hook: F,
) -> Option<usize>
where
    F: Fn(usize) + Clone,
{
    // Call the hook with current n
    hook(n);

    // Check cancellation at the start
    if cancelled.load(Ordering::Relaxed) {
        return None;
    }

    if n <= 1 {
        return Some(n);
    }

    // Check if already computed or in progress
    let future = {
        let mut guard = state.lock();
        match guard.get_mut(&n) {
            Some(State::Memoized(value)) => {
                let value = *value;
                return Some(value);
            }
            Some(State::InProgress(promises)) => {
                // Another thread is computing, create a future and add our promise
                let (future, promise) = Future::new();
                promises.lock().push(promise);
                Some(future)
            }
            None => {
                // We're the first, start computing
                guard.insert(n, State::InProgress(Mutex::new(Vec::new())));
                None
            }
        }
    };

    // If we have a future, wait for another thread to compute
    if let Some(future) = future {
        return future.wait();
    }

    // If the computation falls through, make sure that the cancellation is
    // propagated across the waiting futures by dropping the promises that
    // have been collected so far.
    let Some(n_1) = fibonacci_with_hook(state.clone(), cancelled.clone(), n - 1, hook.clone())
    else {
        state.lock().remove(&n);
        return None;
    };
    let Some(n_2) = fibonacci_with_hook(state.clone(), cancelled.clone(), n - 2, hook.clone())
    else {
        state.lock().remove(&n);
        return None;
    };
    let result = n_1 + n_2;

    // Fulfill all waiting promises and memoize
    {
        let mut guard = state.lock();
        if let Some(State::InProgress(promises)) = guard.remove(&n) {
            let promises = promises.into_inner();
            promises.into_iter().for_each(|promise| {
                promise.fulfill(result);
            });
        }
    };

    {
        let mut guard = state.lock();
        guard.insert(n, State::Memoized(result));
    }

    Some(result)
}

fn fibonacci(
    state: Arc<Mutex<FxHashMap<usize, State>>>,
    cancelled: Arc<AtomicBool>,
    n: usize,
) -> Option<usize> {
    fibonacci_with_hook(state, cancelled, n, |_| {})
}

#[test]
fn test_fibonacci() {
    let state1 = Arc::new(Mutex::new(FxHashMap::default()));
    let cancelled = Arc::new(AtomicBool::new(false));

    for _ in 0..10_000 {
        let state2 = state1.clone();
        let cancelled2 = cancelled.clone();
        let a = std::thread::spawn(move || fibonacci(state2, cancelled2, 100));
        a.join().unwrap();
    }
}

#[test]
fn test_fibonacci_cancellation() {
    let state = Arc::new(Mutex::new(FxHashMap::default()));
    let cancelled = Arc::new(AtomicBool::new(false));

    // Pre-cancel before starting computation
    cancelled.store(true, Ordering::Relaxed);

    // Should return None immediately due to cancellation check
    let result = fibonacci(state.clone(), cancelled.clone(), 100);
    assert_eq!(result, None);
}

#[test]
fn test_fibonacci_cancellation_propagation() {
    let state = Arc::new(Mutex::new(FxHashMap::default()));
    let cancelled = Arc::new(AtomicBool::new(false));

    // Compute a value first
    let result = fibonacci(state.clone(), cancelled.clone(), 10);
    assert_eq!(result, Some(55));

    // Now cancel and try to compute again
    cancelled.store(true, Ordering::Relaxed);

    // Should return None for new computations
    let result = fibonacci(state.clone(), cancelled.clone(), 15);
    assert_eq!(result, None);

    // Should also return None for previously computed values
    let result = fibonacci(state.clone(), cancelled.clone(), 10);
    assert_eq!(result, None);
}

#[test]
fn test_fibonacci_cancellation_mid_computation() {
    use std::sync::{Arc, Barrier};

    let state = Arc::new(Mutex::new(FxHashMap::default()));
    let cancelled = Arc::new(AtomicBool::new(false));

    // Use a barrier to synchronize between threads
    let barrier = Arc::new(Barrier::new(2));

    // Start computing in a separate thread with a synchronization hook
    let state1 = state.clone();
    let cancelled1 = cancelled.clone();
    let barrier1 = barrier.clone();
    let handle1 = std::thread::spawn(move || {
        fibonacci_with_hook(state1, cancelled1, 5, |n| {
            // Signal when we reach n=15 (middle of computation)
            if n == 2 {
                // First barrier: signal we've reached the target
                barrier1.wait();
                // Second barrier: wait for cancellation to be set
                barrier1.wait();
            }
        })
    });

    let state2 = state.clone();
    let cancelled2 = cancelled.clone();
    let barrier2 = barrier.clone();
    let handle2 = std::thread::spawn(move || {
        fibonacci_with_hook(state2, cancelled2, 5, |n| {
            // Signal when we reach n=15 (middle of computation)
            if n == 2 {
                // First barrier: signal we've reached the target
                barrier2.wait();
                // Second barrier: wait for cancellation to be set
                barrier2.wait();
            }
        })
    });

    // Wait for the computation to reach the synchronization point
    barrier.wait();

    // Now we know the computation is at n=15, set cancellation
    cancelled.store(true, Ordering::Relaxed);

    // Release the computation thread to continue
    barrier.wait();

    // Should return None due to cancellation
    let result1 = handle1.join().unwrap();
    assert_eq!(result1, None);

    let result2 = handle2.join().unwrap();
    assert_eq!(result2, None);
}
