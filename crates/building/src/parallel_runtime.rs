//! Implements a highly-parallel version of the [`runtime`].
//!
//! This implementation provides a parallel-safe API through the
//! [`SequentialRuntime`] and [`ParallelRuntime`] structs.
//!
//! As the name implies, the `SequentialRuntime` is intended to be used
//! for sequential tasks such as modifying input queries.
//!
//! On the other hand, the `ParallelRuntime` is intended to be used for
//! parallel-safe tasks such as computing derived queries.
//!
//! [`SequentialRuntime::upgraded`] can be used to temporarily upgrade
//! the `SequentialRuntime` into a `ParallelRuntime`.
//!
//! ```no_run
//! let runtime = SequentialRuntime::default();
//! let (parsed_a, parsed_b) = runtime.upgraded(|runtime| {
//!     rayon::join(
//!         || runtime.parsed(id_a)
//!         || runtime.parsed(id_b)
//!     )
//! });
//! ```
//!
//! # Implementation Notes
//!
//! We implicitly recommend [`rayon`] when _executing_ queries using the
//! `ParallelRuntime`, which naturally raises some concerns about lock
//! contention getting in the way of parallel execution.
//!
//! One notable case where contention may be a bottleneck is the query-level
//! guards that prevent multiple threads from performing the same computation
//! twice, thus wasting work. However, the same query being spammed repeatedly
//! for an extended period of time is very rare outside of testing scenarios.
//!
//! One such testing scenario is implementing a fibonacci query that uses
//! [`rayon::join`]. More specifically, the overlap of dependencies that
//! `fib(n - 1)` has with `fib(n - 2)` is enough for sufficiently large `n`:
//!
//! ```text
//! fib(48) = fib(47) + fib(46)
//! . fib(47) = fib(46) + fib(45)
//!   . fib(46) = fib(45) + fib(44)
//!   . fib(45) = fib(44) + fib(43)
//! . fib(46) = fib(45) + fib(44)
//!   . fib(45) = fib(44) + fib(43)
//!   . fib(44) = fib(43) + fib(42)
//! ```
//!
//! Our hypothesis is that this overlap in tandem with a work-stealing
//! scheduler is enough to stall the thread pool from making progress.
//! The exact of mechanism of why this happens is currently unknown;
//! for the fibonacci query, we've observed that computes for the same
//! key can occur randomly, even with the synchronization mutex.
//!
//! As such, we do not recommend [`rayon`] for _implementing_ queries.
//!
//! tl;dr parallel build systems are hard
//!
//! [`runtime`]: crate::runtime

use std::{
    cell::RefCell,
    mem,
    sync::{
        atomic::{AtomicUsize, Ordering},
        Arc,
    },
};

use files::FileId;
use indexing::FullIndexedModule;
use lowering::FullLoweredModule;
use parking_lot::{Mutex, RwLock};
use parsing::FullParsedModule;
use resolving::FullResolvedModule;
use rustc_hash::{FxHashMap, FxHashSet};
use thread_local::ThreadLocal;

use crate::{ModuleNameId, ModuleNameMap};

/// A unified type for query keys.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum QueryKey {
    FileContent(FileId),
    ModuleFile(ModuleNameId),
    Parsed(FileId),
    Indexed(FileId),
    Resolved(FileId),
    Lowered(FileId),
}

/// A verifying step trace.
#[derive(Debug, Clone)]
pub struct Trace {
    /// Timestamp of when the query was last built.
    pub built: usize,
    /// Timestasmp of when the query was last changed.
    pub changed: usize,
    /// The dependencies used to build the query.
    pub dependencies: Arc<[QueryKey]>,
}

impl Trace {
    fn input(revision: usize) -> Trace {
        Trace { built: revision, changed: revision, dependencies: [].into() }
    }
}

/// Handles work deduplication for the [`ParallelRuntime`].
#[derive(Default, Clone)]
struct Control {
    inner: Arc<RwLock<FxHashMap<QueryKey, Arc<Mutex<()>>>>>,
}

impl Control {
    fn get_or_create(&self, key: QueryKey) -> (Arc<Mutex<()>>, bool) {
        {
            let inner = self.inner.read();
            if let Some(control) = inner.get(&key) {
                return (Arc::clone(control), false);
            }
        }

        let mut inner = self.inner.write();

        let control = Arc::new(Mutex::default());
        inner.insert(key, Arc::clone(&control));

        (control, true)
    }
}

/// Thread-local state for the [`ParallelRuntime`].
#[derive(Default)]
struct Local {
    inner: ThreadLocal<RefCell<LocalState>>,
}

impl Local {
    fn get_or_default(&self) -> &RefCell<LocalState> {
        self.inner.get_or_default()
    }
}

/// Thread-local state for the [`ParallelRuntime`].
#[derive(Default)]
struct LocalState {
    active: Option<QueryKey>,
    dependencies: FxHashMap<QueryKey, FxHashSet<QueryKey>>,
}

impl LocalState {
    fn replace_active(&mut self, key: QueryKey) -> Option<QueryKey> {
        mem::replace(&mut self.active, Some(key))
    }

    fn record_dependency(&mut self, key: QueryKey) {
        if let Some(active) = self.active {
            self.dependencies.entry(active).or_default().insert(key);
        }
    }

    fn dependencies(&self, key: QueryKey) -> Arc<[QueryKey]> {
        self.dependencies
            .get(&key)
            .map(|dependencies| dependencies.iter().copied())
            .unwrap_or_default()
            .collect()
    }
}

/// See module-level documentation.
#[derive(Default)]
pub struct SequentialRuntime {
    revision: AtomicUsize,
    content: Arc<RwLock<FxHashMap<FileId, Arc<str>>>>,
    modules: Arc<RwLock<ModuleNameMap>>,

    parsed: Arc<RwLock<FxHashMap<FileId, FullParsedModule>>>,
    indexed: Arc<RwLock<FxHashMap<FileId, Arc<FullIndexedModule>>>>,
    resolved: Arc<RwLock<FxHashMap<FileId, Arc<FullResolvedModule>>>>,
    lowered: Arc<RwLock<FxHashMap<FileId, Arc<FullLoweredModule>>>>,
    traces: Arc<RwLock<FxHashMap<QueryKey, Trace>>>,
}

impl Clone for SequentialRuntime {
    fn clone(&self) -> SequentialRuntime {
        let revision = self.revision.load(Ordering::SeqCst);
        SequentialRuntime {
            revision: AtomicUsize::new(revision),
            content: self.content.clone(),
            modules: self.modules.clone(),
            parsed: self.parsed.clone(),
            indexed: self.indexed.clone(),
            resolved: self.resolved.clone(),
            lowered: self.lowered.clone(),
            traces: self.traces.clone(),
        }
    }
}

/// See module-level documentation.
pub struct ParallelRuntime<'a> {
    revision: usize,
    content: &'a FxHashMap<FileId, Arc<str>>,
    modules: &'a ModuleNameMap,

    parsed: Arc<RwLock<FxHashMap<FileId, FullParsedModule>>>,
    indexed: Arc<RwLock<FxHashMap<FileId, Arc<FullIndexedModule>>>>,
    resolved: Arc<RwLock<FxHashMap<FileId, Arc<FullResolvedModule>>>>,
    lowered: Arc<RwLock<FxHashMap<FileId, Arc<FullLoweredModule>>>>,
    traces: Arc<RwLock<FxHashMap<QueryKey, Trace>>>,
    control: Control,

    local: Local,
}

impl SequentialRuntime {
    pub fn upgraded<T, F>(&self, f: F) -> T
    where
        F: FnOnce(&ParallelRuntime) -> T,
    {
        // Obtain read locks for the input storage.
        // This ensures that we can run `f` without
        // worrying about inputs being modified.
        let content_guard = self.content.read();
        let modules_guard = self.modules.read();

        let parallel = ParallelRuntime {
            revision: self.revision.load(Ordering::SeqCst),
            content: &*content_guard,
            modules: &*modules_guard,
            parsed: self.parsed.clone(),
            indexed: self.indexed.clone(),
            resolved: self.resolved.clone(),
            lowered: self.lowered.clone(),
            traces: self.traces.clone(),
            local: Local::default(),
            control: Control::default(),
        };

        f(&parallel)
    }

    pub fn set_content(&self, id: FileId, content: Arc<str>) {
        self.content.write().insert(id, content);

        self.revision.fetch_add(1, Ordering::SeqCst);
        let revision = self.revision.load(Ordering::SeqCst);

        self.traces.write().insert(QueryKey::FileContent(id), Trace::input(revision));
    }

    pub fn set_module_file(&self, name: &str, file: FileId) {
        let id = {
            let mut modules = self.modules.write();
            modules.intern_with_file(name, file)
        };

        self.revision.fetch_add(1, Ordering::SeqCst);
        let revision = self.revision.load(Ordering::SeqCst);

        self.traces.write().insert(QueryKey::ModuleFile(id), Trace::input(revision));
    }
}

impl ParallelRuntime<'_> {
    fn with_local<T>(&self, f: impl FnOnce(&mut LocalState) -> T) -> T {
        let mut local = self.local.get_or_default().borrow_mut();
        f(&mut local)
    }

    fn query<T, GetStorage, SetStorage, Compute>(
        &self,
        key: QueryKey,
        get_storage: GetStorage,
        set_storage: SetStorage,
        compute: Compute,
    ) -> T
    where
        T: Clone + Eq,
        GetStorage: Fn() -> Option<(T, Trace)>,
        SetStorage: FnOnce(T),
        Compute: FnOnce() -> T,
    {
        self.with_local(|local| local.record_dependency(key));

        // If we have a cached result for the query, we have to do some
        // validation first to determine if it can be used or it needs
        // to be updated.
        if let Some((cached, trace)) = get_storage() {
            // If the query was built in the current revision, this was
            // computed very recently and we can use the cached value.
            if trace.built == self.revision {
                return cached;
            }

            // Otherwise, we start validating its dependencies by running
            // them. We also keep track of which dependency was `changed`
            // most recently. Consequently, if we end up having to call
            // `compute`, its dependencies are likely to be cached.
            let mut latest_changed = 0;
            for dependency in trace.dependencies.iter() {
                match dependency {
                    QueryKey::FileContent(_) | QueryKey::ModuleFile(_) => (),
                    QueryKey::Parsed(id) => {
                        self.parsed(*id);
                    }
                    QueryKey::Indexed(id) => {
                        self.indexed(*id);
                    }
                    QueryKey::Resolved(id) => {
                        self.resolved(*id);
                    }
                    QueryKey::Lowered(id) => {
                        self.lowered(*id);
                    }
                }
                let traces = self.traces.read();
                if let Some(trace) = traces.get(dependency) {
                    latest_changed = latest_changed.max(trace.changed);
                }
            }

            // If the cached result was built more recently than its
            // dependencies were changed, we update the `built` timestamp
            // to the current revision and return the cached value.
            //
            // Updating the `built` timestamp to the current revision
            // allows subsequent queries to skip validating the dependencies.
            if trace.built >= latest_changed {
                let mut traces = self.traces.write();
                if let Some(trace) = traces.get_mut(&key) {
                    trace.built = self.revision;
                }
                return cached;
            }
        }

        // Obtain a lock for the current query and immediately lock it.
        // If multiple threads are performing the same query, we want to
        // make sure that only a single thread performs the computation
        // and that all other threads should just wait for the result.
        let (control, fresh) = self.control.get_or_create(key);
        let mut _query_guard = control.lock();

        if fresh {
            // Make sure that our thread-local state is up-to-date.
            let previous_active = self.with_local(|local| local.replace_active(key));
            let value = compute();
            self.with_local(|local| local.active = previous_active);

            let built = self.revision;
            let changed = self.revision;
            let dependencies = self.with_local(|local| local.dependencies(key));

            // After computing the value, we need to update its trace.
            //
            // If we determine that the current computed result is equal to the
            // previous cached result, then the `changed` timestamp as well as
            // the cached value need not be updated.
            //
            // Since we prefer to wrap query results in Arc to make cloning
            // cheaper, we can also take advantage of an optimization where
            // if `T: Eq` in `Arc<T>`, `==` would check for pointer equality
            // before structural equality.
            let value = match get_storage() {
                Some((cached, Trace { changed, .. })) if value == cached => {
                    let trace = Trace { built, changed, dependencies };
                    self.traces.write().insert(key, trace);
                    cached
                }
                _ => {
                    let trace = Trace { built, changed, dependencies };
                    self.traces.write().insert(key, trace);
                    set_storage(T::clone(&value));
                    value
                }
            };

            value
        } else {
            let Some((value, _)) = get_storage() else {
                unreachable!("invariant violated: missing value in storage");
            };
            value
        }
    }

    pub fn content(&self, id: FileId) -> Arc<str> {
        self.with_local(|local| local.record_dependency(QueryKey::FileContent(id)));
        let content = self.content.get(&id).expect("invariant violated: invalid query key");
        Arc::clone(content)
    }

    pub fn module_file(&self, name: &str) -> Option<FileId> {
        let id = self.modules.module_id(name)?;
        self.with_local(|local| local.record_dependency(QueryKey::ModuleFile(id)));
        self.modules.file_id(id)
    }

    pub fn parsed(&self, id: FileId) -> FullParsedModule {
        let key = QueryKey::Parsed(id);
        self.query(
            key,
            || {
                let parsed = self.parsed.read();
                let traces = self.traces.read();

                let parsed = parsed.get(&id)?.clone();
                let trace = traces.get(&key)?.clone();

                Some((parsed, trace))
            },
            |value| {
                let mut parsed = self.parsed.write();
                parsed.insert(id, value);
            },
            || {
                let content = self.content(id);

                let lexed = lexing::lex(&content);
                let tokens = lexing::layout(&lexed);

                parsing::parse(&lexed, &tokens)
            },
        )
    }

    pub fn indexed(&self, id: FileId) -> Arc<FullIndexedModule> {
        let key = QueryKey::Indexed(id);
        self.query(
            key,
            || {
                let indexed = self.indexed.read();
                let traces = self.traces.read();

                let indexed = indexed.get(&id)?.clone();
                let trace = traces.get(&key)?.clone();

                Some((indexed, trace))
            },
            |value| {
                let mut indexed = self.indexed.write();
                indexed.insert(id, value);
            },
            || {
                let (parsed, _) = self.parsed(id);
                let cst = parsed.cst();
                Arc::new(indexing::index_module(&cst))
            },
        )
    }

    pub fn resolved(&self, id: FileId) -> Arc<FullResolvedModule> {
        let key = QueryKey::Resolved(id);
        self.query(
            key,
            || {
                let resolved = self.resolved.read();
                let traces = self.traces.read();

                let resolved = resolved.get(&id)?.clone();
                let trace = traces.get(&key)?.clone();

                Some((resolved, trace))
            },
            |value| {
                let mut resolved = self.resolved.write();
                resolved.insert(id, value);
            },
            || {
                let mut external = ParallelRuntimeWrapper { inner: self };
                Arc::new(resolving::resolve_module(&mut external, id))
            },
        )
    }

    pub fn lowered(&self, id: FileId) -> Arc<FullLoweredModule> {
        let key = QueryKey::Lowered(id);
        self.query(
            key,
            || {
                let lowered = self.lowered.read();
                let traces = self.traces.read();

                let lowered = lowered.get(&id)?.clone();
                let trace = traces.get(&key)?.clone();

                Some((lowered, trace))
            },
            |value| {
                let mut lowered = self.lowered.write();
                lowered.insert(id, value);
            },
            || {
                let (parsed, _) = self.parsed(id);
                let indexed = self.indexed(id);
                let cst = parsed.cst();
                Arc::new(lowering::lower_module(&cst, &indexed))
            },
        )
    }
}

/// A wrapper that allows [`ParallelRuntime`] to be passed as `&mut`.
struct ParallelRuntimeWrapper<'a, 'b> {
    inner: &'a ParallelRuntime<'b>,
}

impl resolving::External for ParallelRuntimeWrapper<'_, '_> {
    fn indexed(&mut self, id: FileId) -> Arc<FullIndexedModule> {
        self.inner.indexed(id)
    }

    fn resolved(&mut self, id: FileId) -> Arc<FullResolvedModule> {
        self.inner.resolved(id)
    }

    fn module_file(&mut self, name: &str) -> Option<FileId> {
        self.inner.module_file(name)
    }
}
