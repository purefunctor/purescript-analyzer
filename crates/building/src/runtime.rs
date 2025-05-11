//! Implements the core runtime for the query-based compiler.
//!
//! Our implementation is inspired by the verifying step traces described by
//! the [Build systems à la carte: Theory and practice] paper. One divergence
//! in this implementation is the absence of the result hashes, which are used
//! to verify changes from external sources.
//!
//! Our queries are designed to be pure and hermetic—the only cause for them
//! to be recomputed is a change in their inputs. The runtime currently does
//! not support maintaining incrementality across compilation sessions;
//! however, it's being written with serialisation in mind for the future.
//!
//! [Build systems à la carte: Theory and practice]: https://www.cambridge.org/core/journals/journal-of-functional-programming/article/build-systems-a-la-carte-theory-and-practice/097CE52C750E69BD16B78C318754C7A4

use std::{mem, sync::Arc};

use rowan::ast::AstNode;
use rustc_hash::{FxHashMap, FxHashSet};

use files::FileId;
use indexing::FullIndexedModule;
use lowering::FullLoweredModule;
use resolving::FullResolvedModule;
use syntax::cst;

use super::{ModuleNameId, ModuleNameMap};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum QueryKey {
    FileContent(FileId),
    ModuleFile(ModuleNameId),
    Parsed(FileId),
    Indexed(FileId),
    Resolved(FileId),
    Lowered(FileId),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Trace {
    built: usize,
    changed: usize,
    dependencies: Arc<[QueryKey]>,
}

impl Trace {
    pub fn create_input(revision: usize) -> Trace {
        Trace { built: revision, changed: revision, dependencies: [].into() }
    }

    pub fn create_fresh(revision: usize, dependencies: Arc<[QueryKey]>) -> Trace {
        Trace { built: revision, changed: revision, dependencies }
    }
}

type Content = FxHashMap<FileId, Arc<str>>;

#[derive(Default)]
pub struct Runtime {
    revision: usize,
    traces: FxHashMap<QueryKey, Trace>,

    content: Content,
    modules: ModuleNameMap,

    parsed: FxHashMap<FileId, cst::Module>,
    indexed: FxHashMap<FileId, Arc<FullIndexedModule>>,
    resolved: FxHashMap<FileId, Arc<FullResolvedModule>>,
    lowered: FxHashMap<FileId, Arc<FullLoweredModule>>,

    parent: Option<QueryKey>,
    dependencies: FxHashMap<QueryKey, FxHashSet<QueryKey>>,
}

impl Runtime {
    fn compute<T: Clone>(
        &mut self,
        k: QueryKey,
        compute: impl Fn(&mut Runtime) -> T,
        set_storage: impl Fn(&mut Runtime, T),
    ) -> T {
        let parent = mem::replace(&mut self.parent, Some(k));
        let value = compute(self);

        self.revision += 1;
        let revision = self.revision;

        let dependencies = self
            .dependencies
            .get(&k)
            .map(|dependencies| dependencies.iter().copied())
            .unwrap_or_default()
            .collect();

        let v = Trace::create_fresh(revision, dependencies);
        self.traces.insert(k, v);

        self.parent = parent;

        set_storage(self, T::clone(&value));
        value
    }

    fn query<T: Clone>(
        &mut self,
        k: QueryKey,
        compute: impl Fn(&mut Runtime) -> T,
        get_storage: impl Fn(&Runtime) -> Option<(T, &Trace)>,
        set_storage: impl Fn(&mut Runtime, T),
    ) -> T {
        if let Some(parent) = self.parent {
            self.dependencies.entry(parent).or_default().insert(k);
        }

        let revision = self.revision;
        if let Some((value, trace)) = get_storage(self) {
            if trace.built == revision {
                value
            } else {
                let built = trace.built;
                let dependencies = Arc::clone(&trace.dependencies);

                let mut latest = 0;
                for dependency in dependencies.iter() {
                    match dependency {
                        QueryKey::FileContent(_) => (),
                        QueryKey::ModuleFile(_) => (),
                        QueryKey::Parsed(id) => {
                            self.parse(*id);
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
                    if let Some(dependency) = self.traces.get(dependency) {
                        latest = latest.max(dependency.changed);
                    }
                }

                if built >= latest {
                    if let Some(trace) = self.traces.get_mut(&k) {
                        trace.built = revision;
                    }
                    value
                } else {
                    self.compute(k, compute, set_storage)
                }
            }
        } else {
            self.compute(k, compute, set_storage)
        }
    }

    fn input_set(&mut self, k: QueryKey) {
        self.revision += 1;
        let revision = self.revision;
        let v = Trace::create_input(revision);
        self.traces.insert(k, v);
    }

    fn input_get(&mut self, k: QueryKey) {
        if let Some(parent) = self.parent {
            self.dependencies.entry(parent).or_default().insert(k);
        }
    }
}

impl Runtime {
    pub fn set_content(&mut self, id: FileId, content: Arc<str>) {
        self.content.insert(id, content);
        self.input_set(QueryKey::FileContent(id));
    }

    pub fn content(&mut self, id: FileId) -> Arc<str> {
        self.input_get(QueryKey::FileContent(id));
        let v = self.content.get(&id).expect("invalid violated: invalid query key");
        Arc::clone(v)
    }

    pub fn set_module_file(&mut self, name: &str, file: FileId) {
        let id = self.modules.intern_with_file(name, file);
        self.input_set(QueryKey::ModuleFile(id));
    }

    pub fn module_file(&mut self, name: &str) -> Option<FileId> {
        let id = self.modules.intern(name);
        self.input_get(QueryKey::ModuleFile(id));
        self.modules.file_id(id)
    }
}

impl Runtime {
    pub fn parse(&mut self, id: FileId) -> cst::Module {
        let k = QueryKey::Parsed(id);
        self.query(
            k,
            |this| {
                let content = this.content(id);

                let lexed = lexing::lex(&content);
                let tokens = lexing::layout(&lexed);

                let (node, _) = parsing::parse(&lexed, &tokens);
                cst::Module::cast(node).expect("invariant violated: cannot cast parse result")
            },
            |this| {
                let value = this.parsed.get(&id).cloned()?;
                let trace = this.traces.get(&k)?;
                Some((value, trace))
            },
            |this, value| {
                this.parsed.insert(id, value);
            },
        )
    }

    pub fn indexed(&mut self, id: FileId) -> Arc<FullIndexedModule> {
        let k = QueryKey::Indexed(id);
        self.query(
            k,
            |this| {
                let module = this.parse(id);
                let indexed = indexing::index_module(&module);
                Arc::new(indexed)
            },
            |this| {
                let value = this.indexed.get(&id).cloned()?;
                let trace = this.traces.get(&k)?;
                Some((value, trace))
            },
            |this, value| {
                this.indexed.insert(id, value);
            },
        )
    }

    pub fn resolved(&mut self, id: FileId) -> Arc<FullResolvedModule> {
        let k = QueryKey::Resolved(id);
        self.query(
            k,
            |this| {
                let resolved = resolving::resolve_module(this, id);
                Arc::new(resolved)
            },
            |this| {
                let value = this.resolved.get(&id).cloned()?;
                let trace = this.traces.get(&k)?;
                Some((value, trace))
            },
            |this, value| {
                this.resolved.insert(id, value);
            },
        )
    }

    pub fn lowered(&mut self, id: FileId) -> Arc<FullLoweredModule> {
        let k = QueryKey::Lowered(id);
        self.query(
            k,
            |this| {
                let module = this.parse(id);
                let indexed = this.indexed(id);
                let lowered = lowering::lower_module(&module, &indexed);
                Arc::new(lowered)
            },
            |this| {
                let value = this.lowered.get(&id).cloned()?;
                let trace = this.traces.get(&k)?;
                Some((value, trace))
            },
            |this, value| {
                this.lowered.insert(id, value);
            },
        )
    }
}

impl resolving::External for Runtime {
    fn indexed(&mut self, id: FileId) -> Arc<FullIndexedModule> {
        Runtime::indexed(self, id)
    }

    fn resolved(&mut self, id: FileId) -> Arc<FullResolvedModule> {
        Runtime::resolved(self, id)
    }

    fn module_file(&mut self, name: &str) -> Option<FileId> {
        Runtime::module_file(self, name)
    }
}
