use std::{mem, sync::Arc};

use indexing::FullModuleIndex;
use lowering::FullModuleLower;
use rowan::ast::AstNode;
use rustc_hash::{FxHashMap, FxHashSet};
use syntax::cst;

use crate::files::FileId;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum QueryKey {
    Content(FileId),
    Parse(FileId),
    Index(FileId),
    Lower(FileId),
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

#[derive(Default)]
pub struct Runtime {
    revision: usize,
    traces: FxHashMap<QueryKey, Trace>,

    content: FxHashMap<FileId, Arc<str>>,
    parse: FxHashMap<FileId, cst::Module>,
    index: FxHashMap<FileId, Arc<FullModuleIndex>>,
    lower: FxHashMap<FileId, Arc<FullModuleLower>>,

    parent: Option<QueryKey>,
    dependencies: FxHashMap<QueryKey, FxHashSet<QueryKey>>,
}

impl Runtime {
    pub fn compute<T: Clone>(
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

    pub fn query<T: Clone>(
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
                        QueryKey::Content(_) => (),
                        QueryKey::Parse(id) => {
                            self.parse(*id);
                        }
                        QueryKey::Index(id) => {
                            self.index(*id);
                        }
                        QueryKey::Lower(id) => {
                            self.lower(*id);
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

    pub fn set_content(&mut self, id: FileId, content: Arc<str>) {
        self.content.insert(id, content);

        self.revision += 1;
        let revision = self.revision;

        let k = QueryKey::Content(id);
        let v = Trace::create_input(revision);
        self.traces.insert(k, v);
    }

    pub fn content(&mut self, id: FileId) -> Arc<str> {
        let k = QueryKey::Content(id);
        if let Some(parent) = self.parent {
            self.dependencies.entry(parent).or_default().insert(k);
        }
        let v = self.content.get(&id).expect("invalid violated: invalid query key");
        Arc::clone(v)
    }

    pub fn parse(&mut self, id: FileId) -> cst::Module {
        let k = QueryKey::Parse(id);
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
                let value = this.parse.get(&id).cloned()?;
                let trace = this.traces.get(&k)?;
                Some((value, trace))
            },
            |this, value| {
                this.parse.insert(id, value);
            },
        )
    }

    pub fn index(&mut self, id: FileId) -> Arc<FullModuleIndex> {
        let k = QueryKey::Index(id);
        self.query(
            k,
            |this| {
                let module = this.parse(id);
                let result = indexing::index_module(&module);
                Arc::new(result)
            },
            |this| {
                let value = this.index.get(&id).cloned()?;
                let trace = this.traces.get(&k)?;
                Some((value, trace))
            },
            |this, value| {
                this.index.insert(id, value);
            },
        )
    }

    pub fn lower(&mut self, id: FileId) -> Arc<FullModuleLower> {
        let k = QueryKey::Lower(id);
        self.query(
            k,
            |this| {
                let module = this.parse(id);
                let index = this.index(id);
                let lower =
                    lowering::lower_module(&module, &index.index, &index.relational, &index.source);
                Arc::new(lower)
            },
            |this| {
                let value = this.lower.get(&id).cloned()?;
                let trace = this.traces.get(&k)?;
                Some((value, trace))
            },
            |this, value| {
                this.lower.insert(id, value);
            },
        )
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use crate::files::Files;

    use super::Runtime;

    #[test]
    fn test_basic() {
        let mut runtime = Runtime::default();
        let mut files = Files::default();

        let id = files.insert("./src/Main.purs", "module Main where\n\nlife = 42");
        let content = files.content(id);

        runtime.set_content(id, content);
        let index_a = runtime.index(id);
        let index_b = runtime.index(id);
        assert!(Arc::ptr_eq(&index_a, &index_b));

        let lower_a = runtime.lower(id);
        let lower_b = runtime.lower(id);
        assert!(Arc::ptr_eq(&lower_a, &lower_b));

        runtime.set_content(id, "module Main where\n\n\n\nlife   =   42".into());
        let index_a = runtime.index(id);
        let index_b = runtime.index(id);
        assert!(Arc::ptr_eq(&index_a, &index_b));

        let lower_a = runtime.lower(id);
        let lower_b = runtime.lower(id);
        assert!(Arc::ptr_eq(&lower_a, &lower_b));
    }
}
