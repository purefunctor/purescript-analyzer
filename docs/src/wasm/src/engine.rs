//! A simplified, single-threaded query engine for WASM.

use std::cell::RefCell;
use std::sync::Arc;

use building_types::{ModuleNameId, ModuleNameInterner, QueryProxy, QueryResult};
use checking::{CheckedModule, Type, TypeId, TypeInterner};
use files::{FileId, Files};
use indexing::IndexedModule;
use lowering::LoweredModule;
use parsing::FullParsedModule;
use prim_constants::MODULE_MAP;
use resolving::ResolvedModule;
use rustc_hash::FxHashMap;
use stabilizing::StabilizedModule;

#[derive(Default)]
struct InputStorage {
    content: FxHashMap<FileId, Arc<str>>,
    module: FxHashMap<ModuleNameId, FileId>,
}

#[derive(Default)]
struct DerivedStorage {
    parsed: FxHashMap<FileId, FullParsedModule>,
    stabilized: FxHashMap<FileId, Arc<StabilizedModule>>,
    indexed: FxHashMap<FileId, Arc<IndexedModule>>,
    lowered: FxHashMap<FileId, Arc<LoweredModule>>,
    resolved: FxHashMap<FileId, Arc<ResolvedModule>>,
    bracketed: FxHashMap<FileId, Arc<sugar::Bracketed>>,
    sectioned: FxHashMap<FileId, Arc<sugar::Sectioned>>,
    checked: FxHashMap<FileId, Arc<CheckedModule>>,
}

#[derive(Default)]
struct InternedStorage {
    module: ModuleNameInterner,
    types: TypeInterner,
}

/// Single-threaded query engine for WASM
pub struct WasmQueryEngine {
    files: RefCell<Files>,
    input: RefCell<InputStorage>,
    derived: RefCell<DerivedStorage>,
    interned: RefCell<InternedStorage>,

    prim_id: FileId,
    user_id: Option<FileId>,
}

impl WasmQueryEngine {
    pub fn new() -> Self {
        let mut files = Files::default();
        let mut input = InputStorage::default();
        let mut interned = InternedStorage::default();

        // Load Prim modules
        let mut prim_id = None;
        for (name, module_content) in MODULE_MAP {
            let path = format!("prim://localhost/{name}.purs");
            let id = files.insert(path.as_str(), *module_content);
            input.content.insert(id, Arc::from(*module_content));

            let name_id = interned.module.intern(name);
            input.module.insert(name_id, id);

            if *name == "Prim" {
                prim_id = Some(id);
            }
        }

        Self {
            files: RefCell::new(files),
            input: RefCell::new(input),
            derived: RefCell::new(DerivedStorage::default()),
            interned: RefCell::new(interned),
            prim_id: prim_id.expect("invariant violated: Prim must exist"),
            user_id: None,
        }
    }

    /// Set the user's source code and return its FileId.
    /// Clears caches for the user file only.
    pub fn set_user_source(&mut self, source: &str) -> FileId {
        let id = if let Some(existing_id) = self.user_id {
            // Clear caches for the user file
            let mut derived = self.derived.borrow_mut();
            derived.parsed.remove(&existing_id);
            derived.stabilized.remove(&existing_id);
            derived.indexed.remove(&existing_id);
            derived.lowered.remove(&existing_id);
            derived.resolved.remove(&existing_id);
            derived.bracketed.remove(&existing_id);
            derived.sectioned.remove(&existing_id);
            derived.checked.remove(&existing_id);
            existing_id
        } else {
            let id = self.files.borrow_mut().insert("user://localhost/Main.purs", source);
            self.user_id = Some(id);

            // Register as Main module
            let name_id = self.interned.borrow_mut().module.intern("Main");
            self.input.borrow_mut().module.insert(name_id, id);

            id
        };

        self.input.borrow_mut().content.insert(id, Arc::from(source));
        id
    }

    fn content(&self, id: FileId) -> Arc<str> {
        self.input
            .borrow()
            .content
            .get(&id)
            .cloned()
            .expect("invariant violated: content must exist")
    }
}

impl QueryProxy for WasmQueryEngine {
    type Parsed = FullParsedModule;
    type Stabilized = Arc<StabilizedModule>;
    type Indexed = Arc<IndexedModule>;
    type Lowered = Arc<LoweredModule>;
    type Resolved = Arc<ResolvedModule>;
    type Bracketed = Arc<sugar::Bracketed>;
    type Sectioned = Arc<sugar::Sectioned>;
    type Checked = Arc<CheckedModule>;

    fn parsed(&self, id: FileId) -> QueryResult<Self::Parsed> {
        if let Some(cached) = self.derived.borrow().parsed.get(&id) {
            return Ok(cached.clone());
        }

        let content = self.content(id);
        let lexed = lexing::lex(&content);
        let tokens = lexing::layout(&lexed);
        let parsed = parsing::parse(&lexed, &tokens);

        self.derived.borrow_mut().parsed.insert(id, parsed.clone());
        Ok(parsed)
    }

    fn stabilized(&self, id: FileId) -> QueryResult<Self::Stabilized> {
        if let Some(cached) = self.derived.borrow().stabilized.get(&id) {
            return Ok(cached.clone());
        }

        let (parsed, _) = self.parsed(id)?;
        let node = parsed.syntax_node();
        let stabilized = Arc::new(stabilizing::stabilize_module(&node));

        self.derived.borrow_mut().stabilized.insert(id, stabilized.clone());
        Ok(stabilized)
    }

    fn indexed(&self, id: FileId) -> QueryResult<Self::Indexed> {
        if let Some(cached) = self.derived.borrow().indexed.get(&id) {
            return Ok(cached.clone());
        }

        let (parsed, _) = self.parsed(id)?;
        let stabilized = self.stabilized(id)?;

        let module = parsed.cst();
        let indexed = Arc::new(indexing::index_module(&module, &stabilized));

        self.derived.borrow_mut().indexed.insert(id, indexed.clone());
        Ok(indexed)
    }

    fn lowered(&self, id: FileId) -> QueryResult<Self::Lowered> {
        if let Some(cached) = self.derived.borrow().lowered.get(&id) {
            return Ok(cached.clone());
        }

        let (parsed, _) = self.parsed(id)?;
        let prim = self.resolved(self.prim_id)?;
        let stabilized = self.stabilized(id)?;
        let indexed = self.indexed(id)?;
        let resolved = self.resolved(id)?;

        let module = parsed.cst();
        let lowered =
            Arc::new(lowering::lower_module(id, &module, &prim, &stabilized, &indexed, &resolved));

        self.derived.borrow_mut().lowered.insert(id, lowered.clone());
        Ok(lowered)
    }

    fn resolved(&self, id: FileId) -> QueryResult<Self::Resolved> {
        if let Some(cached) = self.derived.borrow().resolved.get(&id) {
            return Ok(cached.clone());
        }

        let resolved = Arc::new(resolving::resolve_module(self, id)?);

        self.derived.borrow_mut().resolved.insert(id, resolved.clone());
        Ok(resolved)
    }

    fn bracketed(&self, id: FileId) -> QueryResult<Self::Bracketed> {
        if let Some(cached) = self.derived.borrow().bracketed.get(&id) {
            return Ok(cached.clone());
        }

        let lowered = self.lowered(id)?;
        let bracketed = Arc::new(sugar::bracketed(self, &lowered)?);

        self.derived.borrow_mut().bracketed.insert(id, bracketed.clone());
        Ok(bracketed)
    }

    fn sectioned(&self, id: FileId) -> QueryResult<Self::Sectioned> {
        if let Some(cached) = self.derived.borrow().sectioned.get(&id) {
            return Ok(cached.clone());
        }

        let lowered = self.lowered(id)?;
        let sectioned = Arc::new(sugar::sectioned(&lowered));

        self.derived.borrow_mut().sectioned.insert(id, sectioned.clone());
        Ok(sectioned)
    }

    fn checked(&self, id: FileId) -> QueryResult<Self::Checked> {
        if let Some(cached) = self.derived.borrow().checked.get(&id) {
            return Ok(cached.clone());
        }

        let checked = Arc::new(checking::check_module(self, id)?);

        self.derived.borrow_mut().checked.insert(id, checked.clone());
        Ok(checked)
    }

    fn prim_id(&self) -> FileId {
        self.prim_id
    }

    fn module_file(&self, name: &str) -> Option<FileId> {
        let interned = self.interned.borrow();
        let name_id = interned.module.lookup(name)?;
        self.input.borrow().module.get(&name_id).copied()
    }
}

impl checking::ExternalQueries for WasmQueryEngine {
    fn intern_type(&self, t: Type) -> TypeId {
        self.interned.borrow_mut().types.intern(t)
    }

    fn lookup_type(&self, id: TypeId) -> Type {
        self.interned.borrow().types[id].clone()
    }
}

impl resolving::ExternalQueries for WasmQueryEngine {}
impl sugar::ExternalQueries for WasmQueryEngine {}
