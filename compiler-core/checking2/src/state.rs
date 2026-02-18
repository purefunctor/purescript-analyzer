//! Implements the algorithm's core state structures.

use std::mem;

use files::FileId;
use rustc_hash::FxHashMap;

use crate::core::{Depth, Name, SmolStrId, Type, TypeId};
use crate::error::{CheckError, ErrorCrumb, ErrorKind};
use crate::implication::Implications;
use crate::{CheckedModule, ExternalQueries};

/// Manages [`Name`] values for [`CheckState`].
pub struct Names {
    unique: u32,
    file: FileId,
}

impl Names {
    pub fn new(file: FileId) -> Names {
        Names { unique: 0, file }
    }

    pub fn fresh(&mut self) -> Name {
        let unique = self.unique;
        self.unique += 1;
        Name { file: self.file, unique }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum UnificationState {
    Unsolved,
    Solved(TypeId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct UnificationEntry {
    pub depth: Depth,
    pub kind: TypeId,
    pub state: UnificationState,
}

/// Manages unification variables for [`CheckState`].
#[derive(Debug, Default)]
pub struct Unifications {
    entries: Vec<UnificationEntry>,
    unique: u32,
}

impl Unifications {
    pub fn fresh(&mut self, depth: Depth, kind: TypeId) -> u32 {
        let unique = self.unique;

        self.unique += 1;
        self.entries.push(UnificationEntry { depth, kind, state: UnificationState::Unsolved });

        unique
    }

    pub fn get(&self, index: u32) -> &UnificationEntry {
        &self.entries[index as usize]
    }

    pub fn get_mut(&mut self, index: u32) -> &mut UnificationEntry {
        &mut self.entries[index as usize]
    }

    pub fn solve(&mut self, index: u32, solution: TypeId) {
        self.get_mut(index).state = UnificationState::Solved(solution);
    }

    pub fn iter(&self) -> impl Iterator<Item = &UnificationEntry> {
        self.entries.iter()
    }
}

/// Tracks type variable bindings during kind inference.
#[derive(Default)]
pub struct KindScope {
    forall_bindings: FxHashMap<lowering::TypeVariableBindingId, (Name, TypeId)>,
    implicit_bindings:
        FxHashMap<(lowering::GraphNodeId, lowering::ImplicitBindingId), (Name, TypeId)>,
}

impl KindScope {
    pub fn bind_forall(&mut self, id: lowering::TypeVariableBindingId, name: Name, kind: TypeId) {
        self.forall_bindings.insert(id, (name, kind));
    }

    pub fn lookup_forall(&self, id: lowering::TypeVariableBindingId) -> Option<(Name, TypeId)> {
        self.forall_bindings.get(&id).copied()
    }

    pub fn bind_implicit(
        &mut self,
        node: lowering::GraphNodeId,
        id: lowering::ImplicitBindingId,
        name: Name,
        kind: TypeId,
    ) {
        self.implicit_bindings.insert((node, id), (name, kind));
    }

    pub fn lookup_implicit(
        &self,
        node: lowering::GraphNodeId,
        id: lowering::ImplicitBindingId,
    ) -> Option<(Name, TypeId)> {
        self.implicit_bindings.get(&(node, id)).copied()
    }
}

/// The core state structure threaded through the algorithm.
pub struct CheckState {
    pub checked: CheckedModule,

    pub names: Names,
    pub unifications: Unifications,
    pub implications: Implications,
    pub kind_scope: KindScope,
    pub depth: Depth,

    pub crumbs: Vec<ErrorCrumb>,
}

impl CheckState {
    pub fn new(file_id: FileId) -> CheckState {
        CheckState {
            checked: Default::default(),
            names: Names::new(file_id),
            unifications: Default::default(),
            implications: Default::default(),
            kind_scope: Default::default(),
            depth: Depth(0),
            crumbs: Default::default(),
        }
    }

    pub fn with_depth<T>(&mut self, f: impl FnOnce(&mut CheckState) -> T) -> T {
        let depth = self.depth.increment();

        let previous = mem::replace(&mut self.depth, depth);
        let result = f(self);
        self.depth = previous;

        result
    }

    pub fn with_error_crumb<F, T>(&mut self, crumb: ErrorCrumb, f: F) -> T
    where
        F: FnOnce(&mut CheckState) -> T,
    {
        self.crumbs.push(crumb);
        let result = f(self);
        self.crumbs.pop();
        result
    }

    pub fn fresh_unification(&mut self, queries: &impl ExternalQueries, kind: TypeId) -> TypeId {
        let unification = self.unifications.fresh(self.depth, kind);
        queries.intern_type(Type::Unification(unification))
    }

    pub fn fresh_rigid(&mut self, queries: &impl ExternalQueries, kind: TypeId) -> TypeId {
        let name = self.names.fresh();
        queries.intern_type(Type::Rigid(name, self.depth, kind))
    }

    pub fn insert_error(&mut self, kind: ErrorKind) {
        let crumbs = self.crumbs.iter().copied().collect();
        self.checked.errors.push(CheckError { kind, crumbs });
    }

    pub fn push_wanted(&mut self, constraint: TypeId) {
        self.implications.current_mut().wanted.push_back(constraint);
    }

    pub fn push_given(&mut self, constraint: TypeId) {
        self.implications.current_mut().given.push(constraint);
    }

    pub fn with_implication<T>(&mut self, f: impl FnOnce(&mut CheckState) -> T) -> T {
        let id = self.implications.push();
        let result = f(self);
        self.implications.pop(id);
        result
    }

    pub fn pretty_id<Q>(
        &mut self,
        context: &crate::context::CheckContext<Q>,
        id: TypeId,
    ) -> SmolStrId
    where
        Q: ExternalQueries,
    {
        let pretty = crate::core::pretty::print(self, context, id);
        context.queries.intern_smol_str(pretty)
    }
}
