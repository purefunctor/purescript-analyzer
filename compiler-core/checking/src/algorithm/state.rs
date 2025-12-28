//! Implements the algorithm's state.

pub mod unification;
pub use unification::*;

use std::collections::VecDeque;
use std::mem;
use std::sync::Arc;

use building_types::QueryResult;
use files::FileId;
use indexing::{IndexedModule, TermItemId, TypeItemId};
use lowering::{
    BinderId, GraphNodeId, ImplicitBindingId, LetBindingNameGroupId, LoweredModule, RecordPunId,
    TypeVariableBindingId,
};
use rustc_hash::FxHashMap;
use sugar::{Bracketed, Sectioned};

use crate::algorithm::{quantify, constraint, transfer};
use crate::core::{Synonym, Type, TypeId, TypeInterner, debruijn};
use crate::error::{CheckError, ErrorKind, ErrorStep};
use crate::{CheckedModule, ExternalQueries};

/// Manually-managed scope for type-level bindings.
#[derive(Default)]
pub struct TypeScope {
    pub bound: debruijn::Bound,
    pub kinds: debruijn::BoundMap<TypeId>,
}

impl TypeScope {
    pub fn bind_forall(&mut self, id: TypeVariableBindingId, kind: TypeId) -> debruijn::Level {
        let variable = debruijn::Variable::Forall(id);
        let level = self.bound.bind(variable);
        self.kinds.insert(level, kind);
        level
    }

    pub fn lookup_forall(&self, id: TypeVariableBindingId) -> Option<debruijn::Level> {
        let variable = debruijn::Variable::Forall(id);
        self.bound.level_of(variable)
    }

    pub fn forall_binding_kind(&self, id: TypeVariableBindingId) -> Option<TypeId> {
        let variable = debruijn::Variable::Forall(id);
        let level = self.bound.level_of(variable)?;
        self.kinds.get(level).copied()
    }

    pub fn bind_implicit(
        &mut self,
        node: GraphNodeId,
        id: ImplicitBindingId,
        kind: TypeId,
    ) -> debruijn::Level {
        let variable = debruijn::Variable::Implicit { node, id };
        let level = self.bound.level_of(variable).unwrap_or_else(|| self.bound.bind(variable));
        self.kinds.insert(level, kind);
        level
    }

    pub fn lookup_implicit(
        &self,
        node: GraphNodeId,
        id: ImplicitBindingId,
    ) -> Option<debruijn::Level> {
        let variable = debruijn::Variable::Implicit { node, id };
        self.bound.level_of(variable)
    }

    pub fn implicit_binding_kind(
        &self,
        node: GraphNodeId,
        id: ImplicitBindingId,
    ) -> Option<TypeId> {
        let variable = debruijn::Variable::Implicit { node, id };
        let level = self.bound.level_of(variable)?;
        self.kinds.get(level).copied()
    }

    pub fn core_kind(&self, index: debruijn::Index) -> Option<TypeId> {
        let size = self.bound.size();
        let level = index.to_level(size)?;
        self.kinds.get(level).copied()
    }

    pub fn unbind(&mut self, level: debruijn::Level) {
        self.bound.unbind(level);
        self.kinds.unbind(level);
    }

    pub fn size(&self) -> debruijn::Size {
        self.bound.size()
    }
}

/// Manually-managed scope for term-level bindings.
#[derive(Default)]
pub struct TermScope {
    pub binder: FxHashMap<BinderId, TypeId>,
    pub let_binding: FxHashMap<LetBindingNameGroupId, TypeId>,
    pub record_pun: FxHashMap<RecordPunId, TypeId>,
    pub section: FxHashMap<lowering::ExpressionId, TypeId>,
}

impl TermScope {
    pub fn bind_binder(&mut self, id: BinderId, type_id: TypeId) {
        self.binder.insert(id, type_id);
    }

    pub fn lookup_binder(&self, id: BinderId) -> Option<TypeId> {
        self.binder.get(&id).copied()
    }

    pub fn bind_let(&mut self, id: LetBindingNameGroupId, type_id: TypeId) {
        self.let_binding.insert(id, type_id);
    }

    pub fn lookup_let(&self, id: LetBindingNameGroupId) -> Option<TypeId> {
        self.let_binding.get(&id).copied()
    }

    pub fn bind_pun(&mut self, id: RecordPunId, type_id: TypeId) {
        self.record_pun.insert(id, type_id);
    }

    pub fn lookup_pun(&self, id: RecordPunId) -> Option<TypeId> {
        self.record_pun.get(&id).copied()
    }

    pub fn bind_section(&mut self, id: lowering::ExpressionId, type_id: TypeId) {
        self.section.insert(id, type_id);
    }

    pub fn lookup_section(&self, id: lowering::ExpressionId) -> Option<TypeId> {
        self.section.get(&id).copied()
    }
}

/// Signature variable bindings from surface syntax.
#[derive(Default)]
pub struct SurfaceBindings {
    pub term_item: FxHashMap<TermItemId, Arc<[TypeVariableBindingId]>>,
    pub type_item: FxHashMap<TypeItemId, Arc<[TypeVariableBindingId]>>,
    pub let_binding: FxHashMap<LetBindingNameGroupId, Arc<[TypeVariableBindingId]>>,
}

impl SurfaceBindings {
    pub fn insert_term(&mut self, id: TermItemId, v: Arc<[TypeVariableBindingId]>) {
        self.term_item.insert(id, v);
    }

    pub fn get_term(&self, id: TermItemId) -> Option<Arc<[TypeVariableBindingId]>> {
        self.term_item.get(&id).cloned()
    }

    pub fn insert_type(&mut self, id: TypeItemId, v: Arc<[TypeVariableBindingId]>) {
        self.type_item.insert(id, v);
    }

    pub fn get_type(&self, id: TypeItemId) -> Option<Arc<[TypeVariableBindingId]>> {
        self.type_item.get(&id).cloned()
    }

    pub fn insert_let(&mut self, id: LetBindingNameGroupId, v: Arc<[TypeVariableBindingId]>) {
        self.let_binding.insert(id, v);
    }

    pub fn get_let(&self, id: LetBindingNameGroupId) -> Option<Arc<[TypeVariableBindingId]>> {
        self.let_binding.get(&id).cloned()
    }
}

/// Constraint collection context.
#[derive(Default)]
pub struct ConstraintContext {
    pub wanted: VecDeque<TypeId>,
    pub given: Vec<TypeId>,
}

impl ConstraintContext {
    pub fn push_wanted(&mut self, constraint: TypeId) {
        self.wanted.push_back(constraint);
    }

    pub fn push_given(&mut self, constraint: TypeId) {
        self.given.push(constraint);
    }

    pub fn take(&mut self) -> (VecDeque<TypeId>, Vec<TypeId>) {
        (mem::take(&mut self.wanted), mem::take(&mut self.given))
    }
}

#[derive(Default)]
pub struct CheckState {
    pub storage: TypeInterner,
    pub checked: CheckedModule,

    pub type_scope: TypeScope,
    pub term_scope: TermScope,
    pub surface_bindings: SurfaceBindings,

    pub constraints: ConstraintContext,
    pub unification: UnificationContext,
    pub binding_group: BindingGroupContext,

    pub check_steps: Vec<ErrorStep>,
    pub defer_synonym_expansion: bool,
}

#[derive(Default)]
pub struct BindingGroupContext {
    pub terms: FxHashMap<TermItemId, TypeId>,
    pub types: FxHashMap<TypeItemId, TypeId>,
    pub synonyms: FxHashMap<TypeItemId, Synonym>,
}

impl BindingGroupContext {
    pub fn lookup_term(&self, id: TermItemId) -> Option<TypeId> {
        self.terms.get(&id).copied()
    }

    pub fn lookup_type(&self, id: TypeItemId) -> Option<TypeId> {
        self.types.get(&id).copied()
    }

    pub fn lookup_synonym(&self, id: TypeItemId) -> Option<Synonym> {
        self.synonyms.get(&id).copied()
    }
}

pub struct CheckContext<'a, Q>
where
    Q: ExternalQueries,
{
    pub queries: &'a Q,
    pub prim: PrimCore,

    pub id: FileId,
    pub indexed: Arc<IndexedModule>,
    pub lowered: Arc<LoweredModule>,
    pub bracketed: Arc<Bracketed>,
    pub sectioned: Arc<Sectioned>,

    pub prim_indexed: Arc<IndexedModule>,
}

impl<'a, Q> CheckContext<'a, Q>
where
    Q: ExternalQueries,
{
    pub fn new(
        queries: &'a Q,
        state: &mut CheckState,
        id: FileId,
    ) -> QueryResult<CheckContext<'a, Q>> {
        let indexed = queries.indexed(id)?;
        let lowered = queries.lowered(id)?;
        let bracketed = queries.bracketed(id)?;
        let sectioned = queries.sectioned(id)?;
        let prim = PrimCore::collect(queries, state)?;
        let prim_id = queries.prim_id();
        let prim_indexed = queries.indexed(prim_id)?;
        Ok(CheckContext { queries, prim, id, indexed, lowered, bracketed, sectioned, prim_indexed })
    }
}

pub struct PrimCore {
    pub t: TypeId,
    pub function: TypeId,
    pub array: TypeId,
    pub record: TypeId,
    pub number: TypeId,
    pub int: TypeId,
    pub string: TypeId,
    pub char: TypeId,
    pub boolean: TypeId,
    pub partial: TypeId,
    pub constraint: TypeId,
    pub symbol: TypeId,
    pub row: TypeId,
    pub unknown: TypeId,
}

impl PrimCore {
    fn collect(queries: &impl ExternalQueries, state: &mut CheckState) -> QueryResult<PrimCore> {
        let file_id = queries.prim_id();
        let resolved = queries.resolved(file_id)?;

        let mut lookup_type = |name: &str| {
            let prim_type = resolved.lookup_type(&resolved, None, name);

            let (file_id, type_id) =
                prim_type.unwrap_or_else(|| unreachable!("invariant violated: {name} not in Prim"));

            state.storage.intern(Type::Constructor(file_id, type_id))
        };

        Ok(PrimCore {
            t: lookup_type("Type"),
            function: lookup_type("Function"),
            array: lookup_type("Array"),
            record: lookup_type("Record"),
            number: lookup_type("Number"),
            int: lookup_type("Int"),
            string: lookup_type("String"),
            char: lookup_type("Char"),
            boolean: lookup_type("Boolean"),
            partial: lookup_type("Partial"),
            constraint: lookup_type("Constraint"),
            symbol: lookup_type("Symbol"),
            row: lookup_type("Row"),
            unknown: state.storage.intern(Type::Unknown),
        })
    }
}

impl CheckState {
    pub fn term_binding_group<Q>(
        &mut self,
        context: &CheckContext<Q>,
        group: impl IntoIterator<Item = TermItemId>,
    ) where
        Q: ExternalQueries,
    {
        for item in group {
            let t = self.fresh_unification_type(context);
            self.binding_group.terms.insert(item, t);
        }
    }

    pub fn type_binding_group<Q>(
        &mut self,
        context: &CheckContext<Q>,
        group: impl IntoIterator<Item = TypeItemId>,
    ) where
        Q: ExternalQueries,
    {
        for item in group {
            let t = self.fresh_unification_type(context);
            self.binding_group.types.insert(item, t);
        }
    }

    pub fn solve_constraints<Q>(&mut self, context: &CheckContext<Q>) -> QueryResult<()>
    where
        Q: ExternalQueries,
    {
        let (wanted, given) = self.constraints.take();
        let _stuck = constraint::solve_constraints(self, context, wanted, given)?;
        Ok(())
    }

    pub fn commit_binding_group<Q>(&mut self, context: &CheckContext<Q>)
    where
        Q: ExternalQueries,
    {
        for (item_id, type_id) in mem::take(&mut self.binding_group.terms) {
            if let Some((type_id, _)) = quantify::quantify(self, type_id) {
                let type_id = transfer::globalize(self, context, type_id);
                self.checked.terms.insert(item_id, type_id);
            }
        }

        for (item_id, type_id) in mem::take(&mut self.binding_group.types) {
            if let Some((type_id, _)) = quantify::quantify(self, type_id) {
                let type_id = transfer::globalize(self, context, type_id);
                self.checked.types.insert(item_id, type_id);
            }
        }

        for (item_id, mut synonym) in mem::take(&mut self.binding_group.synonyms) {
            if let Some((synonym_type, quantified_variables)) =
                quantify::quantify(self, synonym.synonym_type)
            {
                synonym.quantified_variables = quantified_variables;
                synonym.synonym_type = transfer::globalize(self, context, synonym_type);
                self.checked.synonyms.insert(item_id, synonym);
            }
        }
    }

    pub fn with_error_step<T, F>(&mut self, step: ErrorStep, f: F) -> T
    where
        F: Fn(&mut CheckState) -> T,
    {
        self.check_steps.push(step);
        let r = f(self);
        self.check_steps.pop();
        r
    }

    pub fn insert_error(&mut self, kind: ErrorKind) {
        let step = self.check_steps.iter().copied().collect();
        let error = CheckError { kind, step };
        self.checked.errors.push(error);
    }
}

/// Functions for creating unification variables.
impl CheckState {
    /// Creates a fresh unification variable with the provided domain and kind.
    pub fn fresh_unification_kinded_at(&mut self, domain: debruijn::Size, kind: TypeId) -> TypeId {
        let unification_id = self.unification.fresh(domain, kind);
        self.storage.intern(Type::Unification(unification_id))
    }

    /// Creates a fresh unification variable with the provided kind.
    pub fn fresh_unification_kinded(&mut self, kind: TypeId) -> TypeId {
        let domain = self.type_scope.size();
        self.fresh_unification_kinded_at(domain, kind)
    }

    /// Creates a fresh polykinded unification variable.
    pub fn fresh_unification<Q>(&mut self, context: &CheckContext<Q>) -> TypeId
    where
        Q: ExternalQueries,
    {
        let kind_ty = self.fresh_unification_type(context);
        self.fresh_unification_kinded(kind_ty)
    }

    /// Creates a fresh [`Type`]-kinded unification variable.
    pub fn fresh_unification_type<Q>(&mut self, context: &CheckContext<Q>) -> TypeId
    where
        Q: ExternalQueries,
    {
        self.fresh_unification_kinded(context.prim.t)
    }
}

impl CheckState {
    /// Normalizes unification and bound type variables.
    ///
    /// This function also applies path compression to unification variables,
    /// where if a unification variable `?0` solves to `?1`, which solves to
    /// `Type`, `?0` is solved to `Type` to prune the lookup.
    pub fn normalize_type(&mut self, mut id: TypeId) -> TypeId {
        let mut to_compress = vec![];

        let id = loop {
            match self.storage[id] {
                Type::Unification(unification_id) => {
                    if let UnificationState::Solved(solution) =
                        self.unification.get(unification_id).state
                    {
                        id = solution;
                        to_compress.push(unification_id);
                    } else {
                        break id;
                    }
                }
                _ => break id,
            }
        };

        for unification_id in to_compress {
            self.unification.solve(unification_id, id);
        }

        id
    }

    pub fn make_function(&mut self, arguments: &[TypeId], result: TypeId) -> TypeId {
        arguments.iter().copied().rfold(result, |result, argument| {
            let function = Type::Function(argument, result);
            self.storage.intern(function)
        })
    }
}
