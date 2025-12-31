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

use crate::algorithm::{constraint, quantify, transfer};
use crate::core::{ForallBinder, Synonym, Type, TypeId, TypeInterner, debruijn};
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

    pub fn lookup_forall_kind(&self, id: TypeVariableBindingId) -> Option<TypeId> {
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

    pub fn lookup_implicit_kind(&self, node: GraphNodeId, id: ImplicitBindingId) -> Option<TypeId> {
        let variable = debruijn::Variable::Implicit { node, id };
        let level = self.bound.level_of(variable)?;
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

/// Tracks type variables declared in surface syntax.
///
/// The type checker checks kind/type declarations first before equation
/// declarations. In order to make the type variable binders visible in
/// the equation body, we need to save the [`TypeVariableBindingId`] and
/// then subsequently rebind them as needed.
///
/// Here's a small example for value groups:
///
/// ```purescript
/// identity :: forall a. a -> a
/// identity = impl
///   where
///   impl :: a -> a
///   impl = \a -> a
/// ```
///
/// The type signature is checked in isolation first to elaborate it, then,
/// when checking the body of the value equation, `a` is rebound such its
/// use `impl` in is properly in scope.
///
/// See usages of [`get_term`] and [`get_type`] for more information on
/// how this is used in context.
///
/// [`get_term`]: SurfaceBindings::get_term
/// [`get_type`]: SurfaceBindings::get_type
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

/// Collects wanted and given constraints.
#[derive(Default)]
pub struct ConstraintContext {
    pub wanted: VecDeque<TypeId>,
    pub given: Vec<TypeId>,
}

impl ConstraintContext {
    pub fn push_wanted(&mut self, constraint: TypeId) {
        self.wanted.push_back(constraint);
    }

    pub fn extend_wanted(&mut self, constraints: &[TypeId]) {
        self.wanted.extend(constraints);
    }

    pub fn push_given(&mut self, constraint: TypeId) {
        self.given.push(constraint);
    }

    pub fn take(&mut self) -> (VecDeque<TypeId>, Vec<TypeId>) {
        (mem::take(&mut self.wanted), mem::take(&mut self.given))
    }
}

/// The core state structure threaded through the [`algorithm`].
///
/// [`algorithm`]: crate::algorithm
#[derive(Default)]
pub struct CheckState {
    /// Interns and stores all types created during checking.
    pub storage: TypeInterner,
    /// The output being built, populated by checking rules.
    pub checked: CheckedModule,

    /// Type variable bindings, forall-bound, implicit, core.
    pub type_scope: TypeScope,
    /// Term variable bindings, binders, let names, record puns, sections.
    pub term_scope: TermScope,

    /// Tracks surface variables for rebinding, see struct documentation.
    pub surface_bindings: SurfaceBindings,

    /// Collects wanted/given type class constraints.
    pub constraints: ConstraintContext,
    /// Collects unification variables and solutions.
    pub unification: UnificationContext,
    /// The in-progress binding group; used for recursive declarations.
    pub binding_group: BindingGroupContext,

    /// Error context breadcrumbs for [`CheckedModule::errors`].
    pub check_steps: Vec<ErrorStep>,

    /// Flag that determines when it's appropriate to expand synonyms.
    pub defer_synonym_expansion: bool,
}

#[derive(Clone)]
pub struct CheckedConstructor {
    pub item_id: TermItemId,
    pub arguments: Vec<TypeId>,
}

#[derive(Clone)]
pub struct CheckedDataLike {
    pub kind_variables: Vec<ForallBinder>,
    pub type_variables: Vec<ForallBinder>,
    pub result_kind: TypeId,
    pub constructors: Vec<CheckedConstructor>,
}

#[derive(Default)]
pub struct BindingGroupContext {
    pub terms: FxHashMap<TermItemId, TypeId>,
    pub types: FxHashMap<TypeItemId, TypeId>,
    pub synonyms: FxHashMap<TypeItemId, Synonym>,
    pub residual: FxHashMap<TermItemId, Vec<TypeId>>,
    pub data: FxHashMap<TypeItemId, CheckedDataLike>,
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

/// The core environment structure threaded through the [`algorithm`].
///
/// This structure contains [`queries`], the internally-mutable [`ExternalQueries`]
/// that serves as a proxy to the build system. This structure also contains references
/// to modules and items that the compiler is aware of, like the various prim modules.
///
/// [`algorithm`]: crate::algorithm
/// [`queries`]: CheckContext::queries
pub struct CheckContext<'a, Q>
where
    Q: ExternalQueries,
{
    pub queries: &'a Q,
    pub prim: PrimCore,
    pub prim_int: PrimIntCore,
    pub prim_ordering: PrimOrderingCore,
    pub prim_symbol: PrimSymbolCore,

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
        let prim_int = PrimIntCore::collect(queries)?;
        let prim_ordering = PrimOrderingCore::collect(queries, state)?;
        let prim_symbol = PrimSymbolCore::collect(queries)?;
        let prim_id = queries.prim_id();
        let prim_indexed = queries.indexed(prim_id)?;
        Ok(CheckContext {
            queries,
            prim,
            prim_int,
            prim_ordering,
            prim_symbol,
            id,
            indexed,
            lowered,
            bracketed,
            sectioned,
            prim_indexed,
        })
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

pub struct PrimIntCore {
    pub file_id: FileId,
    pub add: TypeItemId,
    pub mul: TypeItemId,
    pub compare: TypeItemId,
    pub to_string: TypeItemId,
}

impl PrimIntCore {
    fn collect(queries: &impl ExternalQueries) -> QueryResult<PrimIntCore> {
        let file_id = queries
            .module_file("Prim.Int")
            .unwrap_or_else(|| unreachable!("invariant violated: Prim.Int not found"));

        let resolved = queries.resolved(file_id)?;

        let lookup_class = |name: &str| {
            let (_, type_id) = resolved
                .lookup_type(&resolved, None, name)
                .unwrap_or_else(|| unreachable!("invariant violated: {name} not in Prim.Int"));
            type_id
        };

        Ok(PrimIntCore {
            file_id,
            add: lookup_class("Add"),
            mul: lookup_class("Mul"),
            compare: lookup_class("Compare"),
            to_string: lookup_class("ToString"),
        })
    }
}

pub struct PrimOrderingCore {
    pub lt: TypeId,
    pub eq: TypeId,
    pub gt: TypeId,
}

pub struct PrimSymbolCore {
    pub file_id: FileId,
    pub append: TypeItemId,
    pub compare: TypeItemId,
    pub cons: TypeItemId,
}

impl PrimSymbolCore {
    fn collect(queries: &impl ExternalQueries) -> QueryResult<PrimSymbolCore> {
        let file_id = queries
            .module_file("Prim.Symbol")
            .unwrap_or_else(|| unreachable!("invariant violated: Prim.Symbol not found"));

        let resolved = queries.resolved(file_id)?;

        let lookup_class = |name: &str| {
            let (_, type_id) = resolved
                .lookup_type(&resolved, None, name)
                .unwrap_or_else(|| unreachable!("invariant violated: {name} not in Prim.Symbol"));
            type_id
        };

        Ok(PrimSymbolCore {
            file_id,
            append: lookup_class("Append"),
            compare: lookup_class("Compare"),
            cons: lookup_class("Cons"),
        })
    }
}

impl PrimOrderingCore {
    fn collect(
        queries: &impl ExternalQueries,
        state: &mut CheckState,
    ) -> QueryResult<PrimOrderingCore> {
        let file_id = queries
            .module_file("Prim.Ordering")
            .unwrap_or_else(|| unreachable!("invariant violated: Prim.Ordering not found"));

        let resolved = queries.resolved(file_id)?;

        let mut lookup_type = |name: &str| {
            let (file_id, type_id) = resolved
                .lookup_type(&resolved, None, name)
                .unwrap_or_else(|| unreachable!("invariant violated: {name} not in Prim.Ordering"));
            state.storage.intern(Type::Constructor(file_id, type_id))
        };

        Ok(PrimOrderingCore { lt: lookup_type("LT"), eq: lookup_type("EQ"), gt: lookup_type("GT") })
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

    pub fn solve_constraints<Q>(&mut self, context: &CheckContext<Q>) -> QueryResult<Vec<TypeId>>
    where
        Q: ExternalQueries,
    {
        let (wanted, given) = self.constraints.take();
        constraint::solve_constraints(self, context, wanted, given)
    }

    pub fn commit_binding_group<Q>(&mut self, context: &CheckContext<Q>)
    where
        Q: ExternalQueries,
    {
        let mut residuals = mem::take(&mut self.binding_group.residual);
        for (item_id, type_id) in mem::take(&mut self.binding_group.terms) {
            let constraints = residuals.remove(&item_id).unwrap_or_default();
            if let Some(result) = quantify::quantify_with_constraints(self, type_id, constraints) {
                self.with_error_step(ErrorStep::TermDeclaration(item_id), |this| {
                    for constraint in result.ambiguous {
                        let constraint = transfer::globalize(this, context, constraint);
                        this.insert_error(ErrorKind::AmbiguousConstraint { constraint });
                    }
                    for constraint in result.unsatisfied {
                        let constraint = transfer::globalize(this, context, constraint);
                        this.insert_error(ErrorKind::NoInstanceFound { constraint });
                    }
                });

                let type_id = transfer::globalize(self, context, result.quantified);
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

    /// Executes an action with an [`ErrorStep`] in scope.
    pub fn with_error_step<T, F>(&mut self, step: ErrorStep, f: F) -> T
    where
        F: FnOnce(&mut CheckState) -> T,
    {
        self.check_steps.push(step);
        let r = f(self);
        self.check_steps.pop();
        r
    }

    /// Inserts a [`CheckError`] with the [`ErrorStep`] in scope.
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

    /// Helper function for creating [`Type::Function`].
    pub fn make_function(&mut self, arguments: &[TypeId], result: TypeId) -> TypeId {
        arguments.iter().copied().rfold(result, |result, argument| {
            let function = Type::Function(argument, result);
            self.storage.intern(function)
        })
    }
}
