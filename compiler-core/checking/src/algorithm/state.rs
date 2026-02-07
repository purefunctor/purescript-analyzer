pub mod unification;
use itertools::Itertools;
pub use unification::*;

use std::collections::VecDeque;
use std::mem;
use std::sync::Arc;

use building_types::QueryResult;
use files::FileId;
use indexing::{IndexedModule, TermItemId, TypeItemId};
use lowering::{
    BinderId, GraphNodeId, GroupedModule, ImplicitBindingId, LetBindingNameGroupId, LoweredModule,
    RecordPunId, TermOperatorId, TypeItemIr, TypeOperatorId, TypeVariableBindingId,
};
use resolving::ResolvedModule;
use rustc_hash::FxHashMap;
use smol_str::ToSmolStr;
use stabilizing::StabilizedModule;
use sugar::{Bracketed, Sectioned};

use crate::algorithm::exhaustiveness::{
    ExhaustivenessReport, Pattern, PatternConstructor, PatternId, PatternKind, PatternStorage,
};
use crate::algorithm::{constraint, transfer};
use crate::core::{Type, TypeId, TypeInterner, Variable, debruijn, pretty};
use crate::error::{CheckError, ErrorKind, ErrorStep};
use crate::{CheckedModule, ExternalQueries, TypeErrorMessageId};

#[derive(Copy, Clone, Debug)]
pub struct OperatorBranchTypes {
    pub left: TypeId,
    pub right: TypeId,
    pub result: TypeId,
}

/// Manually-managed scope for type-level bindings.
#[derive(Default)]
pub struct TypeScope {
    pub bound: debruijn::Bound,
    pub kinds: debruijn::BoundMap<TypeId>,
    pub operator_node: FxHashMap<TypeOperatorId, OperatorBranchTypes>,
}

impl TypeScope {
    pub fn bind_forall(&mut self, id: TypeVariableBindingId, kind: TypeId) -> debruijn::Level {
        let variable = debruijn::Variable::Forall(id);
        let level = self.bound.bind(variable);
        self.kinds.insert(level, kind);
        level
    }

    pub fn bind_core(&mut self, kind: TypeId) -> debruijn::Level {
        let variable = debruijn::Variable::Core;
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

    /// Unbinds variables starting from a level and returns captured implicit bindings.
    ///
    /// This is used when checking instances to capture the implicit type variables
    /// from the instance head before unbinding, so they can be rebound when checking
    /// instance members.
    pub fn unbind_implicits(&mut self, level: debruijn::Level) -> Vec<InstanceHeadBinding> {
        let mut implicits = vec![];

        for (level, variable) in self.bound.iter_from(level) {
            if let debruijn::Variable::Implicit { node, id } = variable
                && let Some(&kind) = self.kinds.get(level)
            {
                implicits.push(InstanceHeadBinding { node, id, kind });
            }
        }

        self.unbind(level);
        implicits
    }

    pub fn size(&self) -> debruijn::Size {
        self.bound.size()
    }

    pub fn bind_operator_node(&mut self, id: TypeOperatorId, types: OperatorBranchTypes) {
        self.operator_node.insert(id, types);
    }

    pub fn lookup_operator_node(&self, id: TypeOperatorId) -> Option<OperatorBranchTypes> {
        self.operator_node.get(&id).copied()
    }
}

/// Manually-managed scope for term-level bindings.
#[derive(Default)]
pub struct TermScope {
    pub binder: FxHashMap<BinderId, TypeId>,
    pub let_binding: FxHashMap<LetBindingNameGroupId, TypeId>,
    pub record_pun: FxHashMap<RecordPunId, TypeId>,
    pub section: FxHashMap<lowering::ExpressionId, TypeId>,
    pub operator_node: FxHashMap<TermOperatorId, OperatorBranchTypes>,
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

    pub fn bind_operator_node(&mut self, id: TermOperatorId, types: OperatorBranchTypes) {
        self.operator_node.insert(id, types);
    }

    pub fn lookup_operator_node(&self, id: TermOperatorId) -> Option<OperatorBranchTypes> {
        self.operator_node.get(&id).copied()
    }
}

/// A single implicit variable captured from an instance head.
///
/// Instance heads like `instance Show a => Show (Array a)` introduce
/// implicit type variables that need to be visible when checking instance
/// member implementations.
pub struct InstanceHeadBinding {
    pub node: GraphNodeId,
    pub id: ImplicitBindingId,
    pub kind: TypeId,
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
    pub instance_head: FxHashMap<TermItemId, Arc<[InstanceHeadBinding]>>,
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

    pub fn insert_instance_head(&mut self, id: TermItemId, v: Arc<[InstanceHeadBinding]>) {
        self.instance_head.insert(id, v);
    }

    pub fn get_instance_head(&self, id: TermItemId) -> Option<Arc<[InstanceHeadBinding]>> {
        self.instance_head.get(&id).cloned()
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

    /// Interns patterns for exhaustiveness checking.
    pub patterns: PatternStorage,
}

#[derive(Clone)]
pub struct CheckedConstructor {
    pub item_id: TermItemId,
    pub arguments: Vec<TypeId>,
}

#[derive(Clone, Copy)]
pub struct BindingGroupType(pub TypeId);

#[derive(Default)]
pub struct BindingGroupContext {
    pub terms: FxHashMap<TermItemId, TypeId>,
    pub types: FxHashMap<TypeItemId, BindingGroupType>,
}

impl BindingGroupContext {
    pub fn lookup_term(&self, id: TermItemId) -> Option<TypeId> {
        self.terms.get(&id).copied()
    }

    pub fn lookup_type(&self, id: TypeItemId) -> Option<TypeId> {
        self.types.get(&id).map(|binding| binding.0)
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
    pub prim_boolean: PrimBooleanCore,
    pub prim_ordering: PrimOrderingCore,
    pub prim_symbol: PrimSymbolCore,
    pub prim_row: PrimRowCore,
    pub prim_row_list: PrimRowListCore,
    pub prim_coerce: PrimCoerceCore,
    pub prim_type_error: PrimTypeErrorCore,
    pub known_types: KnownTypesCore,
    pub known_terms: KnownTermsCore,
    pub known_reflectable: KnownReflectableCore,
    pub known_generic: Option<KnownGeneric>,

    pub id: FileId,
    pub stabilized: Arc<StabilizedModule>,
    pub indexed: Arc<IndexedModule>,
    pub lowered: Arc<LoweredModule>,
    pub grouped: Arc<GroupedModule>,
    pub bracketed: Arc<Bracketed>,
    pub sectioned: Arc<Sectioned>,
    pub resolved: Arc<ResolvedModule>,

    pub prim_indexed: Arc<IndexedModule>,
    pub prim_resolved: Arc<ResolvedModule>,
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
        let stabilized = queries.stabilized(id)?;
        let indexed = queries.indexed(id)?;
        let lowered = queries.lowered(id)?;
        let grouped = queries.grouped(id)?;
        let bracketed = queries.bracketed(id)?;
        let sectioned = queries.sectioned(id)?;
        let prim = PrimCore::collect(queries, state)?;
        let prim_int = PrimIntCore::collect(queries, state)?;
        let prim_boolean = PrimBooleanCore::collect(queries, state)?;
        let prim_ordering = PrimOrderingCore::collect(queries, state)?;
        let prim_symbol = PrimSymbolCore::collect(queries, state)?;
        let prim_row = PrimRowCore::collect(queries, state)?;
        let prim_row_list = PrimRowListCore::collect(queries, state)?;
        let prim_coerce = PrimCoerceCore::collect(queries)?;
        let prim_type_error = PrimTypeErrorCore::collect(queries, state)?;
        let known_types = KnownTypesCore::collect(queries)?;
        let known_terms = KnownTermsCore::collect(queries)?;
        let known_reflectable = KnownReflectableCore::collect(queries, &mut state.storage)?;
        let known_generic = KnownGeneric::collect(queries, &mut state.storage)?;
        let resolved = queries.resolved(id)?;
        let prim_id = queries.prim_id();
        let prim_indexed = queries.indexed(prim_id)?;
        let prim_resolved = queries.resolved(prim_id)?;
        Ok(CheckContext {
            queries,
            prim,
            prim_int,
            prim_boolean,
            prim_ordering,
            prim_symbol,
            prim_row,
            prim_row_list,
            prim_coerce,
            prim_type_error,
            known_types,
            known_terms,
            known_reflectable,
            known_generic,
            id,
            stabilized,
            indexed,
            lowered,
            grouped,
            bracketed,
            sectioned,
            resolved,
            prim_indexed,
            prim_resolved,
        })
    }
}

struct PrimLookup<'r, 's> {
    resolved: &'r ResolvedModule,
    storage: &'s mut TypeInterner,
    module_name: &'static str,
}

impl<'r, 's> PrimLookup<'r, 's> {
    fn new(
        resolved: &'r ResolvedModule,
        storage: &'s mut TypeInterner,
        module_name: &'static str,
    ) -> Self {
        PrimLookup { resolved, storage, module_name }
    }

    fn type_item(&self, name: &str) -> TypeItemId {
        let (_, type_id) =
            self.resolved.lookup_type(self.resolved, None, name).unwrap_or_else(|| {
                unreachable!("invariant violated: {name} not in {}", self.module_name)
            });
        type_id
    }

    fn type_constructor(&mut self, name: &str) -> TypeId {
        let (file_id, type_id) =
            self.resolved.lookup_type(self.resolved, None, name).unwrap_or_else(|| {
                unreachable!("invariant violated: {name} not in {}", self.module_name)
            });
        self.storage.intern(Type::Constructor(file_id, type_id))
    }

    fn intern(&mut self, ty: Type) -> TypeId {
        self.storage.intern(ty)
    }
}

pub struct PrimCore {
    pub prim_id: FileId,
    pub t: TypeId,
    pub type_to_type: TypeId,
    pub function: TypeId,
    pub function_item: TypeItemId,
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
    pub row_type: TypeId,
    pub unknown: TypeId,
}

impl PrimCore {
    fn collect(queries: &impl ExternalQueries, state: &mut CheckState) -> QueryResult<PrimCore> {
        let prim_id = queries.prim_id();
        let resolved = queries.resolved(prim_id)?;
        let mut lookup = PrimLookup::new(&resolved, &mut state.storage, "Prim");

        let t = lookup.type_constructor("Type");
        let type_to_type = lookup.intern(Type::Function(t, t));

        let row = lookup.type_constructor("Row");
        let row_type = lookup.intern(Type::Application(row, t));

        let function = lookup.type_constructor("Function");
        let function_item = lookup.type_item("Function");

        Ok(PrimCore {
            prim_id,
            t,
            type_to_type,
            function,
            function_item,
            array: lookup.type_constructor("Array"),
            record: lookup.type_constructor("Record"),
            number: lookup.type_constructor("Number"),
            int: lookup.type_constructor("Int"),
            string: lookup.type_constructor("String"),
            char: lookup.type_constructor("Char"),
            boolean: lookup.type_constructor("Boolean"),
            partial: lookup.type_constructor("Partial"),
            constraint: lookup.type_constructor("Constraint"),
            symbol: lookup.type_constructor("Symbol"),
            row,
            row_type,
            unknown: lookup.intern(Type::Unknown),
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
    fn collect(queries: &impl ExternalQueries, state: &mut CheckState) -> QueryResult<PrimIntCore> {
        let file_id = queries
            .module_file("Prim.Int")
            .unwrap_or_else(|| unreachable!("invariant violated: Prim.Int not found"));

        let resolved = queries.resolved(file_id)?;
        let lookup = PrimLookup::new(&resolved, &mut state.storage, "Prim.Int");

        Ok(PrimIntCore {
            file_id,
            add: lookup.type_item("Add"),
            mul: lookup.type_item("Mul"),
            compare: lookup.type_item("Compare"),
            to_string: lookup.type_item("ToString"),
        })
    }
}

pub struct PrimBooleanCore {
    pub true_: TypeId,
    pub false_: TypeId,
}

impl PrimBooleanCore {
    fn collect(
        queries: &impl ExternalQueries,
        state: &mut CheckState,
    ) -> QueryResult<PrimBooleanCore> {
        let file_id = queries
            .module_file("Prim.Boolean")
            .unwrap_or_else(|| unreachable!("invariant violated: Prim.Boolean not found"));

        let resolved = queries.resolved(file_id)?;
        let mut lookup = PrimLookup::new(&resolved, &mut state.storage, "Prim.Boolean");

        Ok(PrimBooleanCore {
            true_: lookup.type_constructor("True"),
            false_: lookup.type_constructor("False"),
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
    fn collect(
        queries: &impl ExternalQueries,
        state: &mut CheckState,
    ) -> QueryResult<PrimSymbolCore> {
        let file_id = queries
            .module_file("Prim.Symbol")
            .unwrap_or_else(|| unreachable!("invariant violated: Prim.Symbol not found"));

        let resolved = queries.resolved(file_id)?;
        let lookup = PrimLookup::new(&resolved, &mut state.storage, "Prim.Symbol");

        Ok(PrimSymbolCore {
            file_id,
            append: lookup.type_item("Append"),
            compare: lookup.type_item("Compare"),
            cons: lookup.type_item("Cons"),
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
        let mut lookup = PrimLookup::new(&resolved, &mut state.storage, "Prim.Ordering");

        Ok(PrimOrderingCore {
            lt: lookup.type_constructor("LT"),
            eq: lookup.type_constructor("EQ"),
            gt: lookup.type_constructor("GT"),
        })
    }
}

pub struct PrimRowCore {
    pub file_id: FileId,
    pub union: TypeItemId,
    pub cons: TypeItemId,
    pub lacks: TypeItemId,
    pub nub: TypeItemId,
}

impl PrimRowCore {
    fn collect(queries: &impl ExternalQueries, state: &mut CheckState) -> QueryResult<PrimRowCore> {
        let file_id = queries
            .module_file("Prim.Row")
            .unwrap_or_else(|| unreachable!("invariant violated: Prim.Row not found"));

        let resolved = queries.resolved(file_id)?;
        let lookup = PrimLookup::new(&resolved, &mut state.storage, "Prim.Row");

        Ok(PrimRowCore {
            file_id,
            union: lookup.type_item("Union"),
            cons: lookup.type_item("Cons"),
            lacks: lookup.type_item("Lacks"),
            nub: lookup.type_item("Nub"),
        })
    }
}

pub struct PrimRowListCore {
    pub file_id: FileId,
    pub row_to_list: TypeItemId,
    pub cons: TypeId,
    pub nil: TypeId,
}

impl PrimRowListCore {
    fn collect(
        queries: &impl ExternalQueries,
        state: &mut CheckState,
    ) -> QueryResult<PrimRowListCore> {
        let file_id = queries
            .module_file("Prim.RowList")
            .unwrap_or_else(|| unreachable!("invariant violated: Prim.RowList not found"));

        let resolved = queries.resolved(file_id)?;
        let mut lookup = PrimLookup::new(&resolved, &mut state.storage, "Prim.RowList");

        Ok(PrimRowListCore {
            file_id,
            row_to_list: lookup.type_item("RowToList"),
            cons: lookup.type_constructor("Cons"),
            nil: lookup.type_constructor("Nil"),
        })
    }
}

pub struct PrimCoerceCore {
    pub file_id: FileId,
    pub coercible: TypeItemId,
}

impl PrimCoerceCore {
    fn collect(queries: &impl ExternalQueries) -> QueryResult<PrimCoerceCore> {
        let file_id = queries
            .module_file("Prim.Coerce")
            .unwrap_or_else(|| unreachable!("invariant violated: Prim.Coerce not found"));

        let resolved = queries.resolved(file_id)?;
        let (_, coercible) = resolved
            .exports
            .lookup_type("Coercible")
            .unwrap_or_else(|| unreachable!("invariant violated: Coercible not in Prim.Coerce"));

        Ok(PrimCoerceCore { file_id, coercible })
    }
}

pub struct PrimTypeErrorCore {
    pub file_id: FileId,
    pub warn: TypeItemId,
    pub fail: TypeItemId,
    pub text: TypeId,
    pub quote: TypeId,
    pub quote_label: TypeId,
    pub beside: TypeId,
    pub above: TypeId,
}

impl PrimTypeErrorCore {
    fn collect(queries: &impl ExternalQueries, state: &mut CheckState) -> QueryResult<Self> {
        let file_id = queries
            .module_file("Prim.TypeError")
            .unwrap_or_else(|| unreachable!("invariant violated: Prim.TypeError not found"));

        let resolved = queries.resolved(file_id)?;
        let mut lookup = PrimLookup::new(&resolved, &mut state.storage, "Prim.TypeError");

        Ok(PrimTypeErrorCore {
            file_id,
            warn: lookup.type_item("Warn"),
            fail: lookup.type_item("Fail"),
            text: lookup.type_constructor("Text"),
            quote: lookup.type_constructor("Quote"),
            quote_label: lookup.type_constructor("QuoteLabel"),
            beside: lookup.type_constructor("Beside"),
            above: lookup.type_constructor("Above"),
        })
    }
}

fn fetch_known_term(
    queries: &impl ExternalQueries,
    m: &str,
    n: &str,
) -> QueryResult<Option<(FileId, indexing::TermItemId)>> {
    let Some(file_id) = queries.module_file(m) else {
        return Ok(None);
    };
    let resolved = queries.resolved(file_id)?;
    let Some((file_id, term_id)) = resolved.exports.lookup_term(n) else {
        return Ok(None);
    };
    Ok(Some((file_id, term_id)))
}

fn fetch_known_type(
    queries: &impl ExternalQueries,
    m: &str,
    n: &str,
) -> QueryResult<Option<(FileId, TypeItemId)>> {
    let Some(file_id) = queries.module_file(m) else {
        return Ok(None);
    };
    let resolved = queries.resolved(file_id)?;
    let Some((file_id, type_id)) = resolved.exports.lookup_type(n) else {
        return Ok(None);
    };
    Ok(Some((file_id, type_id)))
}

fn fetch_known_constructor(
    queries: &impl ExternalQueries,
    storage: &mut TypeInterner,
    m: &str,
    n: &str,
) -> QueryResult<Option<TypeId>> {
    let Some(file_id) = queries.module_file(m) else {
        return Ok(None);
    };
    let resolved = queries.resolved(file_id)?;
    let Some((file_id, type_id)) = resolved.exports.lookup_type(n) else {
        return Ok(None);
    };
    Ok(Some(storage.intern(Type::Constructor(file_id, type_id))))
}

pub struct KnownTypesCore {
    pub eq: Option<(FileId, TypeItemId)>,
    pub eq1: Option<(FileId, TypeItemId)>,
    pub ord: Option<(FileId, TypeItemId)>,
    pub ord1: Option<(FileId, TypeItemId)>,
    pub functor: Option<(FileId, TypeItemId)>,
    pub bifunctor: Option<(FileId, TypeItemId)>,
    pub contravariant: Option<(FileId, TypeItemId)>,
    pub profunctor: Option<(FileId, TypeItemId)>,
    pub foldable: Option<(FileId, TypeItemId)>,
    pub bifoldable: Option<(FileId, TypeItemId)>,
    pub traversable: Option<(FileId, TypeItemId)>,
    pub bitraversable: Option<(FileId, TypeItemId)>,
    pub newtype: Option<(FileId, TypeItemId)>,
    pub generic: Option<(FileId, TypeItemId)>,
}

impl KnownTypesCore {
    fn collect(queries: &impl ExternalQueries) -> QueryResult<KnownTypesCore> {
        let eq = fetch_known_type(queries, "Data.Eq", "Eq")?;
        let eq1 = fetch_known_type(queries, "Data.Eq", "Eq1")?;
        let ord = fetch_known_type(queries, "Data.Ord", "Ord")?;
        let ord1 = fetch_known_type(queries, "Data.Ord", "Ord1")?;
        let functor = fetch_known_type(queries, "Data.Functor", "Functor")?;
        let bifunctor = fetch_known_type(queries, "Data.Bifunctor", "Bifunctor")?;
        let contravariant =
            fetch_known_type(queries, "Data.Functor.Contravariant", "Contravariant")?;
        let profunctor = fetch_known_type(queries, "Data.Profunctor", "Profunctor")?;
        let foldable = fetch_known_type(queries, "Data.Foldable", "Foldable")?;
        let bifoldable = fetch_known_type(queries, "Data.Bifoldable", "Bifoldable")?;
        let traversable = fetch_known_type(queries, "Data.Traversable", "Traversable")?;
        let bitraversable = fetch_known_type(queries, "Data.Bitraversable", "Bitraversable")?;
        let newtype = fetch_known_type(queries, "Data.Newtype", "Newtype")?;
        let generic = fetch_known_type(queries, "Data.Generic.Rep", "Generic")?;
        Ok(KnownTypesCore {
            eq,
            eq1,
            ord,
            ord1,
            functor,
            bifunctor,
            contravariant,
            profunctor,
            foldable,
            bifoldable,
            traversable,
            bitraversable,
            newtype,
            generic,
        })
    }
}

pub struct KnownReflectableCore {
    pub is_symbol: Option<(FileId, TypeItemId)>,
    pub reflectable: Option<(FileId, TypeItemId)>,
    pub ordering: Option<TypeId>,
}

impl KnownReflectableCore {
    fn collect(
        queries: &impl ExternalQueries,
        storage: &mut TypeInterner,
    ) -> QueryResult<KnownReflectableCore> {
        let is_symbol = fetch_known_type(queries, "Data.Symbol", "IsSymbol")?;
        let reflectable = fetch_known_type(queries, "Data.Reflectable", "Reflectable")?;
        let ordering = fetch_known_constructor(queries, storage, "Data.Ordering", "Ordering")?;
        Ok(KnownReflectableCore { is_symbol, reflectable, ordering })
    }
}

pub struct KnownGeneric {
    pub no_constructors: TypeId,
    pub constructor: TypeId,
    pub sum: TypeId,
    pub product: TypeId,
    pub no_arguments: TypeId,
    pub argument: TypeId,
}

impl KnownGeneric {
    fn collect(
        queries: &impl ExternalQueries,
        storage: &mut TypeInterner,
    ) -> QueryResult<Option<KnownGeneric>> {
        let Some(no_constructors) =
            fetch_known_constructor(queries, storage, "Data.Generic.Rep", "NoConstructors")?
        else {
            return Ok(None);
        };
        let Some(constructor) =
            fetch_known_constructor(queries, storage, "Data.Generic.Rep", "Constructor")?
        else {
            return Ok(None);
        };
        let Some(sum) = fetch_known_constructor(queries, storage, "Data.Generic.Rep", "Sum")?
        else {
            return Ok(None);
        };
        let Some(product) =
            fetch_known_constructor(queries, storage, "Data.Generic.Rep", "Product")?
        else {
            return Ok(None);
        };
        let Some(no_arguments) =
            fetch_known_constructor(queries, storage, "Data.Generic.Rep", "NoArguments")?
        else {
            return Ok(None);
        };
        let Some(argument) =
            fetch_known_constructor(queries, storage, "Data.Generic.Rep", "Argument")?
        else {
            return Ok(None);
        };
        Ok(Some(KnownGeneric {
            no_constructors,
            constructor,
            sum,
            product,
            no_arguments,
            argument,
        }))
    }
}

pub struct KnownTermsCore {
    pub otherwise: Option<(FileId, indexing::TermItemId)>,
}

impl KnownTermsCore {
    fn collect(queries: &impl ExternalQueries) -> QueryResult<KnownTermsCore> {
        let otherwise = fetch_known_term(queries, "Data.Boolean", "otherwise")?;
        Ok(KnownTermsCore { otherwise })
    }
}

impl CheckState {
    /// Executes the given closure with a term binding group in scope.
    ///
    /// This inserts unification variables for the given term items such that
    /// recursive or mutually recursive declarations can be checked together.
    pub fn with_term_group<Q, F, T>(
        &mut self,
        context: &CheckContext<Q>,
        group: impl IntoIterator<Item = TermItemId>,
        f: F,
    ) -> T
    where
        Q: ExternalQueries,
        F: FnOnce(&mut Self) -> T,
    {
        for item in group {
            if !self.checked.terms.contains_key(&item) {
                let t = self.fresh_unification_type(context);
                self.binding_group.terms.insert(item, t);
            }
        }

        let result = f(self);
        self.binding_group.terms.clear();
        result
    }

    /// Executes the given closure with a type binding group in scope.
    ///
    /// This inserts unification variables for the given type items such that
    /// recursive or mutually recursive declarations can be checked together.
    pub fn with_type_group<Q, F, T>(
        &mut self,
        context: &CheckContext<Q>,
        group: impl AsRef<[TypeItemId]>,
        f: F,
    ) -> T
    where
        Q: ExternalQueries,
        F: FnOnce(&mut Self) -> T,
    {
        let needs_pending = group.as_ref().iter().filter(|&&item_id| {
            if let Some(TypeItemIr::Operator { .. }) = context.lowered.info.get_type_item(item_id) {
                return false;
            }
            if self.checked.types.contains_key(&item_id) {
                return false;
            }
            true
        });

        for item in needs_pending.copied().collect_vec() {
            let kind = self.fresh_unification_type(context);
            self.binding_group.types.insert(item, BindingGroupType(kind));
        }

        let operators = group.as_ref().iter().filter_map(|&item_id| {
            let TypeItemIr::Operator { resolution, .. } =
                context.lowered.info.get_type_item(item_id)?
            else {
                return None;
            };
            let resolution = resolution.as_ref()?;
            Some((item_id, *resolution))
        });

        for (operator_id, (file_id, item_id)) in operators {
            debug_assert!(
                file_id == context.id && group.as_ref().contains(&item_id),
                "invariant violated: expected local target for operator"
            );

            let kind = self.binding_group.lookup_type(item_id).or_else(|| {
                let kind = self.checked.types.get(&item_id)?;
                Some(transfer::localize(self, context, *kind))
            });

            let kind = kind.expect("invariant violated: expected kind for operator target");
            self.binding_group.types.insert(operator_id, BindingGroupType(kind));
        }

        let result = f(self);
        self.binding_group.types.clear();
        result
    }

    pub fn with_local_givens<T>(&mut self, action: impl FnOnce(&mut Self) -> T) -> T {
        let length = self.constraints.given.len();
        let result = action(self);
        self.constraints.given.drain(length..);
        result
    }

    pub fn solve_constraints<Q>(&mut self, context: &CheckContext<Q>) -> QueryResult<Vec<TypeId>>
    where
        Q: ExternalQueries,
    {
        let (wanted, given) = self.constraints.take();
        let residuals = constraint::solve_constraints(self, context, wanted, &given);

        let after_solve = mem::replace(&mut self.constraints.given, given);
        debug_assert!(after_solve.is_empty(), "invariant violated: non-empty givens");

        residuals
    }


    pub fn report_exhaustiveness(&mut self, exhaustiveness: ExhaustivenessReport) {
        if let Some(patterns) = exhaustiveness.missing {
            let patterns = Arc::from(patterns);
            self.insert_error(ErrorKind::MissingPatterns { patterns });
        }

        if !exhaustiveness.redundant.is_empty() {
            let patterns = Arc::from(exhaustiveness.redundant);
            self.insert_error(ErrorKind::RedundantPatterns { patterns });
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

    /// Interns an error message in [`CheckedModule::error_messages`].
    pub fn intern_error_message(&mut self, message: impl ToSmolStr) -> TypeErrorMessageId {
        self.checked.error_messages.intern(message.to_smolstr())
    }

    /// Renders a local type and interns it in [`CheckedModule::error_messages`].
    pub fn render_local_type<Q>(
        &mut self,
        context: &CheckContext<Q>,
        t: TypeId,
    ) -> TypeErrorMessageId
    where
        Q: ExternalQueries,
    {
        let t = pretty::print_local(self, context, t);
        self.intern_error_message(t)
    }
}

/// Functions for creating unification variables.
impl CheckState {
    /// Creates a fresh unification variable with the provided depth and kind.
    pub fn fresh_unification_kinded_at(&mut self, depth: debruijn::Size, kind: TypeId) -> TypeId {
        let unification_id = self.unification.fresh(depth, kind);
        self.storage.intern(Type::Unification(unification_id))
    }

    /// Creates a fresh unification variable with the provided kind.
    pub fn fresh_unification_kinded(&mut self, kind: TypeId) -> TypeId {
        let depth = self.type_scope.size();
        self.fresh_unification_kinded_at(depth, kind)
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

    /// Creates a fresh skolem variable with the provided kind.
    pub fn fresh_skolem_kinded(&mut self, kind: TypeId) -> TypeId {
        let domain = self.type_scope.size();
        let level = debruijn::Level(domain.0);
        let skolem = Variable::Skolem(level, kind);
        self.storage.intern(Type::Variable(skolem))
    }
}

impl CheckState {
    /// Normalises unification and bound type variables.
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

impl CheckState {
    pub fn allocate_pattern(&mut self, kind: PatternKind, t: TypeId) -> PatternId {
        let pattern = Pattern { kind, t };
        self.patterns.intern(pattern)
    }

    pub fn allocate_constructor(
        &mut self,
        constructor: PatternConstructor,
        t: TypeId,
    ) -> PatternId {
        let kind = PatternKind::Constructor { constructor };
        self.allocate_pattern(kind, t)
    }

    pub fn allocate_wildcard(&mut self, t: TypeId) -> PatternId {
        self.allocate_pattern(PatternKind::Wildcard, t)
    }
}
