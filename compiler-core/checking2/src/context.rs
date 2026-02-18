//! Read-only environment for the type checking algorithm.
//!
//! See documentation for [`CheckContext`] for more information.

use std::sync::Arc;

use building_types::QueryResult;
use files::FileId;
use indexing::{IndexedModule, TermItemId, TypeItemId};
use lowering::{GroupedModule, LoweredModule};
use resolving::ResolvedModule;
use stabilizing::StabilizedModule;
use sugar::{Bracketed, Sectioned};

use crate::ExternalQueries;
use crate::core::{
    Depth, ForallBinder, ForallBinderId, Name, RowType, RowTypeId, Synonym, SynonymId, Type, TypeId,
};

/// The read-only environment threaded through the type checking algorithm.
///
/// This structure holds a reference to [`ExternalQueries`] for interning and
/// making build system queries; Arc references to query results for the current
/// module; and cached lookups for modules and items 'known' by the compiler.
pub struct CheckContext<'q, Q>
where
    Q: ExternalQueries,
{
    pub queries: &'q Q,

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

impl<'q, Q> CheckContext<'q, Q>
where
    Q: ExternalQueries,
{
    pub fn new(queries: &'q Q, id: FileId) -> QueryResult<CheckContext<'q, Q>> {
        let stabilized = queries.stabilized(id)?;
        let indexed = queries.indexed(id)?;
        let lowered = queries.lowered(id)?;
        let grouped = queries.grouped(id)?;
        let bracketed = queries.bracketed(id)?;
        let sectioned = queries.sectioned(id)?;
        let resolved = queries.resolved(id)?;

        let prim = PrimCore::collect(queries)?;
        let prim_int = PrimIntCore::collect(queries)?;
        let prim_boolean = PrimBooleanCore::collect(queries)?;
        let prim_ordering = PrimOrderingCore::collect(queries)?;
        let prim_symbol = PrimSymbolCore::collect(queries)?;
        let prim_row = PrimRowCore::collect(queries)?;
        let prim_row_list = PrimRowListCore::collect(queries)?;
        let prim_coerce = PrimCoerceCore::collect(queries)?;
        let prim_type_error = PrimTypeErrorCore::collect(queries)?;
        let known_types = KnownTypesCore::collect(queries)?;
        let known_terms = KnownTermsCore::collect(queries)?;
        let known_reflectable = KnownReflectableCore::collect(queries)?;
        let known_generic = KnownGeneric::collect(queries)?;

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

impl<'q, Q> CheckContext<'q, Q>
where
    Q: ExternalQueries,
{
    /// Interns a [`Type::Application`] node.
    pub fn intern_application(&self, function: TypeId, argument: TypeId) -> TypeId {
        self.queries.intern_type(Type::Application(function, argument))
    }

    /// Interns a [`Type::KindApplication`] node.
    pub fn intern_kind_application(&self, function: TypeId, argument: TypeId) -> TypeId {
        self.queries.intern_type(Type::KindApplication(function, argument))
    }

    /// Interns a [`Type::OperatorApplication`] node.
    pub fn intern_operator_application(
        &self,
        file_id: FileId,
        type_id: TypeItemId,
        left: TypeId,
        right: TypeId,
    ) -> TypeId {
        self.queries.intern_type(Type::OperatorApplication(file_id, type_id, left, right))
    }

    /// Interns a [`Type::SynonymApplication`] node.
    pub fn intern_synonym_application(&self, synonym_id: SynonymId) -> TypeId {
        self.queries.intern_type(Type::SynonymApplication(synonym_id))
    }

    /// Interns a [`Type::Forall`] node.
    pub fn intern_forall(&self, binder_id: ForallBinderId, inner: TypeId) -> TypeId {
        self.queries.intern_type(Type::Forall(binder_id, inner))
    }

    /// Interns a [`Type::Constrained`] node.
    pub fn intern_constrained(&self, constraint: TypeId, inner: TypeId) -> TypeId {
        self.queries.intern_type(Type::Constrained(constraint, inner))
    }

    /// Interns a [`Type::Function`] node.
    pub fn intern_function(&self, argument: TypeId, result: TypeId) -> TypeId {
        self.queries.intern_type(Type::Function(argument, result))
    }

    /// Interns a [`Type::Kinded`] node.
    pub fn intern_kinded(&self, inner: TypeId, kind: TypeId) -> TypeId {
        self.queries.intern_type(Type::Kinded(inner, kind))
    }

    /// Interns a [`Type::Row`] node.
    pub fn intern_row(&self, row_id: RowTypeId) -> TypeId {
        self.queries.intern_type(Type::Row(row_id))
    }

    /// Interns a [`Type::Rigid`] node.
    pub fn intern_rigid(&self, name: Name, depth: Depth, kind: TypeId) -> TypeId {
        self.queries.intern_type(Type::Rigid(name, depth, kind))
    }

    /// Interns a [`Type::Application`]-based function.
    ///
    /// The types `Function a b` and `a -> b` are equivalent, represented by
    /// [`Type::Application`] and [`Type::Function`] respectively. Normalising
    /// into the application-based form is generally more useful, such as in
    /// the following example:
    ///
    /// ```text
    /// unify(?function_a b, a -> b)
    ///   unify(?function_a b, Function a b) = [ ?function_a := Function a ]
    /// ```
    pub fn intern_function_application(&self, argument: TypeId, result: TypeId) -> TypeId {
        let function_argument = self.intern_application(self.prim.function, argument);
        self.intern_application(function_argument, result)
    }

    /// Looks up the [`Type`] for the given [`TypeId`].
    pub fn lookup_type(&self, id: TypeId) -> Type {
        self.queries.lookup_type(id)
    }

    /// Looks up the [`ForallBinder`] for the given [`ForallBinderId`].
    pub fn lookup_forall_binder(&self, id: ForallBinderId) -> ForallBinder {
        self.queries.lookup_forall_binder(id)
    }

    /// Looks up the [`RowType`] for the given [`RowTypeId`].
    pub fn lookup_row_type(&self, id: RowTypeId) -> RowType {
        self.queries.lookup_row_type(id)
    }

    /// Looks up the [`Synonym`] for the given [`SynonymId`].
    pub fn lookup_synonym(&self, id: SynonymId) -> Synonym {
        self.queries.lookup_synonym(id)
    }

    /// Interns a [`ForallBinder`], returning its [`ForallBinderId`].
    pub fn intern_forall_binder(&self, binder: ForallBinder) -> ForallBinderId {
        self.queries.intern_forall_binder(binder)
    }

    /// Interns a [`RowType`], returning its [`RowTypeId`].
    pub fn intern_row_type(&self, row: RowType) -> RowTypeId {
        self.queries.intern_row_type(row)
    }

    /// Interns a [`Synonym`], returning its [`SynonymId`].
    pub fn intern_synonym(&self, synonym: Synonym) -> SynonymId {
        self.queries.intern_synonym(synonym)
    }
}

struct PrimLookup<'r, 'q, Q>
where
    Q: ExternalQueries,
{
    resolved: &'r ResolvedModule,
    queries: &'q Q,
    module_name: &'static str,
}

impl<'r, 'q, Q: ExternalQueries> PrimLookup<'r, 'q, Q> {
    fn new(resolved: &'r ResolvedModule, queries: &'q Q, module_name: &'static str) -> Self {
        PrimLookup { resolved, queries, module_name }
    }

    fn type_item(&self, name: &str) -> TypeItemId {
        let (_, type_id) = self.resolved.exports.lookup_type(name).unwrap_or_else(|| {
            unreachable!("invariant violated: {name} not in {}", self.module_name)
        });
        type_id
    }

    fn type_constructor(&self, name: &str) -> TypeId {
        let (file_id, type_id) = self.resolved.exports.lookup_type(name).unwrap_or_else(|| {
            unreachable!("invariant violated: {name} not in {}", self.module_name)
        });
        self.queries.intern_type(Type::Constructor(file_id, type_id))
    }

    fn class_item(&self, name: &str) -> TypeItemId {
        let (_, type_id) = self.resolved.exports.lookup_class(name).unwrap_or_else(|| {
            unreachable!("invariant violated: {name} not in {}", self.module_name)
        });
        type_id
    }

    fn class_constructor(&self, name: &str) -> TypeId {
        let (file_id, type_id) = self.resolved.exports.lookup_class(name).unwrap_or_else(|| {
            unreachable!("invariant violated: {name} not in {}", self.module_name)
        });
        self.queries.intern_type(Type::Constructor(file_id, type_id))
    }

    fn intern(&self, ty: Type) -> TypeId {
        self.queries.intern_type(ty)
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
}

impl PrimCore {
    fn collect(queries: &impl ExternalQueries) -> QueryResult<PrimCore> {
        let prim_id = queries.prim_id();
        let resolved = queries.resolved(prim_id)?;
        let lookup = PrimLookup::new(&resolved, queries, "Prim");

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
            partial: lookup.class_constructor("Partial"),
            constraint: lookup.type_constructor("Constraint"),
            symbol: lookup.type_constructor("Symbol"),
            row,
            row_type,
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
        let lookup = PrimLookup::new(&resolved, queries, "Prim.Int");

        Ok(PrimIntCore {
            file_id,
            add: lookup.class_item("Add"),
            mul: lookup.class_item("Mul"),
            compare: lookup.class_item("Compare"),
            to_string: lookup.class_item("ToString"),
        })
    }
}

pub struct PrimBooleanCore {
    pub true_: TypeId,
    pub false_: TypeId,
}

impl PrimBooleanCore {
    fn collect(queries: &impl ExternalQueries) -> QueryResult<PrimBooleanCore> {
        let file_id = queries
            .module_file("Prim.Boolean")
            .unwrap_or_else(|| unreachable!("invariant violated: Prim.Boolean not found"));

        let resolved = queries.resolved(file_id)?;
        let lookup = PrimLookup::new(&resolved, queries, "Prim.Boolean");

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

impl PrimOrderingCore {
    fn collect(queries: &impl ExternalQueries) -> QueryResult<PrimOrderingCore> {
        let file_id = queries
            .module_file("Prim.Ordering")
            .unwrap_or_else(|| unreachable!("invariant violated: Prim.Ordering not found"));

        let resolved = queries.resolved(file_id)?;
        let lookup = PrimLookup::new(&resolved, queries, "Prim.Ordering");

        Ok(PrimOrderingCore {
            lt: lookup.type_constructor("LT"),
            eq: lookup.type_constructor("EQ"),
            gt: lookup.type_constructor("GT"),
        })
    }
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
        let lookup = PrimLookup::new(&resolved, queries, "Prim.Symbol");

        Ok(PrimSymbolCore {
            file_id,
            append: lookup.class_item("Append"),
            compare: lookup.class_item("Compare"),
            cons: lookup.class_item("Cons"),
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
    fn collect(queries: &impl ExternalQueries) -> QueryResult<PrimRowCore> {
        let file_id = queries
            .module_file("Prim.Row")
            .unwrap_or_else(|| unreachable!("invariant violated: Prim.Row not found"));

        let resolved = queries.resolved(file_id)?;
        let lookup = PrimLookup::new(&resolved, queries, "Prim.Row");

        Ok(PrimRowCore {
            file_id,
            union: lookup.class_item("Union"),
            cons: lookup.class_item("Cons"),
            lacks: lookup.class_item("Lacks"),
            nub: lookup.class_item("Nub"),
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
    fn collect(queries: &impl ExternalQueries) -> QueryResult<PrimRowListCore> {
        let file_id = queries
            .module_file("Prim.RowList")
            .unwrap_or_else(|| unreachable!("invariant violated: Prim.RowList not found"));

        let resolved = queries.resolved(file_id)?;
        let lookup = PrimLookup::new(&resolved, queries, "Prim.RowList");

        Ok(PrimRowListCore {
            file_id,
            row_to_list: lookup.class_item("RowToList"),
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
            .lookup_class("Coercible")
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
    fn collect(queries: &impl ExternalQueries) -> QueryResult<PrimTypeErrorCore> {
        let file_id = queries
            .module_file("Prim.TypeError")
            .unwrap_or_else(|| unreachable!("invariant violated: Prim.TypeError not found"));

        let resolved = queries.resolved(file_id)?;
        let lookup = PrimLookup::new(&resolved, queries, "Prim.TypeError");

        Ok(PrimTypeErrorCore {
            file_id,
            warn: lookup.class_item("Warn"),
            fail: lookup.class_item("Fail"),
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
) -> QueryResult<Option<(FileId, TermItemId)>> {
    let Some(file_id) = queries.module_file(m) else {
        return Ok(None);
    };
    let resolved = queries.resolved(file_id)?;
    let Some((file_id, term_id)) = resolved.exports.lookup_term(n) else {
        return Ok(None);
    };
    Ok(Some((file_id, term_id)))
}

fn fetch_known_class(
    queries: &impl ExternalQueries,
    m: &str,
    n: &str,
) -> QueryResult<Option<(FileId, TypeItemId)>> {
    let Some(file_id) = queries.module_file(m) else {
        return Ok(None);
    };
    let resolved = queries.resolved(file_id)?;
    let Some((file_id, type_id)) = resolved.exports.lookup_class(n) else {
        return Ok(None);
    };
    Ok(Some((file_id, type_id)))
}

fn fetch_known_constructor(
    queries: &impl ExternalQueries,
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
    Ok(Some(queries.intern_type(Type::Constructor(file_id, type_id))))
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
        let eq = fetch_known_class(queries, "Data.Eq", "Eq")?;
        let eq1 = fetch_known_class(queries, "Data.Eq", "Eq1")?;
        let ord = fetch_known_class(queries, "Data.Ord", "Ord")?;
        let ord1 = fetch_known_class(queries, "Data.Ord", "Ord1")?;
        let functor = fetch_known_class(queries, "Data.Functor", "Functor")?;
        let bifunctor = fetch_known_class(queries, "Data.Bifunctor", "Bifunctor")?;
        let contravariant =
            fetch_known_class(queries, "Data.Functor.Contravariant", "Contravariant")?;
        let profunctor = fetch_known_class(queries, "Data.Profunctor", "Profunctor")?;
        let foldable = fetch_known_class(queries, "Data.Foldable", "Foldable")?;
        let bifoldable = fetch_known_class(queries, "Data.Bifoldable", "Bifoldable")?;
        let traversable = fetch_known_class(queries, "Data.Traversable", "Traversable")?;
        let bitraversable = fetch_known_class(queries, "Data.Bitraversable", "Bitraversable")?;
        let newtype = fetch_known_class(queries, "Data.Newtype", "Newtype")?;
        let generic = fetch_known_class(queries, "Data.Generic.Rep", "Generic")?;
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
    fn collect(queries: &impl ExternalQueries) -> QueryResult<KnownReflectableCore> {
        let is_symbol = fetch_known_class(queries, "Data.Symbol", "IsSymbol")?;
        let reflectable = fetch_known_class(queries, "Data.Reflectable", "Reflectable")?;
        let ordering = fetch_known_constructor(queries, "Data.Ordering", "Ordering")?;
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
    fn collect(queries: &impl ExternalQueries) -> QueryResult<Option<KnownGeneric>> {
        let Some(no_constructors) =
            fetch_known_constructor(queries, "Data.Generic.Rep", "NoConstructors")?
        else {
            return Ok(None);
        };
        let Some(constructor) =
            fetch_known_constructor(queries, "Data.Generic.Rep", "Constructor")?
        else {
            return Ok(None);
        };
        let Some(sum) = fetch_known_constructor(queries, "Data.Generic.Rep", "Sum")? else {
            return Ok(None);
        };
        let Some(product) = fetch_known_constructor(queries, "Data.Generic.Rep", "Product")? else {
            return Ok(None);
        };
        let Some(no_arguments) =
            fetch_known_constructor(queries, "Data.Generic.Rep", "NoArguments")?
        else {
            return Ok(None);
        };
        let Some(argument) = fetch_known_constructor(queries, "Data.Generic.Rep", "Argument")?
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
    pub otherwise: Option<(FileId, TermItemId)>,
}

impl KnownTermsCore {
    fn collect(queries: &impl ExternalQueries) -> QueryResult<KnownTermsCore> {
        let otherwise = fetch_known_term(queries, "Data.Boolean", "otherwise")?;
        Ok(KnownTermsCore { otherwise })
    }
}
