use id::Id;
use indexmap::IndexMap;
use rowan::ast::{AstNode, AstPtr};
use rustc_hash::FxBuildHasher;
use syntax::cst;

use crate::{LetBindingKind, LetBindingKindId, LetBindingPtr};

use super::{
    BinderId, BinderKind, BinderPtr, ExpressionId, ExpressionKind, ExpressionPtr, TypeId, TypeKind,
    TypePtr,
};

type FxIndexMap<K, V> = IndexMap<K, V, FxBuildHasher>;

/// Mapping from recursive trees to stable IDs.
///
/// # Let Bindings
///
/// Let bindings are handled differently in the [`SourceMap`] as let declarations
/// must be considered in groups rather than individually.
///
/// To accommodate this requirement, we make use of:
/// * [`let_bindings_grouped`], to assign stable IDs to [`LetBindingKind`];
/// * [`let_bindings`] to assign stable IDs to [`LetBindingPtr`] _and_ track
///   the [`LetBindingKind`] that it belongs to.
///
/// [`let_bindings`]: SourceMap::let_bindings
/// [`let_bindings_grouped`]: SourceMap::let_bindings_grouped
#[derive(Debug, Default, PartialEq, Eq)]
pub struct SourceMap {
    types: FxIndexMap<TypePtr, TypeKind>,
    binders: FxIndexMap<BinderPtr, BinderKind>,
    expressions: FxIndexMap<ExpressionPtr, ExpressionKind>,
    pub(crate) let_bindings: FxIndexMap<LetBindingPtr, LetBindingKindId>,
    pub(crate) let_bindings_grouped: Vec<LetBindingKind>,
}

fn insert<K: AstNode, V>(m: &mut FxIndexMap<AstPtr<K>, V>, p: &K, k: V) -> Id<K> {
    let pointer = AstPtr::new(p);
    let index = m.insert_full(pointer, k).0;
    Id::from_raw(index)
}

impl SourceMap {
    pub(crate) fn insert_type(&mut self, ptr: &cst::Type, kind: TypeKind) -> TypeId {
        insert(&mut self.types, ptr, kind)
    }

    pub(crate) fn insert_binder(&mut self, ptr: &cst::Binder, kind: BinderKind) -> BinderId {
        insert(&mut self.binders, ptr, kind)
    }

    pub(crate) fn insert_expression(
        &mut self,
        ptr: &cst::Expression,
        kind: ExpressionKind,
    ) -> ExpressionId {
        insert(&mut self.expressions, ptr, kind)
    }
}

type IndexResult<'t, K, V> = Option<(&'t AstPtr<K>, &'t V)>;

fn index<K: AstNode, V>(m: &FxIndexMap<AstPtr<K>, V>, id: Id<K>) -> IndexResult<K, V> {
    let index = id.into();
    m.get_index(index)
}

impl SourceMap {
    pub fn index_binder(&self, id: BinderId) -> IndexResult<cst::Binder, BinderKind> {
        index(&self.binders, id)
    }

    pub fn index_type(&self, id: TypeId) -> IndexResult<cst::Type, TypeKind> {
        index(&self.types, id)
    }

    pub fn index_expression(
        &self,
        id: ExpressionId,
    ) -> IndexResult<cst::Expression, ExpressionKind> {
        index(&self.expressions, id)
    }
}
