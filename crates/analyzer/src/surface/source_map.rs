//! See documentation for [`SourceMap`].

use rustc_hash::FxHashMap;
use syntax::SyntaxNodePtr;

use super::{BinderId, ExprId, LetNameId, TypeId};

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct LetNamePtr {
    pub annotation_ptr: Option<SyntaxNodePtr>,
    pub equations_ptr: Vec<SyntaxNodePtr>,
}

/// A mapping from surface IDs to CST pointers.
#[derive(Debug, Default, PartialEq, Eq)]
pub struct SourceMap {
    pub(crate) expr_to_cst: FxHashMap<ExprId, SyntaxNodePtr>,
    pub(crate) cst_to_expr: FxHashMap<SyntaxNodePtr, ExprId>,
    pub(crate) let_name_to_cst: FxHashMap<LetNameId, LetNamePtr>,
    pub(crate) cst_to_let_name: FxHashMap<SyntaxNodePtr, LetNameId>,
    pub(crate) binder_to_cst: FxHashMap<BinderId, SyntaxNodePtr>,
    pub(crate) cst_to_binder: FxHashMap<SyntaxNodePtr, BinderId>,
    pub(crate) type_to_cst: FxHashMap<TypeId, SyntaxNodePtr>,
    pub(crate) cst_to_type: FxHashMap<SyntaxNodePtr, TypeId>,
}
