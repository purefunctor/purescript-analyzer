//! See documentation for [`SourceMap`].

use rustc_hash::FxHashMap;
use syntax::SyntaxNodePtr;

use super::{BinderId, ExprId};

/// A mapping from surface IDs to CST pointers.
#[derive(Debug, Default, PartialEq, Eq)]
pub struct SourceMap {
    pub(crate) expr_to_cst: FxHashMap<ExprId, SyntaxNodePtr>,
    pub(crate) cst_to_expr: FxHashMap<SyntaxNodePtr, ExprId>,
    pub(crate) binder_to_cst: FxHashMap<BinderId, SyntaxNodePtr>,
    pub(crate) cst_to_binder: FxHashMap<SyntaxNodePtr, BinderId>,
}
