use itertools::Itertools;

use crate::{
    ExprItem, IndexingError, IndexingErrors, IndexingResult, TypeGroupId, TypeItem, ValueGroupId,
};

pub(super) fn check_index(
    index: &IndexingResult,
    mut errors: Vec<IndexingError>,
) -> IndexingErrors {
    for (_, _, item) in index.nominal.iter_expr() {
        check_expr_item(&mut errors, item);
    }
    for (_, _, item) in index.nominal.iter_type() {
        check_type_item(&mut errors, item);
    }
    IndexingErrors::from(errors)
}

fn check_expr_item(errors: &mut Vec<IndexingError>, item: &ExprItem) {
    if let ExprItem::Value(g) = item {
        check_value_group_id(errors, g);
    }
}

fn check_type_item(errors: &mut Vec<IndexingError>, item: &TypeItem) {
    if let TypeItem::Class(g)
    | TypeItem::Data(g)
    | TypeItem::Newtype(g)
    | TypeItem::Synonym(g)
    | TypeItem::Foreign(g) = item
    {
        check_type_group_id(errors, g)
    }
}

fn check_value_group_id(errors: &mut Vec<IndexingError>, group: &ValueGroupId) {
    let ValueGroupId { signature, equations } = group;
    let signature = signature.iter().copied();
    let equations = equations.iter().copied();
    for (zero, one) in signature.chain(equations).tuple_windows() {
        if !one.consecutive_of(zero) {
            errors.push(IndexingError::NonConsecutive { before: zero, after: one });
        }
    }
}

fn check_type_group_id(errors: &mut Vec<IndexingError>, group: &TypeGroupId) {
    if let TypeGroupId { signature: Some(signature), declaration: Some(declaration), .. } = group {
        if !declaration.consecutive_of(signature) {
            errors.push(IndexingError::NonConsecutive { before: *signature, after: *declaration });
        }
    }
    if let TypeGroupId { declaration: Some(declaration), role: Some(role), .. } = group {
        if !role.consecutive_of(declaration) {
            errors.push(IndexingError::NonConsecutive { before: *declaration, after: *role });
        }
    }
}
