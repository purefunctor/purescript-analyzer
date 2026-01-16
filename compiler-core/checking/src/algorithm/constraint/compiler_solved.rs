use std::cmp::Ordering;
use std::sync::Arc;

use building_types::QueryResult;
use files::FileId;
use indexing::TypeItemId;
use itertools::{EitherOrBoth, izip};
use lowering::StringKind;
use petgraph::algo::has_path_connecting;
use petgraph::graphmap::DiGraphMap;
use rustc_hash::FxHashSet;
use smol_str::SmolStr;

use crate::algorithm::constraint::{self, MatchInstance};
use crate::algorithm::safety::safe_loop;
use crate::algorithm::state::{CheckContext, CheckState};
use crate::algorithm::{derive, kind, substitute};
use crate::core::pretty;
use crate::core::{Role, RowField, RowType};
use crate::error::ErrorKind;
use crate::{ExternalQueries, Type, TypeId};

fn extract_integer(state: &CheckState, id: TypeId) -> Option<i32> {
    let Type::Integer(value) = state.storage[id] else { return None };
    Some(value)
}

fn extract_symbol(state: &CheckState, id: TypeId) -> Option<SmolStr> {
    let Type::String(_, value) = &state.storage[id] else { return None };
    Some(SmolStr::clone(value))
}

pub fn prim_int_add(state: &mut CheckState, arguments: &[TypeId]) -> Option<MatchInstance> {
    let &[left, right, sum] = arguments else {
        return None;
    };

    let left = state.normalize_type(left);
    let right = state.normalize_type(right);
    let sum = state.normalize_type(sum);

    let left_int = extract_integer(state, left);
    let right_int = extract_integer(state, right);
    let sum_int = extract_integer(state, sum);

    match (left_int, right_int, sum_int) {
        (Some(left), Some(right), _) => {
            let result = state.storage.intern(Type::Integer(left + right));
            if constraint::can_unify(state, sum, result).is_apart() {
                return Some(MatchInstance::Apart);
            }
            Some(MatchInstance::Match { constraints: vec![], equalities: vec![(sum, result)] })
        }
        (Some(left), _, Some(sum)) => {
            let result = state.storage.intern(Type::Integer(sum - left));
            if constraint::can_unify(state, right, result).is_apart() {
                return Some(MatchInstance::Apart);
            }
            Some(MatchInstance::Match { constraints: vec![], equalities: vec![(right, result)] })
        }
        (_, Some(right), Some(sum)) => {
            let result = state.storage.intern(Type::Integer(sum - right));
            if constraint::can_unify(state, left, result).is_apart() {
                return Some(MatchInstance::Apart);
            }
            Some(MatchInstance::Match { constraints: vec![], equalities: vec![(left, result)] })
        }
        _ => Some(MatchInstance::Stuck),
    }
}

pub fn prim_int_mul(state: &mut CheckState, arguments: &[TypeId]) -> Option<MatchInstance> {
    let &[left, right, product] = arguments else {
        return None;
    };

    let left = state.normalize_type(left);
    let right = state.normalize_type(right);
    let product = state.normalize_type(product);

    let Some(left_int) = extract_integer(state, left) else {
        return Some(MatchInstance::Stuck);
    };

    let Some(right_int) = extract_integer(state, right) else {
        return Some(MatchInstance::Stuck);
    };

    let result = state.storage.intern(Type::Integer(left_int * right_int));

    if constraint::can_unify(state, product, result).is_apart() {
        return Some(MatchInstance::Apart);
    }

    Some(MatchInstance::Match { constraints: vec![], equalities: vec![(product, result)] })
}

pub fn prim_int_compare<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    arguments: &[TypeId],
    given: &[constraint::ConstraintApplication],
) -> Option<MatchInstance>
where
    Q: ExternalQueries,
{
    let &[left, right, ordering] = arguments else {
        return None;
    };

    let left = state.normalize_type(left);
    let right = state.normalize_type(right);
    let ordering = state.normalize_type(ordering);

    let left_int = extract_integer(state, left);
    let right_int = extract_integer(state, right);

    if let (Some(left_int), Some(right_int)) = (left_int, right_int) {
        let result = match left_int.cmp(&right_int) {
            Ordering::Less => context.prim_ordering.lt,
            Ordering::Equal => context.prim_ordering.eq,
            Ordering::Greater => context.prim_ordering.gt,
        };

        if constraint::can_unify(state, ordering, result).is_apart() {
            return Some(MatchInstance::Apart);
        }

        return Some(MatchInstance::Match {
            constraints: vec![],
            equalities: vec![(ordering, result)],
        });
    }

    prim_int_compare_transitive(state, context, left, right, ordering, given)
}

/// Uses a graph-based approach to derive ordering relationships transitively.
///
/// We build a directed graph where `a -> b` means `a < b`:
/// - `Compare a b LT`: add edge `a -> b`
/// - `Compare a b EQ`: add edges `a -> b` and `b -> a`
/// - `Compare a b GT`: add edge `b -> a` (since `a > b` means `b < a`)
///
/// Then we compute reachability to determine the ordering:
/// - Path from `left` to `right` only: LT
/// - Path from `right` to `left` only: GT
/// - Paths in both directions: EQ
/// - No path: Unknown/Stuck
fn prim_int_compare_transitive<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    left: TypeId,
    right: TypeId,
    ordering: TypeId,
    given: &[constraint::ConstraintApplication],
) -> Option<MatchInstance>
where
    Q: ExternalQueries,
{
    let prim_int = &context.prim_int;
    let lt = context.prim_ordering.lt;
    let eq = context.prim_ordering.eq;
    let gt = context.prim_ordering.gt;

    let mut graph: DiGraphMap<TypeId, ()> = DiGraphMap::new();

    for constraint in given {
        if constraint.file_id != prim_int.file_id || constraint.item_id != prim_int.compare {
            continue;
        }

        let &[a, b, ordering] = constraint.arguments.as_slice() else { continue };

        let a = state.normalize_type(a);
        let b = state.normalize_type(b);

        let ordering = state.normalize_type(ordering);

        if ordering == lt {
            // a < b: add edge a -> b
            graph.add_edge(a, b, ());
        } else if ordering == eq {
            // a = b: add edges both ways
            graph.add_edge(a, b, ());
            graph.add_edge(b, a, ());
        } else if ordering == gt {
            // a > b means b < a: add edge b -> a
            graph.add_edge(b, a, ());
        }
    }

    // Check reachability in both directions
    let left_reaches_right = has_path_connecting(&graph, left, right, None);
    let right_reaches_left = has_path_connecting(&graph, right, left, None);

    let result = match (left_reaches_right, right_reaches_left) {
        (true, true) => eq,
        (true, false) => lt,
        (false, true) => gt,
        (false, false) => return Some(MatchInstance::Stuck),
    };

    if constraint::can_unify(state, ordering, result).is_apart() {
        return Some(MatchInstance::Apart);
    }

    Some(MatchInstance::Match { constraints: vec![], equalities: vec![(ordering, result)] })
}

pub fn prim_int_to_string(state: &mut CheckState, arguments: &[TypeId]) -> Option<MatchInstance> {
    let &[int, symbol] = arguments else {
        return None;
    };

    let int = state.normalize_type(int);
    let symbol = state.normalize_type(symbol);

    let Some(value) = extract_integer(state, int) else {
        return Some(MatchInstance::Stuck);
    };

    let value: SmolStr = value.to_string().into();
    let result = state.storage.intern(Type::String(StringKind::String, value));

    if constraint::can_unify(state, symbol, result).is_apart() {
        return Some(MatchInstance::Apart);
    }

    Some(MatchInstance::Match { constraints: vec![], equalities: vec![(symbol, result)] })
}

pub fn prim_symbol_append(state: &mut CheckState, arguments: &[TypeId]) -> Option<MatchInstance> {
    let &[left, right, appended] = arguments else {
        return None;
    };

    let left = state.normalize_type(left);
    let right = state.normalize_type(right);
    let appended = state.normalize_type(appended);

    let left_symbol = extract_symbol(state, left);
    let right_symbol = extract_symbol(state, right);
    let appended_symbol = extract_symbol(state, appended);

    match (left_symbol, right_symbol, appended_symbol) {
        (Some(left), Some(right), _) => {
            let result: SmolStr = format!("{left}{right}").into();
            let result = state.storage.intern(Type::String(StringKind::String, result));
            Some(MatchInstance::Match { constraints: vec![], equalities: vec![(appended, result)] })
        }
        (_, Some(right), Some(appended)) => {
            if let Some(left_value) = appended.strip_suffix(right.as_str()) {
                let result: SmolStr = left_value.into();
                let result = state.storage.intern(Type::String(StringKind::String, result));
                Some(MatchInstance::Match { constraints: vec![], equalities: vec![(left, result)] })
            } else {
                Some(MatchInstance::Apart)
            }
        }
        (Some(left), _, Some(appended)) => {
            if let Some(right_value) = appended.strip_prefix(left.as_str()) {
                let result: SmolStr = right_value.into();
                let result = state.storage.intern(Type::String(StringKind::String, result));
                Some(MatchInstance::Match {
                    constraints: vec![],
                    equalities: vec![(right, result)],
                })
            } else {
                Some(MatchInstance::Apart)
            }
        }
        _ => Some(MatchInstance::Stuck),
    }
}

pub fn prim_symbol_compare<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    arguments: &[TypeId],
) -> Option<MatchInstance>
where
    Q: ExternalQueries,
{
    let &[left, right, ordering] = arguments else {
        return None;
    };

    let left = state.normalize_type(left);
    let right = state.normalize_type(right);
    let ordering = state.normalize_type(ordering);

    let Some(left_symbol) = extract_symbol(state, left) else {
        return Some(MatchInstance::Stuck);
    };

    let Some(right_symbol) = extract_symbol(state, right) else {
        return Some(MatchInstance::Stuck);
    };

    let result = match left_symbol.cmp(&right_symbol) {
        Ordering::Less => context.prim_ordering.lt,
        Ordering::Equal => context.prim_ordering.eq,
        Ordering::Greater => context.prim_ordering.gt,
    };

    Some(MatchInstance::Match { constraints: vec![], equalities: vec![(ordering, result)] })
}

pub fn prim_symbol_cons(state: &mut CheckState, arguments: &[TypeId]) -> Option<MatchInstance> {
    let &[head, tail, symbol] = arguments else {
        return None;
    };

    let head = state.normalize_type(head);
    let tail = state.normalize_type(tail);
    let symbol = state.normalize_type(symbol);

    let head_symbol = extract_symbol(state, head);
    let tail_symbol = extract_symbol(state, tail);
    let symbol_symbol = extract_symbol(state, symbol);

    match (&head_symbol, &tail_symbol, &symbol_symbol) {
        (Some(head), Some(tail), _) => {
            let mut chars = head.chars();
            if let (Some(c), None) = (chars.next(), chars.next()) {
                let result: SmolStr = format!("{c}{tail}").into();
                let result = state.storage.intern(Type::String(StringKind::String, result));
                Some(MatchInstance::Match {
                    constraints: vec![],
                    equalities: vec![(symbol, result)],
                })
            } else {
                Some(MatchInstance::Apart)
            }
        }
        (_, _, Some(symbol_value)) => {
            let mut chars = symbol_value.chars();
            if let Some(c) = chars.next() {
                if let Some(head_symbol) = head_symbol {
                    let mut head_chars = head_symbol.chars();
                    if head_chars.next() != Some(c) || head_chars.next().is_some() {
                        return Some(MatchInstance::Apart);
                    }
                }

                let head_result: SmolStr = c.to_string().into();
                let tail_result: SmolStr = chars.as_str().into();
                let head_result =
                    state.storage.intern(Type::String(StringKind::String, head_result));
                let tail_result =
                    state.storage.intern(Type::String(StringKind::String, tail_result));
                Some(MatchInstance::Match {
                    constraints: vec![],
                    equalities: vec![(head, head_result), (tail, tail_result)],
                })
            } else {
                Some(MatchInstance::Apart)
            }
        }
        _ => Some(MatchInstance::Stuck),
    }
}

fn extract_closed_row(state: &CheckState, id: TypeId) -> Option<RowType> {
    let Type::Row(row) = &state.storage[id] else { return None };
    if row.tail.is_some() {
        return None;
    }
    Some(row.clone())
}

fn extract_row(state: &CheckState, id: TypeId) -> Option<RowType> {
    let Type::Row(row) = &state.storage[id] else { return None };
    Some(row.clone())
}

fn merge_row_fields(
    state: &mut CheckState,
    left: &[RowField],
    right: &[RowField],
) -> Option<Vec<RowField>> {
    let left = left.iter();
    let right = right.iter();

    let merged_by_label =
        itertools::merge_join_by(left, right, |left, right| left.label.cmp(&right.label));

    let mut result = vec![];
    for field in merged_by_label {
        match field {
            EitherOrBoth::Left(left) => result.push(left.clone()),
            EitherOrBoth::Right(right) => result.push(right.clone()),
            EitherOrBoth::Both(left, right) => {
                let left_type = state.normalize_type(left.id);
                let right_type = state.normalize_type(right.id);
                if constraint::can_unify(state, left_type, right_type).is_apart() {
                    return None;
                }
                result.push(left.clone());
            }
        }
    }

    Some(result)
}

type SubtractResult = (Vec<RowField>, Vec<(TypeId, TypeId)>);

fn subtract_row_fields(
    state: &mut CheckState,
    source: &[RowField],
    to_remove: &[RowField],
) -> Option<SubtractResult> {
    let mut result = vec![];
    let mut equalities = vec![];
    let mut to_remove_iter = to_remove.iter().peekable();

    for field in source {
        if let Some(remove_field) = to_remove_iter.peek() {
            match field.label.cmp(&remove_field.label) {
                Ordering::Less => {
                    result.push(field.clone());
                }
                Ordering::Equal => {
                    let field_ty = state.normalize_type(field.id);
                    let removed_ty = state.normalize_type(remove_field.id);
                    if constraint::can_unify(state, field_ty, removed_ty).is_apart() {
                        return None;
                    }
                    equalities.push((field.id, remove_field.id));
                    to_remove_iter.next();
                }
                Ordering::Greater => {
                    return None;
                }
            }
        } else {
            result.push(field.clone());
        }
    }

    if to_remove_iter.next().is_some() {
        return None;
    }

    Some((result, equalities))
}

pub fn prim_row_union(state: &mut CheckState, arguments: &[TypeId]) -> Option<MatchInstance> {
    let &[left, right, union] = arguments else {
        return None;
    };

    let left = state.normalize_type(left);
    let right = state.normalize_type(right);
    let union = state.normalize_type(union);

    let left_row = extract_closed_row(state, left);
    let right_row = extract_closed_row(state, right);
    let union_row = extract_closed_row(state, union);

    match (left_row, right_row, union_row) {
        (Some(left_row), Some(right_row), _) => {
            if let Some(merged) = merge_row_fields(state, &left_row.fields, &right_row.fields) {
                let result = state.storage.intern(Type::Row(RowType::closed(merged)));
                Some(MatchInstance::Match {
                    constraints: vec![],
                    equalities: vec![(union, result)],
                })
            } else {
                Some(MatchInstance::Apart)
            }
        }
        (_, Some(right_row), Some(union_row)) => {
            if let Some((remaining, mut equalities)) =
                subtract_row_fields(state, &union_row.fields, &right_row.fields)
            {
                let result = state.storage.intern(Type::Row(RowType::closed(remaining)));
                equalities.push((left, result));
                Some(MatchInstance::Match { constraints: vec![], equalities })
            } else {
                Some(MatchInstance::Apart)
            }
        }
        (Some(left_row), _, Some(union_row)) => {
            if let Some((remaining, mut equalities)) =
                subtract_row_fields(state, &union_row.fields, &left_row.fields)
            {
                let result = state.storage.intern(Type::Row(RowType::closed(remaining)));
                equalities.push((right, result));
                Some(MatchInstance::Match { constraints: vec![], equalities })
            } else {
                Some(MatchInstance::Apart)
            }
        }
        _ => Some(MatchInstance::Stuck),
    }
}

pub fn prim_row_cons(state: &mut CheckState, arguments: &[TypeId]) -> Option<MatchInstance> {
    let &[label, a, tail, row] = arguments else {
        return None;
    };

    let label = state.normalize_type(label);
    let a = state.normalize_type(a);
    let tail = state.normalize_type(tail);
    let row = state.normalize_type(row);

    let label_symbol = extract_symbol(state, label);
    let tail_row = extract_closed_row(state, tail);
    let row_row = extract_closed_row(state, row);

    match (label_symbol, tail_row, row_row) {
        (Some(label_value), Some(tail_row), _) => {
            let mut fields = vec![RowField { label: label_value, id: a }];
            fields.extend(tail_row.fields.iter().cloned());

            let result_row = RowType::from_unsorted(fields, None);
            let result = state.storage.intern(Type::Row(result_row));

            Some(MatchInstance::Match { constraints: vec![], equalities: vec![(row, result)] })
        }

        (Some(label_value), _, Some(row_row)) => {
            let mut remaining = vec![];
            let mut found_type = None;

            for field in row_row.fields.iter() {
                if field.label == label_value && found_type.is_none() {
                    found_type = Some(field.id);
                } else {
                    remaining.push(field.clone());
                }
            }

            if let Some(field_type) = found_type {
                let tail_result = state.storage.intern(Type::Row(RowType::closed(remaining)));
                Some(MatchInstance::Match {
                    constraints: vec![],
                    equalities: vec![(a, field_type), (tail, tail_result)],
                })
            } else {
                Some(MatchInstance::Apart)
            }
        }
        _ => Some(MatchInstance::Stuck),
    }
}

pub fn prim_row_lacks(state: &mut CheckState, arguments: &[TypeId]) -> Option<MatchInstance> {
    let &[label, row] = arguments else {
        return None;
    };

    let label = state.normalize_type(label);
    let row = state.normalize_type(row);

    let Some(label_value) = extract_symbol(state, label) else {
        return Some(MatchInstance::Stuck);
    };

    let Some(row_row) = extract_row(state, row) else {
        return Some(MatchInstance::Stuck);
    };

    let has_label = row_row.fields.iter().any(|field| field.label == label_value);

    if has_label {
        Some(MatchInstance::Apart)
    } else if row_row.tail.is_some() {
        Some(MatchInstance::Stuck)
    } else {
        Some(MatchInstance::Match { constraints: vec![], equalities: vec![] })
    }
}

pub fn prim_row_nub(state: &mut CheckState, arguments: &[TypeId]) -> Option<MatchInstance> {
    let &[original, nubbed] = arguments else {
        return None;
    };

    let original = state.normalize_type(original);
    let nubbed = state.normalize_type(nubbed);

    let Some(original_row) = extract_closed_row(state, original) else {
        return Some(MatchInstance::Stuck);
    };

    let mut seen = FxHashSet::default();
    let mut fields = vec![];

    for field in original_row.fields.iter() {
        if seen.insert(field.label.clone()) {
            fields.push(field.clone());
        }
    }

    let result = state.storage.intern(Type::Row(RowType::closed(fields)));
    Some(MatchInstance::Match { constraints: vec![], equalities: vec![(nubbed, result)] })
}

fn extract_row_element_kind<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    type_id: TypeId,
) -> TypeId
where
    Q: ExternalQueries,
{
    let type_id = state.normalize_type(type_id);

    if let Type::Row(ref row_type) = state.storage[type_id]
        && let Some(field) = row_type.fields.first()
        && let Ok(kind) = kind::elaborate_kind(state, context, field.id)
    {
        return kind;
    }

    state.fresh_unification_type(context)
}

pub fn prim_rowlist_row_to_list<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    arguments: &[TypeId],
) -> Option<MatchInstance>
where
    Q: ExternalQueries,
{
    let &[row, list] = arguments else {
        return None;
    };

    let row = state.normalize_type(row);
    let list = state.normalize_type(list);

    let Some(row_row) = extract_closed_row(state, row) else {
        return Some(MatchInstance::Stuck);
    };

    let element_kind = extract_row_element_kind(state, context, row);

    let mut result =
        state.storage.intern(Type::KindApplication(context.prim_row_list.nil, element_kind));

    let cons_kinded =
        state.storage.intern(Type::KindApplication(context.prim_row_list.cons, element_kind));

    for field in row_row.fields.iter().rev() {
        let label_type =
            state.storage.intern(Type::String(StringKind::String, field.label.clone()));

        let cons_label = state.storage.intern(Type::Application(cons_kinded, label_type));
        let cons_type = state.storage.intern(Type::Application(cons_label, field.id));

        result = state.storage.intern(Type::Application(cons_type, result));
    }

    Some(MatchInstance::Match { constraints: vec![], equalities: vec![(list, result)] })
}

enum NewtypeCoercionResult {
    Success(MatchInstance),
    ConstructorNotInScope { file_id: FileId, item_id: TypeItemId },
    NotApplicable,
}

pub fn prim_coercible<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    arguments: &[TypeId],
) -> QueryResult<Option<MatchInstance>>
where
    Q: ExternalQueries,
{
    let &[left, right] = arguments else {
        return Ok(None);
    };

    let left = state.normalize_type(left);
    let right = state.normalize_type(right);

    if left == right {
        return Ok(Some(MatchInstance::Match { constraints: vec![], equalities: vec![] }));
    }

    if is_unification_head(state, left) || is_unification_head(state, right) {
        return Ok(Some(MatchInstance::Stuck));
    }

    let newtype_result = try_newtype_coercion(state, context, left, right)?;
    if let NewtypeCoercionResult::Success(result) = newtype_result {
        return Ok(Some(result));
    }

    if let Some(result) = try_application_coercion(state, context, left, right)? {
        return Ok(Some(result));
    }

    if let Some(result) = try_higher_kinded_coercion(state, context, left, right)? {
        return Ok(Some(result));
    }

    if let Some(result) = try_row_coercion(state, context, left, right) {
        return Ok(Some(result));
    }

    if let NewtypeCoercionResult::ConstructorNotInScope { file_id, item_id } = newtype_result {
        state.insert_error(crate::error::ErrorKind::CoercibleConstructorNotInScope {
            file_id,
            item_id,
        });
    }

    Ok(Some(MatchInstance::Apart))
}

fn is_unification_head(state: &mut CheckState, mut type_id: TypeId) -> bool {
    loop {
        type_id = state.normalize_type(type_id);
        match state.storage[type_id] {
            Type::Unification(_) => return true,
            Type::Application(function, _) | Type::KindApplication(function, _) => {
                type_id = function;
            }
            _ => return false,
        }
    }
}

fn try_newtype_coercion<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    left: TypeId,
    right: TypeId,
) -> QueryResult<NewtypeCoercionResult>
where
    Q: ExternalQueries,
{
    let mut hidden_newtype: Option<(FileId, TypeItemId)> = None;

    if has_type_kind(state, context, left)? {
        if let Some((file_id, type_id)) = derive::extract_type_constructor(state, left) {
            if is_newtype(context, file_id, type_id)? {
                if is_constructor_in_scope(context, file_id, type_id)? {
                    let inner = derive::get_newtype_inner(state, context, file_id, type_id, left)?;
                    let constraint = make_coercible_constraint(state, context, inner, right);
                    return Ok(NewtypeCoercionResult::Success(MatchInstance::Match {
                        constraints: vec![constraint],
                        equalities: vec![],
                    }));
                } else {
                    hidden_newtype = Some((file_id, type_id));
                }
            }
        }
    }

    if has_type_kind(state, context, right)? {
        if let Some((file_id, type_id)) = derive::extract_type_constructor(state, right) {
            if is_newtype(context, file_id, type_id)? {
                if is_constructor_in_scope(context, file_id, type_id)? {
                    let inner = derive::get_newtype_inner(state, context, file_id, type_id, right)?;
                    let constraint = make_coercible_constraint(state, context, left, inner);
                    return Ok(NewtypeCoercionResult::Success(MatchInstance::Match {
                        constraints: vec![constraint],
                        equalities: vec![],
                    }));
                } else if hidden_newtype.is_none() {
                    hidden_newtype = Some((file_id, type_id));
                }
            }
        }
    }

    if let Some((file_id, item_id)) = hidden_newtype {
        return Ok(NewtypeCoercionResult::ConstructorNotInScope { file_id, item_id });
    }

    Ok(NewtypeCoercionResult::NotApplicable)
}

fn has_type_kind<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    type_id: TypeId,
) -> QueryResult<bool>
where
    Q: ExternalQueries,
{
    let kind = kind::elaborate_kind(state, context, type_id)?;
    let kind = state.normalize_type(kind);
    Ok(kind == context.prim.t)
}

fn is_newtype<Q>(
    context: &CheckContext<Q>,
    file_id: FileId,
    type_id: TypeItemId,
) -> QueryResult<bool>
where
    Q: ExternalQueries,
{
    let is_newtype = if file_id == context.id {
        matches!(context.indexed.items[type_id].kind, indexing::TypeItemKind::Newtype { .. })
    } else {
        let indexed = context.queries.indexed(file_id)?;
        matches!(indexed.items[type_id].kind, indexing::TypeItemKind::Newtype { .. })
    };
    Ok(is_newtype)
}

fn is_constructor_in_scope<Q>(
    context: &CheckContext<Q>,
    file_id: FileId,
    item_id: TypeItemId,
) -> QueryResult<bool>
where
    Q: ExternalQueries,
{
    let constructor_term_id = if file_id == context.id {
        context.indexed.pairs.data_constructors(item_id).next()
    } else {
        let indexed = context.queries.indexed(file_id)?;
        indexed.pairs.data_constructors(item_id).next()
    };

    let Some(constructor_term_id) = constructor_term_id else {
        return Ok(false);
    };

    Ok(context.resolved.is_term_in_scope(&context.prim_resolved, file_id, constructor_term_id))
}

fn try_application_coercion<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    left: TypeId,
    right: TypeId,
) -> QueryResult<Option<MatchInstance>>
where
    Q: ExternalQueries,
{
    let Some((left_file, left_id)) = derive::extract_type_constructor(state, left) else {
        return Ok(None);
    };
    let Some((right_file, right_id)) = derive::extract_type_constructor(state, right) else {
        return Ok(None);
    };

    if left_file != right_file || left_id != right_id {
        return Ok(None);
    }

    let left = extract_type_arguments(state, left);
    let right = extract_type_arguments(state, right);

    if left.len() != right.len() {
        return Ok(Some(MatchInstance::Apart));
    }

    let Some(roles) = lookup_roles_for_type(state, context, left_file, left_id)? else {
        return Ok(Some(MatchInstance::Stuck));
    };

    debug_assert_eq!(roles.len(), left.len(), "critical failure: mismatched lengths");
    debug_assert_eq!(roles.len(), right.len(), "critical failure: mismatched lengths");

    let mut constraints = vec![];
    let mut equalities = vec![];

    for (role, &left, &right) in izip!(&*roles, &left, &right) {
        match role {
            Role::Phantom => (),
            Role::Representational => {
                let constraint = make_coercible_constraint(state, context, left, right);
                constraints.push(constraint);
            }
            Role::Nominal => {
                if left != right {
                    if constraint::can_unify(state, left, right).is_apart() {
                        return Ok(Some(MatchInstance::Apart));
                    }
                    equalities.push((left, right));
                }
            }
        }
    }

    Ok(Some(MatchInstance::Match { constraints, equalities }))
}

fn lookup_roles_for_type<Q>(
    state: &CheckState,
    context: &CheckContext<Q>,
    file_id: FileId,
    type_id: TypeItemId,
) -> QueryResult<Option<Arc<[Role]>>>
where
    Q: ExternalQueries,
{
    if file_id == context.id {
        Ok(state.checked.lookup_roles(type_id))
    } else {
        let checked = context.queries.checked(file_id)?;
        Ok(checked.lookup_roles(type_id))
    }
}

fn extract_type_arguments(state: &mut CheckState, type_id: TypeId) -> Vec<TypeId> {
    let mut arguments = vec![];
    let mut current_id = type_id;

    safe_loop! {
        current_id = state.normalize_type(current_id);
        match state.storage[current_id] {
            Type::Application(function, argument) => {
                arguments.push(argument);
                current_id = function;
            }
            Type::KindApplication(function, _) => {
                current_id = function;
            }
            _ => break,
        }
    }

    arguments.reverse();
    arguments
}

fn try_row_coercion<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    left: TypeId,
    right: TypeId,
) -> Option<MatchInstance>
where
    Q: ExternalQueries,
{
    let Type::Row(left_row) = &state.storage[left] else { return None };
    let Type::Row(right_row) = &state.storage[right] else { return None };

    let left_row = left_row.clone();
    let right_row = right_row.clone();

    if left_row.fields.len() != right_row.fields.len() {
        return Some(MatchInstance::Apart);
    }

    let mut constraints = vec![];

    for (left_field, right_field) in izip!(&*left_row.fields, &*right_row.fields) {
        if left_field.label != right_field.label {
            return Some(MatchInstance::Apart);
        }
        let constraint = make_coercible_constraint(state, context, left_field.id, right_field.id);
        constraints.push(constraint);
    }

    match (left_row.tail, right_row.tail) {
        (None, None) => (),
        (Some(left_tail), Some(right_tail)) => {
            let constraint = make_coercible_constraint(state, context, left_tail, right_tail);
            constraints.push(constraint);
        }
        (None, Some(_)) | (Some(_), None) => {
            return Some(MatchInstance::Apart);
        }
    }

    Some(MatchInstance::Match { constraints, equalities: vec![] })
}

fn try_higher_kinded_coercion<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    left: TypeId,
    right: TypeId,
) -> QueryResult<Option<MatchInstance>>
where
    Q: ExternalQueries,
{
    // Let's say we're attempting to coerce the following types:
    //
    // data Maybe :: forall k. k -> Type -> Type
    // data Maybe n a = Just a | Nothing
    //
    // newtype MaybeAlias :: forall k. k -> Type -> Type
    // newtype MaybeAlias n a = MaybeAlias (Maybe n a)
    //
    // solve[Coercible Maybe MaybeAlias]
    //
    // In order to solve coercion for higher-kinded types like
    // this, we need to be able to solve the following coercion.
    //
    // solve[Coercible (Maybe ~a) (MaybeAlias ~a)]
    //
    // To begin, we get the kinds of these types
    //
    // left_kind  := forall k. k -> Type -> Type
    // right_kind := forall k. k -> Type -> Type
    let left_kind = kind::elaborate_kind(state, context, left)?;
    let right_kind = kind::elaborate_kind(state, context, right)?;

    // decompose_kind_for_coercion instantiates the variables into
    // skolem variables, then returns the first argument, which in
    // this case is the already-skolemized `~k`
    //
    // left_kind_applied := Maybe @~k
    // left_domain       := ~k
    let Some((left_kind_applied, left_domain)) =
        decompose_kind_for_coercion(state, left, left_kind)
    else {
        return Ok(None);
    };

    // right_kind_applied := MaybeAlias @~k
    // right_domain       := ~k
    let Some((right_kind_applied, right_domain)) =
        decompose_kind_for_coercion(state, right, right_kind)
    else {
        return Ok(None);
    };

    if constraint::can_unify(state, left_domain, right_domain).is_apart() {
        return Ok(Some(MatchInstance::Apart));
    }

    // Given left_domain ~ right_domain, create a skolem kinded by `~k`
    let argument = state.fresh_skolem_kinded(left_domain);

    // Finally, we can saturated left_kind_applied and right_kind_applied
    //
    // left  := Maybe      @~k (~a :: ~k)
    // right := MaybeAlias @~k (~a :: ~k)
    //
    // Finally, we emit `left <~> right` as a constraint and rely on the
    // remaining rules to solve this for us, particularly newtype coercion.
    let left = state.storage.intern(Type::Application(left_kind_applied, argument));
    let right = state.storage.intern(Type::Application(right_kind_applied, argument));
    let constraint = make_coercible_constraint(state, context, left, right);

    Ok(Some(MatchInstance::Match { constraints: vec![constraint], equalities: vec![] }))
}

fn decompose_kind_for_coercion(
    state: &mut CheckState,
    mut type_id: TypeId,
    mut kind_id: TypeId,
) -> Option<(TypeId, TypeId)> {
    safe_loop! {
        kind_id = state.normalize_type(kind_id);

        let forall = match &state.storage[kind_id] {
            Type::Forall(binder, inner) => Some((binder.kind, binder.level, *inner)),
            Type::Function(domain, _) => return Some((type_id, *domain)),
            _ => return None,
        };

        if let Some((binder_kind, binder_level, inner_kind)) = forall {
            let fresh_kind = state.fresh_skolem_kinded(binder_kind);
            type_id = state.storage.intern(Type::KindApplication(type_id, fresh_kind));
            kind_id = substitute::SubstituteBound::on(state, binder_level, fresh_kind, inner_kind);
        }
    }
}

fn make_coercible_constraint<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    left: TypeId,
    right: TypeId,
) -> TypeId
where
    Q: ExternalQueries,
{
    let coerce = &context.prim_coerce;
    let coercible = state.storage.intern(Type::Constructor(coerce.file_id, coerce.coercible));

    let coercible = state.storage.intern(Type::Application(coercible, left));
    state.storage.intern(Type::Application(coercible, right))
}

pub fn prim_is_symbol(state: &mut CheckState, arguments: &[TypeId]) -> Option<MatchInstance> {
    let &[symbol] = arguments else { return None };
    let symbol = state.normalize_type(symbol);

    if extract_symbol(state, symbol).is_some() {
        Some(MatchInstance::Match { constraints: vec![], equalities: vec![] })
    } else if matches!(state.storage[symbol], Type::Unification(_)) {
        Some(MatchInstance::Stuck)
    } else {
        Some(MatchInstance::Apart)
    }
}

pub fn prim_reflectable<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    arguments: &[TypeId],
) -> Option<MatchInstance>
where
    Q: ExternalQueries,
{
    let &[v, t] = arguments else { return None };

    let v = state.normalize_type(v);
    let t = state.normalize_type(t);

    if extract_symbol(state, v).is_some() {
        let expected = context.prim.string;
        return check_reflectable_match(state, t, expected);
    }

    if extract_integer(state, v).is_some() {
        let expected = context.prim.int;
        return check_reflectable_match(state, t, expected);
    }

    if v == context.prim_boolean.true_ || v == context.prim_boolean.false_ {
        let expected = context.prim.boolean;
        return check_reflectable_match(state, t, expected);
    }

    if v == context.prim_ordering.lt
        || v == context.prim_ordering.eq
        || v == context.prim_ordering.gt
    {
        let Some(expected) = context.known_reflectable.ordering else {
            return Some(MatchInstance::Stuck);
        };
        return check_reflectable_match(state, t, expected);
    }

    if matches!(state.storage[v], Type::Unification(_)) {
        return Some(MatchInstance::Stuck);
    }

    Some(MatchInstance::Apart)
}

fn check_reflectable_match(
    state: &mut CheckState,
    actual: TypeId,
    expected: TypeId,
) -> Option<MatchInstance> {
    if constraint::can_unify(state, actual, expected).is_apart() {
        Some(MatchInstance::Apart)
    } else {
        Some(MatchInstance::Match { constraints: vec![], equalities: vec![(actual, expected)] })
    }
}

/// Decomposes a type application into its constructor and arguments.
fn decompose_application(
    state: &mut CheckState,
    mut type_id: TypeId,
) -> Option<(TypeId, Vec<TypeId>)> {
    let mut arguments = vec![];

    safe_loop! {
        type_id = state.normalize_type(type_id);
        match state.storage[type_id] {
            Type::Application(function, argument) => {
                arguments.push(argument);
                type_id = function;
            }
            Type::KindApplication(function, _) => {
                type_id = function;
            }
            _ => break,
        }
    }

    arguments.reverse();
    Some((type_id, arguments))
}

/// Checks if a type is stuck on a unification variable at its head.
fn is_stuck(state: &mut CheckState, type_id: TypeId) -> bool {
    let type_id = state.normalize_type(type_id);
    matches!(state.storage[type_id], Type::Unification(_))
}

/// Extracts a symbol from a type, returning `None` if stuck on a unification variable.
fn extract_symbol_or_stuck(state: &mut CheckState, id: TypeId) -> Option<String> {
    let id = state.normalize_type(id);

    if matches!(state.storage[id], Type::Unification(_)) {
        return None;
    }

    extract_symbol(state, id).map(|s| s.to_string())
}

/// Extracts a symbol with its kind from a type, returning `None` if stuck.
fn extract_symbol_with_kind(
    state: &mut CheckState,
    id: TypeId,
) -> Option<(StringKind, SmolStr)> {
    let id = state.normalize_type(id);

    if matches!(state.storage[id], Type::Unification(_)) {
        return None;
    }

    match &state.storage[id] {
        Type::String(kind, value) => Some((*kind, SmolStr::clone(value))),
        _ => None,
    }
}

/// Checks if a string is a valid PureScript label.
///
/// Matches the lexer rules: starts with lowercase letter or `_`,
/// continues with alphanumeric, `_`, or `'`.
fn is_valid_label(s: &str) -> bool {
    let mut chars = s.chars();
    match chars.next() {
        Some(c) if c.is_lowercase() || c == '_' => {}
        _ => return false,
    }
    chars.all(|c| c.is_alphanumeric() || c == '_' || c == '\'')
}

/// Renders a label, quoting it if necessary.
///
/// Preserves the original string kind (regular vs raw) when quoting.
fn render_label(kind: StringKind, s: &str) -> String {
    if is_valid_label(s) {
        s.to_string()
    } else {
        match kind {
            StringKind::String => format!(r#""{s}""#),
            StringKind::RawString => format!(r#""""{s}""""#),
        }
    }
}

/// Renders a `Doc` type into a string for custom type error messages.
///
/// Returns `None` if the doc is stuck on a unification variable.
fn render_doc<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    type_id: TypeId,
) -> Option<String>
where
    Q: ExternalQueries,
{
    let type_id = state.normalize_type(type_id);

    if matches!(state.storage[type_id], Type::Unification(_)) {
        return None;
    }

    let (constructor, arguments) = decompose_application(state, type_id)?;
    let prim = &context.prim_type_error;

    if constructor == prim.text {
        let &[symbol] = arguments.as_slice() else { return None };
        extract_symbol_or_stuck(state, symbol)
    } else if constructor == prim.quote {
        let &[t] = arguments.as_slice() else { return None };
        if is_stuck(state, t) {
            return None;
        }
        Some(pretty::print_local(state, context, t))
    } else if constructor == prim.quote_label {
        let &[symbol] = arguments.as_slice() else { return None };
        extract_symbol_with_kind(state, symbol).map(|(kind, s)| render_label(kind, &s))
    } else if constructor == prim.beside {
        let &[left, right] = arguments.as_slice() else { return None };
        let l = render_doc(state, context, left)?;
        let r = render_doc(state, context, right)?;
        Some(format!("{}{}", l, r))
    } else if constructor == prim.above {
        let &[upper, lower] = arguments.as_slice() else { return None };
        let u = render_doc(state, context, upper)?;
        let d = render_doc(state, context, lower)?;
        Some(format!("{}\n{}", u, d))
    } else {
        None
    }
}

/// Solver for `Prim.TypeError.Warn`.
///
/// Emits a custom warning message and satisfies the constraint.
pub fn prim_warn<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    arguments: &[TypeId],
) -> Option<MatchInstance>
where
    Q: ExternalQueries,
{
    let &[doc] = arguments else { return None };

    let Some(message) = render_doc(state, context, doc) else {
        return Some(MatchInstance::Stuck);
    };

    let message_id = state.checked.custom_messages.len() as u32;
    state.checked.custom_messages.push(message);
    state.insert_error(ErrorKind::CustomWarning { message_id });

    Some(MatchInstance::Match { constraints: vec![], equalities: vec![] })
}

/// Solver for `Prim.TypeError.Fail`.
///
/// Emits a custom error message and satisfies the constraint.
pub fn prim_fail<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    arguments: &[TypeId],
) -> Option<MatchInstance>
where
    Q: ExternalQueries,
{
    let &[doc] = arguments else { return None };

    let Some(message) = render_doc(state, context, doc) else {
        return Some(MatchInstance::Stuck);
    };

    let message_id = state.checked.custom_messages.len() as u32;
    state.checked.custom_messages.push(message);
    state.insert_error(ErrorKind::CustomFailure { message_id });

    Some(MatchInstance::Match { constraints: vec![], equalities: vec![] })
}
