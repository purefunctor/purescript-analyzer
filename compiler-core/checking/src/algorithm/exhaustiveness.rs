use std::iter;
use std::sync::Arc;

use building_types::QueryResult;
use files::FileId;
use indexing::TermItemId;
use itertools::Itertools;
use lowering::{BinderId, TermOperatorId};
use rustc_hash::FxHashSet;
use smol_str::SmolStr;
use sugar::OperatorTree;

use crate::algorithm::state::{CheckContext, CheckState, OperatorBranchTypes};
use crate::{ExternalQueries, TypeId};

const MISSING_NAME: SmolStr = SmolStr::new_inline("<MissingName>");

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Pattern {
    kind: PatternKind,
    t: TypeId,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum PatternKind {
    Wildcard,
    Boolean(bool),
    Char(char),
    String(SmolStr),
    Integer(i32),
    Number(bool, SmolStr),
    Array { elements: Vec<PatternId> },
    Record { elements: Vec<RecordElement> },
    Constructor { constructor: Constructor },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Constructor {
    file_id: FileId,
    item_id: TermItemId,
    fields: Vec<PatternId>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum RecordElement {
    Named(SmolStr, PatternId),
    Pun(SmolStr),
}

pub type PatternId = interner::Id<Pattern>;

type PatternVector = Vec<PatternId>;
type PatternMatrix = Vec<PatternVector>;
type WitnessVector = Vec<PatternId>;

struct ExhaustivenessState {
    interner: interner::Interner<Pattern>,
}

impl ExhaustivenessState {
    fn allocate(&mut self, kind: PatternKind, t: TypeId) -> PatternId {
        let pattern = Pattern { kind, t };
        self.interner.intern(pattern)
    }

    fn allocate_wildcard(&mut self, t: TypeId) -> PatternId {
        self.allocate(PatternKind::Wildcard, t)
    }
}

fn lower_binder<Q>(
    check_state: &mut CheckState,
    exhaustiveness_state: &mut ExhaustivenessState,
    context: &CheckContext<Q>,
    id: BinderId,
) -> PatternId
where
    Q: ExternalQueries,
{
    let t = check_state.term_scope.lookup_binder(id).unwrap_or(context.prim.unknown);

    let Some(kind) = context.lowered.info.get_binder_kind(id) else {
        return exhaustiveness_state.allocate_wildcard(t);
    };

    match kind {
        lowering::BinderKind::Typed { binder, .. } => match binder {
            Some(id) => lower_binder(check_state, exhaustiveness_state, context, *id),
            None => exhaustiveness_state.allocate_wildcard(t),
        },
        lowering::BinderKind::OperatorChain { .. } => {
            lower_operator_chain_binder(check_state, exhaustiveness_state, context, id, t)
        }
        lowering::BinderKind::Integer { value } => match value {
            Some(v) => exhaustiveness_state.allocate(PatternKind::Integer(*v), t),
            None => exhaustiveness_state.allocate_wildcard(t),
        },
        lowering::BinderKind::Number { negative, value } => {
            if let Some(value) = value {
                let kind = PatternKind::Number(*negative, SmolStr::clone(value));
                exhaustiveness_state.allocate(kind, t)
            } else {
                exhaustiveness_state.allocate_wildcard(t)
            }
        }
        lowering::BinderKind::Constructor { resolution, arguments } => lower_constructor_binder(
            check_state,
            exhaustiveness_state,
            context,
            resolution,
            arguments,
            t,
        ),
        lowering::BinderKind::Variable { .. } => exhaustiveness_state.allocate_wildcard(t),
        lowering::BinderKind::Named { binder, .. } => match binder {
            Some(id) => lower_binder(check_state, exhaustiveness_state, context, *id),
            None => exhaustiveness_state.allocate_wildcard(t),
        },
        lowering::BinderKind::Wildcard => exhaustiveness_state.allocate_wildcard(t),
        lowering::BinderKind::String { value, .. } => {
            if let Some(value) = value {
                let kind = PatternKind::String(SmolStr::clone(value));
                exhaustiveness_state.allocate(kind, t)
            } else {
                exhaustiveness_state.allocate_wildcard(t)
            }
        }
        lowering::BinderKind::Char { value } => match value {
            Some(v) => exhaustiveness_state.allocate(PatternKind::Char(*v), t),
            None => exhaustiveness_state.allocate_wildcard(t),
        },
        lowering::BinderKind::Boolean { boolean } => {
            exhaustiveness_state.allocate(PatternKind::Boolean(*boolean), t)
        }
        lowering::BinderKind::Array { array } => {
            lower_array_binder(check_state, exhaustiveness_state, context, array, t)
        }
        lowering::BinderKind::Record { record } => {
            lower_record_binder(check_state, exhaustiveness_state, context, record, t)
        }
        lowering::BinderKind::Parenthesized { parenthesized } => match parenthesized {
            Some(id) => lower_binder(check_state, exhaustiveness_state, context, *id),
            None => exhaustiveness_state.allocate_wildcard(t),
        },
    }
}

fn lower_array_binder<Q>(
    check_state: &mut CheckState,
    exhaustiveness_state: &mut ExhaustivenessState,
    context: &CheckContext<Q>,
    array: &[BinderId],
    t: TypeId,
) -> PatternId
where
    Q: ExternalQueries,
{
    let elements = array
        .iter()
        .map(|element| lower_binder(check_state, exhaustiveness_state, context, *element))
        .collect();
    exhaustiveness_state.allocate(PatternKind::Array { elements }, t)
}

fn lower_record_binder<Q>(
    check_state: &mut CheckState,
    exhaustiveness_state: &mut ExhaustivenessState,
    context: &CheckContext<Q>,
    record: &[lowering::BinderRecordItem],
    t: TypeId,
) -> PatternId
where
    Q: ExternalQueries,
{
    let elements = record
        .iter()
        .map(|element| lower_record_element(check_state, exhaustiveness_state, context, element))
        .collect();
    exhaustiveness_state.allocate(PatternKind::Record { elements }, t)
}

fn lower_record_element<Q>(
    check_state: &mut CheckState,
    exhaustiveness_state: &mut ExhaustivenessState,
    context: &CheckContext<Q>,
    element: &lowering::BinderRecordItem,
) -> RecordElement
where
    Q: ExternalQueries,
{
    match element {
        lowering::BinderRecordItem::RecordField { name, value } => {
            let name = name.clone().unwrap_or(MISSING_NAME);
            let value = if let Some(value) = value {
                lower_binder(check_state, exhaustiveness_state, context, *value)
            } else {
                exhaustiveness_state.allocate_wildcard(context.prim.unknown)
            };
            RecordElement::Named(name, value)
        }
        lowering::BinderRecordItem::RecordPun { name, .. } => {
            let name = name.clone().unwrap_or(MISSING_NAME);
            RecordElement::Pun(name)
        }
    }
}

fn lower_constructor_binder<Q>(
    check_state: &mut CheckState,
    exhaustiveness_state: &mut ExhaustivenessState,
    context: &CheckContext<Q>,
    resolution: &Option<(FileId, TermItemId)>,
    arguments: &Arc<[BinderId]>,
    t: TypeId,
) -> PatternId
where
    Q: ExternalQueries,
{
    let Some((file_id, item_id)) = resolution else {
        return exhaustiveness_state.allocate_wildcard(t);
    };

    let fields = arguments
        .iter()
        .map(|argument| lower_binder(check_state, exhaustiveness_state, context, *argument))
        .collect_vec();

    let constructor = Constructor { file_id: *file_id, item_id: *item_id, fields };
    exhaustiveness_state.allocate(PatternKind::Constructor { constructor }, t)
}

fn lower_operator_chain_binder<Q>(
    check_state: &mut CheckState,
    exhaustiveness_state: &mut ExhaustivenessState,
    context: &CheckContext<Q>,
    id: BinderId,
    t: TypeId,
) -> PatternId
where
    Q: ExternalQueries,
{
    let Some(tree) = context.bracketed.binders.get(&id) else {
        return exhaustiveness_state.allocate_wildcard(t);
    };

    let Ok(tree) = tree else {
        return exhaustiveness_state.allocate_wildcard(t);
    };

    lower_operator_tree(check_state, exhaustiveness_state, context, tree, t)
}

fn lower_operator_tree<Q>(
    check_state: &mut CheckState,
    exhaustiveness_state: &mut ExhaustivenessState,
    context: &CheckContext<Q>,
    tree: &OperatorTree<BinderId>,
    t: TypeId,
) -> PatternId
where
    Q: ExternalQueries,
{
    match tree {
        OperatorTree::Leaf(None) => exhaustiveness_state.allocate_wildcard(t),
        OperatorTree::Leaf(Some(binder_id)) => {
            lower_binder(check_state, exhaustiveness_state, context, *binder_id)
        }
        OperatorTree::Branch(operator_id, children) => lower_operator_branch(
            check_state,
            exhaustiveness_state,
            context,
            *operator_id,
            children,
            t,
        ),
    }
}

fn lower_operator_branch<Q>(
    check_state: &mut CheckState,
    exhaustiveness_state: &mut ExhaustivenessState,
    context: &CheckContext<Q>,
    operator_id: TermOperatorId,
    children: &[OperatorTree<BinderId>; 2],
    t: TypeId,
) -> PatternId
where
    Q: ExternalQueries,
{
    let Some((file_id, item_id)) = context.lowered.info.get_term_operator(operator_id) else {
        return exhaustiveness_state.allocate_wildcard(t);
    };

    let Some(OperatorBranchTypes { left, right, result }) =
        check_state.term_scope.lookup_operator_node(operator_id)
    else {
        return exhaustiveness_state.allocate_wildcard(t);
    };

    let [left_tree, right_tree] = children;

    let left_pattern =
        lower_operator_tree(check_state, exhaustiveness_state, context, left_tree, left);

    let right_pattern =
        lower_operator_tree(check_state, exhaustiveness_state, context, right_tree, right);

    let constructor = Constructor { file_id, item_id, fields: vec![left_pattern, right_pattern] };
    exhaustiveness_state.allocate(PatternKind::Constructor { constructor }, result)
}

/// Determines if a [`PatternVector`] is useful with respect to a [`PatternMatrix`].
///
/// A pattern vector is useful if it matches at least one value not matched by
/// any pattern vector in the matrix. This is the core algorithm from Maranget's
/// "Warnings for pattern matching" paper.
fn algorithm_u<Q>(
    check_state: &mut CheckState,
    exhaustiveness_state: &mut ExhaustivenessState,
    context: &CheckContext<Q>,
    matrix: &PatternMatrix,
    vector: &PatternVector,
) -> QueryResult<bool>
where
    Q: ExternalQueries,
{
    // Base case: any pattern is useful against an empty matrix
    if matrix.is_empty() {
        return Ok(true);
    }

    // Base case: an empty pattern vector against non-empty matrix is useless
    let [first_pattern, ..] = vector[..] else {
        return Ok(false);
    };

    let first_pattern = exhaustiveness_state.interner[first_pattern].clone();

    match first_pattern.kind {
        PatternKind::Constructor { constructor } => algorithm_u_constructor(
            check_state,
            exhaustiveness_state,
            context,
            matrix,
            vector,
            constructor,
        ),
        PatternKind::Wildcard => {
            algorithm_u_wildcard(check_state, exhaustiveness_state, context, matrix, vector)
        }
        _ => algorithm_u_other(check_state, exhaustiveness_state, context, matrix, vector),
    }
}

fn algorithm_u_constructor<Q>(
    check_state: &mut CheckState,
    exhaustiveness_state: &mut ExhaustivenessState,
    context: &CheckContext<Q>,
    matrix: &PatternMatrix,
    vector: &PatternVector,
    constructor: Constructor,
) -> QueryResult<bool>
where
    Q: ExternalQueries,
{
    let specialized_matrix =
        specialise_matrix(check_state, exhaustiveness_state, context, &constructor, matrix);

    let Some(specialized_vector) =
        specialise_vector(check_state, exhaustiveness_state, context, &constructor, vector)
    else {
        unreachable!("invariant violated: vector contains constructor");
    };

    algorithm_u(
        check_state,
        exhaustiveness_state,
        context,
        &specialized_matrix,
        &specialized_vector,
    )
}

fn algorithm_u_wildcard<Q>(
    check_state: &mut CheckState,
    exhaustiveness_state: &mut ExhaustivenessState,
    context: &CheckContext<Q>,
    matrix: &PatternMatrix,
    vector: &PatternVector,
) -> QueryResult<bool>
where
    Q: ExternalQueries,
{
    let sigma = extract_sigma(exhaustiveness_state, matrix);
    let complete = sigma_is_complete(context, &sigma)?;

    if complete {
        // If sigma is complete, check if useful for any constructor
        for constructor in sigma {
            let specialized_matrix =
                specialise_matrix(check_state, exhaustiveness_state, context, &constructor, matrix);
            let specialized_vector =
                specialise_vector(check_state, exhaustiveness_state, context, &constructor, vector)
                    .expect("specialising wildcard head must succeed");

            if algorithm_u(
                check_state,
                exhaustiveness_state,
                context,
                &specialized_matrix,
                &specialized_vector,
            )? {
                return Ok(true);
            }
        }
        Ok(false)
    } else {
        // If sigma is incomplete, use default matrix
        let default = default_matrix(exhaustiveness_state, matrix);
        let tail = vector[1..].to_vec();
        algorithm_u(check_state, exhaustiveness_state, context, &default, &tail)
    }
}

fn algorithm_u_other<Q>(
    check_state: &mut CheckState,
    exhaustiveness_state: &mut ExhaustivenessState,
    context: &CheckContext<Q>,
    matrix: &PatternMatrix,
    vector: &PatternVector,
) -> QueryResult<bool>
where
    Q: ExternalQueries,
{
    // For literals and other patterns, treat as incomplete sigma (conservative)
    let default = default_matrix(exhaustiveness_state, matrix);
    let tail = vector[1..].to_vec();
    algorithm_u(check_state, exhaustiveness_state, context, &default, &tail)
}

/// Determines the matching [`WitnessVector`] given a [`PatternMatrix`]
/// and some [`PatternVector`].
///
/// If the pattern vector is useful against the provided matrix, that is,
/// there are cases yet to be covered, this function will return a non-empty
/// list of witnesses. Inversely, if the pattern vector is useless against
/// the provided matrix, that is, the cases are exhaustive, this function
/// will return [`None`].
///
/// So... what exactly are witnesses? In the paper, these are defined as
/// 'value vectors' that are known not to be matched against the pattern
/// matrix but are instantiations of the pattern vector. In our implementation,
/// these witnesses are patterns not covered yet by the matrix.
///
/// The [`algorithm_m_wildcard`] induction is prolific for producing these
/// these witnesses as it compares the constructors that appear in the
/// matrix against the constructors available in the checking environment.
fn algorithm_m<Q>(
    check_state: &mut CheckState,
    exhaustiveness_state: &mut ExhaustivenessState,
    context: &CheckContext<Q>,
    matrix: &PatternMatrix,
    vector: &PatternVector,
) -> QueryResult<Option<Vec<WitnessVector>>>
where
    Q: ExternalQueries,
{
    // Base case: any pattern is its own witness against an empty matrix
    if matrix.is_empty() {
        let vector = vector.clone();
        return Ok(Some(vec![vector]));
    }

    // Base case: an empty pattern vector against non-empty matrix has no witnesses
    let [first_pattern, ..] = vector[..] else {
        return Ok(None);
    };

    let first_pattern = exhaustiveness_state.interner[first_pattern].clone();

    match first_pattern.kind {
        PatternKind::Constructor { constructor } => algorithm_m_constructor(
            check_state,
            exhaustiveness_state,
            context,
            matrix,
            vector,
            constructor,
            first_pattern.t,
        ),
        PatternKind::Wildcard => algorithm_m_wildcard(
            check_state,
            exhaustiveness_state,
            context,
            matrix,
            vector,
            first_pattern.t,
        ),
        _ => algorithm_m_other(
            check_state,
            exhaustiveness_state,
            context,
            matrix,
            vector,
            first_pattern.t,
        ),
    }
}

/// Induction 1
///
/// This function uses specialisation to spread the provided [`Constructor`]
/// over both the [`PatternMatrix`] and the [`PatternVector`], before calling
/// [`algorithm_m`] recursively with the specialised structures.
///
/// The final set of witnesses returned by this induction includes a
/// reconstruction of the original constructor passed to this function.
///
/// See documentation for [`specialise_matrix`] and [`specialise_vector`] for
/// more information on what specialisation entails given a constructor.
fn algorithm_m_constructor<Q>(
    check_state: &mut CheckState,
    exhaustiveness_state: &mut ExhaustivenessState,
    context: &CheckContext<Q>,
    matrix: &PatternMatrix,
    vector: &PatternVector,
    constructor: Constructor,
    first_type: TypeId,
) -> QueryResult<Option<Vec<WitnessVector>>>
where
    Q: ExternalQueries,
{
    let arity = constructor.fields.len();

    let specialized_matrix =
        specialise_matrix(check_state, exhaustiveness_state, context, &constructor, matrix);

    let Some(specialized_vector) =
        specialise_vector(check_state, exhaustiveness_state, context, &constructor, vector)
    else {
        unreachable!("invariant violated: vector contains constructor");
    };

    let witnesses = algorithm_m(
        check_state,
        exhaustiveness_state,
        context,
        &specialized_matrix,
        &specialized_vector,
    )?;

    let Some(witnesses) = witnesses else {
        return Ok(None);
    };

    let witnesses = witnesses.into_iter().map(|witness| {
        let (argument_columns, tail_columns) = witness.split_at(arity);

        let pattern = PatternKind::Constructor {
            constructor: Constructor {
                file_id: constructor.file_id,
                item_id: constructor.item_id,
                fields: argument_columns.to_vec(),
            },
        };

        let constructor = exhaustiveness_state.allocate(pattern, first_type);
        let tail_columns = tail_columns.iter().copied();

        iter::once(constructor).chain(tail_columns).collect()
    });

    let witnesses = witnesses.collect();
    Ok(Some(witnesses))
}

/// Induction 2
///
/// If the first column in the [`PatternVector`] is a wildcard, this function
/// produces witnesses that correspond to patterns not yet covered by the
/// [`PatternMatrix`]. This is where pattern suggestion warnings are built
/// for the compiler!
///
/// This function collects all constructor references from the first column
/// of all rows in the matrix into a collection called the sigma. We handle
/// the structure in different ways:
///
/// If the sigma is complete, for each constructor in the sigma, we apply
/// a rule similar to [`algorithm_m_constructor`] to collect witnesses
/// across all constructors.
///
/// If the sigma is incomplete, we recursively apply [`algorithm_m`] to the
/// [`default_matrix`] of the pattern matrix and the tail columns of the
/// pattern vector. The induction ends if the recursive call is exhaustive.
///
/// If the recursive call returns witnesses, and the sigma is non-empty,
/// we move our attention to generating [`Constructor`] patterns for
/// constructors not present in the sigma. This is what we use for
/// reporting pattern warnings. Otherwise, if the sigma is empty, we
/// simply produce a wildcard pattern.
fn algorithm_m_wildcard<Q>(
    check_state: &mut CheckState,
    exhaustiveness_state: &mut ExhaustivenessState,
    context: &CheckContext<Q>,
    matrix: &PatternMatrix,
    vector: &PatternVector,
    first_type: TypeId,
) -> QueryResult<Option<Vec<WitnessVector>>>
where
    Q: ExternalQueries,
{
    let sigma = extract_sigma(exhaustiveness_state, matrix);
    let complete = sigma_is_complete(context, &sigma)?;

    if complete {
        algorithm_m_wildcard_complete(
            check_state,
            exhaustiveness_state,
            context,
            matrix,
            vector,
            first_type,
            sigma,
        )
    } else {
        algorithm_m_wildcard_incomplete(
            check_state,
            exhaustiveness_state,
            context,
            matrix,
            vector,
            first_type,
            &sigma,
        )
    }
}

fn algorithm_m_wildcard_complete<Q>(
    check_state: &mut CheckState,
    exhaustiveness_state: &mut ExhaustivenessState,
    context: &CheckContext<Q>,
    matrix: &PatternMatrix,
    vector: &PatternVector,
    first_type: TypeId,
    sigma: Vec<Constructor>,
) -> QueryResult<Option<Vec<WitnessVector>>>
where
    Q: ExternalQueries,
{
    let mut all_witnesses = vec![];

    for constructor in sigma {
        let arity = constructor.fields.len();

        let specialized_matrix =
            specialise_matrix(check_state, exhaustiveness_state, context, &constructor, matrix);

        let Some(specialized_vector) =
            specialise_vector(check_state, exhaustiveness_state, context, &constructor, vector)
        else {
            unreachable!("invariant violated: vector contains constructor");
        };

        if let Some(witnesses) = algorithm_m(
            check_state,
            exhaustiveness_state,
            context,
            &specialized_matrix,
            &specialized_vector,
        )? {
            for witness in witnesses {
                let (argument_columns, tail_columns) = witness.split_at(arity);

                let pattern = PatternKind::Constructor {
                    constructor: Constructor {
                        file_id: constructor.file_id,
                        item_id: constructor.item_id,
                        fields: argument_columns.to_vec(),
                    },
                };

                let constructor = exhaustiveness_state.allocate(pattern, first_type);
                let tail_columns = tail_columns.iter().copied();

                let witnesses = iter::once(constructor).chain(tail_columns).collect();
                all_witnesses.push(witnesses);
            }
        }
    }

    if all_witnesses.is_empty() { Ok(None) } else { Ok(Some(all_witnesses)) }
}

fn algorithm_m_wildcard_incomplete<Q>(
    check_state: &mut CheckState,
    exhaustiveness_state: &mut ExhaustivenessState,
    context: &CheckContext<Q>,
    matrix: &PatternMatrix,
    vector: &PatternVector,
    first_type: TypeId,
    sigma: &[Constructor],
) -> QueryResult<Option<Vec<WitnessVector>>>
where
    Q: ExternalQueries,
{
    let default = default_matrix(exhaustiveness_state, matrix);
    let tail_columns = vector[1..].to_vec();

    let witnesses =
        algorithm_m(check_state, exhaustiveness_state, context, &default, &tail_columns)?;

    let Some(witnesses) = witnesses else {
        return Ok(None);
    };

    let head = if let Some((file_id, item_id, arity)) = pick_missing_constructor(context, sigma)? {
        // FIXME: Use unification variables or constructor argument types for these wildcards,
        // rather than `prim.unknown`. Also consider returning multiple missing constructors
        // where possible to improve witness quality.
        let fields = (0..arity)
            .map(|_| exhaustiveness_state.allocate_wildcard(context.prim.unknown))
            .collect_vec();

        let constructor = Constructor { file_id, item_id, fields };
        let pattern = PatternKind::Constructor { constructor };

        exhaustiveness_state.allocate(pattern, first_type)
    } else {
        exhaustiveness_state.allocate_wildcard(first_type)
    };

    Ok(Some(
        witnesses.into_iter().map(|witness| iter::once(head).chain(witness).collect()).collect(),
    ))
}

fn algorithm_m_other<Q>(
    check_state: &mut CheckState,
    exhaustiveness_state: &mut ExhaustivenessState,
    context: &CheckContext<Q>,
    matrix: &PatternMatrix,
    vector: &PatternVector,
    first_type: TypeId,
) -> QueryResult<Option<Vec<WitnessVector>>>
where
    Q: ExternalQueries,
{
    // For literals and other patterns, treat as incomplete sigma (conservative)
    let default = default_matrix(exhaustiveness_state, matrix);
    let tail = vector[1..].to_vec();

    let witnesses = algorithm_m(check_state, exhaustiveness_state, context, &default, &tail)?;

    let Some(witnesses) = witnesses else {
        return Ok(None);
    };

    // Prefix with wildcard
    let head = exhaustiveness_state.allocate_wildcard(first_type);
    Ok(Some(witnesses.into_iter().map(|w| iter::once(head).chain(w).collect()).collect()))
}

/// Specialises a [`PatternMatrix`] given a [`Constructor`].
fn specialise_matrix<Q>(
    check_state: &mut CheckState,
    exhaustiveness_state: &mut ExhaustivenessState,
    context: &CheckContext<Q>,
    expected: &Constructor,
    matrix: &PatternMatrix,
) -> PatternMatrix
where
    Q: ExternalQueries,
{
    let matrix = matrix.iter().filter_map(|row| {
        specialise_vector(check_state, exhaustiveness_state, context, expected, row)
    });
    matrix.collect()
}

/// Specialises a [`PatternVector`] given a [`Constructor`].
fn specialise_vector<Q>(
    _check_state: &mut CheckState,
    exhaustiveness_state: &mut ExhaustivenessState,
    context: &CheckContext<Q>,
    expected: &Constructor,
    vector: &PatternVector,
) -> Option<PatternVector>
where
    Q: ExternalQueries,
{
    let [first_column, ref tail_columns @ ..] = vector[..] else {
        unreachable!("invariant violated: specialise_vector processed empty row");
    };

    let first_column = &exhaustiveness_state.interner[first_column];

    if let PatternKind::Wildcard = first_column.kind {
        let wildcards = expected.fields.iter().map(|&pattern| {
            let t = exhaustiveness_state.interner[pattern].t;
            exhaustiveness_state.allocate_wildcard(t)
        });
        let tail_columns = tail_columns.iter().copied();
        return Some(iter::chain(wildcards, tail_columns).collect());
    }

    let PatternKind::Constructor { constructor } = &first_column.kind else {
        return Some(tail_columns.to_vec());
    };

    if (constructor.file_id, constructor.item_id) != (expected.file_id, expected.item_id) {
        return None;
    }

    Some(iter::chain(&constructor.fields, tail_columns).copied().collect())
}

fn default_matrix(
    exhaustiveness_state: &ExhaustivenessState,
    matrix: &PatternMatrix,
) -> PatternMatrix {
    let filter_map = matrix.iter().filter_map(|row| {
        let [first_column, ref default_columns @ ..] = row[..] else {
            unreachable!("invariant violated: default_matrix processed empty row");
        };
        if let PatternKind::Wildcard = exhaustiveness_state.interner[first_column].kind {
            Some(default_columns.to_vec())
        } else {
            None
        }
    });
    filter_map.collect()
}

/// Key for identifying a unique constructor (file + term item).
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct ConstructorKey(FileId, TermItemId);

/// Extracts the set of constructors (sigma) from the first column of the matrix.
///
/// Returns a list of unique constructors seen in the first column, keeping one
/// representative `Constructor` per distinct constructor id. Non-constructor
/// patterns (wildcards, literals, etc.) are ignored.
fn extract_sigma(
    exhaustiveness_state: &ExhaustivenessState,
    matrix: &PatternMatrix,
) -> Vec<Constructor> {
    let mut seen = FxHashSet::default();
    let mut sigma = vec![];

    for row in matrix {
        let [first_column, ..] = row[..] else {
            continue;
        };
        let pattern = &exhaustiveness_state.interner[first_column];
        if let PatternKind::Constructor { constructor } = &pattern.kind {
            let key = ConstructorKey(constructor.file_id, constructor.item_id);
            if seen.insert(key) {
                sigma.push(Constructor::clone(constructor));
            }
        }
    }

    sigma
}

/// Checks whether the set of constructors (sigma) is complete for the scrutinee type.
///
/// A sigma is complete if it contains all constructors of the data type. If we can't
/// determine the type or its constructors, we conservatively return false (incomplete).
fn sigma_is_complete<Q>(context: &CheckContext<Q>, sigma: &[Constructor]) -> QueryResult<bool>
where
    Q: ExternalQueries,
{
    // Empty sigma is never complete (we need at least one constructor to determine the type)
    let Some(first) = sigma.first() else {
        return Ok(false);
    };

    // Get the indexed module for the constructor's file
    let indexed = context.queries.indexed(first.file_id)?;

    // Find the type this constructor belongs to
    let Some(type_item_id) = indexed.pairs.constructor_type(first.item_id) else {
        return Ok(false);
    };

    // Get all constructors for this type
    let all_constructors: FxHashSet<TermItemId> =
        indexed.pairs.data_constructors(type_item_id).collect();

    // Check if sigma covers all constructors
    let sigma_terms: FxHashSet<TermItemId> = sigma.iter().map(|c| c.item_id).collect();

    Ok(all_constructors.iter().all(|term_id| sigma_terms.contains(term_id)))
}

/// Picks a missing constructor not in sigma, for witness generation.
///
/// Returns the term item id of a constructor not present in sigma, if one exists.
fn pick_missing_constructor<Q>(
    context: &CheckContext<Q>,
    sigma: &[Constructor],
) -> QueryResult<Option<(FileId, TermItemId, usize)>>
where
    Q: ExternalQueries,
{
    let Some(first) = sigma.first() else {
        return Ok(None);
    };

    let indexed = context.queries.indexed(first.file_id)?;

    let Some(type_item_id) = indexed.pairs.constructor_type(first.item_id) else {
        return Ok(None);
    };

    let sigma_terms: FxHashSet<TermItemId> = sigma.iter().map(|c| c.item_id).collect();

    for term_id in indexed.pairs.data_constructors(type_item_id) {
        if !sigma_terms.contains(&term_id) {
            // Get the arity of this constructor from its item info
            let arity = get_constructor_arity(context, first.file_id, term_id)?;
            return Ok(Some((first.file_id, term_id, arity)));
        }
    }

    Ok(None)
}

/// Gets the arity (number of fields) of a constructor.
fn get_constructor_arity<Q>(
    context: &CheckContext<Q>,
    file_id: FileId,
    term_id: TermItemId,
) -> QueryResult<usize>
where
    Q: ExternalQueries,
{
    let on_lowered = |lowered: &lowering::LoweredModule| {
        if let Some(lowering::TermItemIr::Constructor { arguments }) =
            lowered.info.get_term_item(term_id)
        {
            arguments.len()
        } else {
            0
        }
    };
    if file_id == context.id {
        let lowered = &context.lowered;
        Ok(on_lowered(lowered))
    } else {
        let lowered = context.queries.lowered(file_id)?;
        Ok(on_lowered(&lowered))
    }
}
