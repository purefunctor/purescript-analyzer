mod convert;
mod pretty;

use std::iter;

use building_types::QueryResult;
use files::FileId;
use indexing::TermItemId;
use itertools::Itertools;
use rustc_hash::FxHashSet;
use smol_str::SmolStr;

use crate::algorithm::state::{CheckContext, CheckState};
use crate::algorithm::{derive, toolkit};
use crate::{ExternalQueries, TypeId};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Pattern {
    pub kind: PatternKind,
    pub t: TypeId,
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
    pub file_id: FileId,
    pub item_id: TermItemId,
    pub fields: Vec<PatternId>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum RecordElement {
    Named(SmolStr, PatternId),
    Pun(SmolStr),
}

pub type PatternId = interner::Id<Pattern>;
pub type PatternStorage = interner::Interner<Pattern>;

type PatternVector = Vec<PatternId>;
type PatternMatrix = Vec<PatternVector>;
pub type WitnessVector = Vec<PatternId>;

/// Determines if a [`PatternVector`] is useful with respect to a [`PatternMatrix`].
///
/// A pattern vector is useful if it matches at least one value not matched by
/// any pattern vector in the matrix. This is one of the core algorithms from
/// Maranget's "Warnings for pattern matching" paper.
///
/// See [`algorithm_u_constructor`] and [`algorithm_u_wildcard`] for reference.
fn algorithm_u<Q>(
    state: &mut CheckState,
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

    let first_pattern = state.patterns[first_pattern].clone();

    match first_pattern.kind {
        PatternKind::Constructor { constructor } => {
            algorithm_u_constructor(state, context, matrix, vector, constructor)
        }
        PatternKind::Wildcard => {
            algorithm_u_wildcard(state, context, matrix, vector, first_pattern.t)
        }
        _ => algorithm_u_other(state, context, matrix, vector),
    }
}

/// Induction 1
///
/// This function uses specialisation to spread the provided [`Constructor`]
/// over both the [`PatternMatrix`] and the [`PatternVector`], before calling
/// [`algorithm_u`] recursively with the specialised structures.
fn algorithm_u_constructor<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    matrix: &PatternMatrix,
    vector: &PatternVector,
    constructor: Constructor,
) -> QueryResult<bool>
where
    Q: ExternalQueries,
{
    let specialised_matrix = specialise_matrix(state, &constructor, matrix);

    let Some(specialised_vector) = specialise_vector(state, &constructor, vector) else {
        unreachable!("invariant violated: vector contains constructor");
    };

    algorithm_u(state, context, &specialised_matrix, &specialised_vector)
}

/// Induction 2
///
/// This function collects all constructor references from the first column of
/// the matrix into a collection called the sigma.
///
/// If the sigma is complete, for each constructor in the sigma, we specialise
/// the pattern matrix and pattern vector against it. Then, we recursively call
/// [`algorithm_u`] against the specialised structures. The pattern vector is
/// useful if any specialised pattern vector is useful against its specialised
/// pattern matrix.
///
/// If the sigma is incomplete, we recursively call [`algorithm_u`] against the
/// [`default_matrix`] of the pattern matrix and the tail of the pattern vector.
fn algorithm_u_wildcard<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    matrix: &PatternMatrix,
    vector: &PatternVector,
    t: TypeId,
) -> QueryResult<bool>
where
    Q: ExternalQueries,
{
    let sigma = collect_sigma(state, context, matrix, t)?;
    let complete = sigma_is_complete(context, &sigma)?;

    if complete {
        algorithm_u_wildcard_complete(state, context, matrix, vector, sigma)
    } else {
        algorithm_u_wildcard_incomplete(state, context, matrix, vector)
    }
}

fn algorithm_u_wildcard_complete<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    matrix: &PatternMatrix,
    vector: &PatternVector,
    sigma: Sigma,
) -> QueryResult<bool>
where
    Q: ExternalQueries,
{
    for constructor in sigma.constructors {
        let specialised_matrix = specialise_matrix(state, &constructor, matrix);

        let Some(specialised_vector) = specialise_vector(state, &constructor, vector) else {
            unreachable!("invariant violated: vector contains constructor");
        };

        if algorithm_u(state, context, &specialised_matrix, &specialised_vector)? {
            return Ok(true);
        }
    }
    Ok(false)
}

fn algorithm_u_wildcard_incomplete<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    matrix: &PatternMatrix,
    vector: &PatternVector,
) -> QueryResult<bool>
where
    Q: ExternalQueries,
{
    let default = default_matrix(state, matrix);
    let tail_columns = vector[1..].to_vec();
    algorithm_u(state, context, &default, &tail_columns)
}

fn algorithm_u_other<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    matrix: &PatternMatrix,
    vector: &PatternVector,
) -> QueryResult<bool>
where
    Q: ExternalQueries,
{
    // For literals and other patterns, treat as incomplete sigma (conservative)
    let default = default_matrix(state, matrix);
    let tail_columns = vector[1..].to_vec();
    algorithm_u(state, context, &default, &tail_columns)
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
/// these witnesses are pattern vectors that denote values not yet covered by
/// the matrix.
///
/// The [`algorithm_m_wildcard`] induction is prolific for producing these
/// these witnesses as it compares the constructors that appear in the
/// matrix against the constructors available in the checking environment.
fn algorithm_m<Q>(
    state: &mut CheckState,
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

    let first_pattern = state.patterns[first_pattern].clone();

    match first_pattern.kind {
        PatternKind::Constructor { constructor } => {
            algorithm_m_constructor(state, context, matrix, vector, constructor, first_pattern.t)
        }
        PatternKind::Wildcard => {
            algorithm_m_wildcard(state, context, matrix, vector, first_pattern.t)
        }
        _ => algorithm_m_other(state, context, matrix, vector, first_pattern.t),
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
    state: &mut CheckState,
    context: &CheckContext<Q>,
    matrix: &PatternMatrix,
    vector: &PatternVector,
    constructor: Constructor,
    t: TypeId,
) -> QueryResult<Option<Vec<WitnessVector>>>
where
    Q: ExternalQueries,
{
    let arity = constructor.fields.len();

    let specialised_matrix = specialise_matrix(state, &constructor, matrix);

    let Some(specialised_vector) = specialise_vector(state, &constructor, vector) else {
        unreachable!("invariant violated: vector contains constructor");
    };

    let witnesses = algorithm_m(state, context, &specialised_matrix, &specialised_vector)?;

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

        let constructor = state.allocate_pattern(pattern, t);
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
    state: &mut CheckState,
    context: &CheckContext<Q>,
    matrix: &PatternMatrix,
    vector: &PatternVector,
    t: TypeId,
) -> QueryResult<Option<Vec<WitnessVector>>>
where
    Q: ExternalQueries,
{
    let sigma = collect_sigma(state, context, matrix, t)?;
    let complete = sigma_is_complete(context, &sigma)?;
    if complete {
        algorithm_m_wildcard_complete(state, context, matrix, vector, t, &sigma)
    } else {
        algorithm_m_wildcard_incomplete(state, context, matrix, vector, t, &sigma)
    }
}

fn algorithm_m_wildcard_complete<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    matrix: &PatternMatrix,
    vector: &PatternVector,
    t: TypeId,
    sigma: &Sigma,
) -> QueryResult<Option<Vec<WitnessVector>>>
where
    Q: ExternalQueries,
{
    let mut all_witnesses = vec![];

    for constructor in &sigma.constructors {
        let arity = constructor.fields.len();

        let specialised_matrix = specialise_matrix(state, constructor, matrix);

        let Some(specialised_vector) = specialise_vector(state, constructor, vector) else {
            unreachable!("invariant violated: vector contains constructor");
        };

        if let Some(witnesses) =
            algorithm_m(state, context, &specialised_matrix, &specialised_vector)?
        {
            for witness in witnesses {
                let (argument_columns, tail_columns) = witness.split_at(arity);

                let pattern = PatternKind::Constructor {
                    constructor: Constructor {
                        file_id: constructor.file_id,
                        item_id: constructor.item_id,
                        fields: argument_columns.to_vec(),
                    },
                };

                let constructor = state.allocate_pattern(pattern, t);
                let tail_columns = tail_columns.iter().copied();

                let witnesses = iter::once(constructor).chain(tail_columns).collect();
                all_witnesses.push(witnesses);
            }
        }
    }

    if all_witnesses.is_empty() { Ok(None) } else { Ok(Some(all_witnesses)) }
}

fn algorithm_m_wildcard_incomplete<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    matrix: &PatternMatrix,
    vector: &PatternVector,
    t: TypeId,
    sigma: &Sigma,
) -> QueryResult<Option<Vec<WitnessVector>>>
where
    Q: ExternalQueries,
{
    let default = default_matrix(state, matrix);
    let tail_columns = vector[1..].to_vec();

    let witnesses = algorithm_m(state, context, &default, &tail_columns)?;

    let Some(witnesses) = witnesses else {
        return Ok(None);
    };

    let first_column = if let Some(constructor) = sigma.missing.first() {
        let fields = constructor.fields.iter().map(|&t| state.allocate_wildcard(t)).collect_vec();

        let pattern = PatternKind::Constructor {
            constructor: Constructor {
                file_id: constructor.file_id,
                item_id: constructor.item_id,
                fields,
            },
        };

        state.allocate_pattern(pattern, t)
    } else {
        state.allocate_wildcard(t)
    };

    let witness = witnesses
        .into_iter()
        .map(|witness| iter::once(first_column).chain(witness).collect())
        .collect();

    Ok(Some(witness))
}

fn algorithm_m_other<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    matrix: &PatternMatrix,
    vector: &PatternVector,
    t: TypeId,
) -> QueryResult<Option<Vec<WitnessVector>>>
where
    Q: ExternalQueries,
{
    // For literals and other patterns, treat as incomplete sigma (conservative)
    let default = default_matrix(state, matrix);
    let tail = vector[1..].to_vec();

    let witnesses = algorithm_m(state, context, &default, &tail)?;

    let Some(witnesses) = witnesses else {
        return Ok(None);
    };

    // Prefix with wildcard
    let head = state.allocate_wildcard(t);
    Ok(Some(witnesses.into_iter().map(|w| iter::once(head).chain(w).collect()).collect()))
}

/// Specialises a [`PatternMatrix`] given a [`Constructor`].
///
/// See documentation below for [`specialise_vector`].
fn specialise_matrix(
    state: &mut CheckState,
    expected: &Constructor,
    matrix: &PatternMatrix,
) -> PatternMatrix {
    matrix.iter().filter_map(|row| specialise_vector(state, expected, row)).collect()
}

/// Specialises a [`PatternVector`] given a [`Constructor`].
///
/// Specialisation takes a pattern vector and applies the following rules:
/// 1. If the first column is a wildcard, it expands it to `n` wildcards
///    where `n` is the arity of the expected [`Constructor`].
/// 2. It returns `None` for constructors that are not the expected
///    [`Constructor`], which excludes them from the specialised matrix.
///    For example, a pattern vector specialised on `Just` removes `Nothing`.
/// 3. For matching constructors, it 'splats' the fields, effectively turning
///    a pattern vector like `[Just _]` into `[_]` or `[Nothing]` into `[]`.
fn specialise_vector(
    state: &mut CheckState,
    expected: &Constructor,
    vector: &PatternVector,
) -> Option<PatternVector> {
    let [first_column, ref tail_columns @ ..] = vector[..] else {
        unreachable!("invariant violated: specialise_vector processed empty row");
    };

    let first_column = &state.patterns[first_column];

    if let PatternKind::Wildcard = first_column.kind {
        let wildcards = expected.fields.iter().map(|&pattern| {
            let t = state.patterns[pattern].t;
            state.allocate_wildcard(t)
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

fn default_matrix(state: &CheckState, matrix: &PatternMatrix) -> PatternMatrix {
    let filter_map = matrix.iter().filter_map(|row| {
        let [first_column, ref default_columns @ ..] = row[..] else {
            unreachable!("invariant violated: default_matrix processed empty row");
        };
        if let PatternKind::Wildcard = state.patterns[first_column].kind {
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

#[derive(Clone, Debug)]
struct Sigma {
    constructors: Vec<Constructor>,
    missing: Vec<MissingConstructor>,
}

#[derive(Clone, Debug)]
struct MissingConstructor {
    file_id: FileId,
    item_id: TermItemId,
    fields: Vec<TypeId>,
}

/// Extracts the set of constructors (sigma) from the first column of the matrix.
///
/// Returns a list of unique constructors seen in the first column, keeping one
/// representative `Constructor` per distinct constructor id. Non-constructor
/// patterns (wildcards, literals, etc.) are ignored.
fn collect_sigma<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    matrix: &PatternMatrix,
    scrutinee_type: TypeId,
) -> QueryResult<Sigma>
where
    Q: ExternalQueries,
{
    let mut seen = FxHashSet::default();
    let mut constructors = vec![];

    for row in matrix {
        let [first_column, ..] = row[..] else {
            continue;
        };
        let pattern = &state.patterns[first_column];
        if let PatternKind::Constructor { constructor } = &pattern.kind {
            let key = ConstructorKey(constructor.file_id, constructor.item_id);
            if seen.insert(key) {
                constructors.push(Constructor::clone(constructor));
            }
        }
    }

    let missing = collect_missing_constructors(state, context, scrutinee_type, &constructors)?;
    Ok(Sigma { constructors, missing })
}

/// Checks whether the set of constructors (sigma) is complete for the scrutinee type.
///
/// A sigma is complete if it contains all constructors of the data type. If we can't
/// determine the type or its constructors, we conservatively return false (incomplete).
fn sigma_is_complete<Q>(context: &CheckContext<Q>, sigma: &Sigma) -> QueryResult<bool>
where
    Q: ExternalQueries,
{
    // Empty sigma is never complete (we need at least one constructor to determine the type)
    let Some(first) = sigma.constructors.first() else {
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
    let sigma_terms: FxHashSet<TermItemId> = sigma.constructors.iter().map(|c| c.item_id).collect();

    Ok(all_constructors.iter().all(|term_id| sigma_terms.contains(term_id)))
}

fn collect_missing_constructors<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    scrutinee_type: TypeId,
    constructors: &[Constructor],
) -> QueryResult<Vec<MissingConstructor>>
where
    Q: ExternalQueries,
{
    let Some(constructor) = constructors.first() else {
        return Ok(vec![]);
    };

    let indexed = context.queries.indexed(constructor.file_id)?;

    let Some(type_item_id) = indexed.pairs.constructor_type(constructor.item_id) else {
        return Ok(vec![]);
    };

    let sigma: FxHashSet<TermItemId> = constructors.iter().map(|c| c.item_id).collect();
    let arguments = toolkit::extract_all_applications(state, scrutinee_type);

    let mut missing = vec![];
    for item_id in indexed.pairs.data_constructors(type_item_id) {
        let file_id = constructor.file_id;
        if !sigma.contains(&item_id) {
            let fields = constructor_field_types(state, context, file_id, item_id, &arguments)?;
            missing.push(MissingConstructor { file_id, item_id, fields });
        }
    }

    Ok(missing)
}

fn constructor_field_types<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    file_id: FileId,
    term_id: TermItemId,
    arguments: &[TypeId],
) -> QueryResult<Vec<TypeId>>
where
    Q: ExternalQueries,
{
    let constructor_type = derive::lookup_local_term_type(state, context, file_id, term_id)?;
    if let Some(constructor_type) = constructor_type {
        let constructor = toolkit::instantiate_with_arguments(state, constructor_type, arguments);
        let (fields, _) = toolkit::extract_function_arguments(state, constructor);
        Ok(fields)
    } else {
        let arity = get_constructor_arity(context, file_id, term_id)?;
        Ok(iter::repeat_n(context.prim.unknown, arity).collect())
    }
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

pub struct CasePatternReport {
    pub missing: Option<Vec<String>>,
    pub redundant: Vec<String>,
}

/// Checks case branch usefulness and exhaustiveness against scrutinee types.
///
/// Returns a report of missing patterns (if any) and redundant branches.
/// Only unconditional branches are counted toward these checks, as pattern
/// guards do not guarantee coverage.
pub fn check_case_patterns<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    trunk_types: &[TypeId],
    branches: &[lowering::CaseBranch],
) -> QueryResult<CasePatternReport>
where
    Q: ExternalQueries,
{
    if trunk_types.is_empty() {
        return Ok(CasePatternReport { missing: None, redundant: vec![] });
    }

    let mut unconditional = vec![];
    for branch in branches {
        let is_unconditional = matches!(
            &branch.guarded_expression,
            Some(lowering::GuardedExpression::Unconditional { .. }) | None
        );
        if !is_unconditional {
            continue;
        }

        let mut row: PatternVector = vec![];
        for &binder_id in branch.binders.iter() {
            row.push(convert::convert_binder(state, context, binder_id));
        }

        while row.len() < trunk_types.len() {
            let t = trunk_types[row.len()];
            row.push(state.allocate_wildcard(t));
        }

        if !row.is_empty() {
            unconditional.push(row);
        }
    }

    let mut redundant = vec![];
    let mut matrix = vec![];
    for vector in &unconditional {
        let useful = algorithm_u(state, context, &matrix, vector)?;
        if useful {
            matrix.push(PatternVector::clone(vector));
        } else {
            redundant.push(pretty::pretty_witness(context, state, vector));
        }
    }

    let query: PatternVector = trunk_types.iter().map(|&t| state.allocate_wildcard(t)).collect();

    let witnesses = algorithm_m(state, context, &unconditional, &query)?;
    let missing = witnesses.map(|witnesses| {
        witnesses
            .iter()
            .take(5)
            .map(|witness| pretty::pretty_witness(context, state, witness))
            .collect()
    });

    Ok(CasePatternReport { missing, redundant })
}
