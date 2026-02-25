//! Implements surface-generic operator chain inference.

use building_types::QueryResult;
use files::FileId;
use indexing::{TermItemId, TypeItemId};
use lowering::IsElement;
use sugar::OperatorTree;
use sugar::bracketing::BracketingResult;

use crate::ExternalQueries;
use crate::context::CheckContext;
use crate::core::{TypeId, normalise, toolkit, unification};
use crate::source::{terms, types};
use crate::state::CheckState;

#[derive(Copy, Clone, Debug)]
enum OperatorKindMode {
    Infer,
    Check { expected_type: TypeId },
}

pub fn infer_operator_chain<Q, E>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    id: E,
) -> QueryResult<(E::Elaborated, TypeId)>
where
    Q: ExternalQueries,
    E: IsOperator<Q>,
{
    let unknown = (E::unknown_elaborated(context), context.unknown("invalid operator chain"));

    let Some(operator_tree) = E::lookup_tree(context, id) else {
        return Ok(unknown);
    };

    let Ok(operator_tree) = operator_tree else {
        return Ok(unknown);
    };

    traverse_operator_tree(state, context, operator_tree, OperatorKindMode::Infer)
}

fn traverse_operator_tree<Q, E>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    operator_tree: &OperatorTree<E>,
    mode: OperatorKindMode,
) -> QueryResult<(E::Elaborated, TypeId)>
where
    Q: ExternalQueries,
    E: IsOperator<Q>,
{
    let unknown_elaborated = E::unknown_elaborated(context);

    match operator_tree {
        OperatorTree::Leaf(None) => match mode {
            OperatorKindMode::Infer => {
                Ok((unknown_elaborated, context.unknown("missing operator leaf")))
            }
            OperatorKindMode::Check { expected_type } => Ok((unknown_elaborated, expected_type)),
        },

        OperatorTree::Leaf(Some(type_id)) => match mode {
            OperatorKindMode::Infer => E::infer_surface(state, context, *type_id),
            OperatorKindMode::Check { expected_type } => {
                E::check_surface(state, context, *type_id, expected_type)
            }
        },

        OperatorTree::Branch(operator_id, children) => {
            let Some((file_id, item_id)) = E::lookup_operator(context, *operator_id) else {
                return match mode {
                    OperatorKindMode::Infer => {
                        Ok((unknown_elaborated, context.unknown("missing operator resolution")))
                    }
                    OperatorKindMode::Check { expected_type } => {
                        Ok((unknown_elaborated, expected_type))
                    }
                };
            };

            let operator_type = E::lookup_item(state, context, file_id, item_id)?;

            traverse_operator_branch(
                state,
                context,
                (file_id, item_id),
                operator_type,
                children,
                mode,
            )
        }
    }
}

fn traverse_operator_branch<Q, E>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    operator: (FileId, E::ItemId),
    operator_type: TypeId,
    children: &[OperatorTree<E>; 2],
    mode: OperatorKindMode,
) -> QueryResult<(E::Elaborated, TypeId)>
where
    Q: ExternalQueries,
    E: IsOperator<Q>,
{
    let unknown_elaborated = E::unknown_elaborated(context);
    let unknown = match mode {
        OperatorKindMode::Infer => (unknown_elaborated, context.unknown("invalid operator kind")),
        OperatorKindMode::Check { expected_type } => (unknown_elaborated, expected_type),
    };

    let operator_type = toolkit::instantiate_unifications(state, context, operator_type)?;
    let Some((left_type, operator_type)) =
        toolkit::decompose_function(state, context, operator_type)?
    else {
        return Ok(unknown);
    };

    let operator_type = toolkit::instantiate_unifications(state, context, operator_type)?;
    let Some((right_type, result_type)) =
        toolkit::decompose_function(state, context, operator_type)?
    else {
        return Ok(unknown);
    };

    if let OperatorKindMode::Check { expected_type } = mode {
        let _ = unification::subtype(state, context, result_type, expected_type)?;
    }

    let [left_tree, right_tree] = children;

    let (left, _) = traverse_operator_tree(
        state,
        context,
        left_tree,
        OperatorKindMode::Check { expected_type: left_type },
    )?;

    let (right, _) = traverse_operator_tree(
        state,
        context,
        right_tree,
        OperatorKindMode::Check { expected_type: right_type },
    )?;

    E::build(state, context, operator, (left, right), result_type)
}

pub fn elaborate_operator_application_kind<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    file_id: FileId,
    type_id: TypeItemId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let operator_kind = toolkit::lookup_file_type(state, context, file_id, type_id)?;
    let operator_kind = toolkit::instantiate_unifications(state, context, operator_kind)?;
    let operator_function = toolkit::inspect_function(state, context, operator_kind)?;

    if operator_function.arguments.len() >= 2 {
        Ok(operator_function.result)
    } else {
        Ok(context.unknown("invalid operator kind"))
    }
}

pub trait IsOperator<Q: ExternalQueries>: IsElement {
    type ItemId: Copy;
    type Elaborated: Copy;

    fn unknown_elaborated(context: &CheckContext<Q>) -> Self::Elaborated;

    fn lookup_tree<'q>(
        context: &'q CheckContext<Q>,
        id: Self,
    ) -> Option<&'q BracketingResult<Self>>;

    fn lookup_operator(
        context: &CheckContext<Q>,
        id: Self::OperatorId,
    ) -> Option<(FileId, Self::ItemId)>;

    fn lookup_item(
        state: &mut CheckState,
        context: &CheckContext<Q>,
        file_id: FileId,
        item_id: Self::ItemId,
    ) -> QueryResult<TypeId>;

    fn infer_surface(
        state: &mut CheckState,
        context: &CheckContext<Q>,
        id: Self,
    ) -> QueryResult<(Self::Elaborated, TypeId)>;

    fn check_surface(
        state: &mut CheckState,
        context: &CheckContext<Q>,
        id: Self,
        expected: TypeId,
    ) -> QueryResult<(Self::Elaborated, TypeId)>;

    fn build(
        state: &mut CheckState,
        context: &CheckContext<Q>,
        operator: (FileId, Self::ItemId),
        result_tree: (Self::Elaborated, Self::Elaborated),
        result_type: TypeId,
    ) -> QueryResult<(Self::Elaborated, TypeId)>;
}

impl<Q: ExternalQueries> IsOperator<Q> for lowering::ExpressionId {
    type ItemId = TermItemId;
    type Elaborated = ();

    fn unknown_elaborated(_context: &CheckContext<Q>) -> Self::Elaborated {}

    fn lookup_tree<'q>(
        context: &'q CheckContext<Q>,
        id: Self,
    ) -> Option<&'q BracketingResult<Self>> {
        context.bracketed.expressions.get(&id)
    }

    fn lookup_operator(
        context: &CheckContext<Q>,
        id: Self::OperatorId,
    ) -> Option<(FileId, Self::ItemId)> {
        context.lowered.info.get_term_operator(id)
    }

    fn lookup_item(
        state: &mut CheckState,
        context: &CheckContext<Q>,
        file_id: FileId,
        item_id: Self::ItemId,
    ) -> QueryResult<TypeId> {
        toolkit::lookup_file_term_operator(state, context, file_id, item_id)
    }

    fn infer_surface(
        state: &mut CheckState,
        context: &CheckContext<Q>,
        id: Self,
    ) -> QueryResult<(Self::Elaborated, TypeId)> {
        let inferred_type = terms::infer_expression(state, context, id)?;
        Ok(((), inferred_type))
    }

    fn check_surface(
        state: &mut CheckState,
        context: &CheckContext<Q>,
        id: Self,
        expected: TypeId,
    ) -> QueryResult<(Self::Elaborated, TypeId)> {
        let checked_type = terms::check_expression(state, context, id, expected)?;
        Ok(((), checked_type))
    }

    fn build(
        state: &mut CheckState,
        _context: &CheckContext<Q>,
        (_, _): (FileId, Self::ItemId),
        (_, _): (Self::Elaborated, Self::Elaborated),
        result_type: TypeId,
    ) -> QueryResult<(Self::Elaborated, TypeId)> {
        let _ = state;
        Ok(((), result_type))
    }
}

impl<Q: ExternalQueries> IsOperator<Q> for lowering::TypeId {
    type ItemId = indexing::TypeItemId;
    type Elaborated = TypeId;

    fn unknown_elaborated(context: &CheckContext<Q>) -> Self::Elaborated {
        context.unknown("invalid operator chain")
    }

    fn lookup_tree<'q>(
        context: &'q CheckContext<Q>,
        id: Self,
    ) -> Option<&'q BracketingResult<Self>> {
        context.bracketed.types.get(&id)
    }

    fn lookup_operator(
        context: &CheckContext<Q>,
        id: Self::OperatorId,
    ) -> Option<(FileId, Self::ItemId)> {
        context.lowered.info.get_type_operator(id)
    }

    fn lookup_item(
        state: &mut CheckState,
        context: &CheckContext<Q>,
        file_id: FileId,
        item_id: Self::ItemId,
    ) -> QueryResult<TypeId> {
        toolkit::lookup_file_type_operator(state, context, file_id, item_id)
    }

    fn infer_surface(
        state: &mut CheckState,
        context: &CheckContext<Q>,
        id: Self,
    ) -> QueryResult<(Self::Elaborated, TypeId)> {
        types::infer_kind(state, context, id)
    }

    fn check_surface(
        state: &mut CheckState,
        context: &CheckContext<Q>,
        id: Self,
        expected: TypeId,
    ) -> QueryResult<(Self::Elaborated, TypeId)> {
        types::check_kind(state, context, id, expected)
    }

    fn build(
        state: &mut CheckState,
        context: &CheckContext<Q>,
        (file_id, item_id): (FileId, Self::ItemId),
        (left, right): (Self::Elaborated, Self::Elaborated),
        result_kind: TypeId,
    ) -> QueryResult<(Self::Elaborated, TypeId)> {
        let elaborated_type = context.intern_operator_application(file_id, item_id, left, right);
        let result_kind = normalise::normalise(state, context, result_kind)?;
        Ok((elaborated_type, result_kind))
    }
}
