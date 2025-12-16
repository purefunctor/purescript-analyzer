//! Generic operator chain inference

use building_types::QueryResult;
use files::FileId;
use indexing::{TermItemId, TypeItemId};
use lowering::IsElement;
use sugar::OperatorTree;
use sugar::bracketing::BracketingResult;

use crate::ExternalQueries;
use crate::algorithm::state::{CheckContext, CheckState};
use crate::algorithm::{kind, substitute, term, unification};
use crate::core::{ForallBinder, Type, TypeId};

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
    let unknown = (E::unknown_elaborated(context), context.prim.unknown);

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
            OperatorKindMode::Infer => Ok((unknown_elaborated, context.prim.unknown)),
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
                    OperatorKindMode::Infer => Ok((unknown_elaborated, context.prim.unknown)),
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
        OperatorKindMode::Infer => (unknown_elaborated, context.prim.unknown),
        OperatorKindMode::Check { expected_type } => (unknown_elaborated, expected_type),
    };

    let operator_type = instantiate_forall(state, operator_type);
    let operator_type = state.normalize_type(operator_type);

    let Type::Function(left_type, operator_type) = state.storage[operator_type] else {
        return Ok(unknown);
    };

    let operator_type = state.normalize_type(operator_type);
    let Type::Function(right_type, result_type) = state.storage[operator_type] else {
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

    Ok(E::build(state, context, operator, (left, right), result_type))
}

pub fn instantiate_forall(state: &mut CheckState, mut kind_id: TypeId) -> TypeId {
    loop {
        kind_id = state.normalize_type(kind_id);
        if let Type::Forall(ForallBinder { kind, .. }, inner) = state.storage[kind_id] {
            let unification = state.fresh_unification_kinded(kind);
            kind_id = substitute::substitute_bound(state, unification, inner);
        } else {
            break kind_id;
        }
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
    ) -> (Self::Elaborated, TypeId);
}

impl<Q: ExternalQueries> IsOperator<Q> for lowering::TypeId {
    type ItemId = TypeItemId;
    type Elaborated = TypeId;

    fn unknown_elaborated(context: &CheckContext<Q>) -> Self::Elaborated {
        context.prim.unknown
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
        kind::lookup_file_type(state, context, file_id, item_id)
    }

    fn infer_surface(
        state: &mut CheckState,
        context: &CheckContext<Q>,
        id: Self,
    ) -> QueryResult<(Self::Elaborated, TypeId)> {
        kind::infer_surface_kind(state, context, id)
    }

    fn check_surface(
        state: &mut CheckState,
        context: &CheckContext<Q>,
        id: Self,
        expected: TypeId,
    ) -> QueryResult<(Self::Elaborated, TypeId)> {
        kind::check_surface_kind(state, context, id, expected)
    }

    fn build(
        state: &mut CheckState,
        _context: &CheckContext<Q>,
        (file_id, item_id): (FileId, Self::ItemId),
        (left, right): (Self::Elaborated, Self::Elaborated),
        result_kind: TypeId,
    ) -> (Self::Elaborated, TypeId) {
        let elaborated_type =
            state.storage.intern(Type::OperatorApplication(file_id, item_id, left, right));

        let result_kind = state.normalize_type(result_kind);

        (elaborated_type, result_kind)
    }
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
        term::lookup_file_term(state, context, file_id, item_id)
    }

    fn infer_surface(
        state: &mut CheckState,
        context: &CheckContext<Q>,
        id: Self,
    ) -> QueryResult<(Self::Elaborated, TypeId)> {
        let inferred_type = term::infer_expression(state, context, id)?;
        Ok(((), inferred_type))
    }

    fn check_surface(
        state: &mut CheckState,
        context: &CheckContext<Q>,
        id: Self,
        expected: TypeId,
    ) -> QueryResult<(Self::Elaborated, TypeId)> {
        let checked_type = term::check_expression(state, context, id, expected)?;
        Ok(((), checked_type))
    }

    fn build(
        _: &mut CheckState,
        _: &CheckContext<Q>,
        (_, _): (FileId, Self::ItemId),
        (_, _): (Self::Elaborated, Self::Elaborated),
        result_type: TypeId,
    ) -> (Self::Elaborated, TypeId) {
        ((), result_type)
    }
}

impl<Q: ExternalQueries> IsOperator<Q> for lowering::BinderId {
    type ItemId = TermItemId;
    type Elaborated = ();

    fn unknown_elaborated(_context: &CheckContext<Q>) -> Self::Elaborated {}

    fn lookup_tree<'q>(
        context: &'q CheckContext<Q>,
        id: Self,
    ) -> Option<&'q BracketingResult<Self>> {
        context.bracketed.binders.get(&id)
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
        term::lookup_file_term(state, context, file_id, item_id)
    }

    fn infer_surface(
        state: &mut CheckState,
        context: &CheckContext<Q>,
        id: Self,
    ) -> QueryResult<(Self::Elaborated, TypeId)> {
        let inferred_type = term::infer_binder(state, context, id)?;
        Ok(((), inferred_type))
    }

    fn check_surface(
        state: &mut CheckState,
        context: &CheckContext<Q>,
        id: Self,
        expected: TypeId,
    ) -> QueryResult<(Self::Elaborated, TypeId)> {
        let checked_type = term::check_binder(state, context, id, expected)?;
        Ok(((), checked_type))
    }

    fn build(
        _: &mut CheckState,
        _: &CheckContext<Q>,
        (_, _): (FileId, Self::ItemId),
        (_, _): (Self::Elaborated, Self::Elaborated),
        result_type: TypeId,
    ) -> (Self::Elaborated, TypeId) {
        ((), result_type)
    }
}
