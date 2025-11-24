use indexing::TypeItemId;
use itertools::Itertools;
use lowering::{DataIr, NewtypeIr, TermItemIr, TypeItemIr, TypeVariableBinding};
use smol_str::SmolStr;

use crate::ExternalQueries;
use crate::algorithm::{CheckContext, CheckState, convert, kind, transfer, unification};
use crate::core::{ForallBinder, Type, TypeId, Variable};

const MISSING_NAME: SmolStr = SmolStr::new_static("<MissingName>");

pub(crate) fn check_type_item<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TypeItemId,
) where
    Q: ExternalQueries,
{
    let Some(item) = context.lowered.info.get_type_item(item_id) else { return };
    match item {
        TypeItemIr::DataGroup { signature, data, .. } => {
            let Some(DataIr { variables }) = data else { return };
            check_data_like(state, context, item_id, *signature, variables);
        }

        TypeItemIr::NewtypeGroup { signature, newtype, .. } => {
            let Some(NewtypeIr { variables }) = newtype else { return };
            check_data_like(state, context, item_id, *signature, variables);
        }

        TypeItemIr::SynonymGroup { .. } => (),

        TypeItemIr::ClassGroup { .. } => (),

        TypeItemIr::Foreign { signature, .. } => {
            let Some(signature_id) = signature else { return };
            let (inferred_type, _) =
                kind::check_surface_kind(state, context, *signature_id, context.prim.t);
            let inferred_type = transfer::globalize(state, context, inferred_type);
            state.checked.types.insert(item_id, inferred_type);
        }

        TypeItemIr::Operator { .. } => (),
    }
}

fn check_data_like<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TypeItemId,
    signature: Option<lowering::TypeId>,
    variables: &[TypeVariableBinding],
) where
    Q: ExternalQueries,
{
    let signature = signature.map(|id| convert::inspect_signature(state, context, id));

    let (kind_variables, type_variables, result_kind) = if let Some(signature) = signature {
        if variables.len() != signature.arguments.len() {
            todo!("proper arity checking errors innit");
        };

        let variables = variables.iter();
        let arguments = signature.arguments.iter();

        let kinds = variables.zip(arguments).map(|(variable, &argument)| {
            // Use contravariant subtyping for type variables:
            //
            // data Example :: Argument -> Type
            // data Example (a :: Variable) = Example
            //
            // Signature: Argument -> Type
            // Inferred: Variable -> Type
            //
            // Given
            //   Variable -> Type <: Argument -> Type
            //
            // Therefore
            //   [Argument <: Variable, Type <: Type]
            let kind = variable.kind.map_or(argument, |kind| {
                let kind = convert::type_to_core(state, context, kind);
                let valid = unification::subsumes(state, context, argument, kind);
                if valid { kind } else { context.prim.unknown }
            });

            let name = variable.name.clone().unwrap_or(MISSING_NAME);
            (variable.id, variable.visible, name, kind)
        });

        let kinds = kinds.collect_vec();

        let kind_variables = signature.variables;
        let result_kind = signature.result;
        let type_variables = kinds.into_iter().map(|(id, visible, name, kind)| {
            let level = state.bind_forall(id, kind);
            ForallBinder { visible, name, level, kind }
        });

        (kind_variables, type_variables.collect_vec(), result_kind)
    } else {
        let kind_variables = vec![];
        let result_kind = context.prim.t;
        let type_variables = variables.iter().map(|variable| {
            let kind = match variable.kind {
                Some(id) => convert::type_to_core(state, context, id),
                None => state.fresh_unification_type(context),
            };

            let visible = variable.visible;
            let name = variable.name.clone().unwrap_or(MISSING_NAME);
            let level = state.bind_forall(variable.id, kind);
            ForallBinder { visible, name, level, kind }
        });

        (kind_variables, type_variables.collect_vec(), result_kind)
    };

    let data_reference = {
        let size = state.bound.size();
        let reference_type = state.storage.intern(Type::Constructor(context.id, item_id));
        type_variables.iter().cloned().fold(reference_type, |reference_type, variable| {
            let Some(index) = variable.level.to_index(size) else {
                let level = variable.level;
                unreachable!("invariant violated: invalid {level} for {size}");
            };

            let variable = Variable::Bound(index);
            let variable = state.storage.intern(Type::Variable(variable));

            state.storage.intern(Type::Application(reference_type, variable))
        })
    };

    let type_kind = type_variables.iter().rfold(result_kind, |result, variable| {
        state.storage.intern(Type::Function(variable.kind, result))
    });

    if let Some(pending_kind) = state.binding_group.types.get(&item_id) {
        unification::unify(state, context, *pending_kind, type_kind);
    } else {
        let type_kind = kind_variables.iter().rfold(type_kind, |inner, binder| {
            let binder = binder.clone();
            state.storage.intern(Type::Forall(binder, inner))
        });
        state.binding_group.types.insert(item_id, type_kind);
    };

    check_data_constructors(
        state,
        context,
        item_id,
        &kind_variables,
        &type_variables,
        data_reference,
    );

    if let Some(variable) = type_variables.first() {
        state.unbind(variable.level);
    }

    if let Some(variable) = kind_variables.first() {
        state.unbind(variable.level);
    }
}

fn check_data_constructors<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TypeItemId,
    kind_variables: &[ForallBinder],
    type_variables: &[ForallBinder],
    data_reference: TypeId,
) where
    Q: ExternalQueries,
{
    for item_id in context.indexed.pairs.data_constructors(item_id) {
        let Some(TermItemIr::Constructor { arguments }) =
            context.lowered.info.get_term_item(item_id)
        else {
            continue;
        };

        let arguments = arguments.iter().map(|&argument| {
            let (inferred_type, _) =
                kind::check_surface_kind(state, context, argument, context.prim.t);
            inferred_type
        });

        let arguments = arguments.collect_vec();

        let constructor_type = arguments.into_iter().rfold(data_reference, |result, argument| {
            state.storage.intern(Type::Function(argument, result))
        });

        let all_variables = {
            let from_kind = kind_variables.iter();
            let from_type = type_variables.iter();
            from_kind.chain(from_type).cloned()
        };

        let constructor_type = all_variables.rfold(constructor_type, |inner, variable| {
            state.storage.intern(Type::Forall(variable, inner))
        });

        if let Some(pending_type) = state.binding_group.terms.get(&item_id) {
            unification::unify(state, context, *pending_type, constructor_type);
        } else {
            state.binding_group.terms.insert(item_id, constructor_type);
        }
    }
}
