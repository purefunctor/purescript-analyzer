//! Implements syntax-driven elaboration for declarations.

use indexing::TypeItemId;
use itertools::Itertools;
use lowering::{
    ClassIr, DataIr, NewtypeIr, SynonymIr, TermItemIr, TypeItemIr, TypeVariableBinding,
};
use smol_str::SmolStr;

use crate::ExternalQueries;
use crate::algorithm::state::{CheckContext, CheckState};
use crate::algorithm::{convert, kind, substitute, unification};
use crate::core::{ForallBinder, Synonym, Type, TypeId, Variable, debruijn};
use crate::error::{ErrorKind, ErrorStep};

const MISSING_NAME: SmolStr = SmolStr::new_static("<MissingName>");

pub(crate) fn check_type_item<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TypeItemId,
) where
    Q: ExternalQueries,
{
    state.with_error_step(ErrorStep::TypeDeclaration(item_id), |state| {
        let Some(item) = context.lowered.info.get_type_item(item_id) else {
            return;
        };

        match item {
            TypeItemIr::DataGroup { signature, data, .. } => {
                let Some(DataIr { variables }) = data else { return };
                check_data_like(state, context, item_id, *signature, variables);
            }

            TypeItemIr::NewtypeGroup { signature, newtype, .. } => {
                let Some(NewtypeIr { variables }) = newtype else { return };
                check_data_like(state, context, item_id, *signature, variables);
            }

            TypeItemIr::SynonymGroup { signature, synonym } => {
                let Some(SynonymIr { variables, synonym: Some(synonym) }) = synonym else { return };
                check_synonym(state, context, item_id, *signature, variables, *synonym);
            }

            TypeItemIr::ClassGroup { signature, class } => {
                let Some(ClassIr { variables, .. }) = class else { return };
                check_class(state, context, item_id, *signature, variables);
            }

            TypeItemIr::Foreign { signature, .. } => {
                let Some(signature_id) = signature else { return };
                let (inferred_type, _) =
                    kind::check_surface_kind(state, context, *signature_id, context.prim.t);
                state.binding_group.types.insert(item_id, inferred_type);
            }

            TypeItemIr::Operator { .. } => (),
        }
    })
}

fn check_signature_like<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    signature: Option<lowering::TypeId>,
    variables: &[TypeVariableBinding],
    infer_result: impl FnOnce(&mut CheckState) -> TypeId,
) -> Option<(Vec<ForallBinder>, Vec<ForallBinder>, TypeId)>
where
    Q: ExternalQueries,
{
    let signature = signature.map(|id| (id, convert::inspect_signature(state, context, id)));

    let (kind_variables, type_variables, result_kind) =
        if let Some((signature_id, signature)) = signature {
            if variables.len() != signature.arguments.len() {
                state.insert_error(ErrorKind::TypeSignatureVariableMismatch {
                    id: signature_id,
                    expected: 0,
                    actual: 0,
                });

                if let Some(variable) = signature.variables.first() {
                    state.unbind(variable.level);
                }

                return None;
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
            let result_kind = infer_result(state);
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

    Some((kind_variables, type_variables, result_kind))
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
    let Some((kind_variables, type_variables, result_kind)) =
        check_signature_like(state, context, signature, variables, |_| context.prim.t)
    else {
        return;
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

fn check_synonym<Q: ExternalQueries>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TypeItemId,
    signature: Option<lowering::TypeId>,
    variables: &[TypeVariableBinding],
    synonym: lowering::TypeId,
) {
    let Some((kind_variables, type_variables, result_kind)) =
        check_signature_like(state, context, signature, variables, |state| {
            state.fresh_unification_type(context)
        })
    else {
        return;
    };

    let (synonym_type, _) = kind::check_surface_kind(state, context, synonym, result_kind);

    let type_kind = type_variables.iter().rfold(result_kind, |result, binder| {
        state.storage.intern(Type::Function(binder.kind, result))
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

    if let Some(variable) = type_variables.first() {
        state.unbind(variable.level);
    }

    if let Some(variable) = kind_variables.first() {
        state.unbind(variable.level);
    }

    insert_type_synonym(state, item_id, kind_variables, type_variables, synonym_type);
}

fn check_class<Q: ExternalQueries>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TypeItemId,
    signature: Option<lowering::TypeId>,
    variables: &[TypeVariableBinding],
) {
    let Some((kind_variables, type_variables, result_kind)) =
        check_signature_like(state, context, signature, variables, |_| context.prim.constraint)
    else {
        return;
    };

    let class_reference = {
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

    let class_kind = type_variables.iter().rfold(result_kind, |result, variable| {
        state.storage.intern(Type::Function(variable.kind, result))
    });

    if let Some(pending_kind) = state.binding_group.types.get(&item_id) {
        unification::unify(state, context, *pending_kind, class_kind);
    } else {
        let class_kind = kind_variables.iter().rfold(class_kind, |inner, binder| {
            let binder = binder.clone();
            state.storage.intern(Type::Forall(binder, inner))
        });
        state.binding_group.types.insert(item_id, class_kind);
    };

    check_class_members(state, context, item_id, &kind_variables, &type_variables, class_reference);

    if let Some(variable) = type_variables.first() {
        state.unbind(variable.level);
    }

    if let Some(variable) = kind_variables.first() {
        state.unbind(variable.level);
    }
}

fn check_class_members<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TypeItemId,
    kind_variables: &[ForallBinder],
    type_variables: &[ForallBinder],
    class_reference: TypeId,
) where
    Q: ExternalQueries,
{
    for member_id in context.indexed.pairs.class_members(item_id) {
        let Some(TermItemIr::ClassMember { signature }) =
            context.lowered.info.get_term_item(member_id)
        else {
            continue;
        };

        let Some(signature_id) = signature else { continue };

        let (member_type, _) =
            kind::check_surface_kind(state, context, *signature_id, context.prim.t);

        let (member_foralls, member_inner) = member_collect_foralls(state, member_type);

        let shift_amount = member_foralls.len() as u32;
        let shifted_class_reference =
            substitute::shift_indices(state, shift_amount, class_reference);

        let constrained_type =
            state.storage.intern(Type::Constrained(shifted_class_reference, member_inner));

        let all_variables = {
            let from_kind = kind_variables.iter().cloned();
            let from_type = type_variables.iter().cloned();
            let from_member = member_foralls.into_iter();
            from_kind.chain(from_type).chain(from_member)
        };

        let member_type = all_variables.rfold(constrained_type, |inner, variable| {
            state.storage.intern(Type::Forall(variable, inner))
        });

        if let Some(pending_type) = state.binding_group.terms.get(&member_id) {
            unification::unify(state, context, *pending_type, member_type);
        } else {
            state.binding_group.terms.insert(member_id, member_type);
        }
    }
}

fn member_collect_foralls(
    state: &CheckState,
    mut current_id: TypeId,
) -> (Vec<ForallBinder>, TypeId) {
    let mut binders = Vec::new();

    while let Type::Forall(ref binder, inner_id) = state.storage[current_id] {
        let binder = binder.clone();
        binders.push(binder);
        current_id = inner_id;
    }

    (binders, current_id)
}

fn insert_type_synonym(
    state: &mut CheckState,
    item_id: TypeItemId,
    kind_variables: Vec<ForallBinder>,
    type_variables: Vec<ForallBinder>,
    synonym_type: TypeId,
) {
    let synonym_type = type_variables.iter().rfold(synonym_type, |inner, binder| {
        let binder = binder.clone();
        state.storage.intern(Type::Forall(binder, inner))
    });

    let synonym_type = kind_variables.iter().rfold(synonym_type, |inner, binder| {
        let binder = binder.clone();
        state.storage.intern(Type::Forall(binder, inner))
    });

    let quantified_variables = debruijn::Size(0);

    let kind_variables = {
        let length = kind_variables.len();
        debruijn::Size(length as u32)
    };

    let type_variables = {
        let length = type_variables.len();
        debruijn::Size(length as u32)
    };

    let group = Synonym { quantified_variables, kind_variables, type_variables, synonym_type };
    state.binding_group.synonyms.insert(item_id, group);
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
            state.with_error_step(ErrorStep::ConstructorArgument(argument), |state| {
                let (inferred_type, _) =
                    kind::check_surface_kind(state, context, argument, context.prim.t);
                inferred_type
            })
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
