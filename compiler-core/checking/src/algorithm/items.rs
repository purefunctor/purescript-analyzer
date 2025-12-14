//! Implements syntax-driven elaboration for declarations.

use building_types::QueryResult;
use indexing::{TermItemId, TypeItemId};
use itertools::Itertools;
use lowering::{
    ClassIr, DataIr, NewtypeIr, SynonymIr, TermItemIr, TypeItemIr, TypeVariableBinding,
};
use smol_str::SmolStr;

use crate::ExternalQueries;
use crate::algorithm::state::{CheckContext, CheckState};
use crate::algorithm::{inspect, kind, substitute, term, unification};
use crate::core::{ForallBinder, Operator, Synonym, Type, TypeId, Variable, debruijn};
use crate::error::{ErrorKind, ErrorStep};

const MISSING_NAME: SmolStr = SmolStr::new_static("<MissingName>");

pub(crate) fn check_type_item<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TypeItemId,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    state.with_error_step(ErrorStep::TypeDeclaration(item_id), |state| {
        let Some(item) = context.lowered.info.get_type_item(item_id) else {
            return Ok(());
        };

        match item {
            TypeItemIr::DataGroup { signature, data, .. } => {
                let Some(DataIr { variables }) = data else {
                    return Ok(());
                };
                check_data_like(state, context, item_id, *signature, variables)
            }

            TypeItemIr::NewtypeGroup { signature, newtype, .. } => {
                let Some(NewtypeIr { variables }) = newtype else {
                    return Ok(());
                };
                check_data_like(state, context, item_id, *signature, variables)
            }

            TypeItemIr::SynonymGroup { signature, synonym } => {
                let Some(SynonymIr { variables, synonym: Some(synonym) }) = synonym else {
                    return Ok(());
                };
                check_synonym(state, context, item_id, *signature, variables, *synonym)
            }

            TypeItemIr::ClassGroup { signature, class } => {
                let Some(class) = class else {
                    return Ok(());
                };
                check_class(state, context, item_id, *signature, class)
            }

            TypeItemIr::Foreign { signature, .. } => {
                let Some(signature_id) = signature else {
                    return Ok(());
                };
                let (inferred_type, _) =
                    kind::check_surface_kind(state, context, *signature_id, context.prim.t)?;
                state.binding_group.types.insert(item_id, inferred_type);
                Ok(())
            }

            TypeItemIr::Operator { associativity, precedence, resolution } => {
                let Some(associativity) = *associativity else {
                    return Ok(());
                };
                let Some(precedence) = *precedence else {
                    return Ok(());
                };
                let Some((file_id, type_id)) = *resolution else {
                    return Ok(());
                };

                let operator = Operator { associativity, precedence, file_id, type_id };
                state.checked.operators.insert(item_id, operator);

                let id = kind::lookup_file_type(state, context, file_id, type_id)?;

                if !is_binary_operator_type(state, id) {
                    state.insert_error(ErrorKind::InvalidTypeOperator { id });
                }

                state.binding_group.types.insert(item_id, id);

                Ok(())
            }
        }
    })
}

enum SignatureOrigin {
    Syntax,
    Inferred,
}

struct SignatureLike {
    origin: SignatureOrigin,
    kind_variables: Vec<ForallBinder>,
    type_variables: Vec<ForallBinder>,
    result_kind: TypeId,
}

fn check_signature_like<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    signature: Option<lowering::TypeId>,
    variables: &[TypeVariableBinding],
    infer_result: impl FnOnce(&mut CheckState) -> TypeId,
) -> QueryResult<Option<SignatureLike>>
where
    Q: ExternalQueries,
{
    let signature = signature.map(|id| {
        let signature = inspect::inspect_signature(state, context, id)?;
        Ok((id, signature))
    });

    let signature = signature.transpose()?;

    let (origin, kind_variables, type_variables, result_kind) =
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

                return Ok(None);
            };

            let variables = variables.iter();
            let arguments = signature.arguments.iter();

            let kinds = variables
                .zip(arguments)
                .map(|(variable, &argument)| {
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
                    let kind = if let Some(kind_id) = variable.kind {
                        let (kind, _) = kind::infer_surface_kind(state, context, kind_id)?;
                        let valid = unification::subsumes(state, context, argument, kind)?;
                        if valid { kind } else { context.prim.unknown }
                    } else {
                        argument
                    };

                    let name = variable.name.clone().unwrap_or(MISSING_NAME);
                    Ok((variable.id, variable.visible, name, kind))
                })
                .collect::<QueryResult<Vec<_>>>()?;

            let kind_variables = signature.variables;
            let result_kind = signature.result;
            let type_variables = kinds.into_iter().map(|(id, visible, name, kind)| {
                let level = state.bind_forall(id, kind);
                ForallBinder { visible, name, level, kind }
            });

            (SignatureOrigin::Syntax, kind_variables, type_variables.collect_vec(), result_kind)
        } else {
            let kind_variables = vec![];
            let result_kind = infer_result(state);
            let type_variables = variables.iter().map(|variable| {
                let kind = if let Some(id) = variable.kind {
                    let (kind, _) = kind::check_surface_kind(state, context, id, context.prim.t)?;
                    kind
                } else {
                    state.fresh_unification_type(context)
                };

                let visible = variable.visible;
                let name = variable.name.clone().unwrap_or(MISSING_NAME);
                let level = state.bind_forall(variable.id, kind);
                Ok(ForallBinder { visible, name, level, kind })
            });

            let type_variables = type_variables.collect::<QueryResult<Vec<_>>>()?;

            (SignatureOrigin::Inferred, kind_variables, type_variables, result_kind)
        };

    Ok(Some(SignatureLike { origin, kind_variables, type_variables, result_kind }))
}

fn check_data_like<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TypeItemId,
    signature: Option<lowering::TypeId>,
    variables: &[TypeVariableBinding],
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let Some(SignatureLike { origin, kind_variables, type_variables, result_kind }) =
        check_signature_like(state, context, signature, variables, |_| context.prim.t)?
    else {
        return Ok(());
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

    match origin {
        SignatureOrigin::Syntax => {
            insert_checked_kind(state, item_id, &kind_variables, type_kind);
        }
        SignatureOrigin::Inferred => {
            unify_inferred_kind(state, context, item_id, type_kind)?;
        }
    }

    check_data_constructors(
        state,
        context,
        item_id,
        &kind_variables,
        &type_variables,
        data_reference,
    )?;

    if let Some(variable) = type_variables.first() {
        state.unbind(variable.level);
    }

    if let Some(variable) = kind_variables.first() {
        state.unbind(variable.level);
    }

    Ok(())
}

fn check_synonym<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TypeItemId,
    signature: Option<lowering::TypeId>,
    variables: &[TypeVariableBinding],
    synonym: lowering::TypeId,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let Some(SignatureLike { origin, kind_variables, type_variables, result_kind }) =
        check_signature_like(state, context, signature, variables, |state| {
            state.fresh_unification_type(context)
        })?
    else {
        return Ok(());
    };

    let (synonym_type, _) = kind::check_surface_kind(state, context, synonym, result_kind)?;

    let type_kind = type_variables.iter().rfold(result_kind, |result, binder| {
        state.storage.intern(Type::Function(binder.kind, result))
    });

    match origin {
        SignatureOrigin::Syntax => {
            insert_checked_kind(state, item_id, &kind_variables, type_kind);
        }
        SignatureOrigin::Inferred => {
            unify_inferred_kind(state, context, item_id, type_kind)?;
        }
    }

    if let Some(variable) = type_variables.first() {
        state.unbind(variable.level);
    }

    if let Some(variable) = kind_variables.first() {
        state.unbind(variable.level);
    }

    insert_type_synonym(state, item_id, kind_variables, type_variables, synonym_type);

    Ok(())
}

fn check_class<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TypeItemId,
    signature: Option<lowering::TypeId>,
    ClassIr { constraints, variables }: &ClassIr,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let Some(SignatureLike { origin, kind_variables, type_variables, result_kind }) =
        check_signature_like(state, context, signature, variables, |_| context.prim.constraint)?
    else {
        return Ok(());
    };

    let constraints = constraints.iter().map(|&constraint| {
        let (constraint_type, _) =
            kind::check_surface_kind(state, context, constraint, context.prim.constraint)?;
        Ok(constraint_type)
    });

    let _constraints = constraints.collect::<QueryResult<Vec<_>>>()?;

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

    match origin {
        SignatureOrigin::Syntax => {
            insert_checked_kind(state, item_id, &kind_variables, class_kind);
        }
        SignatureOrigin::Inferred => {
            unify_inferred_kind(state, context, item_id, class_kind)?;
        }
    }

    check_class_members(
        state,
        context,
        item_id,
        &kind_variables,
        &type_variables,
        class_reference,
    )?;

    if let Some(variable) = type_variables.first() {
        state.unbind(variable.level);
    }

    if let Some(variable) = kind_variables.first() {
        state.unbind(variable.level);
    }

    Ok(())
}

fn check_class_members<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TypeItemId,
    kind_variables: &[ForallBinder],
    type_variables: &[ForallBinder],
    class_reference: TypeId,
) -> QueryResult<()>
where
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
            kind::check_surface_kind(state, context, *signature_id, context.prim.t)?;

        let (member_foralls, member_inner) = collect_foralls(state, member_type);

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
            let _ = unification::unify(state, context, *pending_type, member_type)?;
        } else {
            state.binding_group.terms.insert(member_id, member_type);
        }
    }

    Ok(())
}

fn collect_foralls(state: &CheckState, mut id: TypeId) -> (Vec<ForallBinder>, TypeId) {
    let mut foralls = vec![];

    while let Type::Forall(ref binder, inner) = state.storage[id] {
        foralls.push(binder.clone());
        id = inner;
    }

    (foralls, id)
}

fn is_binary_operator_type(state: &CheckState, mut id: TypeId) -> bool {
    while let Type::Forall(_, inner_id) = state.storage[id] {
        id = inner_id;
    }

    let Type::Function(_, result_id) = state.storage[id] else {
        return false;
    };

    let Type::Function(_, result_id) = state.storage[result_id] else {
        return false;
    };

    !matches!(state.storage[result_id], Type::Function(_, _))
}

fn insert_checked_kind(
    state: &mut CheckState,
    item_id: TypeItemId,
    kind_variables: &[ForallBinder],
    item_kind: TypeId,
) {
    debug_assert!(
        state.binding_group.lookup_type(item_id).is_none(),
        "invariant violated: binding_group entry exists for checked type item"
    );

    let item_kind = kind_variables.iter().rfold(item_kind, |inner, binder| {
        let binder = binder.clone();
        state.storage.intern(Type::Forall(binder, inner))
    });

    state.binding_group.types.insert(item_id, item_kind);
}

fn unify_inferred_kind<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TypeItemId,
    item_kind: TypeId,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let pending_kind = state
        .binding_group
        .lookup_type(item_id)
        .expect("invariant violated: invalid binding_group in kind inference");

    let _ = unification::unify(state, context, pending_kind, item_kind)?;

    Ok(())
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
) -> QueryResult<()>
where
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
                    kind::check_surface_kind(state, context, argument, context.prim.t)?;
                Ok(inferred_type)
            })
        });

        let arguments = arguments.collect::<QueryResult<Vec<_>>>()?;

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
            let _ = unification::unify(state, context, *pending_type, constructor_type)?;
        } else {
            state.binding_group.terms.insert(item_id, constructor_type);
        }
    }

    Ok(())
}

pub(crate) fn check_term_item<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TermItemId,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    state.with_error_step(ErrorStep::TermDeclaration(item_id), |state| {
        let Some(item) = context.lowered.info.get_term_item(item_id) else {
            return Ok(());
        };
        match item {
            TermItemIr::ClassMember { .. } => Ok(()),

            TermItemIr::Constructor { .. } => Ok(()),

            TermItemIr::Derive { .. } => Ok(()),

            TermItemIr::Foreign { .. } => Ok(()),

            TermItemIr::Instance { .. } => Ok(()),

            TermItemIr::Operator { .. } => Ok(()),

            TermItemIr::ValueGroup { signature, equations } => {
                check_value_group_item(context, item_id, state, *signature, equations)
            }
        }
    })
}

fn check_value_group_item<Q>(
    context: &CheckContext<'_, Q>,
    item_id: TermItemId,
    state: &mut CheckState,
    signature: Option<lowering::TypeId>,
    equations: &[lowering::Equation],
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    if let Some(id) = signature {
        let signature = inspect::inspect_signature(state, context, id)?;
        term::check_value_group(state, context, item_id, (id, signature), equations)
    } else {
        term::infer_value_group(state, context, item_id, equations)
    }
}
