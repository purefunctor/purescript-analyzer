pub mod operator;
pub mod synonym;

use std::sync::Arc;

use building_types::QueryResult;
use files::FileId;
use indexing::TypeItemId;
use lowering::TypeVariableBindingId;
use smol_str::SmolStr;

use crate::ExternalQueries;
use crate::algorithm::safety::safe_loop;
use crate::algorithm::state::{CheckContext, CheckState};
use crate::algorithm::{substitute, transfer, unification};
use crate::core::{ForallBinder, RowField, RowType, Type, TypeId, Variable};
use crate::error::{ErrorKind, ErrorStep};

const MISSING_NAME: SmolStr = SmolStr::new_static("<MissingName>");

#[tracing::instrument(skip_all, name = "infer_surface_kind")]
pub fn infer_surface_kind<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    id: lowering::TypeId,
) -> QueryResult<(TypeId, TypeId)>
where
    Q: ExternalQueries,
{
    state.with_error_step(ErrorStep::InferringKind(id), |state| {
        let (t, k) = infer_surface_kind_core(state, context, id)?;
        crate::trace_fields!(state, context, { inferred_type = t, inferred_kind = k });
        Ok((t, k))
    })
}

fn infer_surface_kind_core<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    id: lowering::TypeId,
) -> QueryResult<(TypeId, TypeId)>
where
    Q: ExternalQueries,
{
    let unknown = (context.prim.unknown, context.prim.unknown);

    let Some(kind) = context.lowered.info.get_type_kind(id) else {
        return Ok(unknown);
    };

    match kind {
        lowering::TypeKind::ApplicationChain { function, arguments } => {
            let Some(function) = function else {
                return Ok(unknown);
            };

            if let Some(synonym) = synonym::parse_synonym_application(state, context, *function)? {
                return synonym::infer_synonym_application(state, context, id, synonym, arguments);
            }

            let (mut t, mut k) = infer_surface_kind(state, context, *function)?;

            for argument in arguments.iter() {
                (t, k) = infer_surface_app_kind(state, context, (t, k), *argument)?
            }

            Ok((t, k))
        }

        lowering::TypeKind::Arrow { argument, result } => {
            let argument = if let Some(argument) = argument {
                let (argument, _) = check_surface_kind(state, context, *argument, context.prim.t)?;
                Some(argument)
            } else {
                None
            };

            let result = if let Some(result) = result {
                let (result, _) = check_surface_kind(state, context, *result, context.prim.t)?;
                Some(result)
            } else {
                None
            };

            let argument = argument.unwrap_or(context.prim.unknown);
            let result = result.unwrap_or(context.prim.unknown);

            let t = state.storage.intern(Type::Function(argument, result));
            let k = context.prim.t;

            Ok((t, k))
        }

        lowering::TypeKind::Constrained { constraint, constrained } => {
            let constraint = if let Some(constraint) = constraint {
                let (constraint, _) =
                    check_surface_kind(state, context, *constraint, context.prim.constraint)?;
                Some(constraint)
            } else {
                None
            };

            let constrained = if let Some(constraint) = constrained {
                let (constrained, _) =
                    check_surface_kind(state, context, *constraint, context.prim.t)?;
                Some(constrained)
            } else {
                None
            };

            let constraint = constraint.unwrap_or(context.prim.unknown);
            let constrained = constrained.unwrap_or(context.prim.unknown);

            let t = state.storage.intern(Type::Constrained(constraint, constrained));
            let k = context.prim.t;

            Ok((t, k))
        }

        lowering::TypeKind::Constructor { resolution } => {
            let Some((file_id, type_id)) = *resolution else { return Ok(unknown) };

            if let Some((synonym, kind)) =
                synonym::lookup_file_synonym(state, context, file_id, type_id)?
            {
                let synonym_info = (file_id, type_id, synonym, kind);
                return synonym::infer_synonym_constructor(state, context, synonym_info, id);
            }

            let t = state.storage.intern(Type::Constructor(file_id, type_id));
            let k = lookup_file_type(state, context, file_id, type_id)?;

            Ok((t, k))
        }

        lowering::TypeKind::Forall { bindings, inner } => {
            let binders = bindings
                .iter()
                .map(|binding| check_type_variable_binding(state, context, binding))
                .collect::<QueryResult<Vec<_>>>()?;

            let inner = if let Some(i) = inner {
                let (inner, _) = check_surface_kind(state, context, *i, context.prim.t)?;
                Some(inner)
            } else {
                None
            };

            let inner = inner.unwrap_or(context.prim.unknown);

            let t = binders.iter().rfold(inner, |inner, binder| {
                let binder = binder.clone();
                state.storage.intern(Type::Forall(binder, inner))
            });

            let k = context.prim.t;

            if let Some(binder) = binders.first() {
                state.type_scope.unbind(binder.level);
            }

            Ok((t, k))
        }

        lowering::TypeKind::Hole => {
            let t = context.prim.unknown;
            let k = state.fresh_unification(context);
            Ok((t, k))
        }

        lowering::TypeKind::Integer { value } => {
            let Some(value) = value else { return Ok(unknown) };

            let t = state.storage.intern(Type::Integer(*value));
            let k = context.prim.int;

            Ok((t, k))
        }

        lowering::TypeKind::Kinded { type_, kind } => {
            let Some(type_) = type_ else { return Ok(unknown) };
            let Some(kind) = kind else { return Ok(unknown) };

            let (k, _) = infer_surface_kind(state, context, *kind)?;
            let (t, _) = check_surface_kind(state, context, *type_, k)?;

            Ok((t, k))
        }

        lowering::TypeKind::Operator { resolution } => {
            let Some((file_id, type_id)) = *resolution else { return Ok(unknown) };

            let t = state.storage.intern(Type::Operator(file_id, type_id));
            let k = lookup_file_type(state, context, file_id, type_id)?;

            Ok((t, k))
        }

        lowering::TypeKind::OperatorChain { .. } => {
            operator::infer_operator_chain_kind(state, context, id)
        }

        lowering::TypeKind::String { kind, value } => {
            let value = value.clone().unwrap_or(MISSING_NAME);

            let t = state.storage.intern(Type::String(*kind, value));
            let k = context.prim.symbol;

            Ok((t, k))
        }

        lowering::TypeKind::Variable { name, resolution } => match resolution {
            Some(lowering::TypeVariableResolution::Forall(forall)) => {
                Ok(infer_forall_variable(state, *forall))
            }

            Some(lowering::TypeVariableResolution::Implicit(implicit)) => {
                Ok(infer_implicit_variable(state, context, implicit))
            }

            None => {
                let name = name.clone().unwrap_or(MISSING_NAME);

                let t = state.storage.intern(Type::Variable(Variable::Free(name)));
                let k = state.fresh_unification(context);

                Ok((t, k))
            }
        },

        lowering::TypeKind::Wildcard => {
            let k = state.fresh_unification_type(context);
            let t = state.fresh_unification_kinded(k);
            Ok((t, k))
        }

        lowering::TypeKind::Record { items, tail } => {
            let (row_t, row_k) = infer_row_kind(state, context, items, tail)?;
            let _ = unification::subtype(state, context, row_k, context.prim.row_type)?;

            let t = state.storage.intern(Type::Application(context.prim.record, row_t));
            let k = context.prim.t;

            Ok((t, k))
        }

        lowering::TypeKind::Row { items, tail } => infer_row_kind(state, context, items, tail),

        lowering::TypeKind::Parenthesized { parenthesized } => {
            let Some(parenthesized) = parenthesized else { return Ok(unknown) };
            infer_surface_kind(state, context, *parenthesized)
        }
    }
}

fn infer_forall_variable(
    state: &mut CheckState,
    forall: TypeVariableBindingId,
) -> (TypeId, TypeId) {
    let level =
        state.type_scope.lookup_forall(forall).expect("invariant violated: TypeScope::bind_forall");
    let k = state
        .type_scope
        .lookup_forall_kind(forall)
        .expect("invariant violated: TypeScope::bind_forall");

    let variable = Variable::Bound(level, k);
    let t = state.storage.intern(Type::Variable(variable));

    (t, k)
}

fn infer_implicit_variable<Q: ExternalQueries>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    implicit: &lowering::ImplicitTypeVariable,
) -> (TypeId, TypeId) {
    let (t, k) = if implicit.binding {
        let kind = state.fresh_unification(context);

        let level = state.type_scope.bind_implicit(implicit.node, implicit.id, kind);
        let variable = Variable::Bound(level, kind);

        (state.storage.intern(Type::Variable(variable)), kind)
    } else {
        let level = state
            .type_scope
            .lookup_implicit(implicit.node, implicit.id)
            .expect("invariant violated: TypeScope::bind_implicit");
        let kind = state
            .type_scope
            .lookup_implicit_kind(implicit.node, implicit.id)
            .expect("invariant violated: TypeScope::bind_implicit");

        let variable = Variable::Bound(level, kind);
        (state.storage.intern(Type::Variable(variable)), kind)
    };

    (t, k)
}

fn infer_row_kind<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    items: &Arc<[lowering::TypeRowItem]>,
    tail: &Option<lowering::TypeId>,
) -> QueryResult<(TypeId, TypeId)>
where
    Q: ExternalQueries,
{
    let mut field_kind = state.fresh_unification_type(context);

    let fields: QueryResult<Vec<_>> = items
        .iter()
        .map(|item| {
            let field_type = if let Some(t) = item.type_ {
                let (t, k) = infer_surface_kind(state, context, t)?;

                let _ = unification::unify(state, context, field_kind, k)?;
                field_kind = state.normalize_type(field_kind);

                Some(t)
            } else {
                None
            };

            let label = item.name.clone().unwrap_or(MISSING_NAME);
            let id = field_type.unwrap_or(context.prim.unknown);

            Ok(RowField { label, id })
        })
        .collect();

    let fields = fields?;

    let tail = if let Some(tail) = tail {
        let (t, k) = infer_surface_kind(state, context, *tail)?;

        let expected_kind = state.storage.intern(Type::Application(context.prim.row, field_kind));
        let _ = unification::subtype(state, context, k, expected_kind)?;

        Some(t)
    } else {
        None
    };

    let row = RowType::from_unsorted(fields, tail);

    let t = state.storage.intern(Type::Row(row));
    let k = state.storage.intern(Type::Application(context.prim.row, field_kind));

    Ok((t, k))
}

pub fn infer_surface_app_kind<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    (function_t, function_k): (TypeId, TypeId),
    argument: lowering::TypeId,
) -> QueryResult<(TypeId, TypeId)>
where
    Q: ExternalQueries,
{
    let function_k = state.normalize_type(function_k);
    match state.storage[function_k] {
        Type::Function(argument_k, result_k) => {
            let (argument_t, _) = check_surface_kind(state, context, argument, argument_k)?;

            let t = state.storage.intern(Type::Application(function_t, argument_t));
            let k = state.normalize_type(result_k);

            Ok((t, k))
        }

        Type::Unification(unification_id) => {
            let argument_u = state.fresh_unification_type(context);
            let result_u = state.fresh_unification_type(context);

            let function_u = state.storage.intern(Type::Function(argument_u, result_u));
            let _ = unification::solve(state, context, unification_id, function_u);

            let (argument_t, _) = check_surface_kind(state, context, argument, argument_u)?;

            let t = state.storage.intern(Type::Application(function_t, argument_t));
            let k = state.normalize_type(result_u);

            Ok((t, k))
        }

        Type::Forall(ref binder, function_k) => {
            let binder_level = binder.level;
            let binder_kind = binder.kind;

            let k = state.normalize_type(binder_kind);
            let t = state.fresh_unification_kinded(k);

            let function_t = state.storage.intern(Type::KindApplication(function_t, t));
            let function_k = substitute::SubstituteBound::on(state, binder_level, t, function_k);

            infer_surface_app_kind(state, context, (function_t, function_k), argument)
        }

        _ => {
            // Even if the function type cannot be applied, the argument must
            // still be inferred. For invalid applications on instance heads,
            // this ensures that implicit variables are bound.
            let (argument_t, _) = infer_surface_kind(state, context, argument)?;

            let function_type = state.render_local_type(context, function_t);
            let function_kind = state.render_local_type(context, function_k);
            let argument_type = state.render_local_type(context, argument_t);
            state.insert_error(ErrorKind::InvalidTypeApplication {
                function_type,
                function_kind,
                argument_type,
            });

            let t = state.storage.intern(Type::Application(function_t, argument_t));
            Ok((t, context.prim.unknown))
        }
    }
}

#[tracing::instrument(skip_all, name = "elaborate_kind")]
pub fn elaborate_kind<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    id: TypeId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    crate::trace_fields!(state, context, { type_ = id });

    let unknown = context.prim.unknown;
    let id = state.normalize_type(id);

    let type_id = match state.storage[id] {
        Type::Application(function, _) => {
            let function_kind = elaborate_kind(state, context, function)?;
            let function_kind = state.normalize_type(function_kind);
            match state.storage[function_kind] {
                Type::Function(_, result) => result,

                Type::Unification(unification_id) => {
                    let domain = state.unification.get(unification_id).domain;

                    let argument_u = state.fresh_unification_kinded_at(domain, context.prim.t);
                    let result_u = state.fresh_unification_kinded_at(domain, context.prim.t);
                    let function = state.storage.intern(Type::Function(argument_u, result_u));

                    let _ = unification::solve(state, context, unification_id, function);
                    result_u
                }

                _ => unknown,
            }
        }

        Type::Constrained(_, _) => context.prim.t,

        Type::Constructor(file_id, type_id) => lookup_file_type(state, context, file_id, type_id)?,

        Type::Forall(_, _) => context.prim.t,

        Type::Function(_, _) => context.prim.t,

        Type::Integer(_) => context.prim.int,

        Type::KindApplication(function, argument) => {
            let function_kind = elaborate_kind(state, context, function)?;
            let function_kind = state.normalize_type(function_kind);
            match state.storage[function_kind] {
                Type::Forall(ref binder, inner) => {
                    let binder_level = binder.level;
                    let argument = state.normalize_type(argument);
                    substitute::SubstituteBound::on(state, binder_level, argument, inner)
                }
                _ => unknown,
            }
        }

        Type::Kinded(_, kind) => kind,

        Type::Operator(file_id, type_id) => lookup_file_type(state, context, file_id, type_id)?,

        Type::OperatorApplication(file_id, type_id, _, _) => {
            operator::elaborate_operator_application_kind(state, context, file_id, type_id)?
        }

        Type::Row(RowType { ref fields, tail }) => {
            let fields = Arc::clone(fields);

            let field_kind = state.fresh_unification_type(context);
            let tail_kind = state.storage.intern(Type::Application(context.prim.row, field_kind));

            for field in fields.iter() {
                let k = elaborate_kind(state, context, field.id)?;
                let _ = unification::unify(state, context, field_kind, k)?;
            }

            if let Some(tail) = tail {
                let k = elaborate_kind(state, context, tail)?;
                let _ = unification::unify(state, context, tail_kind, k)?;
            };

            state.storage.intern(Type::Application(context.prim.row, field_kind))
        }

        Type::String(_, _) => context.prim.symbol,

        Type::SynonymApplication(_, file_id, type_id, ref arguments) => {
            let arguments = Arc::clone(arguments);

            let mut synonym_kind = lookup_file_type(state, context, file_id, type_id)?;

            for _ in arguments.iter() {
                synonym_kind = state.normalize_type(synonym_kind);
                if let Type::Function(_, result_kind) = state.storage[synonym_kind] {
                    synonym_kind = result_kind
                } else {
                    return Ok(unknown);
                }
            }

            synonym_kind
        }

        Type::Unification(unification_id) => state.unification.get(unification_id).kind,

        Type::Variable(ref variable) => match variable {
            Variable::Bound(_, kind) => *kind,
            Variable::Skolem(_, kind) => *kind,
            Variable::Free(_) => unknown,
        },

        Type::Unknown => unknown,
    };

    Ok(type_id)
}

/// Instantiates kind-level foralls using [`Type::KindApplication`].
///
/// If the inferred kind is polymorphic, and the inferred kind is monomorphic,
/// this function adds the necessary kind applications to the inferred type.
/// For example, when checking `RowList.Nil` against `RowList Type`:
///
/// ```text
/// Nil :: forall k. RowList k.
///
/// check(Nil, RowList Type)
///   infer(Nil) -> forall k. RowList k
///   instantiate(Nil) -> (Nil @?t, RowList ?t)
///
/// subtype(RowList ?t, RowList Type)
///   solve(?t, Type)
///
/// t := Nil @Type
/// k := RowList Type
/// ```
fn instantiate_kind_applications(
    state: &mut CheckState,
    mut t: TypeId,
    mut k: TypeId,
    expected_kind: TypeId,
) -> (TypeId, TypeId) {
    let expected_kind = state.normalize_type(expected_kind);

    if matches!(state.storage[expected_kind], Type::Forall(_, _)) {
        return (t, k);
    }

    safe_loop! {
        k = state.normalize_type(k);

        let Type::Forall(ref binder, inner_kind) = state.storage[k] else {
            break;
        };

        let binder_level = binder.level;
        let binder_kind = state.normalize_type(binder.kind);

        let argument_type = state.fresh_unification_kinded(binder_kind);
        t = state.storage.intern(Type::KindApplication(t, argument_type));
        k = substitute::SubstituteBound::on(state, binder_level, argument_type, inner_kind);
    }

    (t, k)
}

#[tracing::instrument(skip_all, name = "check_surface_kind")]
pub fn check_surface_kind<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    id: lowering::TypeId,
    expected_kind: TypeId,
) -> QueryResult<(TypeId, TypeId)>
where
    Q: ExternalQueries,
{
    crate::trace_fields!(state, context, { expected_kind = expected_kind });

    state.with_error_step(ErrorStep::CheckingKind(id), |state| {
        let (inferred_type, inferred_kind) = infer_surface_kind_core(state, context, id)?;

        let (inferred_type, inferred_kind) =
            instantiate_kind_applications(state, inferred_type, inferred_kind, expected_kind);

        let _ = unification::subtype(state, context, inferred_kind, expected_kind)?;

        crate::trace_fields!(state, context, {
            inferred_type = inferred_type,
            inferred_kind = inferred_kind,
            expected_kind = expected_kind
        });

        Ok((inferred_type, inferred_kind))
    })
}

pub fn check_type_variable_binding<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    binding: &lowering::TypeVariableBinding,
) -> QueryResult<ForallBinder>
where
    Q: ExternalQueries,
{
    let visible = binding.visible;
    let name = binding.name.clone().unwrap_or(MISSING_NAME);

    let kind = if let Some(id) = binding.kind {
        let (kind, _) = check_surface_kind(state, context, id, context.prim.t)?;
        kind
    } else {
        state.fresh_unification_type(context)
    };

    let level = state.type_scope.bind_forall(binding.id, kind);
    Ok(ForallBinder { visible, name, level, kind })
}

pub(crate) fn lookup_file_type<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    file_id: FileId,
    type_id: TypeItemId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let type_id = if file_id == context.id {
        if let Some(k) = state.binding_group.lookup_type(type_id) {
            k
        } else if let Some(&k) = state.checked.types.get(&type_id) {
            transfer::localize(state, context, k)
        } else {
            context.prim.unknown
        }
    } else {
        let checked = context.queries.checked(file_id)?;
        if let Some(id) = checked.types.get(&type_id) {
            transfer::localize(state, context, *id)
        } else {
            context.prim.unknown
        }
    };
    Ok(type_id)
}
