//! Implements syntax-driven checking rules for types.

use std::sync::Arc;

use building_types::QueryResult;
use smol_str::SmolStr;

use crate::context::CheckContext;
use crate::core::substitute::SubstituteName;
use crate::core::{ForallBinder, RowField, RowType, Type, TypeId, normalise, toolkit, unification};
use crate::error::{ErrorCrumb, ErrorKind};
use crate::source::{operator, synonym};
use crate::state::CheckState;
use crate::{ExternalQueries, safe_loop};

const MISSING_NAME: SmolStr = SmolStr::new_static("<MissingName>");

/// Checks the kind of a syntax type against a core type.
///
/// This function returns the core type and kind.
pub fn check_kind<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    source_type: lowering::TypeId,
    expected_kind: TypeId,
) -> QueryResult<(TypeId, TypeId)>
where
    Q: ExternalQueries,
{
    state.with_error_crumb(ErrorCrumb::CheckingKind(source_type), |state| {
        check_kind_core(state, context, source_type, expected_kind)
    })
}

fn check_kind_core<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    source_type: lowering::TypeId,
    expected_kind: TypeId,
) -> QueryResult<(TypeId, TypeId)>
where
    Q: ExternalQueries,
{
    let (inferred_type, inferred_kind) = infer_kind(state, context, source_type)?;
    let (inferred_type, inferred_kind) =
        instantiate_kind_applications(state, context, inferred_type, inferred_kind, expected_kind)?;

    unification::subtype(state, context, inferred_kind, expected_kind)?;
    Ok((inferred_type, inferred_kind))
}

pub fn infer_kind<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    id: lowering::TypeId,
) -> QueryResult<(TypeId, TypeId)>
where
    Q: ExternalQueries,
{
    state.with_error_crumb(ErrorCrumb::InferringKind(id), |state| {
        infer_kind_core(state, context, id)
    })
}

fn infer_kind_core<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    id: lowering::TypeId,
) -> QueryResult<(TypeId, TypeId)>
where
    Q: ExternalQueries,
{
    let unknown = |message: &str| {
        let u = context.unknown(message);
        (u, u)
    };

    let Some(kind) = context.lowered.info.get_type_kind(id) else {
        return Ok(unknown("missing syntax"));
    };

    match kind {
        lowering::TypeKind::ApplicationChain { function, arguments } => {
            let Some(function) = function else {
                return Ok(unknown("missing application function"));
            };

            if let Some(synonym) = synonym::parse_synonym_application(state, context, *function)? {
                return synonym::infer_synonym_application(state, context, id, synonym, arguments);
            }

            let (mut t, mut k) = infer_kind(state, context, *function)?;

            for argument in arguments.iter() {
                (t, k) = infer_application_kind(state, context, (t, k), *argument)?;
            }

            Ok((t, k))
        }

        lowering::TypeKind::Arrow { argument, result } => {
            let argument = if let Some(argument) = argument {
                let (argument, _) = check_kind(state, context, *argument, context.prim.t)?;
                argument
            } else {
                context.unknown("missing function argument")
            };

            let result = if let Some(result) = result {
                let (result, _) = check_kind(state, context, *result, context.prim.t)?;
                result
            } else {
                context.unknown("missing function result")
            };

            let t = context.intern_function(argument, result);
            let k = context.prim.t;

            Ok((t, k))
        }

        lowering::TypeKind::Constrained { constraint, constrained } => {
            let constraint = if let Some(constraint) = constraint {
                let (constraint, _) =
                    check_kind(state, context, *constraint, context.prim.constraint)?;
                constraint
            } else {
                context.unknown("missing constraint")
            };

            let constrained = if let Some(constrained) = constrained {
                let (constrained, _) = infer_kind(state, context, *constrained)?;
                constrained
            } else {
                context.unknown("missing constrained")
            };

            let t = context.intern_constrained(constraint, constrained);
            let k = context.prim.t;

            Ok((t, k))
        }

        lowering::TypeKind::Constructor { resolution } => {
            let Some((file_id, type_id)) = *resolution else {
                return Ok(unknown("missing constructor"));
            };

            if let Some(synonym) = toolkit::lookup_file_synonym(state, context, file_id, type_id)? {
                let synonym = (file_id, type_id, synonym.kind, synonym.parameters.len());
                return synonym::infer_synonym_constructor(state, context, synonym, id);
            }
            let t = context.queries.intern_type(Type::Constructor(file_id, type_id));
            let k = toolkit::lookup_file_type(state, context, file_id, type_id)?;

            Ok((t, k))
        }

        lowering::TypeKind::Forall { bindings, inner } => {
            let binders = bindings
                .iter()
                .map(|binding| check_type_variable_binding(state, context, binding))
                .collect::<QueryResult<Vec<_>>>()?;

            let inner = if let Some(inner) = inner {
                let (inner, _) = infer_kind(state, context, *inner)?;
                inner
            } else {
                context.unknown("missing forall inner")
            };

            let t = binders.iter().rfold(inner, |inner, binder| {
                let binder_id = context.intern_forall_binder(*binder);
                context.intern_forall(binder_id, inner)
            });

            let k = context.prim.t;

            Ok((t, k))
        }

        lowering::TypeKind::Hole => {
            let k = state.fresh_unification(context.queries, context.prim.t);
            let t = state.fresh_unification(context.queries, k);
            Ok((t, k))
        }

        lowering::TypeKind::Integer { value } => {
            let t = if let Some(value) = value {
                context.queries.intern_type(Type::Integer(*value))
            } else {
                context.unknown("missing integer value")
            };
            Ok((t, context.prim.int))
        }

        lowering::TypeKind::Kinded { type_, kind } => {
            let k = if let Some(kind) = kind {
                let (k, _) = infer_kind(state, context, *kind)?;
                k
            } else {
                context.unknown("missing kinded kind")
            };
            let t = if let Some(type_) = type_ {
                let (t, _) = check_kind(state, context, *type_, k)?;
                t
            } else {
                context.unknown("missing kinded type")
            };
            Ok((t, k))
        }

        lowering::TypeKind::Operator { resolution } => {
            let Some((file_id, type_id)) = *resolution else {
                return Ok(unknown("missing operator"));
            };

            let t = context.queries.intern_type(Type::OperatorConstructor(file_id, type_id));
            let k = toolkit::lookup_file_type(state, context, file_id, type_id)?;

            Ok((t, k))
        }

        lowering::TypeKind::OperatorChain { .. } => {
            operator::infer_operator_chain(state, context, id)
        }

        lowering::TypeKind::String { kind, value } => {
            let value = value.clone().unwrap_or(MISSING_NAME);
            let id = context.queries.intern_smol_str(value);

            let t = context.queries.intern_type(Type::String(*kind, id));
            let k = context.prim.symbol;

            Ok((t, k))
        }

        lowering::TypeKind::Variable { name, resolution } => match resolution {
            Some(lowering::TypeVariableResolution::Forall(forall)) => {
                let (n, k) = state
                    .bindings
                    .lookup_forall(*forall)
                    .expect("invariant violated: KindScope::bind_forall");

                let t = context.intern_rigid(n, state.depth, k);

                Ok((t, k))
            }

            Some(lowering::TypeVariableResolution::Implicit(implicit)) => {
                if implicit.binding {
                    let n = state.names.fresh();
                    let k = state.fresh_unification(context.queries, context.prim.t);

                    state.bindings.bind_implicit(implicit.node, implicit.id, n, k);
                    let t = context.intern_rigid(n, state.depth, k);

                    Ok((t, k))
                } else {
                    let (n, k) = state
                        .bindings
                        .lookup_implicit(implicit.node, implicit.id)
                        .expect("invariant violated: KindScope::bind_implicit");

                    let t = context.intern_rigid(n, state.depth, k);

                    Ok((t, k))
                }
            }

            None => {
                let name = name.clone().unwrap_or(MISSING_NAME);
                let id = context.queries.intern_smol_str(name);

                let t = context.queries.intern_type(Type::Free(id));
                let k = state.fresh_unification(context.queries, context.prim.t);

                Ok((t, k))
            }
        },

        lowering::TypeKind::Wildcard => {
            let k = state.fresh_unification(context.queries, context.prim.t);
            let t = state.fresh_unification(context.queries, k);
            Ok((t, k))
        }

        lowering::TypeKind::Record { items, tail } => {
            let (row_type, row_kind) = infer_row_kind(state, context, items, tail)?;
            unification::subtype(state, context, row_kind, context.prim.row_type)?;

            let t = context.intern_application(context.prim.record, row_type);
            let k = context.prim.t;

            Ok((t, k))
        }

        lowering::TypeKind::Row { items, tail } => infer_row_kind(state, context, items, tail),

        lowering::TypeKind::Parenthesized { parenthesized } => {
            if let Some(parenthesized) = parenthesized {
                infer_kind(state, context, *parenthesized)
            } else {
                Ok(unknown("missing parenthesized"))
            }
        }
    }
}

/// Instantiates kind-level foralls using [`Type::KindApplication`].
///
/// If the inferred kind is polymorphic and the expected kind is monomorphic,
/// this function adds the necessary kind applications to the inferred type.
fn instantiate_kind_applications<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    mut t: TypeId,
    mut k: TypeId,
    expected_kind: TypeId,
) -> QueryResult<(TypeId, TypeId)>
where
    Q: ExternalQueries,
{
    let expected_kind = normalise::normalise(state, context, expected_kind)?;

    if matches!(context.lookup_type(expected_kind), Type::Forall(_, _)) {
        return Ok((t, k));
    }

    safe_loop! {
        k = normalise::normalise(state, context, k)?;

        let Type::Forall(binder_id, inner_kind) = context.lookup_type(k) else {
            break;
        };

        let binder = context.lookup_forall_binder(binder_id);
        let binder_kind = normalise::normalise(state, context, binder.kind)?;

        let argument_type = state.fresh_unification(context.queries, binder_kind);
        t = context.intern_kind_application(t, argument_type);
        k = SubstituteName::one(state, context, binder.name, argument_type, inner_kind)?;
    }

    Ok((t, k))
}

fn check_type_variable_binding<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    binding: &lowering::TypeVariableBinding,
) -> QueryResult<ForallBinder>
where
    Q: ExternalQueries,
{
    let kind = if let Some(kind_id) = binding.kind {
        let (kind, _) = check_kind(state, context, kind_id, context.prim.t)?;
        kind
    } else {
        state.fresh_unification(context.queries, context.prim.t)
    };

    let visible = binding.visible;
    let name = state.names.fresh();

    let text = if let Some(name) = &binding.name { SmolStr::clone(name) } else { name.as_text() };
    let text = context.queries.intern_smol_str(text);

    state.bindings.bind_forall(binding.id, name, kind);
    Ok(ForallBinder { visible, name, text, kind })
}

pub fn infer_application_kind<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    (function_type, function_kind): (TypeId, TypeId),
    argument: lowering::TypeId,
) -> QueryResult<(TypeId, TypeId)>
where
    Q: ExternalQueries,
{
    let function_kind = normalise::normalise(state, context, function_kind)?;

    match context.lookup_type(function_kind) {
        Type::Function(argument_kind, result_kind) => {
            let (argument_type, _) = check_kind(state, context, argument, argument_kind)?;

            let t = context.intern_application(function_type, argument_type);
            let k = normalise::normalise(state, context, result_kind)?;

            Ok((t, k))
        }

        Type::Unification(unification_id) => {
            let argument_u = state.fresh_unification(context.queries, context.prim.t);
            let result_u = state.fresh_unification(context.queries, context.prim.t);

            let function_u = context.intern_function(argument_u, result_u);
            unification::solve(state, context, function_kind, unification_id, function_u)?;

            let (argument_type, _) = check_kind(state, context, argument, argument_u)?;

            let t = context.intern_application(function_type, argument_type);
            let k = normalise::normalise(state, context, result_u)?;

            Ok((t, k))
        }

        Type::Forall(binder_id, inner_kind) => {
            let binder = context.lookup_forall_binder(binder_id);
            let binder_kind = normalise::normalise(state, context, binder.kind)?;

            let kind_argument = state.fresh_unification(context.queries, binder_kind);
            let function_type = context.intern_kind_application(function_type, kind_argument);
            let function_kind =
                SubstituteName::one(state, context, binder.name, kind_argument, inner_kind)?;

            infer_application_kind(state, context, (function_type, function_kind), argument)
        }

        _ => {
            // Even if the function type cannot be applied, the argument must
            // still be inferred. For invalid applications on instance heads,
            // this ensures that implicit variables are bound.
            let (argument_type, _) = infer_kind(state, context, argument)?;

            let t = context.intern_application(function_type, argument_type);
            let k = context.unknown("cannot apply function type");

            let function_type = state.pretty_id(context, function_type)?;
            let function_kind = state.pretty_id(context, function_kind)?;
            let argument_type = state.pretty_id(context, argument_type)?;

            state.insert_error(ErrorKind::InvalidTypeApplication {
                function_type,
                function_kind,
                argument_type,
            });

            Ok((t, k))
        }
    }
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
    let field_kind = state.fresh_unification(context.queries, context.prim.t);
    let row_kind = context.intern_application(context.prim.row, field_kind);

    let fields = items.iter().map(|item| {
        let label = item.name.clone().unwrap_or(MISSING_NAME);
        let id = if let Some(t) = item.type_ {
            let (t, k) = infer_kind(state, context, t)?;
            unification::unify(state, context, field_kind, k)?;
            t
        } else {
            context.unknown("missing field type")
        };

        Ok(RowField { label, id })
    });

    let fields = fields.collect::<QueryResult<Vec<_>>>()?;

    let tail = if let Some(tail) = tail {
        let (tail_type, tail_kind) = infer_kind(state, context, *tail)?;
        unification::subtype(state, context, tail_kind, row_kind)?;
        Some(tail_type)
    } else {
        None
    };

    let row = RowType::new(fields, tail);
    let row_id = context.intern_row_type(row);
    let row_type = context.intern_row(row_id);

    Ok((row_type, row_kind))
}

pub fn elaborate_kind<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    id: TypeId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let unknown = context.unknown("invalid kind");
    let id = normalise::normalise(state, context, id)?;

    let kind = match context.lookup_type(id) {
        Type::Application(function, _) => {
            let function_kind = elaborate_kind(state, context, function)?;
            let function_kind = normalise::normalise(state, context, function_kind)?;

            match context.lookup_type(function_kind) {
                Type::Function(_, result_kind) => result_kind,

                Type::Unification(unification_id) => {
                    let depth = state.unifications.get(unification_id).depth;

                    let argument_u = state.unifications.fresh(depth, context.prim.t);
                    let argument_u = context.queries.intern_type(Type::Unification(argument_u));

                    let result_u = state.unifications.fresh(depth, context.prim.t);
                    let result_u = context.queries.intern_type(Type::Unification(result_u));

                    let function_u = context.intern_function(argument_u, result_u);
                    unification::solve(state, context, function_kind, unification_id, function_u)?;

                    result_u
                }

                _ => unknown,
            }
        }

        Type::KindApplication(function, argument) => {
            let function_kind = elaborate_kind(state, context, function)?;
            let function_kind = normalise::normalise(state, context, function_kind)?;

            match context.lookup_type(function_kind) {
                Type::Forall(binder_id, inner_kind) => {
                    let binder = context.lookup_forall_binder(binder_id);
                    let argument = normalise::normalise(state, context, argument)?;
                    SubstituteName::one(state, context, binder.name, argument, inner_kind)?
                }
                _ => unknown,
            }
        }

        Type::Constrained(_, _) => context.prim.t,
        Type::Forall(_, _) => context.prim.t,
        Type::Function(_, _) => context.prim.t,

        Type::Constructor(file_id, type_id) => {
            toolkit::lookup_file_type(state, context, file_id, type_id)?
        }

        Type::OperatorConstructor(file_id, type_id) => {
            toolkit::lookup_file_type(state, context, file_id, type_id)?
        }

        Type::OperatorApplication(file_id, type_id, _, _) => {
            operator::elaborate_operator_application_kind(state, context, file_id, type_id)?
        }

        Type::Integer(_) => context.prim.int,
        Type::String(_, _) => context.prim.symbol,
        Type::Kinded(_, kind) => kind,

        Type::Row(row_id) => {
            let row = context.lookup_row_type(row_id);
            let fields = Arc::clone(&row.fields);

            let field_kind = state.fresh_unification(context.queries, context.prim.t);
            let tail_kind = context.intern_application(context.prim.row, field_kind);

            for field in fields.iter() {
                let kind = elaborate_kind(state, context, field.id)?;
                unification::unify(state, context, field_kind, kind)?;
            }

            if let Some(tail) = row.tail {
                let kind = elaborate_kind(state, context, tail)?;
                unification::unify(state, context, tail_kind, kind)?;
            }

            context.intern_application(context.prim.row, field_kind)
        }

        Type::SynonymApplication(synonym_id) => {
            let synonym = context.lookup_synonym(synonym_id);
            let (file_id, type_id) = synonym.reference;
            let arguments = Arc::clone(&synonym.arguments);

            let mut synonym_kind = toolkit::lookup_file_type(state, context, file_id, type_id)?;

            for _ in arguments.iter() {
                synonym_kind = normalise::normalise(state, context, synonym_kind)?;
                match context.lookup_type(synonym_kind) {
                    Type::Function(_, result_kind) => synonym_kind = result_kind,
                    _ => return Ok(unknown),
                }
            }

            synonym_kind
        }

        Type::Unification(unification_id) => state.unifications.get(unification_id).kind,
        Type::Rigid(_, _, kind) => kind,
        Type::Free(_) => unknown,
        Type::Unknown(_) => unknown,
    };

    Ok(kind)
}
