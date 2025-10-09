use crate::{
    check::{CheckContext, CheckState, convert, substitute, unification},
    core::{ForallBinder, Type, TypeId, storage::TypeStorage},
};

pub fn infer_surface_kind<S: TypeStorage>(
    state: &mut CheckState<S>,
    context: &CheckContext,
    id: lowering::TypeId,
) -> (TypeId, TypeId) {
    let default = (context.prim.unknown, context.prim.unknown);

    let Some(kind) = context.lowered.info.get_type_kind(id) else {
        return default;
    };

    match kind {
        lowering::TypeKind::ApplicationChain { function, arguments } => {
            let Some(function) = function else { return default };

            let (mut t, mut k) = infer_surface_kind(state, context, *function);

            for argument in arguments.iter() {
                (t, k) = infer_surface_app_kind(state, context, (t, k), *argument)
            }

            (t, k)
        }

        lowering::TypeKind::Arrow { .. } => {
            let t = convert::type_to_core(state, context, id);
            let k = context.prim.t;
            (t, k)
        }

        lowering::TypeKind::Constrained { .. } => default,

        lowering::TypeKind::Constructor { resolution } => {
            let Some((_, type_id)) = *resolution else { return default };
            let t = convert::type_to_core(state, context, id);
            let k = lookup_prim_type(state, context, type_id);
            (t, k)
        }

        lowering::TypeKind::Forall { .. } => {
            let t = convert::type_to_core(state, context, id);
            let k = context.prim.t;
            (t, k)
        }

        lowering::TypeKind::Hole => {
            let t = convert::type_to_core(state, context, id);
            let k = state.fresh_unification(context);
            (t, k)
        }

        lowering::TypeKind::Integer => {
            let t = convert::type_to_core(state, context, id);
            let k = context.prim.t;
            (t, k)
        }

        lowering::TypeKind::Kinded { type_, kind } => {
            let Some(type_) = type_ else { return default };
            let Some(kind) = kind else { return default };

            let t = convert::type_to_core(state, context, *type_);
            let k = convert::type_to_core(state, context, *kind);

            (t, k)
        }

        lowering::TypeKind::Operator { .. } => default,

        lowering::TypeKind::OperatorChain { .. } => default,

        lowering::TypeKind::String => {
            let t = convert::type_to_core(state, context, id);
            let k = context.prim.t;
            (t, k)
        }

        lowering::TypeKind::Variable { resolution, .. } => {
            let t = convert::type_to_core(state, context, id);

            let k = match resolution {
                Some(lowering::TypeVariableResolution::Forall(forall)) => state
                    .forall_binding_kind(*forall)
                    .expect("invariant violated: CheckState::bind_forall"),
                Some(lowering::TypeVariableResolution::Implicit(implicit)) => state
                    .implicit_binding_kind(implicit.node, implicit.id)
                    .expect("invariant violated: CheckState::bind_implicit"),
                None => state.fresh_unification(context),
            };

            (t, k)
        }

        lowering::TypeKind::Wildcard => default,

        lowering::TypeKind::Record { .. } => default,

        lowering::TypeKind::Row { .. } => default,

        lowering::TypeKind::Parenthesized { parenthesized } => {
            let Some(parenthesized) = parenthesized else { return default };
            infer_surface_kind(state, context, *parenthesized)
        }
    }
}

pub fn infer_surface_app_kind<S>(
    state: &mut CheckState<S>,
    context: &CheckContext,
    (function_t, function_k): (TypeId, TypeId),
    argument: lowering::TypeId,
) -> (TypeId, TypeId)
where
    S: TypeStorage,
{
    match *state.storage.index(function_k) {
        Type::Function(argument_k, result_k) => {
            let (argument_t, _) = check_surface_kind(state, context, argument, argument_k);

            let t = state.storage.intern(Type::Application(function_t, argument_t));
            let k = state.normalize(result_k);

            (t, k)
        }

        Type::Unification(unification, domain) => {
            let codomain = state.bound.level();

            let argument_u = state.fresh_unification_type(context);
            let result_u = state.fresh_unification_type(context);

            let function_u = state.storage.intern(Type::Function(argument_u, result_u));
            state.solve(codomain, unification, domain, function_u);

            let (argument_t, _) = check_surface_kind(state, context, argument, argument_u);

            let t = state.storage.intern(Type::Application(function_t, argument_t));
            let k = state.normalize(result_u);

            (t, k)
        }

        Type::Forall(ForallBinder { kind, .. }, function_k) => {
            let kind_u = state.fresh_unification_kinded(context, kind);

            let function_t = state.storage.intern(Type::KindApplication(function_t, kind_u));
            let function_k = substitute::substitute_bound(state, kind_u, function_k);

            infer_surface_app_kind(state, context, (function_t, function_k), argument)
        }

        _ => (context.prim.unknown, context.prim.unknown),
    }
}

fn check_surface_kind<S>(
    state: &mut CheckState<S>,
    context: &CheckContext,
    id: lowering::TypeId,
    kind: TypeId,
) -> (TypeId, TypeId)
where
    S: TypeStorage,
{
    let (t, k) = infer_surface_kind(state, context, id);
    unification::unify(state, k, kind);
    (t, k)
}

fn lookup_prim_type<S>(
    state: &mut CheckState<S>,
    context: &CheckContext,
    type_id: indexing::TypeItemId,
) -> TypeId
where
    S: TypeStorage,
{
    let item = &context.prim_indexed.items[type_id];

    let Some(name) = &item.name else {
        return context.prim.unknown;
    };

    let t_to_t = state.storage.intern(Type::Function(context.prim.t, context.prim.t));
    let row_t = state.storage.intern(Type::Application(context.prim.row, context.prim.t));

    match name.as_str() {
        "Type" => context.prim.t,
        "Function" => state.storage.intern(Type::Function(context.prim.t, t_to_t)),
        "Array" => t_to_t,
        "Record" => state.storage.intern(Type::Function(row_t, context.prim.t)),
        "Number" => context.prim.t,
        "Int" => context.prim.t,
        "String" => context.prim.t,
        "Char" => context.prim.t,
        "Boolean" => context.prim.t,
        "Partial" => context.prim.constraint,
        "Constraint" => context.prim.t,
        "Symbol" => context.prim.t,
        "Row" => t_to_t,
        _ => context.prim.unknown,
    }
}
