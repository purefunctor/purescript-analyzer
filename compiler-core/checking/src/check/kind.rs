use crate::{
    ExternalQueries,
    check::{CheckContext, CheckState, convert, unification},
    core::{ForallBinder, Type, TypeId, Variable, debruijn},
};

pub fn infer_surface_kind<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    id: lowering::TypeId,
) -> (TypeId, TypeId)
where
    Q: ExternalQueries,
{
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
            let k = context.prim.int;
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

pub fn infer_surface_app_kind<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    (function_t, function_k): (TypeId, TypeId),
    argument: lowering::TypeId,
) -> (TypeId, TypeId)
where
    Q: ExternalQueries,
{
    let function_k = state.normalize_type(function_k);
    match state.storage[function_k] {
        Type::Function(argument_k, result_k) => {
            let (argument_t, _) = check_surface_kind(state, context, argument, argument_k);

            let t = state.storage.intern(Type::Application(function_t, argument_t));
            let k = state.normalize_type(result_k);

            (t, k)
        }

        Type::Unification(unification_id) => {
            let argument_u = state.fresh_unification_type(context);
            let result_u = state.fresh_unification_type(context);

            let function_u = state.storage.intern(Type::Function(argument_u, result_u));
            let _ = solve(state, context, unification_id, function_u);

            let (argument_t, _) = check_surface_kind(state, context, argument, argument_u);

            let t = state.storage.intern(Type::Application(function_t, argument_t));
            let k = state.normalize_type(result_u);

            (t, k)
        }

        Type::Forall(ForallBinder { level, kind, .. }, function_k) => {
            let k = state.normalize_type(kind);
            let t = state.fresh_unification_kinded(k);

            state.bind_with_type(level, t, k);

            let function_t = state.storage.intern(Type::KindApplication(function_t, t));
            let result_t =
                infer_surface_app_kind(state, context, (function_t, function_k), argument);

            state.unbind(level);

            result_t
        }

        _ => (context.prim.unknown, context.prim.unknown),
    }
}

pub fn elaborate_kind<Q>(state: &mut CheckState, context: &CheckContext<Q>, id: TypeId) -> TypeId
where
    Q: ExternalQueries,
{
    let id = state.normalize_type(id);
    match state.storage[id] {
        Type::Application(_, _) => context.prim.unknown,

        Type::Constructor(_, type_id) => lookup_prim_type(state, context, type_id),

        Type::Forall(_, _) => context.prim.t,

        Type::Function(_, _) => context.prim.t,

        Type::KindApplication(_, _) => context.prim.unknown,

        Type::Unification(unification_id) => state.unification.get(unification_id).kind,

        Type::Variable(ref variable) => match variable {
            Variable::Implicit(_) => context.prim.unknown,
            Variable::Skolem(_) => context.prim.unknown,
            Variable::Bound(index) => {
                let size = state.bound.size();

                let Some(level) = index.to_level(size) else {
                    return context.prim.unknown;
                };

                let Some(kind) = state.kinds.get(level) else {
                    return context.prim.unknown;
                };

                *kind
            }
            Variable::Free(_) => context.prim.unknown,
        },

        Type::Unknown => context.prim.unknown,
    }
}

pub fn solve<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    unification_id: u32,
    solution: TypeId,
) -> Option<u32>
where
    Q: ExternalQueries,
{
    let codomain = state.bound.size();
    let occurs = Some(unification_id);

    if !state.promote_type(occurs, codomain, unification_id, solution) {
        return None;
    }

    let unification_kind = state.unification.get(unification_id).kind;
    let solution_kind = elaborate_kind(state, context, solution);
    unification::unify(state, context, unification_kind, solution_kind);

    state.unification.solve(unification_id, solution);

    Some(unification_id)
}

pub fn check_surface_kind<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    id: lowering::TypeId,
    kind: TypeId,
) -> (TypeId, TypeId)
where
    Q: ExternalQueries,
{
    let (surface_t, surface_k) = infer_surface_kind(state, context, id);
    unification::unify(state, context, surface_k, kind);
    (surface_t, surface_k)
}

fn lookup_prim_type<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    type_id: indexing::TypeItemId,
) -> TypeId
where
    Q: ExternalQueries,
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
        "Proxy" => {
            let t = state.storage.intern(Type::Variable(Variable::Bound(debruijn::Index(0))));

            let function = state.storage.intern(Type::Function(t, context.prim.t));

            let forall = state.storage.intern(Type::Forall(
                ForallBinder {
                    visible: false,
                    name: "t".into(),
                    level: debruijn::Level(1),
                    kind: t,
                },
                function,
            ));

            state.storage.intern(Type::Forall(
                ForallBinder {
                    visible: false,
                    name: "k".into(),
                    level: debruijn::Level(0),
                    kind: context.prim.t,
                },
                forall,
            ))
        }
        _ => context.prim.unknown,
    }
}
