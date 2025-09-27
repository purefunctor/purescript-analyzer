use std::fmt::Debug;

use indexing::{IndexedModule, TermItemKind};
use itertools::Itertools;
use lowering::{ImplicitTypeVariable, LoweredModule};

use crate::{
    core::{ForallBinder, Type, TypeId, TypeStorage, Variable},
    debruijn,
    state::{CheckContext, CheckState},
};

fn core_of_cst<S: TypeStorage>(
    state: &mut CheckState<S>,
    context: &CheckContext,
    id: lowering::TypeId,
) -> TypeId {
    let Some(kind) = context.lowered.info.get_type_kind(id) else {
        return state.storage.unknown();
    };
    match kind {
        lowering::TypeKind::ApplicationChain { function, arguments } => {
            let function = function
                .map(|id| core_of_cst(state, context, id))
                .unwrap_or_else(|| state.storage.unknown());
            arguments.iter().fold(function, |function, argument| {
                let argument = core_of_cst(state, context, *argument);
                state.storage.intern(Type::Application(function, argument))
            })
        }
        lowering::TypeKind::Arrow { argument, result } => {
            let argument = argument
                .map(|id| core_of_cst(state, context, id))
                .unwrap_or_else(|| state.storage.unknown());
            let result = result
                .map(|id| core_of_cst(state, context, id))
                .unwrap_or_else(|| state.storage.unknown());
            state.storage.intern(Type::Function(argument, result))
        }
        lowering::TypeKind::Constrained { .. } => state.storage.unknown(),
        lowering::TypeKind::Constructor { .. } => state.storage.unknown(),
        lowering::TypeKind::Forall { bindings, type_ } => {
            // TODO: after creating the type, get the level of the
            // outermost type so we can unbind it right after. This
            // would allow us to properly lower the following code:
            //
            // (forall a. a -> a) -> (forall a. a -> a)
            let binders = bindings.iter().filter_map(|binding| {
                let visible = binding.visible;
                let name = binding.name.clone()?;
                let kind = binding
                    .kind
                    .map(|id| core_of_cst(state, context, id))
                    .unwrap_or_else(|| state.storage.unknown());
                let id = debruijn::Variable::Forall(binding.id);
                let level = state.bound.bind(id);
                Some(ForallBinder { visible, name, level, kind })
            });

            let binders = binders.collect_vec().into_iter();
            let inner = type_
                .map(|id| core_of_cst(state, context, id))
                .unwrap_or_else(|| state.storage.unknown());

            let id = binders
                .rfold(inner, |inner, binder| state.storage.intern(Type::Forall(binder, inner)));

            if let Type::Forall(ForallBinder { level, .. }, _) = state.storage.index(id) {
                state.bound.unbind(*level);
            }

            id
        }
        lowering::TypeKind::Hole => state.storage.unknown(),
        lowering::TypeKind::Integer => state.storage.unknown(),
        lowering::TypeKind::Kinded { .. } => state.storage.unknown(),
        lowering::TypeKind::Operator { .. } => state.storage.unknown(),
        lowering::TypeKind::OperatorChain { .. } => state.storage.unknown(),
        lowering::TypeKind::String => state.storage.unknown(),
        lowering::TypeKind::Variable { name, resolution } => {
            let Some(resolution) = resolution else {
                let name = name.clone();
                let kind = Variable::Free(name);
                return state.storage.intern(Type::Variable(kind));
            };
            // TODO: Implement a "context" mechanism here, where if we
            // encounter an implicit type variable, we must be inside
            // a blessed context for these rules to kick in. The blessed
            // context also maintains a hash map of already-bound implicit
            // variables such that we don't allocate multiple
            match resolution {
                lowering::TypeVariableResolution::Forall(id) => {
                    let id = debruijn::Variable::Forall(*id);
                    let index = state.bound.index_of(id);
                    let kind = Variable::Bound(index);
                    state.storage.intern(Type::Variable(kind))
                }
                lowering::TypeVariableResolution::Implicit(ImplicitTypeVariable {
                    binding,
                    node,
                    id,
                }) => {
                    let id = debruijn::Variable::Implicit { node: *node, id: *id };
                    if *binding {
                        let level = state.bound.bind(id);
                        let kind = Variable::Implicit(level);
                        state.storage.intern(Type::Variable(kind))
                    } else {
                        let index = state.bound.index_of(id);
                        let kind = Variable::Bound(index);
                        state.storage.intern(Type::Variable(kind))
                    }
                }
            }
        }
        lowering::TypeKind::Wildcard => state.storage.unknown(),
        lowering::TypeKind::Record { .. } => state.storage.unknown(),
        lowering::TypeKind::Row { .. } => state.storage.unknown(),
        lowering::TypeKind::Parenthesized { parenthesized } => parenthesized
            .map(|id| core_of_cst(state, context, id))
            .unwrap_or_else(|| state.storage.unknown()),
    }
}

pub fn check_module(
    storage: &mut (impl TypeStorage + Debug),
    indexed: &IndexedModule,
    lowered: &LoweredModule,
) {
    let foreign = indexed.items.iter_terms().filter_map(|(id, item)| {
        if let TermItemKind::Foreign { .. } = item.kind { Some(id) } else { None }
    });

    let instance = indexed.items.iter_terms().filter_map(|(id, item)| {
        if let TermItemKind::Instance { .. } = item.kind { Some(id) } else { None }
    });

    let mut state = CheckState::new(storage);
    let env = CheckContext::new(indexed, lowered);

    for id in foreign {
        if let Some(lowering::TermItemIr::Foreign { signature }) = lowered.info.get_term_item(id) {
            let _ = signature.map(|id| core_of_cst(&mut state, &env, id));
        }
    }

    for id in instance {
        if let Some(lowering::TermItemIr::Instance { arguments, constraints, members, .. }) =
            lowered.info.get_term_item(id)
        {
            arguments.iter().for_each(|id| {
                core_of_cst(&mut state, &env, *id);
            });
            constraints.iter().for_each(|id| {
                core_of_cst(&mut state, &env, *id);
            });
            members.iter().for_each(|group| {
                if let Some(id) = group.signature {
                    core_of_cst(&mut state, &env, id);
                }
            });
        }
    }

    dbg!(storage);
}
