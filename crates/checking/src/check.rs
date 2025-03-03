use std::fmt::Debug;

use indexing::{FullModuleIndex, TermItem};
use itertools::Itertools;
use lowering::FullModuleLower;

use crate::{
    core::{CoreStorage, ForallBinder, Type, TypeId},
    debruijn,
};

#[derive(Debug)]
pub struct Context<'s, S>
where
    S: CoreStorage,
{
    storage: &'s mut S,
    unique: u32,
    bound: debruijn::Bound,
}

impl<'s, S> Context<'s, S>
where
    S: CoreStorage,
{
    pub fn new(storage: &'s mut S) -> Context<'s, S> {
        let unique = 0;
        let bound = debruijn::Bound::default();
        Context { storage, unique, bound }
    }
}

pub struct Environment<'e> {
    index: &'e FullModuleIndex,
    lower: &'e FullModuleLower,
}

impl<'e> Environment<'e> {
    pub fn new(index: &'e FullModuleIndex, lower: &'e FullModuleLower) -> Environment<'e> {
        Environment { index, lower }
    }
}

fn core_of_cst<S: CoreStorage>(
    c: &mut Context<S>,
    e: &Environment,
    id: lowering::TypeId,
) -> TypeId {
    let Some(kind) = e.lower.intermediate.index_type_kind(id) else {
        return c.storage.unknown();
    };
    match kind {
        lowering::TypeKind::ApplicationChain { function, arguments } => {
            let _ = function.map(|id| core_of_cst(c, e, id)).unwrap_or_else(|| c.storage.unknown());
            let _ = arguments.iter().copied().map(|id| core_of_cst(c, e, id)).collect_vec();
            c.storage.unknown()
        }
        lowering::TypeKind::Arrow { argument, result } => {
            let argument =
                argument.map(|id| core_of_cst(c, e, id)).unwrap_or_else(|| c.storage.unknown());
            let result =
                result.map(|id| core_of_cst(c, e, id)).unwrap_or_else(|| c.storage.unknown());
            c.storage.allocate(Type::Function(argument, result))
        }
        lowering::TypeKind::Constrained { .. } => c.storage.unknown(),
        lowering::TypeKind::Constructor { .. } => c.storage.unknown(),
        lowering::TypeKind::Forall { bindings, r#type } => {
            let binders = bindings.iter().filter_map(|binding| {
                let visible = binding.visible;
                let name = binding.name.clone()?;
                let id = debruijn::Binding::Forall(binding.id);
                let level = c.bound.bind(id);
                Some(ForallBinder { visible, name, level })
            });
            let binders = binders.collect_vec().into_iter();
            let inner = r#type.map(|id| core_of_cst(c, e, id));
            let inner = inner.unwrap_or_else(|| c.storage.unknown());
            binders.rfold(inner, |inner, binder| c.storage.allocate(Type::Forall(binder, inner)))
        }
        lowering::TypeKind::Hole => c.storage.unknown(),
        lowering::TypeKind::Integer => c.storage.unknown(),
        lowering::TypeKind::Kinded { .. } => c.storage.unknown(),
        lowering::TypeKind::Operator { .. } => c.storage.unknown(),
        lowering::TypeKind::OperatorChain { .. } => c.storage.unknown(),
        lowering::TypeKind::String => c.storage.unknown(),
        lowering::TypeKind::Variable { resolution, .. } => {
            let Some(resolution) = resolution else {
                return c.storage.unknown();
            };
            match resolution {
                lowering::TypeVariableResolution::Forall(id) => {
                    let id = debruijn::Binding::Forall(*id);
                    let index = c.bound.index_of(id);
                    c.storage.allocate(Type::Variable(index))
                }
                lowering::TypeVariableResolution::Implicit { binding, node, id } => {
                    let id = debruijn::Binding::Implicit(*node, *id);
                    if *binding {
                        let level = c.bound.bind(id);
                        c.storage.allocate(Type::ImplicitBinder(level))
                    } else {
                        let index = c.bound.index_of(id);
                        c.storage.allocate(Type::ImplicitVariable(index))
                    }
                }
            }
        }
        lowering::TypeKind::Wildcard => c.storage.unknown(),
        lowering::TypeKind::Record { .. } => c.storage.unknown(),
        lowering::TypeKind::Row { .. } => c.storage.unknown(),
        lowering::TypeKind::Parenthesized { parenthesized } => {
            let _ = parenthesized
                .map(|id| core_of_cst(c, e, id))
                .unwrap_or_else(|| c.storage.unknown());
            c.storage.unknown()
        }
    }
}

pub fn check_module(
    storage: &mut (impl CoreStorage + Debug),
    index: &FullModuleIndex,
    lower: &FullModuleLower,
) {
    let foreign = index
        .index
        .iter_term_item()
        .filter_map(|(item, id)| if let TermItem::Foreign { .. } = id { Some(item) } else { None });

    let instance = index.index.iter_term_item().filter_map(|(item, id)| {
        if let TermItem::Instance { .. } = id { Some(item) } else { None }
    });

    let mut context = Context::new(storage);
    let environment = Environment::new(index, lower);

    for id in foreign {
        if let Some(lowering::TermItemIr::Foreign { signature }) =
            lower.intermediate.index_term_item(id)
        {
            let _ = signature.map(|id| core_of_cst(&mut context, &environment, id));
        }
    }

    for id in instance {
        if let Some(lowering::TermItemIr::Instance { arguments, constraints, .. }) =
            lower.intermediate.index_term_item(id)
        {
            arguments.iter().for_each(|id| {
                core_of_cst(&mut context, &environment, *id);
            });
            constraints.iter().for_each(|id| {
                core_of_cst(&mut context, &environment, *id);
            })
        }
    }

    dbg!(storage);
}
