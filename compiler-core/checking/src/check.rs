use std::fmt::Debug;

use indexing::{IndexedModule, TermItemKind};
use itertools::Itertools;
use lowering::{ImplicitTypeVariable, LoweredModule};

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
    #[allow(unused)]
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
    #[allow(unused)]
    index: &'e IndexedModule,
    lower: &'e LoweredModule,
}

impl<'e> Environment<'e> {
    pub fn new(index: &'e IndexedModule, lower: &'e LoweredModule) -> Environment<'e> {
        Environment { index, lower }
    }
}

fn core_of_cst<S: CoreStorage>(
    c: &mut Context<S>,
    e: &Environment,
    id: lowering::TypeId,
) -> TypeId {
    let Some(kind) = e.lower.info.get_type_kind(id) else {
        return c.storage.unknown();
    };
    match kind {
        lowering::TypeKind::ApplicationChain { function, arguments } => {
            let function =
                function.map(|id| core_of_cst(c, e, id)).unwrap_or_else(|| c.storage.unknown());
            arguments.iter().fold(function, |function, argument| {
                let argument = core_of_cst(c, e, *argument);
                c.storage.allocate(Type::Application(function, argument))
            })
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
        lowering::TypeKind::Forall { bindings, type_ } => {
            let binders = bindings.iter().filter_map(|binding| {
                let visible = binding.visible;
                let name = binding.name.clone()?;
                let kind = binding
                    .kind
                    .map(|id| core_of_cst(c, e, id))
                    .unwrap_or_else(|| c.storage.unknown());
                let id = debruijn::Binding::Forall(binding.id);
                let level = c.bound.bind(id);
                Some(ForallBinder { visible, name, level, kind })
            });
            let binders = binders.collect_vec().into_iter();
            let inner = type_.map(|id| core_of_cst(c, e, id));
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
                lowering::TypeVariableResolution::Implicit(ImplicitTypeVariable {
                    binding,
                    node,
                    id,
                }) => {
                    let id = debruijn::Binding::Implicit(*node, *id);
                    if *binding {
                        let level = c.bound.bind(id);
                        c.storage.allocate(Type::Implicit(level))
                    } else {
                        let index = c.bound.index_of(id);
                        c.storage.allocate(Type::Variable(index))
                    }
                }
            }
        }
        lowering::TypeKind::Wildcard => c.storage.unknown(),
        lowering::TypeKind::Record { .. } => c.storage.unknown(),
        lowering::TypeKind::Row { .. } => c.storage.unknown(),
        lowering::TypeKind::Parenthesized { parenthesized } => {
            parenthesized.map(|id| core_of_cst(c, e, id)).unwrap_or_else(|| c.storage.unknown())
        }
    }
}

pub fn check_module(
    storage: &mut (impl CoreStorage + Debug),
    index: &IndexedModule,
    lower: &LoweredModule,
) {
    let foreign = index.items.iter_terms().filter_map(|(id, item)| {
        if let TermItemKind::Foreign { .. } = item.kind { Some(id) } else { None }
    });

    let instance = index.items.iter_terms().filter_map(|(id, item)| {
        if let TermItemKind::Instance { .. } = item.kind { Some(id) } else { None }
    });

    let mut context = Context::new(storage);
    let environment = Environment::new(index, lower);

    for id in foreign {
        if let Some(lowering::TermItemIr::Foreign { signature }) = lower.info.get_term_item(id) {
            let _ = signature.map(|id| core_of_cst(&mut context, &environment, id));
        }
    }

    for id in instance {
        if let Some(lowering::TermItemIr::Instance { arguments, constraints, members, .. }) =
            lower.info.get_term_item(id)
        {
            arguments.iter().for_each(|id| {
                core_of_cst(&mut context, &environment, *id);
            });
            constraints.iter().for_each(|id| {
                core_of_cst(&mut context, &environment, *id);
            });
            members.iter().for_each(|group| {
                if let Some(id) = group.signature {
                    core_of_cst(&mut context, &environment, id);
                }
            });
        }
    }

    dbg!(storage);
}
