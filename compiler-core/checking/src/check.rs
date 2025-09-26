use std::fmt::Debug;

use indexing::{IndexedModule, TermItemKind};
use itertools::Itertools;
use lowering::{ImplicitTypeVariable, LoweredModule};
use smol_str::SmolStr;

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
                    .map(|id| core_of_cst(c, e, id))
                    .unwrap_or_else(|| c.storage.unknown());
                let id = debruijn::Variable::Forall(binding.id);
                let level = c.bound.bind(id);
                Some(ForallBinder { visible, name, level, kind })
            });

            let binders = binders.collect_vec().into_iter();
            let inner =
                type_.map(|id| core_of_cst(c, e, id)).unwrap_or_else(|| c.storage.unknown());

            let id = binders
                .rfold(inner, |inner, binder| c.storage.allocate(Type::Forall(binder, inner)));

            if let Type::Forall(ForallBinder { level, .. }, _) = c.storage.index(id) {
                c.bound.unbind(*level);
            }

            id
        }
        lowering::TypeKind::Hole => c.storage.unknown(),
        lowering::TypeKind::Integer => c.storage.unknown(),
        lowering::TypeKind::Kinded { .. } => c.storage.unknown(),
        lowering::TypeKind::Operator { .. } => c.storage.unknown(),
        lowering::TypeKind::OperatorChain { .. } => c.storage.unknown(),
        lowering::TypeKind::String => c.storage.unknown(),
        lowering::TypeKind::Variable { name, resolution } => {
            let Some(resolution) = resolution else {
                let name = name.as_ref().map(SmolStr::clone);
                return c.storage.allocate(Type::Free(name));
            };
            // TODO: Implement a "context" mechanism here, where if we
            // encounter an implicit type variable, we must be inside
            // a blessed context for these rules to kick in. The blessed
            // context also maintains a hash map of already-bound implicit
            // variables such that we don't allocate multiple
            match resolution {
                lowering::TypeVariableResolution::Forall(id) => {
                    let id = debruijn::Variable::Forall(*id);
                    let index = c.bound.index_of(id);
                    c.storage.allocate(Type::Variable(index))
                }
                lowering::TypeVariableResolution::Implicit(ImplicitTypeVariable {
                    binding,
                    node,
                    id,
                }) => {
                    let id = debruijn::Variable::Implicit { node: *node, id: *id };
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
    indexed: &IndexedModule,
    lowered: &LoweredModule,
) {
    let foreign = indexed.items.iter_terms().filter_map(|(id, item)| {
        if let TermItemKind::Foreign { .. } = item.kind { Some(id) } else { None }
    });

    let instance = indexed.items.iter_terms().filter_map(|(id, item)| {
        if let TermItemKind::Instance { .. } = item.kind { Some(id) } else { None }
    });

    let mut context = Context::new(storage);
    let environment = Environment::new(indexed, lowered);

    for id in foreign {
        if let Some(lowering::TermItemIr::Foreign { signature }) = lowered.info.get_term_item(id) {
            let _ = signature.map(|id| core_of_cst(&mut context, &environment, id));
        }
    }

    for id in instance {
        if let Some(lowering::TermItemIr::Instance { arguments, constraints, members, .. }) =
            lowered.info.get_term_item(id)
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
