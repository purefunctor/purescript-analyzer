use std::fmt::Debug;

use indexing::{FullModuleIndex, TermItem, TypeItem};
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
        lowering::TypeKind::ApplicationChain { .. } => c.storage.unknown(),
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
                let level = c.bound.bind(binding.id);
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
                    let index = c.bound.index_of(*id);
                    c.storage.allocate(Type::Variable(index))
                }
                lowering::TypeVariableResolution::Instance(_) => c.storage.unknown(),
                lowering::TypeVariableResolution::InstanceBinder => c.storage.unknown(),
            }
            // TODO: Implement binding for Instance/InstanceBinder. Here are some cases to handle:
            //
            // instance Eq a
            //
            // instance TypeEq a a
            //
            // instance Ord a => Eq a
            //
            // We'll first encounter InstanceBinder when checking the instance head. If we were to
            // bind the names naively, we'll encounter a case where a name would have already been
            // bound (see TypeEq a a). We'll need to do some additional bookkeeping to manage
            // implicit foralls.
            //
            // The first time we see an InstanceBinder, we push it into the scope, giving us a de
            // Bruijn index. Subsequent usages of the InstanceBinder would simply return that de
            // Bruijn index. Likewise, any usage of Instance would refer back to that type
            // variable.
            //
            // Unlike explicitly-bound type variables, we have no identity to anchor to for
            // implicit type variables. As such, we must create them on the fly when lowering types
            // for instance heads. Although, we sort of do in a way? If we used an interner in the
            // Constraint graph node, we could obtain an ID that's specific to that scope. That
            // should be a sufficient anchor for de Bruijn indices _that is not_ SmolStr.
            //
            // Although, what if type variables didn't resolve to TypeVariableBindingId directly,
            // but to the scope nodes that they were allocated in? Then, we'll need to perform the
            // resolution _here_
            //
            // Wait, are we cooking something here?
            //
            // Forall(GraphNodeId, usize)
            // Constraint(GraphNodeId, usize)
            //
            // These are very good anchors, no?
        }
        lowering::TypeKind::Wildcard => c.storage.unknown(),
        lowering::TypeKind::Record { .. } => c.storage.unknown(),
        lowering::TypeKind::Row { .. } => c.storage.unknown(),
        lowering::TypeKind::Parenthesized { .. } => c.storage.unknown(),
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
        if let Some(lowering::TermItemIr::Instance { arguments, .. }) =
            lower.intermediate.index_term_item(id)
        {
            arguments.iter().for_each(|id| {
                core_of_cst(&mut context, &environment, *id);
            });
        }
    }

    dbg!(storage);
}
