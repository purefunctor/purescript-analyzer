use indexing::{FullModuleIndex, TermItem};
use lowering::FullModuleLower;
use smol_str::SmolStr;

use crate::core::{CoreStorage, ForallBinder, Type, TypeId};

#[derive(Debug)]
pub struct Context<'s, S>
where
    S: CoreStorage,
{
    storage: &'s mut S,
    unique: u32,
}

impl<'s, S> Context<'s, S>
where
    S: CoreStorage,
{
    pub fn new(storage: &'s mut S) -> Context<'s, S> {
        let unique = 0;
        Context { storage, unique }
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
            let inner =
                r#type.map(|id| core_of_cst(c, e, id)).unwrap_or_else(|| c.storage.unknown());

            let forall = bindings.iter().try_rfold(inner, |inner, binding| {
                let name = binding.name.as_ref()?;

                let visible = binding.visible;
                let name = SmolStr::clone(name);

                let binder = ForallBinder { visible, name };
                Some(c.storage.allocate(Type::Forall(binder, inner)))
            });

            forall.unwrap_or_else(|| c.storage.unknown())
        }
        lowering::TypeKind::Hole => c.storage.unknown(),
        lowering::TypeKind::Integer => c.storage.unknown(),
        lowering::TypeKind::Kinded { .. } => c.storage.unknown(),
        lowering::TypeKind::Operator { .. } => c.storage.unknown(),
        lowering::TypeKind::OperatorChain { .. } => c.storage.unknown(),
        lowering::TypeKind::String => c.storage.unknown(),
        lowering::TypeKind::Variable { name, resolution } => {
            let (Some(name), Some(_)) = (name, resolution) else {
                return c.storage.unknown();
            };
            let name = SmolStr::clone(&name);
            c.storage.allocate(Type::Variable(name))
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
    storage: &mut impl CoreStorage,
    index: &FullModuleIndex,
    lower: &FullModuleLower,
) {
    let foreign = index
        .index
        .iter_term_item()
        .filter_map(|(item, id)| if let TermItem::Foreign { .. } = id { Some(item) } else { None });

    let mut context = Context::new(storage);
    let environment = Environment::new(index, lower);

    for id in foreign {
        if let Some(lowering::TermItemIr::Foreign { signature }) =
            lower.intermediate.index_term_item(id)
        {
            let _ = signature.map(|id| core_of_cst(&mut context, &environment, id));
        }
    }
}
