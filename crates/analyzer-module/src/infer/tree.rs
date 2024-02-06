//! ASTs for type inference
//!
//! Unlike the definition from [`surface`], [`CoreType`] is interned through
//! [`salsa`]'s interning mechanism rather than an [`Arena`] that gets passed
//! around. The primary reason for this difference is the fact that type
//! inference operates at a more global context. An [`Arena`] is impractical
//! as it would mean that types cannot be shared across units of compilation 
//! i.e. files
//!
//! [`Arena`]: la_arena::Arena
//! [`surface`]: crate::surface

use pretty::{DocAllocator, DocBuilder};

use crate::{id::InFile, index::nominal::DataGroupId, surface::Name, InferenceDatabase};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CoreTypeId(salsa::InternId);

impl salsa::InternKey for CoreTypeId {
    fn from_intern_id(v: salsa::InternId) -> Self {
        CoreTypeId(v)
    }

    fn as_intern_id(&self) -> salsa::InternId {
        self.0
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CoreType {
    Application(CoreTypeId, CoreTypeId),
    Constructor(InFile<DataGroupId>),
    Forall(Name, CoreTypeId),
    Function(CoreTypeId, CoreTypeId),
    Unification(InFile<u32>),
    Variable(Name),
    NotImplemented,
}

pub fn pretty_print(db: &impl InferenceDatabase, t: CoreTypeId) -> String {
    let allocator = pretty::Arena::<()>::default();
    let t = pretty_print_core(db, &allocator, t);
    format!("{}", t.pretty(100))
}

fn pretty_print_core<'a, A>(
    db: &impl InferenceDatabase,
    allocator: &'a A,
    t: CoreTypeId,
) -> DocBuilder<'a, A, ()>
where
    A: DocAllocator<'a, ()>,
{
    match db.lookup_intern_type(t) {
        CoreType::Application(function, argument) => {
            let function = pretty_print_core(db, allocator, function);
            let argument = pretty_print_core(db, allocator, argument);
            function.append(" ").append(argument)
        }
        CoreType::Constructor(id) => {
            let (surface, _) = db.file_surface(id.file_id);
            if let Some(data_declaration) = surface.body.data_declaration(id.value) {
                allocator.text(data_declaration.name.as_ref().to_string())
            } else {
                allocator.text("?")
            }
        }
        CoreType::Forall(argument, inner) => {
            let inner = pretty_print_core(db, allocator, inner);
            allocator
                .text("forall ")
                .append(argument.as_ref().to_string())
                .append(". ")
                .append(inner)
        }
        CoreType::Function(argument, result) => {
            let argument = pretty_print_core(db, allocator, argument);
            let result = pretty_print_core(db, allocator, result);
            argument.append(" -> ").append(result)
        }
        CoreType::Unification(_) => allocator.text("?"),
        CoreType::Variable(name) => allocator.text(name.as_ref().to_string()),
        CoreType::NotImplemented => allocator.text("?"),
    }
}
