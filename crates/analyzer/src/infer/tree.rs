//! ASTs for type inference.
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

use crate::{
    id::InFile,
    index::nominal::{ClassGroupId, DataGroupId},
    surface::Name,
    InferenceDatabase,
};

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
    Constrained(CoreTypeId, CoreTypeId),
    Constructor(InFile<ConstructorId>),
    Forall(CoreTypeVariable, CoreTypeId),
    Function(CoreTypeId, CoreTypeId),
    Primitive(Name),
    Unification(InFile<u32>),
    Variable(Name),
    NotImplemented,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ConstructorId {
    Class(ClassGroupId),
    Data(DataGroupId),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CoreTypeVariable {
    Name(Name),
    Kinded(Name, CoreTypeId),
}

impl CoreTypeVariable {
    pub fn name(&self) -> &Name {
        match self {
            CoreTypeVariable::Name(name) => name,
            CoreTypeVariable::Kinded(name, _) => name,
        }
    }
}

pub fn pretty_print<Db>(db: &Db, t: CoreTypeId) -> String
where
    Db: InferenceDatabase + ?Sized,
{
    let allocator = pretty::Arena::<()>::default();
    let t = pretty_print_core(db, &allocator, t);
    format!("{}", t.pretty(100))
}

fn pretty_print_core<'a, A, Db>(db: &Db, allocator: &'a A, t: CoreTypeId) -> DocBuilder<'a, A, ()>
where
    A: DocAllocator<'a, ()>,
    Db: InferenceDatabase + ?Sized,
{
    match db.lookup_intern_type(t) {
        CoreType::Application(function, argument) => {
            let function = pretty_print_core(db, allocator, function);
            let argument = pretty_print_core(db, allocator, argument);
            function.append(" ").append(argument)
        }
        CoreType::Constrained(constraint, constrained) => {
            let constraint = pretty_print_core(db, allocator, constraint);
            let constrained = pretty_print_core(db, allocator, constrained);
            constraint.append(" => ").append(constrained)
        }
        CoreType::Constructor(id) => {
            let (surface, _) = db.file_surface(id.file_id);
            let name = match id.value {
                ConstructorId::Class(class_id) => surface
                    .body
                    .class_declaration(class_id)
                    .map(|class_declaration| &class_declaration.name),
                ConstructorId::Data(data_id) => surface
                    .body
                    .data_declaration(data_id)
                    .map(|data_declaration| &data_declaration.name),
            };
            if let Some(name) = name {
                allocator.text(name.as_ref().to_string())
            } else {
                allocator.text("?")
            }
        }
        CoreType::Forall(variable, inner) => {
            let mut result = allocator.text("forall ").append(variable.name().as_ref().to_string());

            let mut current_inner = inner;
            while let CoreType::Forall(variable, inner) = db.lookup_intern_type(current_inner) {
                result = result.append(" ").append(variable.name().as_ref().to_string());
                current_inner = inner;
            }

            result.append(". ").append(pretty_print_core(db, allocator, current_inner))
        }
        CoreType::Function(argument, result) => {
            let argument = pretty_print_core(db, allocator, argument);
            let result = pretty_print_core(db, allocator, result);
            argument.append(" -> ").append(result)
        }
        CoreType::Primitive(name) => allocator.text(name.as_ref().to_string()),
        CoreType::Unification(id) => allocator.text("?").append(id.value.to_string()),
        CoreType::Variable(name) => allocator.text(name.as_ref().to_string()),
        CoreType::NotImplemented => allocator.text("?"),
    }
}
