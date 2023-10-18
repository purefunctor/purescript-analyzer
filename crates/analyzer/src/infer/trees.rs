use syntax::ast;

use crate::id::{AstId, InFile};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(salsa::InternId);

impl salsa::InternKey for TypeId {
    fn from_intern_id(v: salsa::InternId) -> TypeId {
        TypeId(v)
    }

    fn as_intern_id(&self) -> salsa::InternId {
        self.0
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Function(Box<[TypeId]>, TypeId),
    Literal(Literal),
    NotImplemented,
    Unification(Unification),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Literal {
    Int,
    Number,
    String,
    Char,
    Boolean,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Unification {
    Local(u32, InFile<AstId<ast::ValueDeclaration>>),
    Global(InFile<AstId<ast::ValueDeclaration>>),
}
