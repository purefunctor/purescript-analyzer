pub mod debruijn;
pub mod pretty;

use files::FileId;
use indexing::TypeItemId;
use smol_str::SmolStr;

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum BuiltinConstructor {
    Boolean,
    Int,
    Number,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ForallBinder {
    pub visible: bool,
    pub name: SmolStr,
    pub level: debruijn::Level,
    pub kind: TypeId,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Variable {
    Implicit(debruijn::Level),
    Skolem(debruijn::Level),
    Bound(debruijn::Index),
    Free(SmolStr),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Application(TypeId, TypeId),
    Constructor(FileId, TypeItemId),
    Forall(ForallBinder, TypeId),
    Function(TypeId, TypeId),
    KindApplication(TypeId, TypeId),
    Unification(u32),
    Variable(Variable),
    Unknown,
}

pub type TypeId = interner::Id<Type>;

pub type TypeInterner = interner::Interner<Type>;
