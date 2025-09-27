pub mod debruijn;
pub mod storage;

pub use storage::TypeStorage;

use files::FileId;
use indexing::TypeItemId;
use la_arena::Idx;
use smol_str::SmolStr;

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum BuiltinConstructor {
    Boolean,
    Int,
    Number,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ForallBinder {
    pub visible: bool,
    pub name: SmolStr,
    pub level: debruijn::Level,
    pub kind: TypeId,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Unification {
    pub unique: u32,
    pub level: u32,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Variable {
    Implicit(debruijn::Level),
    Skolem(debruijn::Level),
    Bound(debruijn::Index),
    Free(Option<SmolStr>),
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Type {
    Application(TypeId, TypeId),
    Constructor(FileId, TypeItemId),
    Forall(ForallBinder, TypeId),
    Function(TypeId, TypeId),
    Unification(Unification),
    Variable(Variable),
    Unknown,
}

pub type TypeId = Idx<Type>;
