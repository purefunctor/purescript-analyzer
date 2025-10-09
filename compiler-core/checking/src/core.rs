pub mod debruijn;
pub mod pretty;
pub mod storage;

use std::sync::Arc;

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

pub type Spine = Arc<[debruijn::Level]>;

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Type {
    Application(TypeId, TypeId),
    Constructor(FileId, TypeItemId),
    Forall(ForallBinder, TypeId),
    Function(TypeId, TypeId),
    KindApplication(TypeId, TypeId),
    Lambda(TypeId),
    Unification(u32, Spine),
    Variable(Variable),
    Unknown,
}

pub type TypeId = Idx<Type>;
