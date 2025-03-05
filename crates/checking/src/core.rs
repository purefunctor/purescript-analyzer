use files::FileId;
use indexing::TypeItemId;
use la_arena::Idx;
use smol_str::SmolStr;

use crate::debruijn;

pub trait CoreStorage {
    fn unknown(&self) -> TypeId;

    fn allocate(&mut self, ty: Type) -> TypeId;

    fn index(&self, id: TypeId) -> &Type;
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum BuiltinConstructor {
    Boolean,
    Int,
    Number,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Constructor {
    Builtin(BuiltinConstructor),
    Module(FileId, TypeItemId),
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
pub enum Type {
    Application(TypeId, TypeId),
    Constructor(Constructor),
    Forall(ForallBinder, TypeId),
    Function(TypeId, TypeId),
    Implicit(debruijn::Level),
    Unification(Unification),
    Variable(debruijn::Index),
    Unknown,
}

pub type TypeId = Idx<Type>;
