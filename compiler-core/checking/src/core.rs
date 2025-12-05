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
    Skolem(debruijn::Level, TypeId),
    Bound(debruijn::Index),
    Free(SmolStr),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Application(TypeId, TypeId),
    Constrained(TypeId, TypeId),
    Constructor(FileId, TypeItemId),
    Forall(ForallBinder, TypeId),
    Function(TypeId, TypeId),
    Integer(i32),
    KindApplication(TypeId, TypeId),
    Kinded(TypeId, TypeId),
    Operator(FileId, TypeItemId),
    String(lowering::StringKind, SmolStr),
    Unification(u32),
    Variable(Variable),
    Unknown,
}

pub type TypeId = interner::Id<Type>;

pub type TypeInterner = interner::Interner<Type>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Operator {
    pub associativity: lowering::Associativity,
    pub precedence: u8,
    pub file_id: FileId,
    pub type_id: TypeItemId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Synonym {
    pub quantified_variables: debruijn::Size,
    pub kind_variables: debruijn::Size,
    pub type_variables: debruijn::Size,
    pub synonym_type: TypeId,
}

impl Synonym {
    pub fn has_arguments(&self) -> bool {
        self.type_variables != debruijn::Size(0)
    }

    pub fn with_synonym_type(mut self, synonym_type: TypeId) -> Synonym {
        self.synonym_type = synonym_type;
        self
    }
}
