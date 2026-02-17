//! Implements core type structures.

pub mod fold;
pub mod generalise;
pub mod normalise;
pub mod substitute;
pub mod unification;
pub mod walk;

use std::sync::Arc;

use files::FileId;
use indexing::TypeItemId;
use itertools::Itertools;
use smol_str::SmolStr;

/// A globally unique identity for a rigid type variable.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Name {
    pub file: FileId,
    pub unique: u32,
}

/// A marker used to represent binding levels of variables.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Depth(pub u32);

impl Depth {
    pub fn increment(self) -> Depth {
        Depth(self.0 + 1)
    }
}

/// Carries information about a type variable under a forall.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ForallBinder {
    /// Whether this binder is visible to type applications.
    pub visible: bool,
    /// The unique identity attached to the type variable.
    pub name: Name,
    /// The kind of the type variable.
    pub kind: TypeId,
}

/// Represents a row type.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RowType {
    /// A stable-sorted list representing `Map<Label, NonEmptyList<Type>>`.
    pub fields: Arc<[RowField]>,
    /// Closed row if [`None`]; Open row if [`Some`].
    pub tail: Option<TypeId>,
}

impl RowType {
    pub fn new(fields: impl IntoIterator<Item = RowField>, tail: Option<TypeId>) -> RowType {
        let mut fields = fields.into_iter().collect_vec();
        fields.sort_by(|a, b| a.label.cmp(&b.label));
        RowType { fields: Arc::from(fields), tail }
    }
}

/// A field in a row type.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RowField {
    /// The label of the row field.
    pub label: SmolStr,
    /// The [`Type`] of the row field.
    pub id: TypeId,
}

/// The saturation of a synonym application.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Saturation {
    /// Fully applied synonym.
    Full,
    /// Partially applied synonym.
    Partial,
}

/// Represents a type synonym.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Synonym {
    /// Whether fully or partially applied.
    pub saturation: Saturation,
    /// The reference to the synonym type.
    pub reference: (FileId, TypeItemId),
    /// Arguments to the synonym constructor.
    pub arguments: Arc<[TypeId]>,
}

/// The core type representation used by the checker after name resolution.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    /// Type application, `Array Int`.
    Application(TypeId, TypeId),
    /// Kind application, `Proxy @Int`.
    KindApplication(TypeId, TypeId),
    /// Binary type operator application, `Fields + ()`.
    OperatorApplication(FileId, TypeItemId, TypeId, TypeId),
    /// Type synonym application, see [`Synonym`].
    SynonymApplication(SynonymId),

    /// A universally quantified type, `forall a. a -> a`.
    Forall(ForallBinderId, TypeId),
    /// A constrained type, `Constraint => Constrained`.
    Constrained(TypeId, TypeId),
    /// A function type, `a -> b`.
    Function(TypeId, TypeId),
    /// A type with an explicit kind, `T :: K`.
    Kinded(TypeId, TypeId),

    /// A resolved type constructor reference.
    Constructor(FileId, TypeItemId),
    /// A resolved type operator reference.
    OperatorConstructor(FileId, TypeItemId),

    /// A type-level integer literal, `42`.
    Integer(i32),
    /// A type-level string literal, `"life"`.
    String(lowering::StringKind, SmolStrId),
    /// A row type, see [`RowType`].
    Row(RowTypeId),

    /// A bound skolem variable that can only unify with itself.
    Rigid(Name, Depth, TypeId),
    /// A unification variable that can be solved to another [`Type`].
    Unification(u32),

    /// A type variable that did not resolve to a binder.
    Free(SmolStrId),
    /// Recovery marker for exceptional type checker paths.
    Unknown(SmolStrId),
}

/// The role of a type parameter for safe coercions.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Role {
    Phantom,
    Representational,
    Nominal,
}

pub type ForallBinderId = interner::Id<ForallBinder>;
pub type RowTypeId = interner::Id<RowType>;
pub type SynonymId = interner::Id<Synonym>;
pub type SmolStrId = interner::Id<SmolStr>;
pub type TypeId = interner::Id<Type>;
