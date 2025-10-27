use crate::source::*;
use crate::{TermItemId, TypeItemId};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ItemKind {
    DataSignature(DataSignatureId),
    DataEquation(DataEquationId),
    NewtypeSignature(NewtypeSignatureId),
    NewtypeEquation(NewtypeEquationId),
    Constructor(DataConstructorId),
    SynonymSignature(TypeSignatureId),
    SynonymEquation(TypeEquationId),
    ClassSignature(ClassSignatureId),
    ClassDeclaration(ClassDeclarationId),
    ClassMember(ClassMemberId),
    ValueSignature(ValueSignatureId),
    ValueEquation(ValueEquationId),
    ForeignData(ForeignDataId),
    ForeignValue(ForeignValueId),
    Operator(InfixId),
    Role(TypeRoleId),
    Term(TermItemId),
    Type(TypeItemId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExistingKind {
    Term(TermItemId),
    Type(TypeItemId),
}

#[derive(Debug, PartialEq, Eq)]
pub enum IndexingError {
    DuplicateItem { kind: ItemKind, existing: ExistingKind },
    MismatchedItem { kind: ItemKind, existing: ExistingKind },
    InvalidRole { id: TypeRoleId, existing: Option<TypeItemId> },
    InvalidExport { id: ExportItemId },
    DuplicateExport { id: ExportItemId, existing: ExportItemId },
    DuplicateImport { id: ImportItemId, existing: ImportItemId },
}
