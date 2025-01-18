use crate::{ClassMemberId, ConstructorId, DeclarationId, ExprItemId, InstanceId, TypeItemId};

/// Errors emitted during indexing.
#[derive(Debug, PartialEq, Eq)]
pub enum IndexingError {
    DuplicateExprItem { item_id: ExprItemId, duplicate: Duplicate },
    DuplicateTypeItem { item_id: TypeItemId, duplicate: Duplicate },

    EmptySignature { signature: DeclarationId },
    EmptyRole { role: DeclarationId },

    DuplicateDeclaration { declaration: DeclarationId, duplicate: DeclarationId },
    DuplicateSignature { signature: DeclarationId, duplicate: DeclarationId },

    InvalidRole { role: DeclarationId },
    NonConsecutive { before: DeclarationId, after: DeclarationId },
}

/// The kind of a duplicate item.
#[derive(Debug, PartialEq, Eq)]
pub enum Duplicate {
    Constructor(ConstructorId),
    ClassMember(ClassMemberId),
    Instance(InstanceId),
    Declaration(DeclarationId),
}
