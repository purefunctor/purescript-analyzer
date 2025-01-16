use crate::{ClassMemberId, ConstructorId, DeclarationId, ExprItemId, InstanceId, TypeItemId};

/// Errors emitted during indexing.
#[derive(Debug, PartialEq, Eq)]
pub enum IndexingError {
    DuplicateExprItem { item_id: ExprItemId, duplicate: Duplicate },
    DuplicateTypeItem { item_id: TypeItemId, duplicate: Duplicate },

    EmptyRole { role: DeclarationId },
    EmptySignature { signature: DeclarationId },

    EarlyDeclaration { declaration: DeclarationId, signature: DeclarationId },
    EarlyRole { role: DeclarationId },

    DuplicateDeclaration { declaration: DeclarationId },
    DuplicateSignature { signature: DeclarationId },
}

/// The kind of a duplicate item.
#[derive(Debug, PartialEq, Eq)]
pub enum Duplicate {
    Constructor(ConstructorId),
    ClassMember(ClassMemberId),
    Instance(InstanceId),
    Declaration(DeclarationId),
}
