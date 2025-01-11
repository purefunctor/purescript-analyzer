use crate::{ClassMemberId, ConstructorId, DeclarationId, ExprItemId, InstanceId, TypeItemId};

/// Errors emitted during indexing.
#[derive(Debug, PartialEq, Eq)]
pub enum IndexingError {
    LateSignature { declaration: DeclarationId, signature: DeclarationId },
    DuplicateExprItem { item_id: ExprItemId, duplicate: Duplicate },
    DuplicateTypeItem { item_id: TypeItemId, duplicate: Duplicate },
}

/// The kind of a duplicate item.
#[derive(Debug, PartialEq, Eq)]
pub enum Duplicate {
    Constructor(ConstructorId),
    ClassMember(ClassMemberId),
    Instance(InstanceId),
    Declaration(DeclarationId),
}
