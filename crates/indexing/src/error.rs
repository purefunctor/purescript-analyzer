use crate::{ClassMemberId, ConstructorId, DeclarationId, ExprItemId, InstanceId, SourceMap, TypeItemId};

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
    InvalidOrder { early: DeclarationId, late: DeclarationId },
    NonConsecutive { first: DeclarationId, second: DeclarationId },
}

impl IndexingError {
    pub fn message(&self) -> String {
        match &self {
            IndexingError::DuplicateExprItem { item_id, duplicate } => {
                format!("Duplicate expression item: {:?} and {:?}", item_id, duplicate)
            }
            IndexingError::DuplicateTypeItem { item_id, duplicate } => {
                format!("Duplicate type item: {:?} and {:?}", item_id, duplicate)
            }
            IndexingError::EmptySignature { signature } => {
                format!("Empty signature for {:?}", signature)
            }
            IndexingError::EmptyRole { role } => {
                format!("Empty role: {:?}", role)
            }
            IndexingError::DuplicateDeclaration { declaration, duplicate } => {
                format!("Duplicate declaration: {:?} and {:?}", declaration, duplicate)
            }
            IndexingError::DuplicateSignature { signature, duplicate } => {
                format!("Duplicate signature: {:?} and {:?}", signature, duplicate)
            }
            IndexingError::InvalidRole { role } => {
                format!("Invalid role: {:?}", role)
            }
            IndexingError::InvalidOrder { early, late } => {
                format!("Invalid order: {:?} and {:?}", early, late)
            }
            IndexingError::NonConsecutive { first, second } => {
                format!("Non-consecutive: {:?} and {:?}", first, second)
            }
        }
    }
}

/// The kind of a duplicate item.
#[derive(Debug, PartialEq, Eq)]
pub enum Duplicate {
    Constructor(ConstructorId),
    ClassMember(ClassMemberId),
    Instance(InstanceId),
    Declaration(DeclarationId),
}
