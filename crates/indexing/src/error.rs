use crate::{
    ClassMemberId, ConstructorId, DeclarationId, ExportItemId, ExprItemId, InstanceId, TypeItemId,
};

/// Errors emitted during indexing.
#[derive(Debug, PartialEq, Eq)]
pub enum IndexingError {
    DuplicateExport { existing: ExportItemId, duplicate: ExportItemId },
    InvalidTypeItemExport { export_id: ExportItemId, type_id: TypeItemId },

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

/// The kind of a duplicate item.
#[derive(Debug, PartialEq, Eq)]
pub enum Duplicate {
    Constructor(ConstructorId),
    ClassMember(ClassMemberId),
    Instance(InstanceId),
    Declaration(DeclarationId),
}
