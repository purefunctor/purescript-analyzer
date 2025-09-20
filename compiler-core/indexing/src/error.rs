use rowan::TextRange;
use syntax::SyntaxNodePtr;

use crate::{FullIndexedModule, TermItemId, TermItemKind, TypeItemId, source::*};

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

impl FullIndexedModule {
    pub fn item_kind_range(&self, kind: ItemKind) -> TextRange {
        match kind {
            ItemKind::DataSignature(i) => self.source[i].syntax_node_ptr().text_range(),
            ItemKind::DataEquation(i) => self.source[i].syntax_node_ptr().text_range(),
            ItemKind::NewtypeSignature(i) => self.source[i].syntax_node_ptr().text_range(),
            ItemKind::NewtypeEquation(i) => self.source[i].syntax_node_ptr().text_range(),
            ItemKind::Constructor(i) => self.source[i].syntax_node_ptr().text_range(),
            ItemKind::SynonymSignature(i) => self.source[i].syntax_node_ptr().text_range(),
            ItemKind::SynonymEquation(i) => self.source[i].syntax_node_ptr().text_range(),
            ItemKind::ClassSignature(i) => self.source[i].syntax_node_ptr().text_range(),
            ItemKind::ClassDeclaration(i) => self.source[i].syntax_node_ptr().text_range(),
            ItemKind::ClassMember(i) => self.source[i].syntax_node_ptr().text_range(),
            ItemKind::ValueSignature(i) => self.source[i].syntax_node_ptr().text_range(),
            ItemKind::ValueEquation(i) => self.source[i].syntax_node_ptr().text_range(),
            ItemKind::ForeignData(i) => self.source[i].syntax_node_ptr().text_range(),
            ItemKind::ForeignValue(i) => self.source[i].syntax_node_ptr().text_range(),
            ItemKind::Operator(i) => self.source[i].syntax_node_ptr().text_range(),
            ItemKind::Role(i) => self.source[i].syntax_node_ptr().text_range(),
            ItemKind::Term(i) => self.term_item_range(i),
            ItemKind::Type(i) => self.type_item_range(i),
        }
    }

    pub fn existing_kind_range(&self, kind: ExistingKind) -> TextRange {
        match kind {
            ExistingKind::Term(i) => self.term_item_range(i),
            ExistingKind::Type(i) => self.type_item_range(i),
        }
    }

    pub fn term_item_range(&self, id: TermItemId) -> TextRange {
        let item = &self.items.terms[id];
        match &item.kind {
            TermItemKind::ClassMember { id } => self.source[*id].syntax_node_ptr().text_range(),
            TermItemKind::Constructor { id } => self.source[*id].syntax_node_ptr().text_range(),
            TermItemKind::Derive { id } => self.source[*id].syntax_node_ptr().text_range(),
            TermItemKind::Foreign { id } => self.source[*id].syntax_node_ptr().text_range(),
            TermItemKind::Instance { id } => self.source[*id].syntax_node_ptr().text_range(),
            TermItemKind::Operator { id } => self.source[*id].syntax_node_ptr().text_range(),
            TermItemKind::Value { signature, equations } => {
                let signature = signature.map(|id| self.source[id].syntax_node_ptr().text_range());
                let equations =
                    equations.last().map(|&id| self.source[id].syntax_node_ptr().text_range());
                signature
                    .zip(equations)
                    .map(|(signature, equations)| signature.cover(equations))
                    .expect("invariant violated: no signature and no equation")
            }
        }
    }

    pub fn term_item_ptr(&self, id: TermItemId) -> Vec<SyntaxNodePtr> {
        let item = &self.items[id];
        match &item.kind {
            TermItemKind::ClassMember { id } => vec![self.source[*id].syntax_node_ptr()],
            TermItemKind::Constructor { id } => vec![self.source[*id].syntax_node_ptr()],
            TermItemKind::Derive { id } => vec![self.source[*id].syntax_node_ptr()],
            TermItemKind::Foreign { id } => vec![self.source[*id].syntax_node_ptr()],
            TermItemKind::Instance { id } => vec![self.source[*id].syntax_node_ptr()],
            TermItemKind::Operator { id } => vec![self.source[*id].syntax_node_ptr()],
            TermItemKind::Value { signature, equations } => {
                let signature = signature.map(|id| self.source[id].syntax_node_ptr()).into_iter();
                let equations = equations.iter().map(|&id| self.source[id].syntax_node_ptr());
                signature.chain(equations).collect()
            }
        }
    }

    pub fn type_item_range(&self, id: TypeItemId) -> TextRange {
        let item = &self.items.types[id];
        match &item.kind {
            crate::TypeItemKind::Data { signature, equation, role } => {
                let signature = signature.map(|id| self.source[id].syntax_node_ptr().text_range());
                let equation = equation.map(|id| self.source[id].syntax_node_ptr().text_range());
                let role = role.map(|id| self.source[id].syntax_node_ptr().text_range());
                [signature, equation, role]
                    .into_iter()
                    .flatten()
                    .reduce(|init, next| init.cover(next))
                    .expect("invariant violated: no signature, equation, or role")
            }
            crate::TypeItemKind::Newtype { signature, equation, role } => {
                let signature = signature.map(|id| self.source[id].syntax_node_ptr().text_range());
                let equation = equation.map(|id| self.source[id].syntax_node_ptr().text_range());
                let role = role.map(|id| self.source[id].syntax_node_ptr().text_range());
                [signature, equation, role]
                    .into_iter()
                    .flatten()
                    .reduce(|init, next| init.cover(next))
                    .expect("invariant violated: no signature, equation, or role")
            }
            crate::TypeItemKind::Synonym { signature, equation } => {
                let signature = signature.map(|id| self.source[id].syntax_node_ptr().text_range());
                let equation = equation.map(|id| self.source[id].syntax_node_ptr().text_range());
                [signature, equation]
                    .into_iter()
                    .flatten()
                    .reduce(|init, next| init.cover(next))
                    .expect("invariant violated: no signature or equation")
            }
            crate::TypeItemKind::Class { signature, declaration } => {
                let signature = signature.map(|id| self.source[id].syntax_node_ptr().text_range());
                let equation = declaration.map(|id| self.source[id].syntax_node_ptr().text_range());
                [signature, equation]
                    .into_iter()
                    .flatten()
                    .reduce(|init, next| init.cover(next))
                    .expect("invariant violated: no signature or declaration")
            }
            crate::TypeItemKind::Foreign { id, .. } => {
                self.source[*id].syntax_node_ptr().text_range()
            }
            crate::TypeItemKind::Operator { id } => self.source[*id].syntax_node_ptr().text_range(),
        }
    }

    pub fn type_item_ptr(&self, id: TypeItemId) -> Vec<SyntaxNodePtr> {
        let item = &self.items.types[id];
        match &item.kind {
            crate::TypeItemKind::Data { signature, equation, role } => {
                let signature = signature.map(|id| self.source[id].syntax_node_ptr());
                let equation = equation.map(|id| self.source[id].syntax_node_ptr());
                let role = role.map(|id| self.source[id].syntax_node_ptr());
                [signature, equation, role].into_iter().flatten().collect()
            }
            crate::TypeItemKind::Newtype { signature, equation, role } => {
                let signature = signature.map(|id| self.source[id].syntax_node_ptr());
                let equation = equation.map(|id| self.source[id].syntax_node_ptr());
                let role = role.map(|id| self.source[id].syntax_node_ptr());
                [signature, equation, role].into_iter().flatten().collect()
            }
            crate::TypeItemKind::Synonym { signature, equation } => {
                let signature = signature.map(|id| self.source[id].syntax_node_ptr());
                let equation = equation.map(|id| self.source[id].syntax_node_ptr());
                [signature, equation].into_iter().flatten().collect()
            }
            crate::TypeItemKind::Class { signature, declaration } => {
                let signature = signature.map(|id| self.source[id].syntax_node_ptr());
                let equation = declaration.map(|id| self.source[id].syntax_node_ptr());
                [signature, equation].into_iter().flatten().collect()
            }
            crate::TypeItemKind::Foreign { id, .. } => vec![self.source[*id].syntax_node_ptr()],
            crate::TypeItemKind::Operator { id } => vec![self.source[*id].syntax_node_ptr()],
        }
    }

    pub fn export_range(&self, id: ExportItemId) -> TextRange {
        self.source[id].syntax_node_ptr().text_range()
    }

    pub fn import_range(&self, id: ImportItemId) -> TextRange {
        self.source[id].syntax_node_ptr().text_range()
    }
}
