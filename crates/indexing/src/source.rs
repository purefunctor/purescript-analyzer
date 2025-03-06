//! Enables offset-independent annotations
//!
//! Text ranges are often used to annotate syntax nodes with additional
//! information. However, they're unsuitable for tracking information
//! across non-semantic changes such as editing whitespace or comments.
//!
//! The syntax tree can be thought of as a combination of two pieces of
//! information, offsets and structures. Non-semantic edits will always
//! change the offsets but only semantic edits can change the structure.
//!
//! The [`IndexingSource`] provides offset-independent indices [`Idx`]
//! which can be used in place of text ranges [`AstPtr`]. These indices
//! are solely dependent on the structure bit of the syntax tree—they're
//! unaffected by non-semantic edits in a source file.
//!
//! An incremental compiler such as this project takes advantage of this
//! property for caching. The information that it collects is entirely
//! dependent on the structure bit of a source file, eliminating most
//! cache misses arising from shifting offsets.
//!
//! [`Idx`]: la_arena::Idx
//! [`AstPtr`]: rowan::ast::AstPtr
use syntax::cst;

syntax::create_source! {
    pub struct IndexingSource {
        export: cst::ExportItem as ExportItem,
        import_statement: cst::ImportStatement as Import,

        data_signature: cst::DataSignature as DataSignature,
        data_equation: cst::DataEquation as DataEquation,
        data_constructor: cst::DataConstructor as DataConstructor,

        newtype_signature: cst::NewtypeSignature as NewtypeSignature,
        newtype_equation: cst::NewtypeEquation as NewtypeEquation,

        type_role: cst::TypeRoleDeclaration as TypeRole,
        type_signature: cst::TypeSynonymSignature as TypeSignature,
        type_equation: cst::TypeSynonymEquation as TypeEquation,

        class_signature: cst::ClassSignature as ClassSignature,
        class_declaration: cst::ClassDeclaration as ClassDeclaration,
        class_member: cst::ClassMemberStatement as ClassMember,

        value_signature: cst::ValueSignature as ValueSignature,
        value_equation: cst::ValueEquation as ValueEquation,

        chain: cst::InstanceChain as InstanceChain,
        instance: cst::InstanceDeclaration as Instance,
        instance_member: cst::InstanceMemberStatement as InstanceMember,
        derive: cst::DeriveDeclaration as Derive,

        infix: cst::InfixDeclaration as Infix,
        foreign_data: cst::ForeignImportDataDeclaration as ForeignData,
        foreign_value: cst::ForeignImportValueDeclaration as ForeignValue,
    }
}
