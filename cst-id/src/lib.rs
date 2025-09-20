use rowan::{WalkEvent, ast::AstNode};
use syntax::{SyntaxNode, cst};

syntax::create_source! {
    pub struct CstId {
        export_item: cst::ExportItem as ExportItem,
        import_item: cst::ImportItem as ImportItem,

        import_statement: cst::ImportStatement as Import,
        declaration: cst::Declaration as Declaration,

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

        binder: cst::Binder as Binder,
        expression: cst::Expression as Expression,
        r#type: cst::Type as Type,

        type_variable_binding: cst::TypeVariableBinding as TypeVariableBinding,
        do_statement: cst::DoStatement as DoStatement,

        let_binding_signature: cst::LetBindingSignature as LetBindingSignature,
        let_binding_equation: cst::LetBindingEquation as LetBindingEquation,
        instance_signature_statement: cst::InstanceSignatureStatement as InstanceSignature,
        instance_equation_statement: cst::InstanceEquationStatement as InstanceEquation,

        term_operator: cst::TermOperator as TermOperator,
        type_operator: cst::TypeOperator as TypeOperator,
    }
}

macro_rules! cast {
    ($accumulator:expr, $node:expr, $trait:path, $name_0:ident: $cst_0:path, $($name:ident: $cst:path $(,)?)*) => {{
        const _ASSERT: fn() = || {
            fn implements<T: $trait>() {}
            implements::<$cst_0>();
            $(
                implements::<$cst>();
            )*
        };

        paste::paste! {
            let kind = $node.kind();
            if $cst_0::can_cast(kind) {
                let Some(cst) = <$cst_0>::cast(SyntaxNode::clone(&$node)) else {
                    continue;
                };
                $accumulator.[<allocate_ $name_0>](&cst);
            }
            $(
                else if $cst::can_cast(kind) {
                    let Some(cst) = <$cst>::cast(SyntaxNode::clone(&$node)) else {
                        continue;
                    };
                    $accumulator.[<allocate_ $name>](&cst);
                }
            )*
        }
    }};
}

pub fn cst_id(node: &SyntaxNode) -> CstId {
    let mut accumulator = CstId::default();

    for event in node.preorder() {
        let WalkEvent::Enter(node) = event else {
            continue;
        };

        cast!(accumulator, node, cst::IsCstEnum,
            binder: cst::Binder,
            expression: cst::Expression,
            r#type: cst::Type,

            declaration: cst::Declaration,
            do_statement: cst::DoStatement,
            instance_member: cst::InstanceMemberStatement,

            export_item: cst::ExportItem,
            import_item: cst::ImportItem,
        );

        cast!(accumulator, node, cst::IsCstStruct,
            term_operator: cst::TermOperator,
            type_operator: cst::TypeOperator,

            type_variable_binding: cst::TypeVariableBinding,

            let_binding_signature: cst::LetBindingSignature,
            let_binding_equation: cst::LetBindingEquation,

            instance_signature_statement: cst::InstanceSignatureStatement,
            instance_equation_statement: cst::InstanceEquationStatement,

            import_statement: cst::ImportStatement,
            data_signature: cst::DataSignature,
            data_equation: cst::DataEquation,
            data_constructor: cst::DataConstructor,

            newtype_signature: cst::NewtypeSignature,
            newtype_equation: cst::NewtypeEquation,

            type_role: cst::TypeRoleDeclaration,
            type_signature: cst::TypeSynonymSignature,
            type_equation: cst::TypeSynonymEquation,

            class_signature: cst::ClassSignature,
            class_declaration: cst::ClassDeclaration,
            class_member: cst::ClassMemberStatement,

            value_signature: cst::ValueSignature,
            value_equation: cst::ValueEquation,

            chain: cst::InstanceChain,
            instance: cst::InstanceDeclaration,
            derive: cst::DeriveDeclaration,

            infix: cst::InfixDeclaration,
            foreign_data: cst::ForeignImportDataDeclaration,
            foreign_value: cst::ForeignImportValueDeclaration,
        );
    }

    accumulator
}
