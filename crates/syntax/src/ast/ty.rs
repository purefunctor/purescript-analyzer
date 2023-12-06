//! of the Kingdom

use rowan::ast::AstNode;

use super::{NameRef, OneOrMore, QualifiedName};

_create_ast_v!(
    Type,
    ApplicationType(ApplicationType),
    ArrowType(ArrowType),
    ConstructorType(ConstructorType),
    IntegerType(IntegerType),
    KindedType(KindedType),
    OperatorNameType(OperatorNameType),
    ParenthesizedType(ParenthesizedType),
    RecordType(RecordType),
    RowType(RowType),
    StringType(StringType),
    TypeOperatorChain(TypeOperatorChain),
    VariableType(VariableType),
    WildcardType(WildcardType)
);

impl ArrowType {
    pub fn argument(&self) -> Option<Type> {
        Type::cast(self.node.first_child()?)
    }

    pub fn result(&self) -> Option<Type> {
        Type::cast(self.node.last_child()?)
    }
}

impl ApplicationType {
    pub fn head(&self) -> Option<Type> {
        Type::cast(self.node.first_child()?)
    }

    pub fn spine(&self) -> Option<OneOrMore<Type>> {
        OneOrMore::cast(self.node.last_child()?)
    }
}

impl ConstructorType {
    pub fn qualified_name(&self) -> Option<QualifiedName> {
        QualifiedName::cast(self.node.first_child()?)
    }
}

impl ParenthesizedType {
    pub fn ty(&self) -> Option<Type> {
        Type::cast(self.node.first_child()?)
    }
}

impl VariableType {
    pub fn name_ref(&self) -> Option<NameRef> {
        NameRef::cast(self.node.first_child()?)
    }
}
