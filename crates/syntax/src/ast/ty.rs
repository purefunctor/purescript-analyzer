//! of the Kingdom

use rowan::ast::AstNode;

use super::{OneOrMore, QualifiedName};

_create_ast_v!(
    Type,
    ApplicationType(ApplicationType),
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
