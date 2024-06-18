use rowan::ast::AstNode;

use crate::SyntaxToken;

use super::{ArgumentList, Name, QualifiedName};

_create_ast_v!(
    Binder,
    ConstructorBinder(ConstructorBinder),
    LiteralBinder(LiteralBinder),
    NegativeBinder(NegativeBinder),
    ParenthesizedBinder(ParenthesizedBinder),
    TypedBinder(TypedBinder),
    VariableBinder(VariableBinder),
    WildcardBinder(WildcardBinder)
);

_create_ast!(BinderList);

_has_children!(BinderList<Binder>);

impl ConstructorBinder {
    pub fn qualified_name(&self) -> Option<QualifiedName> {
        QualifiedName::cast(self.node.first_child()?)
    }

    pub fn fields(&self) -> Option<ArgumentList<Binder>> {
        ArgumentList::cast(self.node.children().nth(1)?)
    }
}

impl ParenthesizedBinder {
    pub fn binder(&self) -> Option<Binder> {
        Binder::cast(self.node.first_child()?)
    }
}

impl NegativeBinder {
    pub fn minus(&self) -> Option<SyntaxToken> {
        self.node.first_token()
    }

    pub fn literal(&self) -> Option<LiteralBinder> {
        LiteralBinder::cast(self.node.first_child()?)
    }
}

impl VariableBinder {
    pub fn name(&self) -> Option<Name> {
        Name::cast(self.node.first_child()?)
    }
}
