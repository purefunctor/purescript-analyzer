use rowan::ast::AstNode;

use super::{Binder, Binding, Name, ZeroOrMore};

_create_ast_v!(
    Declaration,
    AnnotationDeclaration(AnnotationDeclaration),
    ForeignDataDeclaration(ForeignDataDeclaration),
    ValueDeclaration(ValueDeclaration)
);

_create_ast_v!(
    LetBinding,
    LetBindingName(LetBindingName),
    LetBindingPattern(LetBindingPattern),
    LetBindingSignature(LetBindingSignature)
);

impl AnnotationDeclaration {
    pub fn name(&self) -> Option<Name> {
        Name::cast(self.node.first_child()?)
    }
}

impl ForeignDataDeclaration {
    pub fn name(&self) -> Option<Name> {
        Name::cast(self.node.first_child()?)
    }
}

impl ValueDeclaration {
    pub fn name(&self) -> Option<Name> {
        Name::cast(self.node.first_child()?)
    }

    pub fn binders(&self) -> Option<ZeroOrMore<Binder>> {
        ZeroOrMore::cast(self.node.children().nth(1)?)
    }

    pub fn binding(&self) -> Option<Binding> {
        Binding::cast(self.node.last_child()?)
    }
}

impl LetBindingName {
    pub fn name(&self) -> Option<Name> {
        Name::cast(self.node.first_child()?)
    }

    pub fn binders(&self) -> Option<ZeroOrMore<Binder>> {
        ZeroOrMore::cast(self.node.children().nth(1)?)
    }

    pub fn binding(&self) -> Option<Binding> {
        Binding::cast(self.node.last_child()?)
    }
}

impl LetBindingSignature {
    pub fn name(&self) -> Option<Name> {
        Name::cast(self.node.first_child()?)
    }
}
