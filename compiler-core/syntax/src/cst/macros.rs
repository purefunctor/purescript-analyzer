macro_rules! create_cst_struct {
    ($($kind:ident),* $(,)?) => {
        $(
            #[derive(Debug, Clone, PartialEq, Eq, Hash)]
            pub struct $kind {
                node: crate::SyntaxNode,
            }

            impl rowan::ast::AstNode for $kind {
                type Language = crate::PureScript;

                fn can_cast(kind: crate::SyntaxKind) -> bool
                where
                    Self: Sized,
                {
                    matches!(kind, crate::SyntaxKind::$kind)
                }

                fn cast(node: crate::SyntaxNode) -> Option<Self>
                where
                    Self: Sized,
                {
                    if Self::can_cast(node.kind()) {
                        Some(Self { node })
                    } else {
                        None
                    }
                }

                fn syntax(&self) -> &crate::SyntaxNode {
                    &self.node
                }
            }
        )+
    };
}

macro_rules! create_cst_enum {
    ($kind:ident | $key_0:ident$(|$key:ident)*) => {
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        pub enum $kind {
            $key_0($key_0),
            $(
                $key($key),
            )*
        }

        create_cst_struct!($key_0);
        $(
            create_cst_struct!($key);
        )*

        impl rowan::ast::AstNode for $kind {
            type Language = crate::PureScript;

            fn can_cast(kind: crate::SyntaxKind) -> bool
            where
                Self: Sized,
            {
                $key_0::can_cast(kind) $(|| $key::can_cast(kind))*
            }

            fn cast(node: crate::SyntaxNode) -> Option<Self>
            where
                Self: Sized,
            {
                if $key_0::can_cast(node.kind()) {
                    Some($kind::$key_0($key_0::cast(node)?))
                } $(else if $key::can_cast(node.kind()) {
                    Some($kind::$key($key::cast(node)?))
                })* else {
                    None
                }
            }

            fn syntax(&self) -> &crate::SyntaxNode {
                match self {
                    $kind::$key_0(t) => t.syntax(),
                    $(
                        $kind::$key(t) => t.syntax(),
                    )*
                }
            }
        }
    };
}

macro_rules! has_token {
    ($kind:ident $(|$name:ident() -> $token:ident)+) => {
        impl $kind {
            $(
                pub fn $name(&self) -> Option<crate::SyntaxToken> {
                    rowan::ast::support::token(self.syntax(), crate::SyntaxKind::$token)
                }
            )+
        }
    };
}

macro_rules! has_tokens {
    ($kind:ident $(|$name:ident() -> $token:ident)+) => {
        impl $kind {
            $(
                pub fn $name(&self) -> impl Iterator<Item = crate::SyntaxToken> {
                    self.syntax().children_with_tokens().filter_map(|n| {
                        let t = n.into_token()?;
                        if t.kind() == crate::SyntaxKind::UPPER {
                            Some(t)
                        } else {
                            None
                        }
                    })
                }
            )+
        }
    };
}

macro_rules! has_child {
    ($kind:ident $(|$name:ident() -> $child:ident)+) => {
        impl $kind {
            $(
                pub fn $name(&self) -> Option<crate::cst::$child> {
                    rowan::ast::support::child(self.syntax())
                }
            )+
        }
    };
}

macro_rules! has_children {
    ($kind:ident $(|$name:ident() -> $child:ident)+) => {
        impl $kind {
            $(
                pub fn $name(&self) -> rowan::ast::AstChildren<crate::cst::$child> {
                    rowan::ast::support::children(self.syntax())
                }
            )+
        }
    };
}
