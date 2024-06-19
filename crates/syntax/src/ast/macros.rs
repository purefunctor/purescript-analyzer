macro_rules! _create_ast {
    ($($kind:ident),+) => {
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

macro_rules! _create_ast_t {
    ($($kind:ident),+) => {
        $(
            #[derive(Debug, Clone, PartialEq, Eq, Hash)]
            pub struct $kind<T> {
                node: crate::SyntaxNode,
                _kind: std::marker::PhantomData<fn() -> T>
            }

            impl<T> rowan::ast::AstNode for $kind<T> {
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
                        Some(Self { node, _kind: std::marker::PhantomData::default() })
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

macro_rules! _create_ast_t2 {
    ($($kind:ident),+) => {
        $(
            #[derive(Debug, Clone, PartialEq, Eq, Hash)]
            pub struct $kind<T, U> {
                node: crate::SyntaxNode,
                _kind_t: std::marker::PhantomData<fn() -> T>,
                _kind_u: std::marker::PhantomData<fn() -> U>,
            }

            impl<T, U> rowan::ast::AstNode for $kind<T, U> {
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
                        Some(Self { node, _kind_t: std::marker::PhantomData::default(), _kind_u: std::marker::PhantomData::default() })
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

macro_rules! _create_ast_v {
    ($kind:ident, $key_0:ident($value_0:ident)$(,$key:ident($value:ident))*) => {
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        pub enum $kind {
            $key_0($value_0),
            $(
                $key($value),
            )*
        }

        _create_ast!($value_0);
        $(
            _create_ast!($value);
        )*

        impl rowan::ast::AstNode for $kind {
            type Language = crate::PureScript;

            fn can_cast(kind: crate::SyntaxKind) -> bool
            where
                Self: Sized,
            {
                $value_0::can_cast(kind) $(|| $value::can_cast(kind))*
            }

            fn cast(node: crate::SyntaxNode) -> Option<Self>
            where
                Self: Sized,
            {
                if $value_0::can_cast(node.kind()) {
                    Some($kind::$key_0($value_0::cast(node)?))
                } $(else if $value::can_cast(node.kind()) {
                    Some($kind::$key($value::cast(node)?))
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

macro_rules! _has_children {
    ($kind_0:ident<$item_0:ident> $(,$kind:ident<$item:ident>)*) => {
        impl $kind_0 {
            pub fn children(&self) -> rowan::ast::AstChildren<$item_0> {
                rowan::ast::support::children(&self.node)
            }
        }

        $(
            impl $kind {
                pub fn children(&self) -> rowan::ast::AstChildren<$item> {
                    rowan::ast::support::children(&self.node)
                }
            }
        )*
    };
}
