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

macro_rules! associated_declarations {
    ($kind:ident where $statement_0:ident$(| $statement:ident)*) => {
        paste::paste! {
            pub enum [<$kind Statement>] {
                $statement_0(crate::cst::$statement_0),
                $(
                    $statement(crate::cst::$statement),
                )*
            }

            impl rowan::ast::AstNode for [<$kind Statement>] {
                type Language = crate::PureScript;

                fn can_cast(kind: crate::SyntaxKind) -> bool
                where
                    Self: Sized,
                {
                    crate::cst::$statement_0::can_cast(kind) $(|| crate::cst::$statement::can_cast(kind))*
                }

                fn cast(node: crate::SyntaxNode) -> Option<Self>
                where
                    Self: Sized,
                {
                    if crate::cst::$statement_0::can_cast(node.kind()) {
                        Some([<$kind Statement>]::$statement_0(crate::cst::$statement_0::cast(node)?))
                    } $(else if crate::cst::$statement::can_cast(node.kind()) {
                        Some([<$kind Statement>]::$statement(crate::cst::$statement::cast(node)?))
                    })* else {
                        None
                    }
                }

                fn syntax(&self) -> &crate::SyntaxNode {
                    match self {
                        [<$kind Statement>]::$statement_0(t) => t.syntax(),
                        $(
                            [<$kind Statement>]::$statement(t) => t.syntax(),
                        )*
                    }
                }
            }
        }
    }
}
