use std::sync::Arc;

use rowan::{ast::AstNode, NodeOrToken};
use smol_str::SmolStr;
use syntax::{ast, SyntaxKind};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Module {
    body: Arc<[Declaration]>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Declaration {
    ValueDeclaration { name: SmolStr, binders: Arc<[Binder]>, binding: Binding },
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Binding {
    // FIXME: add guarded expressions
    UnconditionalBinding { expr: WhereExpr },
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct WhereExpr {
    // FIXME: add let bindings
    expr: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Expr {
    Literal(LiteralExpr),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum LiteralExpr {
    Int(usize),
    Number(SmolStr),
    String(SmolStr),
    Char(SmolStr),
    Boolean(bool),
    Array(Arc<[Expr]>),
    Record(Arc<[RecordItem<Expr>]>),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum RecordItem<T> {
    RecordPun(SmolStr),
    RecordField(SmolStr, T),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Binder {
    Literal(LiteralBinder),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum LiteralBinder {
    Int(bool, usize),
    Number(bool, SmolStr),
    String(SmolStr),
    Char(SmolStr),
    Boolean(bool),
    Array(Arc<[Binder]>),
    Record(Arc<[RecordItem<Binder>]>),
}

pub fn lower_module(t: ast::Module) -> Option<Module> {
    let body = t.body()?.declarations()?.children().filter_map(|t| lower_declaration(t)).collect();
    Some(Module { body })
}

pub fn lower_declaration(t: ast::Declaration) -> Option<Declaration> {
    match t {
        ast::Declaration::AnnotationDeclaration(_) => None,
        ast::Declaration::ValueDeclaration(t) => {
            let name = t.name()?.as_str()?;
            let binders = t.binders()?.children().filter_map(|t| lower_binder(t)).collect();
            let binding = lower_binding(t.binding()?)?;
            Some(Declaration::ValueDeclaration { name, binders, binding })
        }
    }
}

pub fn lower_binding(t: ast::Binding) -> Option<Binding> {
    match t {
        ast::Binding::UnconditionalBinding(t) => {
            let expr = lower_where_expr(t.where_expression()?)?;
            Some(Binding::UnconditionalBinding { expr })
        }
        ast::Binding::GuardedBinding(_) => None,
    }
}

pub fn lower_where_expr(t: ast::WhereExpression) -> Option<WhereExpr> {
    let expr = lower_expr(t.expression()?)?;
    Some(WhereExpr { expr })
}

pub fn lower_expr(t: ast::Expression) -> Option<Expr> {
    match t {
        ast::Expression::LiteralExpression(t) => Some(Expr::Literal(lower_literal_expr(t)?)),
        _ => None,
    }
}

pub fn lower_literal_expr(t: ast::LiteralExpression) -> Option<LiteralExpr> {
    match t.syntax().first_child_or_token()? {
        NodeOrToken::Node(n) => match n.kind() {
            SyntaxKind::LiteralArray => {
                let literal =
                    ast::Wrapped::<ast::Separated<ast::Expression>>::cast(n.first_child()?)?;

                let contents = if let Some(separated) = literal.child() {
                    separated.children().filter_map(|t| lower_expr(t)).collect()
                } else {
                    vec![].into()
                };

                Some(LiteralExpr::Array(contents))
            }
            SyntaxKind::LiteralRecord => {
                let literal =
                    ast::Wrapped::<ast::Separated<ast::RecordItem>>::cast(n.first_child()?)?;

                let contents = if let Some(separated) = literal.child() {
                    separated
                        .children()
                        .filter_map(|t| match t {
                            ast::RecordItem::RecordField(t) => {
                                let name = t.name()?.as_str()?;
                                let value = lower_expr(t.value()?)?;
                                Some(RecordItem::RecordField(name, value))
                            }
                            ast::RecordItem::RecordPun(t) => {
                                let name = t.name_ref()?.as_str()?;
                                Some(RecordItem::RecordPun(name))
                            }
                        })
                        .collect()
                } else {
                    vec![].into()
                };

                Some(LiteralExpr::Record(contents))
            }
            _ => None,
        },
        NodeOrToken::Token(t) => match t.kind() {
            SyntaxKind::LiteralInteger => Some(LiteralExpr::Int(t.text().parse().ok()?)),
            SyntaxKind::LiteralNumber => Some(LiteralExpr::Number(t.text().into())),
            SyntaxKind::LiteralString | SyntaxKind::LiteralRawString => {
                Some(LiteralExpr::String(t.text().into()))
            }
            SyntaxKind::LiteralChar => Some(LiteralExpr::Char(t.text().into())),
            SyntaxKind::LiteralTrue => Some(LiteralExpr::Boolean(true)),
            SyntaxKind::LiteralFalse => Some(LiteralExpr::Boolean(false)),
            _ => None,
        },
    }
}

pub fn lower_binder(t: ast::Binder) -> Option<Binder> {
    match t {
        ast::Binder::LiteralBinder(t) => Some(Binder::Literal(lower_literal_binder(t)?)),
        _ => None,
    }
}

pub fn lower_literal_binder(t: ast::LiteralBinder) -> Option<LiteralBinder> {
    match t.syntax().first_child_or_token()? {
        NodeOrToken::Node(n) => match n.kind() {
            SyntaxKind::LiteralArray => {
                let literal = ast::Wrapped::<ast::Separated<ast::Binder>>::cast(n.first_child()?)?;

                let contents = if let Some(separated) = literal.child() {
                    separated.children().filter_map(|t| lower_binder(t)).collect()
                } else {
                    vec![].into()
                };

                Some(LiteralBinder::Array(contents))
            }
            SyntaxKind::LiteralRecord => {
                let literal =
                    ast::Wrapped::<ast::Separated<ast::RecordItem>>::cast(n.first_child()?)?;

                let contents = if let Some(separated) = literal.child() {
                    separated
                        .children()
                        .filter_map(|t| match t {
                            ast::RecordItem::RecordField(t) => {
                                let name = t.name()?.as_str()?;
                                let value = lower_binder(t.value()?)?;
                                Some(RecordItem::RecordField(name, value))
                            }
                            ast::RecordItem::RecordPun(t) => {
                                let name = t.name_ref()?.as_str()?;
                                Some(RecordItem::RecordPun(name))
                            }
                        })
                        .collect()
                } else {
                    vec![].into()
                };

                Some(LiteralBinder::Record(contents))
            }
            _ => None,
        },
        NodeOrToken::Token(t) => match t.kind() {
            SyntaxKind::LiteralInteger => Some(LiteralBinder::Int(false, t.text().parse().ok()?)),
            SyntaxKind::LiteralNumber => Some(LiteralBinder::Number(false, t.text().into())),
            SyntaxKind::LiteralString | SyntaxKind::LiteralRawString => {
                Some(LiteralBinder::String(t.text().into()))
            }
            SyntaxKind::LiteralChar => Some(LiteralBinder::Char(t.text().into())),
            SyntaxKind::LiteralTrue => Some(LiteralBinder::Boolean(true)),
            SyntaxKind::LiteralFalse => Some(LiteralBinder::Boolean(false)),
            _ => None,
        },
    }
}
