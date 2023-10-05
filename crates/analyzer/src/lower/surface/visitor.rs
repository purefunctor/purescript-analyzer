use la_arena::Arena;

use super::{Binder, BinderId, Binding, Expr, ExprId, Literal, ValueDeclarationData};

pub trait Visitor<'a>: Sized {
    fn expr_arena(&self) -> &'a Arena<Expr>;

    fn binder_arena(&self) -> &'a Arena<Binder>;

    fn visit_expr(&mut self, expr_id: ExprId) {
        default_visit_expr(self, expr_id);
    }

    fn visit_binder(&mut self, binder_id: BinderId) {
        default_visit_binder(self, binder_id);
    }

    fn visit_value_declaration(&mut self, value_declaration: &'a ValueDeclarationData) {
        default_visit_value_declaration(self, value_declaration);
    }
}

pub fn default_visit_expr<'a, V>(visitor: &mut V, expr_id: ExprId)
where
    V: Visitor<'a>,
{
    let expr_arena = visitor.expr_arena();
    match &expr_arena[expr_id] {
        Expr::Literal(literal) => match literal {
            Literal::Array(items) => items.iter().for_each(|item| {
                visitor.visit_expr(*item);
            }),
            Literal::Record(items) => {
                items.iter().for_each(|item| match item {
                    super::RecordItem::RecordPun(_) => (),
                    super::RecordItem::RecordField(_, value) => {
                        visitor.visit_expr(*value);
                    }
                });
            }
            _ => (),
        },
        Expr::Variable(_) => (),
    }
}

pub fn default_visit_binder<'a, V>(visitor: &mut V, binder_id: BinderId)
where
    V: Visitor<'a>,
{
    let binder_arena = visitor.binder_arena();
    match &binder_arena[binder_id] {
        Binder::Constructor { fields, .. } => {
            fields.iter().copied().for_each(|field| {
                visitor.visit_binder(field);
            });
        }
        Binder::Literal(literal) => match literal {
            Literal::Array(items) => {
                items.iter().copied().for_each(|item| {
                    visitor.visit_binder(item);
                });
            }
            Literal::Record(items) => {
                items.iter().for_each(|item| match item {
                    super::RecordItem::RecordPun(_) => (),
                    super::RecordItem::RecordField(_, value) => {
                        visitor.visit_binder(*value);
                    }
                });
            }
            _ => (),
        },
        Binder::Negative(_) => (),
        Binder::Parenthesized(parenthesized) => {
            visitor.visit_binder(*parenthesized);
        }
        Binder::Variable(_) => (),
        Binder::Wildcard => (),
    }
}

pub fn default_visit_value_declaration<'a, V>(
    visitor: &mut V,
    value_declaration: &'a ValueDeclarationData,
) where
    V: Visitor<'a>,
{
    for binder_id in value_declaration.binders.iter() {
        visitor.visit_binder(*binder_id);
    }
    match &value_declaration.binding {
        Binding::Unconditional { where_expr } => {
            visitor.visit_expr(where_expr.expr_id);
        }
    }
}
