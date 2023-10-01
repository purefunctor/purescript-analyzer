use la_arena::Arena;

use super::{Binder, BinderId, Expr, ExprId, Literal};

pub trait Visitor<'a> {
    fn expr_arena(&self) -> &'a Arena<Expr>;

    fn binder_arena(&self) -> &'a Arena<Binder>;

    fn visit_expr(&mut self, expr_id: ExprId);

    fn visit_binder(&mut self, binder_id: BinderId);
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
