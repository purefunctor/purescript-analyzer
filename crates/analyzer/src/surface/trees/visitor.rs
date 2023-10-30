use la_arena::Arena;

use super::{
    Binder, BinderId, Binding, Expr, ExprId, LetBinding, Literal, ValueDeclarationData, WhereExpr,
};

pub trait Visitor<'a>: Sized {
    fn expr_arena(&self) -> &'a Arena<Expr>;

    fn binder_arena(&self) -> &'a Arena<Binder>;

    fn visit_expr(&mut self, expr_id: ExprId) {
        default_visit_expr(self, expr_id);
    }

    fn visit_binder(&mut self, binder_id: BinderId) {
        default_visit_binder(self, binder_id);
    }

    fn visit_binding(&mut self, binding: &'a Binding) {
        default_visit_binding(self, binding);
    }

    fn visit_where_expr(&mut self, where_expr: &'a WhereExpr) {
        default_visit_where_expr(self, where_expr);
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
        Expr::Application(function, arguments) => {
            visitor.visit_expr(*function);
            for argument in arguments.iter() {
                visitor.visit_expr(*argument);
            }
        }
        Expr::Constructor(_) => (),
        Expr::Lambda(binders, body) => {
            for binder in binders.iter() {
                visitor.visit_binder(*binder);
            }
            visitor.visit_expr(*body);
        }
        Expr::LetIn(let_bindings, let_body) => {
            for let_binding in let_bindings.iter() {
                match let_binding {
                    LetBinding::Name { binding, .. } => match binding {
                        Binding::Unconditional { where_expr } => {
                            visitor.visit_expr(where_expr.expr_id);
                        }
                    },
                }
                visitor.visit_expr(*let_body);
            }
        }
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

pub fn default_visit_binding<'a, V>(visitor: &mut V, binding: &'a Binding)
where
    V: Visitor<'a>,
{
    match binding {
        Binding::Unconditional { where_expr } => {
            visitor.visit_where_expr(where_expr);
        }
    }
}

pub fn default_visit_where_expr<'a, V>(visitor: &mut V, where_expr: &'a WhereExpr)
where
    V: Visitor<'a>,
{
    for let_binding in where_expr.let_bindings.iter() {
        match let_binding {
            LetBinding::Name { binding, .. } => {
                visitor.visit_binding(binding);
            }
        }
    }
    visitor.visit_expr(where_expr.expr_id);
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
    visitor.visit_binding(&value_declaration.binding);
}
