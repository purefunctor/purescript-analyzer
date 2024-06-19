//! For implementing generic traversals.

use super::tree::*;

pub trait Visitor<'ast>: Sized {
    fn arena(&self) -> &'ast SurfaceArena;

    fn visit_binder(&mut self, binder_id: BinderId) {
        default_visit_binder(self, binder_id);
    }

    fn visit_expr(&mut self, expr_id: ExprId) {
        default_visit_expr(self, expr_id);
    }

    fn visit_let_bindings(&mut self, let_bindings: &[LetBinding]) {
        default_visit_let_bindings(self, let_bindings);
    }

    fn visit_type(&mut self, type_id: TypeId) {
        default_visit_type(self, type_id);
    }
}

pub fn default_visit_binder<'ast, V>(visitor: &mut V, binder_id: BinderId)
where
    V: Visitor<'ast>,
{
    let arena = visitor.arena();
    match &arena[binder_id] {
        Binder::Constructor { fields, .. } => {
            for field in fields {
                visitor.visit_binder(*field);
            }
        }
        Binder::Literal(literal) => match literal {
            Literal::Array(items) => {
                for item in items {
                    visitor.visit_binder(*item);
                }
            }
            Literal::Record(items) => {
                for item in items {
                    if let RecordItem::RecordField(_, value) = item {
                        visitor.visit_binder(*value)
                    }
                }
            }
            _ => (),
        },
        Binder::Negative(_) => (),
        Binder::Parenthesized(parenthesized) => visitor.visit_binder(*parenthesized),
        Binder::Variable(_) => (),
        Binder::Wildcard => (),
        Binder::NotImplemented => (),
    }
}

pub fn default_visit_expr<'ast, V>(visitor: &mut V, expr_id: ExprId)
where
    V: Visitor<'ast>,
{
    let arena = visitor.arena();
    match &arena[expr_id] {
        Expr::Application(function, arguments) => {
            visitor.visit_expr(*function);
            for argument in arguments {
                visitor.visit_expr(*argument);
            }
        }
        Expr::Constructor(_) => (),
        Expr::Lambda(binders, body) => {
            for binder in binders {
                visitor.visit_binder(*binder);
            }
            visitor.visit_expr(*body);
        }
        Expr::LetIn(_, body) => {
            visitor.visit_expr(*body);
        }
        Expr::Literal(literal) => match literal {
            Literal::Array(items) => {
                for item in items {
                    visitor.visit_expr(*item);
                }
            }
            Literal::Record(items) => {
                for item in items {
                    if let RecordItem::RecordField(_, value) = item {
                        visitor.visit_expr(*value)
                    }
                }
            }
            _ => (),
        },
        Expr::Variable(_) => (),
        Expr::NotImplemented => (),
    }
}

pub fn default_visit_let_bindings<'ast, V>(visitor: &mut V, let_bindings: &[LetBinding])
where
    V: Visitor<'ast>,
{
    let arena = visitor.arena();
    for let_binding in let_bindings {
        match let_binding {
            LetBinding::Name { id } => {
                let let_name = &arena[*id];
                if let Some(annotation) = let_name.annotation {
                    visitor.visit_type(annotation);
                }
                for equation in &let_name.equations {
                    for binder in &equation.binders {
                        visitor.visit_binder(*binder);
                    }
                    match &equation.binding {
                        Binding::Unconditional { where_expr } => {
                            visitor.visit_let_bindings(&where_expr.let_bindings);
                            visitor.visit_expr(where_expr.expr_id);
                        }
                        Binding::Guarded { guarded_exprs } => {
                            for guarded_expr in guarded_exprs {
                                for pattern_guard in &guarded_expr.pattern_guards {
                                    if let Some(binder_id) = pattern_guard.binder_id {
                                        visitor.visit_binder(binder_id);
                                    }
                                    visitor.visit_expr(pattern_guard.expr_id);
                                }
                                visitor.visit_let_bindings(&guarded_expr.where_expr.let_bindings);
                                visitor.visit_expr(guarded_expr.where_expr.expr_id);
                            }
                        }
                    }
                }
            }
            LetBinding::Pattern { binder, where_expr } => {
                visitor.visit_binder(*binder);
                visitor.visit_let_bindings(&where_expr.let_bindings);
                visitor.visit_expr(where_expr.expr_id);
            }
        }
    }
}

pub fn default_visit_type<'ast, V>(visitor: &mut V, type_id: TypeId)
where
    V: Visitor<'ast>,
{
    let arena = visitor.arena();
    match &arena[type_id] {
        Type::Arrow(arguments, result) => {
            for argument in arguments {
                visitor.visit_type(*argument);
            }
            visitor.visit_type(*result);
        }
        Type::Application(function, arguments) => {
            visitor.visit_type(*function);
            for argument in arguments {
                visitor.visit_type(*argument);
            }
        }
        Type::Constrained(constraint, constrained) => {
            visitor.visit_type(*constraint);
            visitor.visit_type(*constrained);
        }
        Type::Constructor(_) => (),
        Type::Forall(_, inner) => {
            visitor.visit_type(*inner);
        }
        Type::Parenthesized(parenthesized) => visitor.visit_type(*parenthesized),
        Type::Variable(_) => (),
        Type::NotImplemented => (),
    }
}
