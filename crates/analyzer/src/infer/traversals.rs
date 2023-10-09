use la_arena::Arena;
use smol_str::SmolStr;

use crate::{
    lower::{
        visitor::Visitor, Binder, Binding, Expr, ExprId, LetBinding, Literal, ValueDeclarationData,
        WhereExpr,
    },
    scope::ValueDeclarationScope,
    FxIndexMap,
};

pub(crate) struct InferValueDeclarationContext<'a> {
    expr_arena: &'a Arena<Expr>,
    binder_arena: &'a Arena<Binder>,
    value_declaration_scope: &'a ValueDeclarationScope,
    pub(crate) type_per_expr: FxIndexMap<ExprId, SmolStr>,
    pub(crate) type_per_name: FxIndexMap<SmolStr, SmolStr>,
}

impl<'a> InferValueDeclarationContext<'a> {
    pub(crate) fn new(
        value_declaration_data: &'a ValueDeclarationData,
        value_declaration_scope: &'a ValueDeclarationScope,
    ) -> InferValueDeclarationContext<'a> {
        let expr_arena = &value_declaration_data.expr_arena;
        let binder_arena = &value_declaration_data.binder_arena;
        // FIXME: IndexMap just makes it easier to debug...
        let type_per_expr = FxIndexMap::default();
        let type_per_name = FxIndexMap::default();
        InferValueDeclarationContext {
            expr_arena,
            binder_arena,
            value_declaration_scope,
            type_per_expr,
            type_per_name,
        }
    }
}

impl<'a> Visitor<'a> for InferValueDeclarationContext<'a> {
    fn expr_arena(&self) -> &'a Arena<Expr> {
        self.expr_arena
    }

    fn binder_arena(&self) -> &'a Arena<Binder> {
        self.binder_arena
    }

    fn visit_expr(&mut self, expr_id: ExprId) {
        match &self.expr_arena[expr_id] {
            Expr::LetIn(let_bindings, let_body) => {
                for let_binding in let_bindings.iter() {
                    match let_binding {
                        LetBinding::Name { name, binding } => match binding {
                            Binding::Unconditional { where_expr } => {
                                self.visit_expr(where_expr.expr_id);
                                if let Some(binding_ty) =
                                    self.type_per_expr.get(&where_expr.expr_id)
                                {
                                    self.type_per_name
                                        .insert(name.as_ref().into(), binding_ty.clone());
                                }
                            }
                        },
                    }
                }
                self.visit_expr(*let_body);
                if let Some(body_ty) = self.type_per_expr.get(let_body) {
                    self.type_per_expr.insert(expr_id, body_ty.clone());
                } else {
                    self.type_per_expr.insert(expr_id, "Unknown".into());
                }
            }
            Expr::Literal(literal) => match literal {
                Literal::Array(elements) => {
                    for element in elements.iter() {
                        self.visit_expr(*element);
                    }

                    let mut homogeneous = true;
                    let mut elements = elements.iter();
                    if let Some(head) = elements.next() {
                        if let Some(head_ty) = self.type_per_expr.get(head) {
                            for tail in elements {
                                if let Some(tail_ty) = self.type_per_expr.get(tail) {
                                    if head_ty != tail_ty {
                                        homogeneous = false;
                                        break;
                                    }
                                }
                            }

                            if homogeneous {
                                self.type_per_expr
                                    .insert(expr_id, format!("Array {}", head_ty).into());
                            } else {
                                self.type_per_expr.insert(expr_id, "Array Unknown".into());
                            }
                        }
                    } else {
                        self.type_per_expr.insert(expr_id, "Array Unknown".into());
                    }
                }
                Literal::Record(_) => (),
                Literal::Int(_) => {
                    self.type_per_expr.insert(expr_id, "Int".into());
                }
                Literal::Number(_) => {
                    self.type_per_expr.insert(expr_id, "Number".into());
                }
                Literal::String(_) => {
                    self.type_per_expr.insert(expr_id, "String".into());
                }
                Literal::Char(_) => {
                    self.type_per_expr.insert(expr_id, "Char".into());
                }
                Literal::Boolean(_) => {
                    self.type_per_expr.insert(expr_id, "Boolean".into());
                }
            },
            Expr::Variable(qualified) => {
                let scope_id = self.value_declaration_scope.expr_scope(expr_id);
                if let Some(_) = self.value_declaration_scope.resolve(scope_id, &qualified.value) {
                    if let Some(qualified_ty) = self.type_per_name.get(qualified.value.as_ref()) {
                        self.type_per_expr.insert(expr_id, qualified_ty.clone());
                    }
                }
            }
        }
    }

    fn visit_where_expr(&mut self, where_expr: &'a WhereExpr) {
        for let_binding in where_expr.let_bindings.iter() {
            match let_binding {
                LetBinding::Name { name, binding } => match binding {
                    Binding::Unconditional { where_expr } => {
                        self.visit_where_expr(where_expr);
                        if let Some(binding_ty) = self.type_per_expr.get(&where_expr.expr_id) {
                            self.type_per_name.insert(name.as_ref().into(), binding_ty.clone());
                        }
                    }
                },
            }
        }
        self.visit_expr(where_expr.expr_id);
    }
}
