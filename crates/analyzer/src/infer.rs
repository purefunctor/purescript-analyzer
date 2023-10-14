use la_arena::Arena;
use rustc_hash::FxHashMap;
use syntax::ast;

use crate::{
    id::{AstId, InFile},
    lower::{
        self,
        visitor::{default_visit_binder, default_visit_expr, Visitor},
        Binder, BinderId, Expr, ExprId, ValueDeclarationData,
    },
    LowerDatabase,
};

#[salsa::query_group(InferStorage)]
pub trait InferDatabase: LowerDatabase {
    #[salsa::invoke(infer_value_declaration_query)]
    fn infer_value_declaration(&self, id: InFile<AstId<ast::ValueDeclaration>>) -> ();
}

fn infer_value_declaration_query(db: &dyn InferDatabase, id: InFile<AstId<ast::ValueDeclaration>>) {
    let value_declaration = db.lower_value_declaration(id);
    let mut context = InferValueDeclarationContext::new(db, id, &value_declaration);
    context.visit_value_declaration(&value_declaration);
    dbg!(&context.type_per_expr);
    dbg!(&context.type_per_binder);
    dbg!(&context.constraints);
    dbg!(&context.declaration_type);
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Type {
    Function(Box<Type>, Box<Type>),
    Literal(Literal),
    Unknown(Unknown),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Literal {
    Int,
    Number,
    String,
    Char,
    Boolean,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Unknown {
    Local(InFile<AstId<ast::ValueDeclaration>>, u32),
    Global(InFile<AstId<ast::ValueDeclaration>>),
}

struct InferValueDeclarationContext<'a> {
    db: &'a dyn InferDatabase,
    id: InFile<AstId<ast::ValueDeclaration>>,
    value_declaration: &'a ValueDeclarationData,

    constraints: Vec<(Type, Type)>,
    fresh_index: u32,
    type_per_expr: FxHashMap<ExprId, Type>,
    type_per_binder: FxHashMap<BinderId, Type>,
    declaration_type: Option<Type>,
}

impl<'a> InferValueDeclarationContext<'a> {
    fn new(
        db: &'a dyn InferDatabase,
        id: InFile<AstId<ast::ValueDeclaration>>,
        value_declaration: &'a ValueDeclarationData,
    ) -> InferValueDeclarationContext<'a> {
        let constraints = vec![];
        let fresh_index = 0;
        let type_per_expr = FxHashMap::default();
        let type_per_binder = FxHashMap::default();
        let declaration_type = None;
        InferValueDeclarationContext {
            db,
            id,
            value_declaration,
            constraints,
            fresh_index,
            type_per_expr,
            type_per_binder,
            declaration_type,
        }
    }

    fn fresh_unknown(&mut self) -> Type {
        let index = self.fresh_index;
        self.fresh_index += 1;
        Type::Unknown(Unknown::Local(self.id, index))
    }
}

impl<'a> Visitor<'a> for InferValueDeclarationContext<'a> {
    fn expr_arena(&self) -> &'a Arena<Expr> {
        &self.value_declaration.expr_arena
    }

    fn binder_arena(&self) -> &'a Arena<Binder> {
        &self.value_declaration.binder_arena
    }

    fn visit_expr(&mut self, expr_id: ExprId) {
        match &self.expr_arena()[expr_id] {
            Expr::Application(function, arguments) => match &arguments[..] {
                [argument] => {
                    // FIXME: Add return type for Visitor.
                    self.visit_expr(*function);
                    self.visit_expr(*argument);

                    let return_ty = self.fresh_unknown();

                    let function_ty = self.type_per_expr.get(function).unwrap().clone();
                    let argument_ty = self.type_per_expr.get(argument).unwrap().clone();

                    self.constraints.push((
                        function_ty,
                        Type::Function(Box::new(argument_ty), Box::new(return_ty.clone())).clone(),
                    ));

                    self.type_per_expr.insert(expr_id, return_ty);
                }
                _ => todo!("We haven't implemented this yet."),
            },
            Expr::LetIn(_, _) => default_visit_expr(self, expr_id),
            Expr::Literal(literal) => match literal {
                lower::Literal::Array(_) => default_visit_expr(self, expr_id),
                lower::Literal::Record(_) => default_visit_expr(self, expr_id),
                lower::Literal::Int(_) => {
                    self.type_per_expr.insert(expr_id, Type::Literal(Literal::Int));
                }
                lower::Literal::Number(_) => {
                    self.type_per_expr.insert(expr_id, Type::Literal(Literal::Number));
                }
                lower::Literal::String(_) => {
                    self.type_per_expr.insert(expr_id, Type::Literal(Literal::String));
                }
                lower::Literal::Char(_) => {
                    self.type_per_expr.insert(expr_id, Type::Literal(Literal::Char));
                }
                lower::Literal::Boolean(_) => {
                    self.type_per_expr.insert(expr_id, Type::Literal(Literal::Boolean));
                }
            },
            Expr::Variable(qualified) => {
                let nominal_map = self.db.nominal_map(self.id.file_id);
                let id = nominal_map.get_value(&qualified.value).unwrap()[0];
                let variable_ty = Type::Unknown(Unknown::Global(id));
                self.type_per_expr.insert(expr_id, variable_ty);
            }
        }
    }

    fn visit_binder(&mut self, binder_id: BinderId) {
        match &self.binder_arena()[binder_id] {
            Binder::Wildcard => {
                let binder_ty = self.fresh_unknown();
                self.type_per_binder.insert(binder_id, binder_ty);
            }
            _ => default_visit_binder(self, binder_id),
        }
    }

    fn visit_value_declaration(&mut self, value_declaration: &'a ValueDeclarationData) {
        for binder_id in value_declaration.binders.iter() {
            self.visit_binder(*binder_id);
        }
        self.visit_binding(&value_declaration.binding);

        let binder_ty = match &value_declaration.binders[..] {
            [binder_id] => self.type_per_binder.get(binder_id).unwrap().clone(),
            _ => todo!("We don't support this yet."),
        };

        let binding_ty = match &value_declaration.binding {
            lower::Binding::Unconditional { where_expr } => {
                self.type_per_expr.get(&where_expr.expr_id).unwrap().clone()
            }
        };

        self.declaration_type = Some(Type::Function(Box::new(binder_ty), Box::new(binding_ty)));
    }
}
