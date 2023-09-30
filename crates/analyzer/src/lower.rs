//! Database for lowering the CST.

mod source_map;
pub mod surface;

use std::sync::Arc;

use la_arena::Arena;
use rowan::{
    ast::{AstNode, SyntaxNodePtr},
    NodeOrToken,
};
use syntax::{ast, SyntaxKind};

pub use source_map::*;
pub use surface::*;

use crate::{
    id::{AstId, InFile},
    names::{Name, Qualified},
    ResolverDatabase, SourceDatabase,
};

#[salsa::query_group(LowerStorage)]
pub trait LowerDatabase: SourceDatabase + ResolverDatabase {
    fn lower_value_declaration(
        &self,
        id: InFile<AstId<ast::ValueDeclaration>>,
    ) -> Arc<ValueDeclarationData>;

    fn lower_value_declaration_with_source_map(
        &self,
        id: InFile<AstId<ast::ValueDeclaration>>,
    ) -> (Arc<ValueDeclarationData>, Arc<SourceMap>);
}

fn lower_value_declaration(
    db: &dyn LowerDatabase,
    id: InFile<AstId<ast::ValueDeclaration>>,
) -> Arc<ValueDeclarationData> {
    db.lower_value_declaration_with_source_map(id).0
}

fn lower_value_declaration_with_source_map(
    db: &dyn LowerDatabase,
    id: InFile<AstId<ast::ValueDeclaration>>,
) -> (Arc<ValueDeclarationData>, Arc<SourceMap>) {
    let root = db.parse_file(id.file_id);
    let ptr = db.positional_map(id.file_id).ast_ptr(id.value);
    let value_declaration = ptr.to_node(&root);

    let context = LowerContext::default();
    let (value_declaration_data, source_map) =
        context.lower_value_declaration(&value_declaration).unwrap();

    (Arc::new(value_declaration_data), Arc::new(source_map))

    // let binders = value_declaration
    //     .binders()
    //     .map(|binders| binders.children().map(|binder| todo!()))
    //     .unwrap()
    //     .collect();
    // let binding =
    //     value_declaration.binding().and_then(|binding| context.lower_binding(&binding)).unwrap();

    // let LowerContext { expr_arena, source_map } = context;

    // // TODO: Find annotation and sibglings using nominal map
    // let value_declaration_data = ValueDeclarationData {
    //     expr_arena,
    //     binders,
    //     binding,
    //     annotation: None,
    //     siblings: Arc::from([]),
    // };

    // (Arc::new(value_declaration_data), Arc::new(source_map))
}

#[derive(Default)]
struct LowerContext {
    expr_arena: Arena<Expr>,
    binder_arena: Arena<Binder>,
    source_map: SourceMap,
}

impl LowerContext {
    fn alloc_expr(&mut self, lowered: Expr, ast: &ast::Expression) -> ExprId {
        let expr_id = self.expr_arena.alloc(lowered);
        self.source_map.expr_to_cst.insert(expr_id, SyntaxNodePtr::new(ast.syntax()));
        self.source_map.cst_to_expr.insert(SyntaxNodePtr::new(ast.syntax()), expr_id);
        expr_id
    }

    fn alloc_binder(&mut self, lowered: Binder, ast: &ast::Binder) -> BinderId {
        let binder_id = self.binder_arena.alloc(lowered);
        self.source_map.binder_to_cst.insert(binder_id, SyntaxNodePtr::new(ast.syntax()));
        self.source_map.cst_to_binder.insert(SyntaxNodePtr::new(ast.syntax()), binder_id);
        binder_id
    }

    fn lower_value_declaration(
        mut self,
        value_declaration: &ast::ValueDeclaration,
    ) -> Option<(ValueDeclarationData, SourceMap)> {
        let binders = value_declaration
            .binders()?
            .children()
            .map(|binder| self.lower_binder(&binder))
            .collect::<Option<_>>()?;

        let binding =
            value_declaration.binding().and_then(|binding| self.lower_binding(&binding))?;

        let value_declaration_data = ValueDeclarationData {
            expr_arena: self.expr_arena,
            binder_arena: self.binder_arena,
            binders,
            binding,
            annotation: None,
            siblings: Arc::from([]),
        };

        Some((value_declaration_data, self.source_map))
    }

    // FIXME: use unknown if we can't convert
    fn lower_binder(&mut self, binder: &ast::Binder) -> Option<BinderId> {
        let lowered = {
            match binder {
                ast::Binder::ConstructorBinder(constructor) => {
                    let name = Qualified::try_from(constructor.qualified_name()?).ok()?;
                    let fields = constructor
                        .fields()?
                        .children()
                        .filter_map(|field| self.lower_binder(&field))
                        .collect();
                    Some(Binder::Constructor { name, fields })
                }
                ast::Binder::LiteralBinder(_) => None,
                ast::Binder::NegativeBinder(_) => None,
                ast::Binder::ParenthesizedBinder(parenthesized) => {
                    Some(Binder::Parenthesized(self.lower_binder(&parenthesized.binder()?)?))
                }
                ast::Binder::TypedBinder(_) => None,
                ast::Binder::VariableBinder(variable) => {
                    Some(Binder::Variable(Name::try_from(variable.name()?).ok()?))
                }
                ast::Binder::WildcardBinder(_) => Some(Binder::Wildcard),
            }
        };
        lowered.map(|lowered| self.alloc_binder(lowered, binder))
    }

    fn lower_binding(&mut self, binding: &ast::Binding) -> Option<Binding> {
        match binding {
            ast::Binding::UnconditionalBinding(unconditional) => {
                let where_expression = unconditional.where_expression()?;
                let where_expr = self.lower_where_expr(&where_expression)?;
                Some(surface::Binding::Unconditional { where_expr })
            }
            ast::Binding::GuardedBinding(_) => None,
        }
    }

    fn lower_where_expr(&mut self, where_expression: &ast::WhereExpression) -> Option<WhereExpr> {
        let expression = where_expression.expression()?;
        let expr_id = self.lower_expr(&expression)?;
        Some(WhereExpr { expr_id })
    }

    fn lower_expr(&mut self, expression: &ast::Expression) -> Option<ExprId> {
        let lowered = match expression {
            ast::Expression::LiteralExpression(literal) => Some(self.lower_lit(literal)?),
            ast::Expression::VariableExpression(variable) => Some(self.lower_variable(variable)?),
            _ => None,
        };
        lowered.map(|lowered| self.alloc_expr(lowered, expression))
    }

    fn lower_lit(&mut self, literal: &ast::LiteralExpression) -> Option<Expr> {
        let lit = match literal.syntax().first_child_or_token()? {
            NodeOrToken::Node(n) => match n.kind() {
                SyntaxKind::LiteralArray => {
                    let wrapped =
                        ast::Wrapped::<ast::Separated<ast::Expression>>::cast(n.first_child()?)?;

                    let contents = wrapped
                        .child()
                        .and_then(|separated| {
                            separated
                                .children()
                                .map(|expression| self.lower_expr(&expression))
                                .collect()
                        })
                        .unwrap_or_default();

                    Some(Literal::Array(contents))
                }
                SyntaxKind::LiteralRecord => {
                    let wrapped =
                        ast::Wrapped::<ast::Separated<ast::RecordItem>>::cast(n.first_child()?)?;

                    let contents = wrapped
                        .child()
                        .and_then(|separated| {
                            separated
                                .children()
                                .map(|item| match item {
                                    ast::RecordItem::RecordField(field) => {
                                        let name = field.name()?.as_str()?;
                                        let value = self.lower_expr(&field.value()?)?;
                                        Some(RecordItem::RecordField(name, value))
                                    }
                                    ast::RecordItem::RecordPun(pun) => {
                                        let name = pun.name()?.as_str()?;
                                        Some(RecordItem::RecordPun(name))
                                    }
                                })
                                .collect()
                        })
                        .unwrap_or_default();

                    Some(Literal::Record(contents))
                }
                _ => None,
            },
            NodeOrToken::Token(t) => match t.kind() {
                SyntaxKind::LiteralInteger => Some(Literal::Int(t.text().parse().ok()?)),
                SyntaxKind::LiteralNumber => Some(Literal::Number(t.text().into())),
                SyntaxKind::LiteralString | SyntaxKind::LiteralRawString => {
                    Some(Literal::String(t.text().into()))
                }
                SyntaxKind::LiteralChar => Some(Literal::Char(t.text().into())),
                SyntaxKind::LiteralTrue => Some(Literal::Boolean(true)),
                SyntaxKind::LiteralFalse => Some(Literal::Boolean(false)),
                _ => None,
            },
        }?;

        Some(Expr::Literal(lit))
    }

    fn lower_variable(&mut self, variable: &ast::VariableExpression) -> Option<Expr> {
        let qualified = variable.qualified_name()?;
        let name_ref = qualified.try_into().ok()?;
        Some(Expr::Variable(name_ref))
    }
}
