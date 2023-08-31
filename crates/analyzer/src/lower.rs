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

    let mut context = LowerContext::default();
    let binding =
        value_declaration.binding().and_then(|binding| context.lower_binding(&binding)).unwrap();

    let LowerContext { expr_arena, source_map } = context;

    // TODO: Find annotation and sibglings using nominal map
    let value_declaration_data =
        ValueDeclarationData { expr_arena, binding, annotation: None, siblings: Arc::from([]) };

    (Arc::new(value_declaration_data), Arc::new(source_map))
}

#[derive(Default)]
struct LowerContext {
    expr_arena: Arena<Expr>,
    source_map: SourceMap,
}

impl LowerContext {
    fn alloc_expr(&mut self, expr: Expr, expression: &ast::Expression) -> ExprId {
        let expr_id = self.expr_arena.alloc(expr);
        self.source_map.expr_to_cst.insert(expr_id, SyntaxNodePtr::new(expression.syntax()));
        self.source_map.cst_to_expr.insert(SyntaxNodePtr::new(expression.syntax()), expr_id);
        expr_id
    }

    fn lower_binding(&mut self, binding: &ast::Binding) -> Option<Binding> {
        match binding {
            ast::Binding::UnconditionalBinding(unconditional) => {
                // FIXME: WhereExpr
                let expression = unconditional.where_expression()?.expression()?;
                let expr_id = self.lower_expr(&expression)?;
                Some(surface::Binding::Unconditional { expr_id })
            }
            ast::Binding::GuardedBinding(_) => None,
        }
    }

    fn lower_expr(&mut self, expression: &ast::Expression) -> Option<ExprId> {
        let expr = match expression {
            ast::Expression::LiteralExpression(literal) => {
                Some(Expr::Lit(self.lower_lit(literal)?))
            }
            _ => None,
        };
        expr.map(|expr| self.alloc_expr(expr, expression))
    }

    fn lower_lit(&mut self, node: &ast::LiteralExpression) -> Option<Lit> {
        match node.syntax().first_child_or_token()? {
            NodeOrToken::Node(_) => None,
            NodeOrToken::Token(t) => match t.kind() {
                SyntaxKind::LiteralInteger => Some(Lit::Int(t.text().parse().ok()?)),
                // SyntaxKind::LiteralNumber => Some(Literal::Number(t.text().into())),
                // SyntaxKind::LiteralString | SyntaxKind::LiteralRawString => {
                //     Some(Literal::String(t.text().into()))
                // }
                // SyntaxKind::LiteralChar => Some(Literal::Char(t.text().into())),
                // SyntaxKind::LiteralTrue => Some(Literal::Boolean(true)),
                // SyntaxKind::LiteralFalse => Some(Literal::Boolean(false)),
                _ => None,
            },
        }
    }
}
