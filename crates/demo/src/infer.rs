//! Queries related to type inference.

use std::sync::Arc;

use files::FileId;
use la_arena::{Arena, Idx};
use rowan::{
    ast::{AstNode, AstPtr},
    NodeOrToken,
};
use rustc_hash::FxHashMap;
use smol_str::SmolStr;
use syntax::{ast, SyntaxKind, SyntaxNode, SyntaxNodePtr};

use crate::{id::InFileAstId, surface::SurfaceDatabase};

#[derive(Debug, Default, PartialEq, Eq)]
pub struct ValueInfer {
    pub expr_type: FxHashMap<ExprId, Type>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Unknown,
    Int,
    Number,
    String,
    Char,
    Boolean,
    Array(Arc<Type>),
    // Record(Arc<[(SmolStr, Type)]>),
}

#[derive(Debug, PartialEq, Eq)]
pub struct ValueData {
    expr_arena: Arena<Expr>,
    pub name: SmolStr,
    pub expr_id: ExprId,
}

#[derive(Debug, Default, PartialEq, Eq)]
pub struct SourceMap {
    expr_to_cst: FxHashMap<ExprId, SyntaxNodePtr>,
    cst_to_expr: FxHashMap<SyntaxNodePtr, ExprId>,
}

impl SourceMap {
    pub fn insert_expr(&mut self, expr_id: ExprId, expr_ptr: &ast::Expression) {
        self.expr_to_cst.insert(expr_id, AstPtr::new(expr_ptr).syntax_node_ptr());
        self.cst_to_expr.insert(AstPtr::new(expr_ptr).syntax_node_ptr(), expr_id);
    }

    pub fn get_expr_id(&self, expr_ptr: &SyntaxNode) -> Option<ExprId> {
        self.cst_to_expr.get(&SyntaxNodePtr::new(expr_ptr)).cloned()
    }

    pub fn get_expr_ptr(&self, expr_id: ExprId) -> Option<SyntaxNodePtr> {
        self.expr_to_cst.get(&expr_id).cloned()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
    Literal(Literal<ExprId>),
    Variable(SmolStr),
}

pub type ExprId = Idx<Expr>;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Literal<T> {
    Int(usize),
    Number(SmolStr),
    String(SmolStr),
    Char(SmolStr),
    Boolean(bool),
    Array(Arc<[T]>),
    Record(Arc<[RecordItem<T>]>),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum RecordItem<T> {
    RecordPun(SmolStr),
    RecordField(SmolStr, T),
}

#[salsa::query_group(InferStorage)]
pub trait InferDatabase: SurfaceDatabase {
    fn infer_value_declaration(&self, id: InFileAstId<ast::ValueDeclaration>) -> Arc<ValueInfer>;

    fn lower_value_declaration(&self, id: InFileAstId<ast::ValueDeclaration>) -> Arc<ValueData>;

    fn lower_value_declaration_with_source_map(
        &self,
        id: InFileAstId<ast::ValueDeclaration>,
    ) -> (Arc<ValueData>, Arc<SourceMap>);
}

fn infer_value_declaration(
    db: &dyn InferDatabase,
    id: InFileAstId<ast::ValueDeclaration>,
) -> Arc<ValueInfer> {
    dbg!("Called infer_value_declaration...");
    let mut value_infer = ValueInfer::default();
    let value_data = db.lower_value_declaration(id);
    infer_expression(db, id.file_id, &mut value_infer, &value_data.expr_arena, value_data.expr_id);
    Arc::new(value_infer)
}

fn infer_expression(
    db: &dyn InferDatabase,
    file_id: FileId,
    value_infer: &mut ValueInfer,
    expr_arena: &Arena<Expr>,
    expr_id: ExprId,
) {
    match &expr_arena[expr_id] {
        Expr::Literal(literal) => match literal {
            Literal::Int(_) => {
                value_infer.expr_type.insert(expr_id, Type::Int);
            }
            Literal::Number(_) => {
                value_infer.expr_type.insert(expr_id, Type::Number);
            }
            Literal::String(_) => {
                value_infer.expr_type.insert(expr_id, Type::String);
            }
            Literal::Char(_) => {
                value_infer.expr_type.insert(expr_id, Type::Char);
            }
            Literal::Boolean(_) => {
                value_infer.expr_type.insert(expr_id, Type::Boolean);
            }
            Literal::Array(expr_ids) => {
                let mut expr_ids = expr_ids.iter();
                if let Some(head_id) = expr_ids.next() {
                    infer_expression(db, file_id, value_infer, expr_arena, *head_id);
                    let head_ty = value_infer.expr_type.get(head_id).unwrap().clone();

                    let mut is_consistent = true;
                    for tail_id in expr_ids {
                        infer_expression(db, file_id, value_infer, expr_arena, *tail_id);
                        let tail_ty = value_infer.expr_type.get(tail_id).unwrap().clone();

                        if head_ty != tail_ty {
                            is_consistent &= false;
                        }
                    }

                    let expr_ty =
                        if is_consistent { Type::Array(Arc::new(head_ty)) } else { Type::Unknown };

                    value_infer.expr_type.insert(expr_id, expr_ty);
                } else {
                    value_infer.expr_type.insert(expr_id, Type::Array(Arc::new(Type::Unknown)));
                }
            }
            Literal::Record(_) => todo!("Not supported, come back later..."),
        },
        Expr::Variable(name) => {
            if let Some(variable_id) = db.nominal_map(file_id).get_value(name) {
                let variable_data = db.lower_value_declaration(variable_id);
                let infer_result = db.infer_value_declaration(variable_id);
                let variable_ty =
                    infer_result.expr_type.get(&variable_data.expr_id).unwrap().clone();
                value_infer.expr_type.insert(expr_id, variable_ty);
            } else {
                value_infer.expr_type.insert(expr_id, Type::Unknown);
            }
        }
    }
}

fn lower_value_declaration(
    db: &dyn InferDatabase,
    id: InFileAstId<ast::ValueDeclaration>,
) -> Arc<ValueData> {
    dbg!("Called lower_value_declaration...");
    db.lower_value_declaration_with_source_map(id).0
}

fn lower_value_declaration_with_source_map(
    db: &dyn InferDatabase,
    id: InFileAstId<ast::ValueDeclaration>,
) -> (Arc<ValueData>, Arc<SourceMap>) {
    dbg!("Called lower_value_declaration_with_source_map...");

    let mut lower_context = LowerContext::default();
    let root = &db.parse_file(id.file_id).syntax;

    let (name, expr_id) = {
        let declaration = db.positional_map(id.file_id).get(id.value).to_node(root);
        let name = declaration.name().unwrap().as_str().unwrap();
        let expr_id = match declaration.binding().unwrap() {
            ast::Binding::UnconditionalBinding(declaration) => {
                let node = declaration.where_expression().unwrap().expression().unwrap();
                lower_context.lower_expression(&node).unwrap()
            }
            ast::Binding::GuardedBinding(_) => todo!("Not supported, come back later..."),
        };
        (name, expr_id)
    };

    let LowerContext { expr_arena, source_map } = lower_context;
    (Arc::new(ValueData { expr_arena, name, expr_id }), Arc::new(source_map))
}

#[derive(Default)]
pub(crate) struct LowerContext {
    expr_arena: Arena<Expr>,
    source_map: SourceMap,
}

impl LowerContext {
    fn alloc_expr(&mut self, expr: Expr, expr_ptr: &ast::Expression) -> ExprId {
        let expr_id = self.expr_arena.alloc(expr);
        self.source_map.insert_expr(expr_id, expr_ptr);
        expr_id
    }

    fn lower_expression(&mut self, node: &ast::Expression) -> Option<ExprId> {
        dbg!(node);
        let expr = match node {
            ast::Expression::LiteralExpression(literal) => {
                Some(Expr::Literal(self.lower_literal_expression(literal)?))
            }
            ast::Expression::VariableExpression(variable) => {
                Some(Expr::Variable(variable.qualified_name()?.name_ref()?.as_str()?))
            }
            _ => None,
        };

        expr.map(|expr| self.alloc_expr(expr, node))
    }

    fn lower_literal_expression(&mut self, t: &ast::LiteralExpression) -> Option<Literal<ExprId>> {
        match t.syntax().first_child_or_token()? {
            NodeOrToken::Node(n) => match n.kind() {
                SyntaxKind::LiteralArray => {
                    let literal =
                        ast::Wrapped::<ast::Separated<ast::Expression>>::cast(n.first_child()?)?;

                    let contents = if let Some(separated) = literal.child() {
                        separated.children().filter_map(|t| self.lower_expression(&t)).collect()
                    } else {
                        vec![].into()
                    };

                    Some(Literal::Array(contents))
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
                                    let value = self.lower_expression(&t.value()?)?;
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
        }
    }
}
