use std::{ops::Index, sync::Arc};

use la_arena::{Arena, Idx};
use rowan::{
    ast::{AstNode, AstPtr},
    NodeOrToken,
};
use rustc_hash::FxHashMap;
use smol_str::SmolStr;
use syntax::{ast, SyntaxKind, SyntaxNode, SyntaxNodePtr};

use crate::declaration_map::{DeclarationId, DeclarationMap};

pub(crate) type BinderId = Idx<Binder>;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum Binder {
    Literal(Literal<BinderId>),
}

pub(crate) type ExprId = Idx<Expr>;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum Expr {
    Literal(Literal<ExprId>),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]

pub(crate) enum Literal<T> {
    Int(usize),
    Number(SmolStr),
    String(SmolStr),
    Char(SmolStr),
    Boolean(bool),
    Array(Arc<[T]>),
    Record(Arc<[RecordItem<T>]>),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum RecordItem<T> {
    RecordPun(SmolStr),
    RecordField(SmolStr, T),
}

#[derive(Default)]
pub(crate) struct LowerContext {
    expr_arena: Arena<Expr>,
    binder_arena: Arena<Binder>,
    source_map: SourceMap,
}

impl LowerContext {
    fn alloc_expr(&mut self, expr: Expr, expr_ptr: AstPtr<ast::Expression>) -> ExprId {
        let expr_id = self.expr_arena.alloc(expr);
        self.source_map.insert_expr(expr_id, expr_ptr);
        expr_id
    }

    fn lower_expression(&mut self, node: &ast::Expression) -> Option<ExprId> {
        let expr = match node {
            ast::Expression::LiteralExpression(literal) => {
                Some(Expr::Literal(self.lower_literal_expression(&literal)?))
            }
            _ => None,
        };

        if let Some(expr) = expr {
            Some(self.alloc_expr(expr, AstPtr::new(node)))
        } else {
            None
        }
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

impl LowerContext {
    fn alloc_binder(&mut self, t: Binder) -> BinderId {
        self.binder_arena.alloc(t)
    }

    fn lower_binder(&mut self, t: &ast::Binder) -> Option<BinderId> {
        match t {
            ast::Binder::LiteralBinder(t) => {
                let literal = self.lower_literal_binder(t)?;
                Some(self.alloc_binder(Binder::Literal(literal)))
            }
            _ => None,
        }
    }

    fn lower_literal_binder(&mut self, t: &ast::LiteralBinder) -> Option<Literal<BinderId>> {
        match t.syntax().first_child_or_token()? {
            NodeOrToken::Node(n) => match n.kind() {
                SyntaxKind::LiteralArray => {
                    let literal =
                        ast::Wrapped::<ast::Separated<ast::Binder>>::cast(n.first_child()?)?;

                    let contents = if let Some(separated) = literal.child() {
                        separated.children().filter_map(|t| self.lower_binder(&t)).collect()
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
                                    let value = self.lower_binder(&t.value()?)?;
                                    Some(RecordItem::RecordField(name, value))
                                }
                                ast::RecordItem::RecordPun(t) => {
                                    let name = t.name()?.as_str()?;
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

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct DeclarationData {
    expr_arena: Arena<Expr>,
    binder_arena: Arena<Binder>,
    pub(crate) source_map: SourceMap,

    // FIXME: create an enum for different body types...
    name: SmolStr,
    binders: Arc<[BinderId]>,
    expression: ExprId,
}

#[derive(Debug, Default, PartialEq, Eq)]
pub(crate) struct SourceMap {
    expr_to_cst: FxHashMap<ExprId, SyntaxNodePtr>,
    cst_to_expr: FxHashMap<SyntaxNodePtr, ExprId>,
}

impl SourceMap {
    pub(crate) fn insert_expr(&mut self, expr_id: ExprId, expr_ptr: AstPtr<ast::Expression>) {
        self.expr_to_cst.insert(expr_id, expr_ptr.syntax_node_ptr());
        self.cst_to_expr.insert(expr_ptr.syntax_node_ptr(), expr_id);
    }

    pub(crate) fn get_expr_id(&self, expr_ptr: &SyntaxNode) -> Option<ExprId> {
        self.cst_to_expr.get(&SyntaxNodePtr::new(expr_ptr)).cloned()
    }

    pub(crate) fn get_expr_ptr(&self, expr_id: ExprId) -> Option<SyntaxNodePtr> {
        self.expr_to_cst.get(&expr_id).cloned()
    }
}

#[derive(Debug, Default, PartialEq, Eq)]
pub(crate) struct InferenceResult {
    expr_type: FxHashMap<ExprId, Type>,
}

impl Index<ExprId> for InferenceResult {
    type Output = Type;

    fn index(&self, expr_id: ExprId) -> &Self::Output {
        self.expr_type.get(&expr_id).unwrap()
    }
}

#[salsa::query_group(SurfaceDatabase)]
pub(crate) trait Surface {
    #[salsa::input]
    fn syntax_and_declaration_map(&self) -> (SyntaxNode, Arc<DeclarationMap>);

    #[salsa::invoke(lower_declaration_query)]
    fn lower_declaration(&self, declaration_id: DeclarationId) -> Arc<DeclarationData>;

    #[salsa::invoke(infer_declaration_query)]
    fn infer_declaration(&self, declaration_id: DeclarationId) -> Arc<InferenceResult>;
}

#[salsa::database(SurfaceDatabase)]
#[derive(Default)]
pub struct SurfaceImpl {
    storage: salsa::Storage<SurfaceImpl>,
}

impl salsa::Database for SurfaceImpl {}

fn lower_declaration_query(
    db: &dyn Surface,
    declaration_id: DeclarationId,
) -> Arc<DeclarationData> {
    let (node, declaration_map) = db.syntax_and_declaration_map();
    let ptr = declaration_map.get::<ast::Declaration>(declaration_id);

    match ptr.to_node(&node) {
        ast::Declaration::AnnotationDeclaration(_) => {
            todo!("Cannot lower annotation declarations yet...")
        }
        ast::Declaration::ValueDeclaration(t) => {
            let mut lower_context = LowerContext::default();

            let name = t.name().unwrap().as_str().unwrap();

            let binders: Arc<[BinderId]> = t
                .binders()
                .unwrap()
                .children()
                .map(|t| lower_context.lower_binder(&t).unwrap())
                .collect();

            let expression: ExprId = match t.binding().unwrap() {
                ast::Binding::UnconditionalBinding(t) => lower_context
                    .lower_expression(&t.where_expression().unwrap().expression().unwrap())
                    .unwrap(),
                ast::Binding::GuardedBinding(_) => todo!("Cannot lower guarded bindings yet..."),
            };

            let LowerContext { expr_arena, binder_arena, source_map } = lower_context;

            Arc::new(DeclarationData {
                expr_arena,
                binder_arena,
                source_map,
                name,
                binders,
                expression,
            })
        }
    }
}

fn infer_declaration_query(
    db: &dyn Surface,
    declaration_id: DeclarationId,
) -> Arc<InferenceResult> {
    let declaration_data = db.lower_declaration(declaration_id);
    let mut infer_context = InferContext::new(&declaration_data.expr_arena);
    infer_context.infer_expression(declaration_data.expression);
    let InferContext { expr_type, .. } = infer_context;
    Arc::new(InferenceResult { expr_type })
}

struct InferContext<'a> {
    expr_arena: &'a Arena<Expr>,
    expr_type: FxHashMap<ExprId, Type>,
}

impl<'a> InferContext<'a> {
    fn new(expr_arena: &'a Arena<Expr>) -> InferContext<'a> {
        let expr_type = FxHashMap::default();
        InferContext { expr_arena, expr_type }
    }

    fn infer_expression(&mut self, expr_id: ExprId) {
        let ty = match &self.expr_arena[expr_id] {
            Expr::Literal(literal) => match literal {
                Literal::Int(_) => Type::Int,
                Literal::Number(_) => Type::Number,
                Literal::String(_) => Type::String,
                Literal::Char(_) => Type::Char,
                Literal::Boolean(_) => Type::Boolean,
                Literal::Array(expr_ids) => {
                    let mut expr_ids = expr_ids.iter().cloned();
                    if let Some(head_id) = expr_ids.next() {
                        self.infer_expression(head_id);
                        let head_ty = self.expr_type.get(&head_id).unwrap().clone();
                        for tail_id in expr_ids {
                            self.infer_expression(tail_id);
                            let tail_ty = self.expr_type.get(&tail_id).unwrap().clone();
                            if head_ty != tail_ty {
                                self.expr_type
                                    .insert(expr_id, Type::Array(Arc::new(Type::Unknown)));
                                return;
                            }
                        }
                        Type::Array(Arc::new(head_ty))
                    } else {
                        Type::Unknown
                    }
                }
                Literal::Record(_) => todo!(),
            },
        };

        self.expr_type.insert(expr_id, ty);
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum Type {
    Unknown,
    Int,
    Number,
    String,
    Char,
    Boolean,
    Array(Arc<Type>),
    Record(Arc<[(SmolStr, Type)]>),
}
