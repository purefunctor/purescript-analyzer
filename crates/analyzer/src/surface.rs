//! Database for lowering the CST.

mod source_map;
mod trees;

use std::sync::Arc;

use files::FileId;
use la_arena::Arena;
use rowan::{
    ast::{AstNode, SyntaxNodePtr},
    NodeOrToken,
};
use smol_str::SmolStr;
use syntax::{ast, PureScript, SyntaxKind};

pub use source_map::*;
pub use trees::*;

use crate::{
    id::{AstId, InFile},
    names::{Name, Qualified},
    resolver::PositionalMap,
    ResolverDatabase, SourceDatabase,
};

#[salsa::query_group(LowerStorage)]
pub trait SurfaceDatabase: SourceDatabase + ResolverDatabase {
    fn surface_data(&self, id: InFile<AstId<ast::DataDeclaration>>) -> Arc<DataDeclarationData>;

    fn surface_foreign_data(
        &self,
        id: InFile<AstId<ast::ForeignDataDeclaration>>,
    ) -> Arc<ForeignDataDeclarationData>;

    fn surface_value_declaration(
        &self,
        id: InFile<AstId<ast::ValueDeclaration>>,
    ) -> Arc<ValueDeclarationData>;

    fn surface_value_declaration_with_source_map(
        &self,
        id: InFile<AstId<ast::ValueDeclaration>>,
    ) -> (Arc<ValueDeclarationData>, Arc<SourceMap>);

    fn surface_value_annotation_declaration(
        &self,
        id: InFile<AstId<ast::ValueAnnotationDeclaration>>,
    ) -> Arc<ValueAnnotationDeclarationData>;
}

fn surface_data(
    db: &dyn SurfaceDatabase,
    id: InFile<AstId<ast::DataDeclaration>>,
) -> Arc<DataDeclarationData> {
    let root = db.parse_file(id.file_id);
    let positional_map = db.positional_map(id.file_id);
    let ptr = positional_map.ast_ptr(id.value);
    let data_declaration = ptr.to_node(&root);

    let context = LowerContext::default();
    let data_declaration_data =
        context.surface_data(id.file_id, positional_map, &data_declaration).unwrap();

    Arc::new(data_declaration_data)
}

fn surface_foreign_data(
    db: &dyn SurfaceDatabase,
    id: InFile<AstId<ast::ForeignDataDeclaration>>,
) -> Arc<ForeignDataDeclarationData> {
    let root = db.parse_file(id.file_id);
    let ptr = db.positional_map(id.file_id).ast_ptr(id.value);
    let foreign_data_declaration = ptr.to_node(&root);

    let context = LowerContext::default();
    let foreign_data_declaration_data =
        context.surface_foreign_data(&foreign_data_declaration).unwrap();

    Arc::new(foreign_data_declaration_data)
}

fn surface_value_declaration(
    db: &dyn SurfaceDatabase,
    id: InFile<AstId<ast::ValueDeclaration>>,
) -> Arc<ValueDeclarationData> {
    db.surface_value_declaration_with_source_map(id).0
}

fn surface_value_declaration_with_source_map(
    db: &dyn SurfaceDatabase,
    id: InFile<AstId<ast::ValueDeclaration>>,
) -> (Arc<ValueDeclarationData>, Arc<SourceMap>) {
    let root = db.parse_file(id.file_id);
    let ptr = db.positional_map(id.file_id).ast_ptr(id.value);
    let value_declaration = ptr.to_node(&root);

    let context = LowerContext::default();
    let (value_declaration_data, source_map) =
        context.surface_value_declaration(&value_declaration).unwrap();

    (Arc::new(value_declaration_data), Arc::new(source_map))
}

fn surface_value_annotation_declaration(
    db: &dyn SurfaceDatabase,
    id: InFile<AstId<ast::ValueAnnotationDeclaration>>,
) -> Arc<ValueAnnotationDeclarationData> {
    let root = db.parse_file(id.file_id);
    let ptr = db.positional_map(id.file_id).ast_ptr(id.value);
    let value_annotation_declaration = ptr.to_node(&root);

    let context = LowerContext::default();
    let value_annotation_declaration_data =
        context.surface_value_annotation_declaration(&value_annotation_declaration).unwrap();

    Arc::new(value_annotation_declaration_data)
}

#[derive(Default)]
struct LowerContext {
    type_arena: Arena<Type>,
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

    fn alloc_type(&mut self, lowered: Type, ast: &ast::Type) -> TypeId {
        let type_id = self.type_arena.alloc(lowered);
        self.source_map.type_to_cst.insert(type_id, SyntaxNodePtr::new(ast.syntax()));
        self.source_map.cst_to_type.insert(SyntaxNodePtr::new(ast.syntax()), type_id);
        type_id
    }
}

impl LowerContext {
    fn surface_data(
        mut self,
        file_id: FileId,
        positional_map: Arc<PositionalMap>,
        data_declaration: &ast::DataDeclaration,
    ) -> Option<DataDeclarationData> {
        let name = Name::try_from(data_declaration.name()?).ok()?;
        let constructors = data_declaration
            .constructors()?
            .children()
            .map(|constructor| {
                let id = positional_map.ast_id(&constructor).in_file(file_id);
                Some((id, self.lower_data_constructor(&constructor)?))
            })
            .collect::<Option<_>>()?;
        Some(DataDeclarationData { type_arena: self.type_arena, name, constructors })
    }

    fn surface_foreign_data(
        mut self,
        foreign_data_declaration: &ast::ForeignDataDeclaration,
    ) -> Option<ForeignDataDeclarationData> {
        let name = Name::try_from(foreign_data_declaration.name()?).ok()?;
        let type_id = self.lower_type(&foreign_data_declaration.ty()?)?;
        Some(ForeignDataDeclarationData { type_arena: self.type_arena, name, type_id })
    }

    fn surface_value_declaration(
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
            siblings: Box::from([]),
        };

        Some((value_declaration_data, self.source_map))
    }

    fn surface_value_annotation_declaration(
        mut self,
        value_annotation_declaration: &ast::ValueAnnotationDeclaration,
    ) -> Option<ValueAnnotationDeclarationData> {
        let ty = self.lower_type(&value_annotation_declaration.ty()?)?;
        Some(ValueAnnotationDeclarationData { type_arena: self.type_arena, ty })
    }
}

impl LowerContext {
    fn lower_data_constructor(
        &mut self,
        constructor: &ast::DataConstructor,
    ) -> Option<DataConstructorData> {
        let name = Name::try_from(constructor.name()?).ok()?;
        let fields = constructor
            .fields()?
            .children()
            .map(|field| self.lower_type(&field))
            .collect::<Option<_>>()?;
        Some(DataConstructorData { name, fields })
    }

    // FIXME: use unknown if we can't convert
    fn lower_binder(&mut self, binder: &ast::Binder) -> Option<BinderId> {
        let lowered = {
            match binder {
                ast::Binder::ConstructorBinder(constructor) => {
                    let name = Qualified::try_from(constructor.qualified_name()?).ok()?;
                    let fields = constructor
                        .fields()
                        .map(|fields| {
                            fields
                                .children()
                                .filter_map(|field| self.lower_binder(&field))
                                .collect()
                        })
                        .unwrap_or_default();
                    Some(Binder::Constructor { name, fields })
                }
                ast::Binder::LiteralBinder(literal) => self.lower_binder_literal(literal),
                ast::Binder::NegativeBinder(negative) => self.lower_negative(negative),
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

    fn lower_binder_literal(&mut self, literal: &ast::LiteralBinder) -> Option<Binder> {
        let literal = self.lower_literal(
            &literal.syntax().first_child_or_token()?,
            |this, inner_binder| this.lower_binder(inner_binder),
            |item| match item {
                ast::RecordItem::RecordField(field) => field.name()?.as_str(),
                ast::RecordItem::RecordPun(pun) => pun.name()?.as_str(),
            },
        )?;

        Some(Binder::Literal(literal))
    }

    fn lower_negative(&mut self, negative: &ast::NegativeBinder) -> Option<Binder> {
        match negative.literal()?.syntax().first_child_or_token()? {
            NodeOrToken::Node(_) => None,
            NodeOrToken::Token(token) => match token.kind() {
                SyntaxKind::LiteralInteger => {
                    Some(Binder::Negative(IntOrNumber::Int(token.text().parse().ok()?)))
                }
                SyntaxKind::LiteralNumber => {
                    Some(Binder::Negative(IntOrNumber::Number(token.text().into())))
                }
                _ => None,
            },
        }
    }

    fn lower_binding(&mut self, binding: &ast::Binding) -> Option<Binding> {
        match binding {
            ast::Binding::UnconditionalBinding(unconditional) => {
                let where_expression = unconditional.where_expression()?;
                let where_expr = self.lower_where_expr(&where_expression)?;
                Some(Binding::Unconditional { where_expr })
            }
            ast::Binding::GuardedBinding(_) => None,
        }
    }

    fn lower_where_expr(&mut self, where_expression: &ast::WhereExpression) -> Option<WhereExpr> {
        let let_bindings = where_expression
            .let_bindings()
            .and_then(|let_bindings| self.lower_let_bindings(&let_bindings))
            .unwrap_or_default();

        let expr_id = self.lower_expr(&where_expression.expression()?)?;

        Some(WhereExpr { expr_id, let_bindings })
    }

    fn lower_expr(&mut self, expression: &ast::Expression) -> Option<ExprId> {
        let lowered = match expression {
            ast::Expression::ApplicationExpression(application) => {
                Some(self.lower_application(application)?)
            }
            ast::Expression::ConstructorExpression(constructor) => {
                Some(self.lower_constructor(constructor)?)
            }
            ast::Expression::LambdaExpression(lambda) => Some(self.lower_lambda(lambda)?),
            ast::Expression::LetInExpression(let_in) => Some(self.lower_let_in(let_in)?),
            ast::Expression::LiteralExpression(literal) => Some(self.lower_expr_literal(literal)?),
            ast::Expression::VariableExpression(variable) => Some(self.lower_variable(variable)?),
            _ => None,
        };
        lowered.map(|lowered| self.alloc_expr(lowered, expression))
    }

    fn lower_application(&mut self, application: &ast::ApplicationExpression) -> Option<Expr> {
        let function = self.lower_expr(&application.head()?)?;
        let arguments = application
            .spine()?
            .children()
            .filter_map(|argument| match argument {
                ast::Argument::TermArgument(e) => self.lower_expr(&e.expression()?),
                ast::Argument::TypeArgument(_) => None,
            })
            .collect();
        Some(Expr::Application(function, arguments))
    }

    fn lower_constructor(&mut self, constructor: &ast::ConstructorExpression) -> Option<Expr> {
        let qualified = constructor.qualified_name()?;
        let name_ref = qualified.try_into().ok()?;
        Some(Expr::Constructor(name_ref))
    }

    fn lower_lambda(&mut self, lambda: &ast::LambdaExpression) -> Option<Expr> {
        let binders = lambda
            .binders()?
            .children()
            .map(|binder| self.lower_binder(&binder))
            .collect::<Option<_>>()?;
        let body = self.lower_expr(&lambda.body()?)?;
        Some(Expr::Lambda(binders, body))
    }

    fn lower_let_in(&mut self, let_in: &ast::LetInExpression) -> Option<Expr> {
        let let_bindings = self.lower_let_bindings(&let_in.let_bindings()?)?;
        let let_body = self.lower_expr(&let_in.body()?)?;
        Some(Expr::LetIn(let_bindings, let_body))
    }

    fn lower_expr_literal(&mut self, literal: &ast::LiteralExpression) -> Option<Expr> {
        let literal = self.lower_literal(
            &literal.syntax().first_child_or_token()?,
            |this, inner_expression| this.lower_expr(inner_expression),
            |item| match item {
                ast::RecordItem::RecordField(field) => field.name()?.as_str(),
                ast::RecordItem::RecordPun(pun) => pun.name_ref()?.as_str(),
            },
        )?;

        Some(Expr::Literal(literal))
    }

    fn lower_literal<A, I>(
        &mut self,
        literal: &syntax::SyntaxElement,
        lower_inner: impl Fn(&mut Self, &A) -> Option<I>,
        item_name: impl Fn(&ast::RecordItem) -> Option<SmolStr>,
    ) -> Option<Literal<I>>
    where
        A: AstNode<Language = PureScript>,
    {
        match literal {
            NodeOrToken::Node(n) => match n.kind() {
                SyntaxKind::LiteralArray => {
                    let wrapped = ast::Wrapped::<ast::Separated<A>>::cast(n.first_child()?)?;

                    let contents: Box<[I]> = wrapped
                        .child()
                        .and_then(|separated| {
                            separated.children().map(|inner| lower_inner(self, &inner)).collect()
                        })
                        .unwrap_or_default();

                    Some(Literal::Array(contents))
                }
                SyntaxKind::LiteralRecord => {
                    let wrapped =
                        ast::Wrapped::<ast::Separated<ast::RecordItem>>::cast(n.first_child()?)?;

                    let contents: Box<[RecordItem<I>]> = wrapped
                        .child()
                        .and_then(|separated| {
                            separated
                                .children()
                                .map(|item| {
                                    let name = item_name(&item)?;
                                    match &item {
                                        ast::RecordItem::RecordField(field) => {
                                            let value = lower_inner(self, &field.value()?)?;
                                            Some(RecordItem::RecordField(name, value))
                                        }
                                        ast::RecordItem::RecordPun(_) => {
                                            Some(RecordItem::RecordPun(name))
                                        }
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
        }
    }

    fn lower_variable(&mut self, variable: &ast::VariableExpression) -> Option<Expr> {
        let qualified = variable.qualified_name()?;
        let name_ref = qualified.try_into().ok()?;
        Some(Expr::Variable(name_ref))
    }

    fn lower_let_bindings(
        &mut self,
        let_bindings: &ast::OneOrMore<ast::LetBinding>,
    ) -> Option<Box<[LetBinding]>> {
        let_bindings
            .children()
            .map(|let_binding| match let_binding {
                ast::LetBinding::LetBindingName(let_binding_name) => {
                    let name = Name::try_from(let_binding_name.name()?).ok()?;
                    let binding = self.lower_binding(&let_binding_name.binding()?)?;
                    Some(LetBinding::Name { name, binding })
                }
                ast::LetBinding::LetBindingPattern(_) => None,
                ast::LetBinding::LetBindingSignature(_) => None,
            })
            .collect()
    }

    fn lower_type(&mut self, t: &ast::Type) -> Option<TypeId> {
        let lowered = match t {
            ast::Type::ArrowType(arrow) => self.lower_arrow_type(arrow),
            ast::Type::ApplicationType(application) => self.lower_application_type(application),
            ast::Type::ConstructorType(constructor) => self.lower_constructor_type(constructor),
            ast::Type::IntegerType(_) => None,
            ast::Type::KindedType(_) => None,
            ast::Type::OperatorNameType(_) => None,
            ast::Type::ParenthesizedType(parenthesized) => {
                Some(Type::Parenthesized(self.lower_type(&parenthesized.ty()?)?))
            }
            ast::Type::RecordType(_) => None,
            ast::Type::RowType(_) => None,
            ast::Type::StringType(_) => None,
            ast::Type::TypeOperatorChain(_) => None,
            ast::Type::VariableType(variable) => self.lower_variable_type(variable),
            ast::Type::WildcardType(_) => None,
        };
        lowered.map(|lowered| self.alloc_type(lowered, t))
    }

    fn lower_arrow_type(&mut self, arrow: &ast::ArrowType) -> Option<Type> {
        let mut arguments = vec![self.lower_type(&arrow.argument()?)?];
        let mut result = arrow.result()?;

        while let ast::Type::ArrowType(arrow) = &result {
            arguments.push(self.lower_type(&arrow.argument()?)?);
            result = arrow.result()?;
        }
        let result = self.lower_type(&result)?;

        Some(Type::Arrow(arguments.into(), result))
    }

    fn lower_application_type(&mut self, application: &ast::ApplicationType) -> Option<Type> {
        let head = self.lower_type(&application.head()?)?;
        let spine = application
            .spine()?
            .children()
            .map(|argument| self.lower_type(&argument))
            .collect::<Option<_>>()?;
        Some(Type::Application(head, spine))
    }

    fn lower_constructor_type(&mut self, constructor: &ast::ConstructorType) -> Option<Type> {
        let qualified = constructor.qualified_name()?;
        let name_ref = qualified.try_into().ok()?;
        Some(Type::Constructor(name_ref))
    }

    fn lower_variable_type(&self, variable: &ast::VariableType) -> Option<Type> {
        let name_ref = variable.name_ref()?.try_into().ok()?;
        Some(Type::Variable(name_ref))
    }
}
