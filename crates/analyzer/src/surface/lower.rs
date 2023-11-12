use std::{mem, sync::Arc};

use la_arena::Arena;
use rowan::{ast::AstNode, NodeOrToken};
use smol_str::SmolStr;
use syntax::{ast, PureScript, SyntaxKind, SyntaxNodePtr};

use crate::{
    id::InFile,
    names::{Name, Qualified},
    resolver::ValueGroupId,
    surface::LetNameEquation,
    SurfaceDatabase,
};

use super::{
    Binder, BinderId, Binding, Expr, ExprId, IntOrNumber, LetBinding, LetBindingId,
    LetNameAnnotation, LetNameGroup, LetNameGroupPtr, Literal, RecordItem, SourceMap, Type, TypeId,
    ValueAnnotation, ValueEquation, ValueGroup, WhereExpr, WithArena,
};

#[derive(Default)]
pub(crate) struct SurfaceContext {
    expr_arena: Arena<Expr>,
    let_binding_arena: Arena<LetBinding>,
    let_name_group_arena: Arena<LetNameGroup>,
    binder_arena: Arena<Binder>,
    type_arena: Arena<Type>,
    source_map: SourceMap,
}

impl SurfaceContext {
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

impl SurfaceContext {
    pub(crate) fn value_surface_query(
        db: &dyn SurfaceDatabase,
        id: InFile<ValueGroupId>,
    ) -> Arc<WithArena<ValueGroup>> {
        db.value_surface_with_source_map(id).0
    }

    pub(crate) fn value_surface_with_source_map_query(
        db: &dyn SurfaceDatabase,
        id: InFile<ValueGroupId>,
    ) -> (Arc<WithArena<ValueGroup>>, Arc<SourceMap>) {
        let nominal_map = db.nominal_map(id.file_id);
        let group_data = nominal_map.value_group_data(id);
        let mut surface_context = SurfaceContext::default();

        let name = group_data.name.clone();
        let annotation = group_data.annotation.and_then(|annotation| {
            let annotation = annotation.in_file(id.file_id).to_ast(db);
            surface_context.lower_value_annotation(&annotation)
        });
        let equations = group_data
            .equations
            .iter()
            .map(|equation_id| {
                let equation_ast = equation_id.in_file(id.file_id).to_ast(db);
                (*equation_id, surface_context.lower_value_equation(&equation_ast).unwrap())
            })
            .collect();

        let surface_group = WithArena::new(
            surface_context.expr_arena,
            surface_context.let_binding_arena,
            surface_context.binder_arena,
            surface_context.type_arena,
            ValueGroup { name, annotation, equations },
        );
        let source_map = surface_context.source_map;

        (Arc::new(surface_group), Arc::new(source_map))
    }
}

impl SurfaceContext {
    fn lower_value_annotation(
        &mut self,
        ast: &ast::ValueAnnotationDeclaration,
    ) -> Option<ValueAnnotation> {
        let ty = self.lower_type(&ast.ty()?)?;
        Some(ValueAnnotation { ty })
    }

    fn lower_value_equation(
        &mut self,
        ast: &ast::ValueEquationDeclaration,
    ) -> Option<ValueEquation> {
        let binders = ast
            .binders()?
            .children()
            .map(|binder| self.lower_binder(&binder))
            .collect::<Option<_>>()?;
        let binding = ast.binding().and_then(|binding| self.lower_binding(&binding))?;
        Some(ValueEquation { binders, binding })
    }
}

impl SurfaceContext {
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

    fn lower_let_bindings(
        &mut self,
        let_bindings: &ast::OneOrMore<ast::LetBinding>,
    ) -> Option<Box<[LetBindingId]>> {
        enum Head {
            Annotation(LetNameAnnotation, SyntaxNodePtr),
            Equation(LetNameEquation, SyntaxNodePtr),
        }

        enum State {
            Initial,
            LetNameGroup { name: Name, head: Head, rest: Vec<(LetNameEquation, SyntaxNodePtr)> },
        }

        let mut current_state = State::Initial;

        let allocate_let_name_group =
            |this: &mut Self,
             name: Name,
             head: Head,
             rest: Vec<(LetNameEquation, SyntaxNodePtr)>| {
                let mut annotation = None;
                let mut annotation_ptr = None;
                let mut equations = vec![];
                let mut equations_ptr = vec![];

                match head {
                    Head::Annotation(head_annotation, head_ptr) => {
                        annotation = Some(head_annotation);
                        annotation_ptr = Some(head_ptr);
                    }
                    Head::Equation(head_equation, head_ptr) => {
                        equations.push(head_equation);
                        equations_ptr.push(head_ptr);
                    }
                }

                for (rest_equation, rest_ptr) in rest {
                    equations.push(rest_equation);
                    equations_ptr.push(rest_ptr);
                }

                let let_name_group_id =
                    this.let_name_group_arena.alloc(LetNameGroup { name, annotation, equations });

                annotation_ptr.iter().cloned().for_each(|annotation_ptr| {
                    this.source_map.cst_to_let_name_group.insert(annotation_ptr, let_name_group_id);
                });
                equations_ptr.iter().cloned().for_each(|equation_ptr| {
                    this.source_map.cst_to_let_name_group.insert(equation_ptr, let_name_group_id);
                });

                let let_name_group_ptr = LetNameGroupPtr { annotation_ptr, equations_ptr };

                this.source_map.let_name_group_to_cst.insert(let_name_group_id, let_name_group_ptr);
            };

        let push_let_name_group = |this: &mut Self, state: &mut State, name: Name, start: Head| {
            let rest = vec![];
            let previous_state =
                mem::replace(state, State::LetNameGroup { name, head: start, rest });
            if let State::LetNameGroup { name, head, rest } = previous_state {
                allocate_let_name_group(this, name, head, rest);
            }
        };

        for let_binding in let_bindings.children() {
            match let_binding {
                ast::LetBinding::LetBindingName(let_binding_name) => {
                    let equation_name = Name::try_from(let_binding_name.name()?).ok()?;
                    match &mut current_state {
                        State::LetNameGroup { name: current_name, rest: current_rest, .. }
                            if equation_name == *current_name =>
                        {
                            current_rest.push((
                                self.lower_let_binding_name(&let_binding_name)?,
                                SyntaxNodePtr::new(let_binding_name.syntax()),
                            ));
                        }
                        _ => {
                            let name = equation_name;
                            let head = Head::Equation(
                                self.lower_let_binding_name(&let_binding_name)?,
                                SyntaxNodePtr::new(let_binding_name.syntax()),
                            );
                            push_let_name_group(self, &mut current_state, name, head);
                        }
                    }
                }
                ast::LetBinding::LetBindingPattern(_) => todo!(),
                ast::LetBinding::LetBindingSignature(let_binding_signature) => {
                    let annotation_name = Name::try_from(let_binding_signature.name()?).ok()?;
                    match &mut current_state {
                        State::LetNameGroup { name: current_name, .. }
                            if annotation_name == *current_name =>
                        {
                            panic!("Invalid position for a signature!");
                        }
                        _ => {
                            let name = annotation_name;
                            let head = Head::Annotation(
                                self.lower_let_binding_signature(&let_binding_signature)?,
                                SyntaxNodePtr::new(let_binding_signature.syntax()),
                            );
                            push_let_name_group(self, &mut current_state, name, head);
                        }
                    }
                }
            }
        }

        if let State::LetNameGroup { name, head, rest } = current_state {
            allocate_let_name_group(self, name, head, rest);
        }

        dbg!(&self.source_map.let_name_group_to_cst);
        dbg!(&self.source_map.cst_to_let_name_group);

        todo!()
    }

    fn lower_let_binding_name(
        &mut self,
        let_binding_name: &ast::LetBindingName,
    ) -> Option<LetNameEquation> {
        let binders = let_binding_name
            .binders()?
            .children()
            .map(|binder| self.lower_binder(&binder))
            .collect::<Option<_>>()?;
        let binding = self.lower_binding(&let_binding_name.binding()?)?;
        Some(LetNameEquation { binders, binding })
    }

    fn lower_let_binding_signature(
        &mut self,
        let_binding_signature: &ast::LetBindingSignature,
    ) -> Option<LetNameAnnotation> {
        let ty = self.lower_type(&let_binding_signature.ty()?)?;
        Some(LetNameAnnotation { ty })
    }
}

impl SurfaceContext {
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
                ast::Binder::NegativeBinder(negative) => self.lower_binder_negative(negative),
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

    fn lower_binder_negative(&mut self, negative: &ast::NegativeBinder) -> Option<Binder> {
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
}

impl SurfaceContext {
    fn lower_expr(&mut self, expression: &ast::Expression) -> Option<ExprId> {
        let lowered = match expression {
            ast::Expression::ApplicationExpression(application) => {
                Some(self.lower_expr_application(application)?)
            }
            ast::Expression::ConstructorExpression(constructor) => {
                Some(self.lower_expr_constructor(constructor)?)
            }
            ast::Expression::LambdaExpression(lambda) => Some(self.lower_expr_lambda(lambda)?),
            ast::Expression::LetInExpression(let_in) => Some(self.lower_expr_let_in(let_in)?),
            ast::Expression::LiteralExpression(literal) => Some(self.lower_expr_literal(literal)?),
            ast::Expression::VariableExpression(variable) => {
                Some(self.lower_expr_variable(variable)?)
            }
            _ => None,
        };
        lowered.map(|lowered| self.alloc_expr(lowered, expression))
    }

    fn lower_expr_application(&mut self, application: &ast::ApplicationExpression) -> Option<Expr> {
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

    fn lower_expr_constructor(&mut self, constructor: &ast::ConstructorExpression) -> Option<Expr> {
        let qualified = constructor.qualified_name()?;
        let name_ref = qualified.try_into().ok()?;
        Some(Expr::Constructor(name_ref))
    }

    fn lower_expr_lambda(&mut self, lambda: &ast::LambdaExpression) -> Option<Expr> {
        let binders = lambda
            .binders()?
            .children()
            .map(|binder| self.lower_binder(&binder))
            .collect::<Option<_>>()?;
        let body = self.lower_expr(&lambda.body()?)?;
        Some(Expr::Lambda(binders, body))
    }

    fn lower_expr_let_in(&mut self, let_in: &ast::LetInExpression) -> Option<Expr> {
        let let_bindings = self.lower_let_bindings(&let_in.let_bindings()?)?;
        let let_body = self.lower_expr(&let_in.body()?)?;
        Some(Expr::LetIn(let_bindings, let_body))
    }

    fn lower_expr_variable(&mut self, variable: &ast::VariableExpression) -> Option<Expr> {
        let qualified = variable.qualified_name()?;
        let name_ref = qualified.try_into().ok()?;
        Some(Expr::Variable(name_ref))
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
}

impl SurfaceContext {
    fn lower_type(&mut self, t: &ast::Type) -> Option<TypeId> {
        let lowered = match t {
            ast::Type::ArrowType(arrow) => self.lower_type_arrow(arrow),
            ast::Type::ApplicationType(application) => self.lower_type_application(application),
            ast::Type::ConstructorType(constructor) => self.lower_type_constructor(constructor),
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
            ast::Type::VariableType(variable) => self.lower_type_variable(variable),
            ast::Type::WildcardType(_) => None,
        };
        lowered.map(|lowered| self.alloc_type(lowered, t))
    }

    fn lower_type_arrow(&mut self, arrow: &ast::ArrowType) -> Option<Type> {
        let mut arguments = vec![self.lower_type(&arrow.argument()?)?];
        let mut result = arrow.result()?;

        while let ast::Type::ArrowType(arrow) = &result {
            arguments.push(self.lower_type(&arrow.argument()?)?);
            result = arrow.result()?;
        }
        let result = self.lower_type(&result)?;

        Some(Type::Arrow(arguments.into(), result))
    }

    fn lower_type_application(&mut self, application: &ast::ApplicationType) -> Option<Type> {
        let head = self.lower_type(&application.head()?)?;
        let spine = application
            .spine()?
            .children()
            .map(|argument| self.lower_type(&argument))
            .collect::<Option<_>>()?;
        Some(Type::Application(head, spine))
    }

    fn lower_type_constructor(&mut self, constructor: &ast::ConstructorType) -> Option<Type> {
        let qualified = constructor.qualified_name()?;
        let name_ref = qualified.try_into().ok()?;
        Some(Type::Constructor(name_ref))
    }

    fn lower_type_variable(&self, variable: &ast::VariableType) -> Option<Type> {
        let name_ref = variable.name_ref()?.try_into().ok()?;
        Some(Type::Variable(name_ref))
    }
}

impl SurfaceContext {
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
}
