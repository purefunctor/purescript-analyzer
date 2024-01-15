//! Conversion from the CST to AST

use std::sync::Arc;

use files::FileId;
use itertools::Itertools;
use rowan::{
    ast::{AstChildren, AstNode, SyntaxNodePtr},
    NodeOrToken,
};
use syntax::{ast, PureScript, SyntaxKind, SyntaxNode};

use crate::{
    id::AstId,
    index::{
        nominal::{DataGroup, ValueGroup},
        NominalMap, PositionalMap,
    },
    interner::InDb,
    SurfaceDatabase,
};

use super::tree::*;

struct Ctx {
    arena: SurfaceArena,
    source_map: SourceMap,

    syntax_node: SyntaxNode,
    nominal_map: Arc<NominalMap>,
    positional_map: Arc<PositionalMap>,
}

impl Ctx {
    fn ast_of<N: AstNode<Language = PureScript>>(&self, id: AstId<N>) -> N {
        self.positional_map.ast_ptr(id).to_node(&self.syntax_node)
    }

    fn alloc_expr(&mut self, lowered: Expr, ast: Option<&ast::Expression>) -> ExprId {
        let expr_id = self.arena.alloc_expr(lowered);
        if let Some(ast) = ast {
            let expr_ptr = SyntaxNodePtr::new(ast.syntax());
            self.source_map.expr_to_cst.insert(expr_id, expr_ptr);
            self.source_map.cst_to_expr.insert(expr_ptr, expr_id);
        }
        expr_id
    }

    fn nil_expr(&mut self) -> ExprId {
        self.alloc_expr(Expr::NotImplemented, None)
    }

    fn alloc_binder(&mut self, lowered: Binder, ast: Option<&ast::Binder>) -> BinderId {
        let binder_id = self.arena.alloc_binder(lowered);
        if let Some(ast) = ast {
            let binder_ptr = SyntaxNodePtr::new(ast.syntax());
            self.source_map.binder_to_cst.insert(binder_id, binder_ptr);
            self.source_map.cst_to_binder.insert(binder_ptr, binder_id);
        }
        binder_id
    }

    fn nil_binder(&mut self) -> BinderId {
        self.alloc_binder(Binder::NotImplemented, None)
    }

    fn alloc_type(&mut self, lowered: Type, ast: Option<&ast::Type>) -> TypeId {
        let type_id = self.arena.alloc_ty(lowered);
        if let Some(ast) = ast {
            let type_ptr = SyntaxNodePtr::new(ast.syntax());
            self.source_map.type_to_cst.insert(type_id, type_ptr);
            self.source_map.cst_to_type.insert(type_ptr, type_id);
        }
        type_id
    }

    fn nil_type(&mut self) -> TypeId {
        self.alloc_type(Type::NotImplemented, None)
    }
}

fn lower_qualified_name(
    db: &dyn SurfaceDatabase,
    qualified: ast::QualifiedName,
) -> Qualified<Name> {
    let prefix = qualified.prefix().and_then(|prefix| prefix.in_db(db)).map(ModuleName::from_raw);
    let value = Name::from_raw(
        qualified
            .name_ref()
            .and_then(|name| name.in_db(db))
            .unwrap_or_else(|| db.interner().intern("$Invalid$")),
    );
    Qualified { prefix, value }
}

fn lower_module(ctx: &mut Ctx, db: &dyn SurfaceDatabase, module: ast::Module) -> Module {
    let header = module.header().map(|header| lower_header(ctx, db, header)).unwrap_or_else(|| {
        let name = ModuleName::from_raw(db.interner().intern("$Invalid$"));
        let export_list = ExportList { items: vec![], explicit: false };
        ModuleHeader { name, export_list }
    });
    let imports = module.imports().map(|imports| lower_imports(db, imports)).unwrap_or_else(|| {
        let declarations = vec![];
        ModuleImports { declarations }
    });
    let body = module.body().map(|body| lower_module_body(ctx, db, body)).unwrap_or_else(|| {
        let declarations = vec![];
        ModuleBody { declarations }
    });
    Module { header, imports, body }
}

fn lower_header(
    ctx: &mut Ctx,
    db: &dyn SurfaceDatabase,
    header: ast::ModuleHeader,
) -> ModuleHeader {
    let name = ModuleName::from_raw(
        header
            .name()
            .and_then(|name| name.in_db(db))
            .unwrap_or_else(|| db.interner().intern("$Invalid$")),
    );
    let export_list = lower_export_list(ctx, db, header.export_list());

    ModuleHeader { name, export_list }
}

fn lower_export_list(
    ctx: &mut Ctx,
    db: &dyn SurfaceDatabase,
    export_list: Option<ast::ExportList>,
) -> ExportList {
    match export_list {
        Some(export_list) => {
            let export_items =
                export_list.child().and_then(|export_list| Some(export_list.child()?.children()));

            let items = export_items
                .map(|export_items| lower_export_items(db, export_items))
                .unwrap_or_default();
            let explicit = true;

            ExportList { items, explicit }
        }
        None => {
            let items = ctx
                .nominal_map
                .iter_value_group()
                .map(|(_, value_group)| {
                    let name = Name::from_raw(Arc::clone(&value_group.name));
                    ExportItem::ExportValue(name)
                })
                .collect();
            let explicit = false;

            ExportList { items, explicit }
        }
    }
}

fn lower_export_items(
    db: &dyn SurfaceDatabase,
    export_items: AstChildren<ast::ExportItem>,
) -> Vec<ExportItem> {
    export_items
        .filter_map(|export_item| match export_item {
            ast::ExportItem::ExportType(t) => {
                let name = Name::from_raw(t.name_ref()?.in_db(db)?);
                let data_members =
                    t.data_members().map(|data_members| lower_data_members(db, data_members));
                Some(ExportItem::ExportType(name, data_members))
            }
            ast::ExportItem::ExportValue(v) => {
                let name = Name::from_raw(v.name_ref()?.in_db(db)?);
                Some(ExportItem::ExportValue(name))
            }
        })
        .collect()
}

fn lower_imports(db: &dyn SurfaceDatabase, imports: ast::ModuleImports) -> ModuleImports {
    let declarations = imports
        .imports()
        .map(|imports| {
            imports
                .children()
                .map(|import_declaration| lower_import_declaration(db, import_declaration))
                .collect()
        })
        .unwrap_or_default();
    ModuleImports { declarations }
}

fn lower_import_declaration(
    db: &dyn SurfaceDatabase,
    import_declaration: ast::ImportDeclaration,
) -> ImportDeclaration {
    let name = ModuleName::from_raw(
        import_declaration
            .module_name()
            .and_then(|module_name| module_name.in_db(db))
            .unwrap_or_else(|| db.interner().intern("$Invalid$")),
    );

    let qualified_as = import_declaration
        .import_qualified()
        .and_then(|qualified| Some(ModuleName::from_raw(qualified.module_name()?.in_db(db)?)));

    let import_list =
        import_declaration.import_list().map(|import_list| lower_import_list(db, import_list));

    ImportDeclaration { name, qualified_as, import_list }
}

fn lower_import_list(db: &dyn SurfaceDatabase, import_list: ast::ImportList) -> ImportList {
    let hiding = import_list.hiding_token().is_some();
    let items = import_list
        .import_items()
        .map(|import_items| {
            import_items
                .children()
                .filter_map(|import_item| match import_item {
                    ast::ImportItem::ImportClass(_) => None,
                    ast::ImportItem::ImportOp(_) => None,
                    ast::ImportItem::ImportType(t) => {
                        let name = Name::from_raw(t.name_ref()?.in_db(db)?);
                        let data_members = t
                            .data_members()
                            .map(|data_members| lower_data_members(db, data_members));
                        Some(ImportItem::ImportType(name, data_members))
                    }
                    ast::ImportItem::ImportTypeOp(_) => None,
                    ast::ImportItem::ImportValue(v) => {
                        let name = Name::from_raw(v.name_ref()?.in_db(db)?);
                        Some(ImportItem::ImportValue(name))
                    }
                })
                .collect()
        })
        .unwrap_or_default();
    ImportList { items, hiding }
}

fn lower_data_members(db: &dyn SurfaceDatabase, data_members: ast::DataMembers) -> DataMembers {
    match data_members {
        ast::DataMembers::DataAll(_) => DataMembers::DataAll,
        ast::DataMembers::DataEnumerated(e) => {
            let names = e
                .constructors()
                .map(|names| {
                    names
                        .children()
                        .filter_map(|name| Some(Name::from_raw(name.in_db(db)?)))
                        .collect()
                })
                .unwrap_or_default();
            DataMembers::DataEnumerated(names)
        }
    }
}

fn lower_module_body(ctx: &mut Ctx, db: &dyn SurfaceDatabase, body: ast::ModuleBody) -> ModuleBody {
    let declarations = body
        .declarations()
        .map(|declarations| lower_declarations(ctx, db, declarations.children()))
        .unwrap_or_default();
    ModuleBody { declarations }
}

fn lower_declarations(
    ctx: &mut Ctx,
    db: &dyn SurfaceDatabase,
    _: AstChildren<ast::Declaration>,
) -> Vec<Declaration> {
    let nominal_map = Arc::clone(&ctx.nominal_map);

    let mut all = vec![];

    // For declarations that exist in groups, we prefer using the nominal map
    // representations during lowering rather than accessing the AST directly
    // simply to avoid redundant work.
    all.extend(nominal_map.iter_data_group().map(|(_, data_group)| {
        Declaration::DataDeclaration(lower_data_group(ctx, db, data_group))
    }));

    all.extend(nominal_map.iter_value_group().map(|(_, value_group)| {
        Declaration::ValueDeclaration(lower_value_group(ctx, db, value_group))
    }));

    all
}

// ===== Section: DataGroup ===== //

fn lower_data_group(
    ctx: &mut Ctx,
    db: &dyn SurfaceDatabase,
    data_group: &DataGroup,
) -> DataDeclaration {
    let name = Name::from_raw(Arc::clone(&data_group.name));
    let annotation = data_group.annotation.map(|id| {
        let annotation = ctx.ast_of(id);
        lower_data_annotation(ctx, db, annotation)
    });

    let equation = ctx.ast_of(data_group.equation);
    let variables = equation
        .variables()
        .map(|variables| lower_type_variable_binding(ctx, db, variables.children()))
        .unwrap_or_default();

    let constructors = data_group
        .constructors
        .values()
        .map(|id| {
            let constructor = ctx.ast_of(*id);
            (*id, lower_data_constructor(ctx, db, constructor))
        })
        .collect();

    DataDeclaration { name, annotation, variables, constructors }
}

fn lower_data_annotation(
    ctx: &mut Ctx,
    db: &dyn SurfaceDatabase,
    annotation: ast::DataAnnotation,
) -> TypeId {
    annotation.kind().map(|kind| lower_type(ctx, db, &kind)).unwrap_or_else(|| ctx.nil_type())
}

fn lower_data_constructor(
    ctx: &mut Ctx,
    db: &dyn SurfaceDatabase,
    constructor: ast::DataConstructor,
) -> DataConstructor {
    let name = Name::from_raw(
        constructor
            .name()
            .and_then(|name| name.in_db(db))
            .unwrap_or_else(|| db.interner().intern("$Invalid$")),
    );
    let fields = constructor
        .fields()
        .map(|fields| fields.children().map(|field| lower_type(ctx, db, &field)).collect())
        .unwrap_or_default();
    DataConstructor { name, fields }
}

// ===== Section: ValueGroup ===== //

fn lower_value_group(
    ctx: &mut Ctx,
    db: &dyn SurfaceDatabase,
    value_group: &ValueGroup,
) -> ValueDeclaration {
    let name = Name::from_raw(Arc::clone(&value_group.name));
    let annotation = value_group.annotation.map(|id| {
        let annotation = ctx.ast_of(id);
        lower_value_annotation(ctx, db, annotation)
    });

    let equations = value_group
        .equations
        .iter()
        .map(|equation_id| {
            let equation = ctx.ast_of(*equation_id);
            lower_value_equation(ctx, db, equation)
        })
        .collect();

    ValueDeclaration { name, annotation, equations }
}

fn lower_value_annotation(
    ctx: &mut Ctx,
    db: &dyn SurfaceDatabase,
    annotation: ast::ValueAnnotationDeclaration,
) -> TypeId {
    annotation.ty().map(|ty| lower_type(ctx, db, &ty)).unwrap_or_else(|| ctx.nil_type())
}

fn lower_value_equation(
    ctx: &mut Ctx,
    db: &dyn SurfaceDatabase,
    equation: ast::ValueEquationDeclaration,
) -> ValueEquation {
    let binders = equation
        .binders()
        .map(|binders| binders.children().map(|binder| lower_binder(ctx, db, &binder)).collect())
        .unwrap_or_default();
    let binding = equation
        .binding()
        .map(|binding| lower_binding(ctx, db, binding))
        .unwrap_or_else(|| todo!("FIXME: support nil binding"));
    ValueEquation { binders, binding }
}

fn lower_binding(ctx: &mut Ctx, db: &dyn SurfaceDatabase, binding: ast::Binding) -> Binding {
    match binding {
        ast::Binding::UnconditionalBinding(unconditional) => {
            let where_expr = unconditional
                .where_expression()
                .map(|where_expr| lower_where_expr(ctx, db, where_expr))
                .unwrap_or_else(|| todo!("FIXME: support nil where_expr"));
            Binding::Unconditional { where_expr }
        }
        ast::Binding::GuardedBinding(_) => todo!("FIXME: lowering for guarded binding"),
    }
}

fn lower_where_expr(
    ctx: &mut Ctx,
    db: &dyn SurfaceDatabase,
    where_expr: ast::WhereExpression,
) -> WhereExpr {
    let let_bindings = where_expr
        .let_bindings()
        .map(|let_bindings| lower_let_bindings(ctx, db, &let_bindings))
        .unwrap_or_default();
    let expr_id = where_expr
        .expression()
        .map(|expression| lower_expr(ctx, db, &expression))
        .unwrap_or_else(|| ctx.nil_expr());
    WhereExpr { expr_id, let_bindings }
}

// ===== Section: LetBinding ===== //

fn lower_let_bindings(
    ctx: &mut Ctx,
    db: &dyn SurfaceDatabase,
    let_bindings: &ast::OneOrMore<ast::LetBinding>,
) -> Vec<LetBinding> {
    #[derive(Debug, PartialEq, Eq)]
    enum GroupKey {
        Name(Name),
        Pattern,
    }

    let let_groups = let_bindings.children().group_by(|let_binding| match let_binding {
        ast::LetBinding::LetBindingName(n) => {
            let name = Name::from_raw(n.name()?.in_db(db)?);
            Some(GroupKey::Name(name))
        }
        ast::LetBinding::LetBindingPattern(_) => Some(GroupKey::Pattern),
        ast::LetBinding::LetBindingSignature(s) => {
            let name = Name::from_raw(s.name()?.in_db(db)?);
            Some(GroupKey::Name(name))
        }
    });

    let let_bindings = let_groups
        .into_iter()
        .filter_map(|(key, mut group)| match key? {
            GroupKey::Name(name) => {
                let mut annotation = None;
                let mut annotation_ptr = None;
                let mut equations = vec![];
                let mut equations_ptr = vec![];

                match group.next()? {
                    ast::LetBinding::LetBindingName(n) => {
                        equations.push(lower_let_binding_name(ctx, db, &n));
                        equations_ptr.push(SyntaxNodePtr::new(n.syntax()));
                    }
                    ast::LetBinding::LetBindingPattern(_) => {
                        unreachable!("invariant violated: impossible");
                    }
                    ast::LetBinding::LetBindingSignature(s) => {
                        annotation = Some(lower_let_binding_signature(ctx, db, &s));
                        annotation_ptr = Some(SyntaxNodePtr::new(s.syntax()));
                    }
                }

                equations.extend(group.filter_map(|let_binding| {
                    if let ast::LetBinding::LetBindingName(n) = let_binding {
                        equations_ptr.push(SyntaxNodePtr::new(n.syntax()));
                        Some(lower_let_binding_name(ctx, db, &n))
                    } else {
                        None
                    }
                }));

                let id = ctx.arena.alloc_let_name(LetName { name, annotation, equations });
                annotation_ptr.iter().cloned().for_each(|annotation_ptr| {
                    ctx.source_map.cst_to_let_name.insert(annotation_ptr, id);
                });
                equations_ptr.iter().cloned().for_each(|equation_ptr| {
                    ctx.source_map.cst_to_let_name.insert(equation_ptr, id);
                });
                ctx.source_map
                    .let_name_to_cst
                    .insert(id, LetNamePtr { annotation_ptr, equations_ptr });

                Some(LetBinding::Name { id })
            }
            GroupKey::Pattern => {
                if let ast::LetBinding::LetBindingPattern(pattern) = group.next()? {
                    let binder = pattern
                        .binder()
                        .map(|binder| lower_binder(ctx, db, &binder))
                        .unwrap_or_else(|| ctx.nil_binder());
                    let where_expr = pattern
                        .where_expr()
                        .map(|where_expr| lower_where_expr(ctx, db, where_expr))
                        .unwrap_or_else(|| todo!("FIXME: support nil where_expr"));
                    Some(LetBinding::Pattern { binder, where_expr })
                } else {
                    unreachable!("invariant violated: impossible");
                }
            }
        })
        .collect();

    let_bindings
}

fn lower_let_binding_name(
    ctx: &mut Ctx,
    db: &dyn SurfaceDatabase,
    let_binding_name: &ast::LetBindingName,
) -> LetNameEquation {
    let binders = let_binding_name
        .binders()
        .map(|binders| binders.children().map(|binder| lower_binder(ctx, db, &binder)).collect())
        .unwrap_or_default();
    let binding = let_binding_name
        .binding()
        .map(|binding| lower_binding(ctx, db, binding))
        .unwrap_or_else(|| todo!("FIXME: support nil binding"));
    LetNameEquation { binders, binding }
}

fn lower_let_binding_signature(
    ctx: &mut Ctx,
    db: &dyn SurfaceDatabase,
    let_binding_signature: &ast::LetBindingSignature,
) -> TypeId {
    let_binding_signature.ty().map(|ty| lower_type(ctx, db, &ty)).unwrap_or_else(|| ctx.nil_type())
}

// ===== Section: Literal ===== //

fn lower_literal<T, I>(
    ctx: &mut Ctx,
    db: &dyn SurfaceDatabase,
    literal: syntax::SyntaxElement,
    lower_inner: impl Fn(&mut Ctx, &dyn SurfaceDatabase, Option<&T>) -> I,
) -> Literal<I>
where
    T: AstNode<Language = PureScript>,
{
    match literal {
        NodeOrToken::Node(n) => match n.kind() {
            SyntaxKind::LiteralArray => {
                type Contents<T> = ast::Wrapped<ast::Separated<T>>;

                let contents = n
                    .first_child()
                    .and_then(Contents::cast)
                    .and_then(|wrapped| Some(wrapped.child()?.children()));

                let contents = contents
                    .map(|contents| {
                        contents.map(|inner| lower_inner(ctx, db, Some(&inner))).collect()
                    })
                    .unwrap_or_default();

                Literal::Array(contents)
            }
            SyntaxKind::LiteralRecord => {
                type Contents = ast::Wrapped<ast::Separated<ast::RecordItem>>;

                let contents = n
                    .first_child()
                    .and_then(Contents::cast)
                    .and_then(|wrapped| Some(wrapped.child()?.children()));

                let contents = contents
                    .map(|contents| {
                        contents
                            .map(|item| match item {
                                ast::RecordItem::RecordField(field) => {
                                    let name = field
                                        .name()
                                        .and_then(|name| name.in_db(db))
                                        .unwrap_or_else(|| db.interner().intern("$Invalid$"));
                                    let value = lower_inner(ctx, db, field.value().as_ref());
                                    RecordItem::RecordField(name, value)
                                }
                                ast::RecordItem::RecordPun(pun) => {
                                    let name = pun
                                        .name()
                                        .and_then(|name| name.in_db(db))
                                        .unwrap_or_else(|| db.interner().intern("$Invalid$"));
                                    RecordItem::RecordPun(name)
                                }
                            })
                            .collect()
                    })
                    .unwrap_or_default();

                Literal::Record(contents)
            }
            _ => unreachable!("Impossible."),
        },
        NodeOrToken::Token(t) => match t.kind() {
            SyntaxKind::LiteralInteger => {
                let value = t.text().parse().ok().unwrap();
                Literal::Int(value)
            }
            SyntaxKind::LiteralNumber => {
                let value = db.interner().intern(t.text());
                Literal::Number(value)
            }
            SyntaxKind::LiteralString | SyntaxKind::LiteralRawString => {
                let value = db.interner().intern(t.text());
                Literal::String(value)
            }
            SyntaxKind::LiteralChar => {
                let value = db.interner().intern(t.text());
                Literal::Char(value)
            }
            SyntaxKind::LiteralTrue => Literal::Boolean(true),
            SyntaxKind::LiteralFalse => Literal::Boolean(false),
            _ => unreachable!("Impossible."),
        },
    }
}

// ===== Section: Expr ===== //

fn lower_expr(ctx: &mut Ctx, db: &dyn SurfaceDatabase, expr: &ast::Expression) -> ExprId {
    let lowered = match expr {
        ast::Expression::AdoExpression(_) => Expr::NotImplemented,
        ast::Expression::ApplicationExpression(_) => Expr::NotImplemented,
        ast::Expression::CaseExpression(_) => Expr::NotImplemented,
        ast::Expression::ConstructorExpression(_) => Expr::NotImplemented,
        ast::Expression::DoExpression(_) => Expr::NotImplemented,
        ast::Expression::ExpressionInfixChain(_) => Expr::NotImplemented,
        ast::Expression::ExpressionOperatorChain(_) => Expr::NotImplemented,
        ast::Expression::IfThenElseExpression(_) => Expr::NotImplemented,
        ast::Expression::LambdaExpression(_) => Expr::NotImplemented,
        ast::Expression::LetInExpression(_) => Expr::NotImplemented,
        ast::Expression::LiteralExpression(_) => Expr::NotImplemented,
        ast::Expression::OperatorNameExpression(_) => Expr::NotImplemented,
        ast::Expression::ParenthesizedExpression(_) => Expr::NotImplemented,
        ast::Expression::RecordAccessExpression(_) => Expr::NotImplemented,
        ast::Expression::RecordUpdateExpression(_) => Expr::NotImplemented,
        ast::Expression::TypedExpression(_) => Expr::NotImplemented,
        ast::Expression::VariableExpression(_) => Expr::NotImplemented,
    };
    ctx.alloc_expr(lowered, Some(expr))
}

// ===== Section: Binder ===== //

fn lower_binder(ctx: &mut Ctx, db: &dyn SurfaceDatabase, binder: &ast::Binder) -> BinderId {
    let lowered = match binder {
        ast::Binder::ConstructorBinder(c) => lower_binder_constructor(ctx, db, c),
        ast::Binder::LiteralBinder(l) => lower_binder_literal(ctx, db, l),
        ast::Binder::NegativeBinder(_) => Binder::NotImplemented,
        ast::Binder::ParenthesizedBinder(p) => lower_binder_parenthesized(ctx, db, p),
        ast::Binder::TypedBinder(_) => Binder::NotImplemented,
        ast::Binder::VariableBinder(v) => lower_binder_variable(db, v),
        ast::Binder::WildcardBinder(_) => Binder::Wildcard,
    };
    ctx.alloc_binder(lowered, Some(binder))
}

fn lower_binder_constructor(
    ctx: &mut Ctx,
    db: &dyn SurfaceDatabase,
    constructor: &ast::ConstructorBinder,
) -> Binder {
    let name = constructor
        .qualified_name()
        .map(|qualified| lower_qualified_name(db, qualified))
        .unwrap_or_else(|| {
            let prefix = None;
            let value = Name::from_raw(db.interner().intern("$Invalid$"));
            Qualified { prefix, value }
        });
    let fields = constructor
        .fields()
        .map(|fields| fields.children().map(|field| lower_binder(ctx, db, &field)).collect())
        .unwrap_or_default();
    Binder::Constructor { name, fields }
}

fn lower_binder_literal(
    ctx: &mut Ctx,
    db: &dyn SurfaceDatabase,
    literal: &ast::LiteralBinder,
) -> Binder {
    literal
        .syntax()
        .first_child_or_token()
        .map(|literal| {
            Binder::Literal(lower_literal(ctx, db, literal, |ctx, db, binder| {
                binder
                    .map(|binder| lower_binder(ctx, db, binder))
                    .unwrap_or_else(|| ctx.nil_binder())
            }))
        })
        .unwrap_or_else(|| Binder::NotImplemented)
}

fn lower_binder_parenthesized(
    ctx: &mut Ctx,
    db: &dyn SurfaceDatabase,
    parenthesized: &ast::ParenthesizedBinder,
) -> Binder {
    Binder::Parenthesized(
        parenthesized
            .binder()
            .map(|binder| lower_binder(ctx, db, &binder))
            .unwrap_or_else(|| ctx.nil_binder()),
    )
}

fn lower_binder_variable(db: &dyn SurfaceDatabase, variable: &ast::VariableBinder) -> Binder {
    let name = Name::from_raw(
        variable
            .name()
            .and_then(|name| name.in_db(db))
            .unwrap_or_else(|| db.interner().intern("$Invalid$")),
    );
    Binder::Variable(name)
}

// ===== Section: Type ===== //

fn lower_type(ctx: &mut Ctx, db: &dyn SurfaceDatabase, ty: &ast::Type) -> TypeId {
    let lowered = match ty {
        ast::Type::ApplicationType(a) => lower_type_application(ctx, db, a),
        ast::Type::ArrowType(a) => lower_type_arrow(ctx, db, a),
        ast::Type::ConstructorType(c) => lower_type_constructor(db, c),
        ast::Type::IntegerType(_) => Type::NotImplemented,
        ast::Type::KindedType(_) => Type::NotImplemented,
        ast::Type::OperatorNameType(_) => Type::NotImplemented,
        ast::Type::ParenthesizedType(p) => lower_type_parenthesized(ctx, db, p),
        ast::Type::RecordType(_) => Type::NotImplemented,
        ast::Type::RowType(_) => Type::NotImplemented,
        ast::Type::StringType(_) => Type::NotImplemented,
        ast::Type::TypeOperatorChain(_) => Type::NotImplemented,
        ast::Type::VariableType(v) => lower_type_variable(db, v),
        ast::Type::WildcardType(_) => Type::NotImplemented,
    };
    ctx.alloc_type(lowered, Some(ty))
}

fn lower_type_application(
    ctx: &mut Ctx,
    db: &dyn SurfaceDatabase,
    application: &ast::ApplicationType,
) -> Type {
    let head =
        application.head().map(|head| lower_type(ctx, db, &head)).unwrap_or_else(|| ctx.nil_type());
    let spine = application
        .spine()
        .map(|spine| spine.children().map(|argument| lower_type(ctx, db, &argument)).collect())
        .unwrap_or_default();

    Type::Application(head, spine)
}

fn lower_type_arrow(ctx: &mut Ctx, db: &dyn SurfaceDatabase, arrow: &ast::ArrowType) -> Type {
    let mut arguments = vec![arrow
        .argument()
        .map(|argument| lower_type(ctx, db, &argument))
        .unwrap_or_else(|| ctx.nil_type())];
    let mut result = arrow.result();

    while let Some(ast::Type::ArrowType(arrow)) = &result {
        arguments.push(
            arrow
                .argument()
                .map(|argument| lower_type(ctx, db, &argument))
                .unwrap_or_else(|| ctx.nil_type()),
        );
        result = arrow.result();
    }

    let result =
        result.map(|result| lower_type(ctx, db, &result)).unwrap_or_else(|| ctx.nil_type());

    Type::Arrow(arguments, result)
}

fn lower_type_constructor(db: &dyn SurfaceDatabase, constructor: &ast::ConstructorType) -> Type {
    let name = constructor
        .qualified_name()
        .map(|qualified| lower_qualified_name(db, qualified))
        .unwrap_or_else(|| {
            let prefix = None;
            let value = Name::from_raw(db.interner().intern("$Invalid$"));
            Qualified { prefix, value }
        });

    Type::Constructor(name)
}

fn lower_type_parenthesized(
    ctx: &mut Ctx,
    db: &dyn SurfaceDatabase,
    parenthesized: &ast::ParenthesizedType,
) -> Type {
    Type::Parenthesized(
        parenthesized.ty().map(|ty| lower_type(ctx, db, &ty)).unwrap_or_else(|| ctx.nil_type()),
    )
}

fn lower_type_variable(db: &dyn SurfaceDatabase, variable: &ast::VariableType) -> Type {
    let name = Name::from_raw(
        variable
            .name_ref()
            .and_then(|name| name.in_db(db))
            .unwrap_or_else(|| db.interner().intern("$Invalid$")),
    );
    Type::Variable(name)
}

fn lower_type_variable_binding(
    ctx: &mut Ctx,
    db: &dyn SurfaceDatabase,
    variable_bindings: AstChildren<ast::TypeVariableBinding>,
) -> Vec<TypeVariable> {
    variable_bindings
        .filter_map(|variable| match variable {
            ast::TypeVariableBinding::TypeVariableKinded(k) => {
                let name = Name::from_raw(k.name()?.in_db(db)?);
                let kind = lower_type(ctx, db, &k.kind()?);
                Some(TypeVariable::Kinded(name, kind))
            }
            ast::TypeVariableBinding::TypeVariableName(n) => {
                let name = Name::from_raw(n.name()?.in_db(db)?);
                Some(TypeVariable::Name(name))
            }
        })
        .collect()
}

pub(super) fn file_surface_query(
    db: &dyn SurfaceDatabase,
    file_id: FileId,
) -> (Arc<Module>, Arc<SurfaceArena>) {
    let node = db.parse_file(file_id);

    ast::Source::<ast::Module>::cast(node)
        .and_then(|source| {
            let mut ctx = Ctx {
                arena: Default::default(),
                source_map: Default::default(),
                syntax_node: db.parse_file(file_id),
                nominal_map: db.nominal_map(file_id),
                positional_map: db.positional_map(file_id),
            };
            let module = lower_module(&mut ctx, db, source.child()?);
            Some((Arc::new(module), Arc::new(ctx.arena)))
        })
        .unwrap_or_else(|| {
            unreachable!("impossible: empty source files have a traversable CST");
        })
}
