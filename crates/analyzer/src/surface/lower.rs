//! Conversion from the CST to AST

use std::sync::Arc;

use files::FileId;
use itertools::Itertools;
use rowan::{
    ast::{AstChildren, AstNode, AstPtr},
    NodeOrToken,
};
use rustc_hash::FxHashMap;
use syntax::{ast, PureScript, SyntaxKind, SyntaxNode, SyntaxNodePtr};

use crate::{
    id::AstId,
    index::{
        nominal::{ClassGroup, ClassGroupId, DataGroup, DataGroupId, ValueGroup, ValueGroupId},
        NominalMap, PositionalMap,
    },
    interner::InDb,
    SurfaceDatabase,
};

use super::tree::*;

// region: Context

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

    fn alloc_let_name(
        &mut self,
        lowered: LetName,
        annotation_ptr: Option<SyntaxNodePtr>,
        equations_ptr: Vec<SyntaxNodePtr>,
    ) -> LetNameId {
        let id = self.arena.alloc_let_name(lowered);
        if let Some(annotation_ptr) = annotation_ptr {
            self.source_map.cst_to_let_name.insert(annotation_ptr, id);
        }
        for equation_ptr in &equations_ptr {
            self.source_map.cst_to_let_name.insert(*equation_ptr, id);
        }
        self.source_map.let_name_to_cst.insert(id, LetNamePtr { annotation_ptr, equations_ptr });
        id
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

    fn alloc_type(&mut self, lowered: Type, ast: Option<&ast::Type>) -> TypeId {
        let type_id = self.arena.alloc_ty(lowered);
        if let Some(ast) = ast {
            let type_ptr = SyntaxNodePtr::new(ast.syntax());
            self.source_map.type_to_cst.insert(type_id, type_ptr);
            self.source_map.cst_to_type.insert(type_ptr, type_id);
        }
        type_id
    }
}

// endregion

// region: Names

fn lower_name(db: &dyn SurfaceDatabase, name: Option<ast::Name>) -> Name {
    Name::from_raw(
        name.and_then(|name| name.in_db(db)).unwrap_or_else(|| db.interner().intern("$Invalid")),
    )
}

fn lower_name_ref(db: &dyn SurfaceDatabase, name_ref: Option<ast::NameRef>) -> Name {
    Name::from_raw(
        name_ref
            .and_then(|name_ref| name_ref.in_db(db))
            .unwrap_or_else(|| db.interner().intern("$Invalid")),
    )
}

fn lower_module_name(db: &dyn SurfaceDatabase, module_name: Option<ast::ModuleName>) -> ModuleName {
    ModuleName::from_raw(
        module_name
            .and_then(|module_name| module_name.in_db(db))
            .unwrap_or_else(|| db.interner().intern("$Invalid")),
    )
}

fn lower_qualified_prefix(db: &dyn SurfaceDatabase, prefix: ast::QualifiedPrefix) -> ModuleName {
    ModuleName::from_raw(prefix.in_db(db).unwrap_or_else(|| db.interner().intern("$Invalid")))
}

fn lower_qualified_name(
    db: &dyn SurfaceDatabase,
    qualified: Option<ast::QualifiedName>,
) -> Qualified<Name> {
    if let Some(qualified) = qualified {
        let prefix = qualified.prefix().map(|prefix| lower_qualified_prefix(db, prefix));
        let value = lower_name_ref(db, qualified.name_ref());
        Qualified { prefix, value }
    } else {
        let prefix = None;
        let value = lower_name_ref(db, None);
        Qualified { prefix, value }
    }
}

// endregion

// region: Module

fn lower_module(ctx: &mut Ctx, db: &dyn SurfaceDatabase, module: ast::Module) -> Module {
    let header = lower_header(db, module.header());
    let imports = lower_imports(db, module.imports());
    let body = lower_module_body(ctx, db, module.body());
    Module { header, imports, body }
}

// endregion

// region: ModuleHeader

fn lower_header(db: &dyn SurfaceDatabase, header: Option<ast::ModuleHeader>) -> ModuleHeader {
    if let Some(header) = header {
        let name = lower_module_name(db, header.name());
        let export_list =
            header.export_list().map(|export_list| lower_export_list(db, export_list));
        ModuleHeader { name, export_list }
    } else {
        let name = lower_module_name(db, None);
        let export_list = None;
        ModuleHeader { name, export_list }
    }
}

fn lower_export_list(db: &dyn SurfaceDatabase, export_list: ast::ExportList) -> ExportList {
    let items = export_list
        .export_items()
        .map(|export_items| {
            export_items.children().map(|export_item| lower_export_item(db, export_item)).collect()
        })
        .unwrap_or_default();
    ExportList { items }
}

fn lower_export_item(db: &dyn SurfaceDatabase, export_item: ast::ExportItem) -> ExportItem {
    match export_item {
        ast::ExportItem::ExportClass(c) => {
            let name = lower_name_ref(db, c.name_ref());
            ExportItem::ExportClass(name)
        }
        ast::ExportItem::ExportType(t) => {
            let name = lower_name_ref(db, t.name_ref());
            let data_members =
                t.data_members().map(|data_members| lower_data_members(db, data_members));
            ExportItem::ExportType(name, data_members)
        }
        ast::ExportItem::ExportValue(v) => {
            let name = lower_name_ref(db, v.name_ref());
            ExportItem::ExportValue(name)
        }
    }
}

fn lower_imports(db: &dyn SurfaceDatabase, imports: Option<ast::ModuleImports>) -> ModuleImports {
    imports
        .and_then(|imports| {
            let declarations = imports
                .imports()?
                .children()
                .map(|import_declaration| lower_import_declaration(db, import_declaration))
                .collect();
            Some(ModuleImports { declarations })
        })
        .unwrap_or_else(|| {
            let declarations = vec![];
            ModuleImports { declarations }
        })
}

fn lower_import_declaration(
    db: &dyn SurfaceDatabase,
    import_declaration: ast::ImportDeclaration,
) -> ImportDeclaration {
    let name = lower_module_name(db, import_declaration.module_name());

    let qualified_as = import_declaration
        .import_qualified()
        .map(|import_qualified| lower_module_name(db, import_qualified.module_name()));

    let import_list =
        import_declaration.import_list().map(|import_list| lower_import_list(db, import_list));

    ImportDeclaration { name, qualified_as, import_list }
}

fn lower_import_list(db: &dyn SurfaceDatabase, import_list: ast::ImportList) -> ImportList {
    let hiding = import_list.hiding_token().is_some();
    let items = import_list
        .import_items()
        .map(|import_items| {
            import_items.children().map(|import_item| lower_import_item(db, import_item)).collect()
        })
        .unwrap_or_default();
    ImportList { items, hiding }
}

fn lower_import_item(db: &dyn SurfaceDatabase, import_item: ast::ImportItem) -> ImportItem {
    match import_item {
        ast::ImportItem::ImportClass(c) => {
            let name = lower_name_ref(db, c.name_ref());
            ImportItem::ImportClass(name)
        }
        ast::ImportItem::ImportOp(_) => todo!("ImportOp"),
        ast::ImportItem::ImportType(t) => {
            let name = lower_name_ref(db, t.name_ref());
            let data_members =
                t.data_members().map(|data_members| lower_data_members(db, data_members));
            ImportItem::ImportType(name, data_members)
        }
        ast::ImportItem::ImportTypeOp(_) => todo!("ImportTypeOp"),
        ast::ImportItem::ImportValue(v) => {
            let name = lower_name_ref(db, v.name_ref());
            ImportItem::ImportValue(name)
        }
    }
}

fn lower_data_members(db: &dyn SurfaceDatabase, data_members: ast::DataMembers) -> DataMembers {
    match data_members {
        ast::DataMembers::DataAll(_) => DataMembers::DataAll,
        ast::DataMembers::DataEnumerated(e) => {
            let names = e
                .constructors()
                .map(|names| names.children().map(|name| lower_name_ref(db, Some(name))).collect())
                .unwrap_or_default();
            DataMembers::DataEnumerated(names)
        }
    }
}

// endregion

// region: ModuleBody

fn lower_module_body(
    ctx: &mut Ctx,
    db: &dyn SurfaceDatabase,
    body: Option<ast::ModuleBody>,
) -> ModuleBody {
    if let Some(declarations) = body.and_then(|body| Some(body.declarations()?.children())) {
        lower_declarations(ctx, db, declarations)
    } else {
        ModuleBody::default()
    }
}

// For declarations that exist in groups, we prefer using the nominal map
// representations during lowering rather than accessing the AST directly.
fn lower_declarations(
    ctx: &mut Ctx,
    db: &dyn SurfaceDatabase,
    _declarations: AstChildren<ast::Declaration>,
) -> ModuleBody {
    let nominal_map = Arc::clone(&ctx.nominal_map);
    let mut module_body = ModuleBody::default();

    let mut index = 0;
    module_body.declarations.extend(nominal_map.iter_class_group().map(|(id, class_group)| {
        module_body.class_declarations.insert(id.value, index);
        index += 1;
        Declaration::ClassDeclaration(lower_class_group(ctx, db, id.value, class_group))
    }));

    module_body.declarations.extend(nominal_map.iter_data_group().map(|(id, data_group)| {
        module_body.data_declarations.insert(id.value, index);
        index += 1;
        Declaration::DataDeclaration(lower_data_group(ctx, db, id.value, data_group))
    }));

    module_body.declarations.extend(nominal_map.iter_value_group().map(|(id, value_group)| {
        module_body.value_declarations.insert(id.value, index);
        index += 1;
        Declaration::ValueDeclaration(lower_value_group(ctx, db, id.value, value_group))
    }));

    module_body
}

// endregion

// region: DataGroup

fn lower_data_group(
    ctx: &mut Ctx,
    db: &dyn SurfaceDatabase,
    id: DataGroupId,
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

    DataDeclaration { id, name, annotation, variables, constructors }
}

fn lower_data_annotation(
    ctx: &mut Ctx,
    db: &dyn SurfaceDatabase,
    annotation: ast::DataAnnotation,
) -> TypeId {
    lower_type(ctx, db, annotation.kind())
}

fn lower_data_constructor(
    ctx: &mut Ctx,
    db: &dyn SurfaceDatabase,
    constructor: ast::DataConstructor,
) -> DataConstructor {
    let name = lower_name(db, constructor.name());
    let fields = constructor
        .fields()
        .map(|fields| fields.children().map(|field| lower_type(ctx, db, Some(field))).collect())
        .unwrap_or_default();
    DataConstructor { name, fields }
}

// endregion

// region: ClassGroup

fn lower_class_group(
    ctx: &mut Ctx,
    db: &dyn SurfaceDatabase,
    id: ClassGroupId,
    class_group: &ClassGroup,
) -> ClassDeclaration {
    let name = Name::from_raw(Arc::clone(&class_group.name));
    let signature = class_group.signature.map(|id| {
        let signature = ctx.ast_of(id);
        lower_class_signature(ctx, db, signature)
    });

    let declaration = ctx.ast_of(class_group.declaration);
    let constraints = lower_class_constraints(ctx, db, declaration.constraints());
    let variables = lower_class_variables(ctx, db, declaration.variables());
    let fundeps = lower_class_fundeps(db, declaration.fundeps());
    let members = lower_class_members(ctx, db, class_group.members.values().copied());

    ClassDeclaration { id, name, signature, constraints, variables, fundeps, members }
}

fn lower_class_signature(
    ctx: &mut Ctx,
    db: &dyn SurfaceDatabase,
    signature: ast::ClassSignature,
) -> TypeId {
    lower_type(ctx, db, signature.kind())
}

fn lower_class_constraints(
    ctx: &mut Ctx,
    db: &dyn SurfaceDatabase,
    constraints: Option<ast::ClassConstraints>,
) -> Vec<TypeId> {
    if let Some(constraints) = constraints {
        constraints.children().map(|constraint| lower_type(ctx, db, Some(constraint))).collect_vec()
    } else {
        vec![]
    }
}

fn lower_class_variables(
    ctx: &mut Ctx,
    db: &dyn SurfaceDatabase,
    variables: Option<ast::ClassVariables>,
) -> Vec<TypeVariable> {
    if let Some(variables) = variables {
        lower_type_variable_binding(ctx, db, variables.children())
    } else {
        vec![]
    }
}

fn lower_class_fundeps(
    db: &dyn SurfaceDatabase,
    fundeps: Option<ast::ClassFundeps>,
) -> Vec<FunctionalDependency> {
    if let Some(fundeps) = fundeps {
        fundeps
            .children()
            .map(|fundep| match fundep {
                ast::Fundep::Determined(determined) => {
                    let rhs = lower_fundep_variables(db, determined.rhs());
                    FunctionalDependency::Determined(rhs)
                }
                ast::Fundep::Determines(determines) => {
                    let lhs = lower_fundep_variables(db, determines.lhs());
                    let rhs = lower_fundep_variables(db, determines.rhs());
                    FunctionalDependency::Determines(lhs, rhs)
                }
            })
            .collect_vec()
    } else {
        vec![]
    }
}

fn lower_fundep_variables(
    db: &dyn SurfaceDatabase,
    variables: Option<ast::FundepVariables>,
) -> Vec<Name> {
    if let Some(variables) = variables {
        variables.children().map(|variable| lower_name(db, Some(variable))).collect_vec()
    } else {
        vec![]
    }
}

fn lower_class_members(
    ctx: &mut Ctx,
    db: &dyn SurfaceDatabase,
    members: impl Iterator<Item = AstId<ast::ClassMember>>,
) -> FxHashMap<AstId<ast::ClassMember>, ClassMember> {
    members
        .map(|id| {
            let member = ctx.ast_of(id);
            let name = lower_name(db, member.name());
            let ty = lower_type(ctx, db, member.ty());
            (id, ClassMember { name, ty })
        })
        .collect()
}

// endregion

// region: ValueGroup

fn lower_value_group(
    ctx: &mut Ctx,
    db: &dyn SurfaceDatabase,
    id: ValueGroupId,
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

    ValueDeclaration { id, name, annotation, equations }
}

fn lower_value_annotation(
    ctx: &mut Ctx,
    db: &dyn SurfaceDatabase,
    annotation: ast::ValueAnnotationDeclaration,
) -> TypeId {
    lower_type(ctx, db, annotation.ty())
}

fn lower_value_equation(
    ctx: &mut Ctx,
    db: &dyn SurfaceDatabase,
    equation: ast::ValueEquationDeclaration,
) -> ValueEquation {
    let binders = equation
        .binders()
        .map(|binders| {
            binders.children().map(|binder| lower_binder(ctx, db, Some(binder))).collect()
        })
        .unwrap_or_default();
    let binding = lower_binding(ctx, db, equation.binding());
    ValueEquation { binders, binding }
}

fn lower_binding(
    ctx: &mut Ctx,
    db: &dyn SurfaceDatabase,
    binding: Option<ast::Binding>,
) -> Binding {
    if let Some(binding) = binding {
        match binding {
            ast::Binding::UnconditionalBinding(unconditional) => {
                let where_expr = lower_where_expr(ctx, db, unconditional.where_expression());
                Binding::Unconditional { where_expr }
            }
            ast::Binding::GuardedBinding(_) => todo!("FIXME: fix support for guarded binding."),
        }
    } else {
        let where_expr = lower_where_expr(ctx, db, None);
        Binding::Unconditional { where_expr }
    }
}

fn lower_where_expr(
    ctx: &mut Ctx,
    db: &dyn SurfaceDatabase,
    where_expr: Option<ast::WhereExpression>,
) -> WhereExpr {
    if let Some(where_expr) = where_expr {
        let let_bindings = where_expr
            .let_bindings()
            .map(|let_bindings| lower_let_bindings(ctx, db, &let_bindings))
            .unwrap_or_default();
        let expr_id = lower_expr(ctx, db, where_expr.expression());
        WhereExpr { let_bindings, expr_id }
    } else {
        let let_bindings = vec![];
        let expr_id = lower_expr(ctx, db, None);
        WhereExpr { expr_id, let_bindings }
    }
}

// endregion

// region: LetBinding

fn lower_let_bindings(
    ctx: &mut Ctx,
    db: &dyn SurfaceDatabase,
    let_bindings: &ast::OneOrMore<ast::LetBinding>,
) -> Vec<LetBinding> {
    #[derive(Debug, PartialEq, Eq)]
    enum GroupKey {
        Name(Name),
        Pattern(AstPtr<ast::LetBindingPattern>),
    }

    let let_groups = let_bindings.children().group_by(|let_binding| match let_binding {
        ast::LetBinding::LetBindingName(n) => {
            let name = Name::from_raw(n.name()?.in_db(db)?);
            Some(GroupKey::Name(name))
        }
        ast::LetBinding::LetBindingPattern(p) => Some(GroupKey::Pattern(AstPtr::new(p))),
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

                let id = ctx.alloc_let_name(
                    LetName { name, annotation, equations },
                    annotation_ptr,
                    equations_ptr,
                );

                Some(LetBinding::Name { id })
            }
            GroupKey::Pattern(_) => {
                if let ast::LetBinding::LetBindingPattern(pattern) = group.next()? {
                    let binder = lower_binder(ctx, db, pattern.binder());
                    let where_expr = lower_where_expr(ctx, db, pattern.where_expr());
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
        .map(|binders| {
            binders.children().map(|binder| lower_binder(ctx, db, Some(binder))).collect()
        })
        .unwrap_or_default();
    let binding = lower_binding(ctx, db, let_binding_name.binding());
    LetNameEquation { binders, binding }
}

fn lower_let_binding_signature(
    ctx: &mut Ctx,
    db: &dyn SurfaceDatabase,
    let_binding_signature: &ast::LetBindingSignature,
) -> TypeId {
    lower_type(ctx, db, let_binding_signature.ty())
}

// endregion

// region: Literal

fn lower_literal<T, I>(
    ctx: &mut Ctx,
    db: &dyn SurfaceDatabase,
    literal: syntax::SyntaxElement,
    lower_inner: impl Fn(&mut Ctx, &dyn SurfaceDatabase, Option<T>) -> I,
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
                        contents.map(|inner| lower_inner(ctx, db, Some(inner))).collect()
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
                                    let name = lower_name(db, field.name());
                                    let value = lower_inner(ctx, db, field.value());
                                    RecordItem::RecordField(name, value)
                                }
                                ast::RecordItem::RecordPun(pun) => {
                                    let name = lower_name(db, pun.name());
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

// endregion

// region: Expr

fn lower_expr(ctx: &mut Ctx, db: &dyn SurfaceDatabase, expr: Option<ast::Expression>) -> ExprId {
    if let Some(expr) = expr {
        let lowered = match &expr {
            ast::Expression::AdoExpression(_) => Expr::NotImplemented,
            ast::Expression::ApplicationExpression(a) => lower_expr_application(ctx, db, a),
            ast::Expression::CaseExpression(_) => Expr::NotImplemented,
            ast::Expression::ConstructorExpression(c) => lower_expr_constructor(db, c),
            ast::Expression::DoExpression(_) => Expr::NotImplemented,
            ast::Expression::ExpressionInfixChain(_) => Expr::NotImplemented,
            ast::Expression::ExpressionOperatorChain(_) => Expr::NotImplemented,
            ast::Expression::IfThenElseExpression(_) => Expr::NotImplemented,
            ast::Expression::LambdaExpression(l) => lower_expr_lambda(ctx, db, l),
            ast::Expression::LetInExpression(l) => lower_expr_let_in(ctx, db, l),
            ast::Expression::LiteralExpression(l) => lower_expr_literal(ctx, db, l),
            ast::Expression::OperatorNameExpression(_) => Expr::NotImplemented,
            ast::Expression::ParenthesizedExpression(_) => Expr::NotImplemented,
            ast::Expression::RecordAccessExpression(_) => Expr::NotImplemented,
            ast::Expression::RecordUpdateExpression(_) => Expr::NotImplemented,
            ast::Expression::TypedExpression(_) => Expr::NotImplemented,
            ast::Expression::VariableExpression(v) => lower_expr_variable(db, v),
        };
        ctx.alloc_expr(lowered, Some(&expr))
    } else {
        ctx.alloc_expr(Expr::NotImplemented, None)
    }
}

fn lower_expr_application(
    ctx: &mut Ctx,
    db: &dyn SurfaceDatabase,
    application: &ast::ApplicationExpression,
) -> Expr {
    let function = lower_expr(ctx, db, application.head());
    let arguments = application
        .spine()
        .map(|spine| {
            spine
                .children()
                .map(|argument| match argument {
                    ast::Argument::TermArgument(t) => lower_expr(ctx, db, t.expression()),
                    ast::Argument::TypeArgument(_) => todo!("TypeArgument"),
                })
                .collect()
        })
        .unwrap_or_default();
    Expr::Application(function, arguments)
}

fn lower_expr_constructor(
    db: &dyn SurfaceDatabase,
    constructor: &ast::ConstructorExpression,
) -> Expr {
    let name = lower_qualified_name(db, constructor.qualified_name());
    Expr::Constructor(name)
}

fn lower_expr_lambda(
    ctx: &mut Ctx,
    db: &dyn SurfaceDatabase,
    lambda: &ast::LambdaExpression,
) -> Expr {
    let binders = lambda
        .binders()
        .map(|binders| {
            binders.children().map(|binder| lower_binder(ctx, db, Some(binder))).collect()
        })
        .unwrap_or_default();
    let body = lower_expr(ctx, db, lambda.body());
    Expr::Lambda(binders, body)
}

fn lower_expr_let_in(
    ctx: &mut Ctx,
    db: &dyn SurfaceDatabase,
    let_in: &ast::LetInExpression,
) -> Expr {
    let let_bindings = let_in
        .let_bindings()
        .map(|let_bindings| lower_let_bindings(ctx, db, &let_bindings))
        .unwrap_or_default();
    let body = lower_expr(ctx, db, let_in.body());
    Expr::LetIn(let_bindings, body)
}

fn lower_expr_literal(
    ctx: &mut Ctx,
    db: &dyn SurfaceDatabase,
    literal: &ast::LiteralExpression,
) -> Expr {
    if let Some(literal) = literal.syntax().first_child_or_token() {
        Expr::Literal(lower_literal(ctx, db, literal, lower_expr))
    } else {
        Expr::NotImplemented
    }
}

fn lower_expr_variable(db: &dyn SurfaceDatabase, variable: &ast::VariableExpression) -> Expr {
    let name = lower_qualified_name(db, variable.qualified_name());
    Expr::Variable(name)
}

// endregion

// region: Binder

fn lower_binder(ctx: &mut Ctx, db: &dyn SurfaceDatabase, binder: Option<ast::Binder>) -> BinderId {
    if let Some(binder) = &binder {
        let lowered = match binder {
            ast::Binder::ConstructorBinder(c) => lower_binder_constructor(ctx, db, c),
            ast::Binder::LiteralBinder(l) => lower_binder_literal(ctx, db, l),
            ast::Binder::NegativeBinder(n) => lower_binder_negative(db, n),
            ast::Binder::ParenthesizedBinder(p) => lower_binder_parenthesized(ctx, db, p),
            ast::Binder::TypedBinder(_) => Binder::NotImplemented,
            ast::Binder::VariableBinder(v) => lower_binder_variable(db, v),
            ast::Binder::WildcardBinder(_) => Binder::Wildcard,
        };
        ctx.alloc_binder(lowered, Some(binder))
    } else {
        ctx.alloc_binder(Binder::NotImplemented, None)
    }
}

fn lower_binder_constructor(
    ctx: &mut Ctx,
    db: &dyn SurfaceDatabase,
    constructor: &ast::ConstructorBinder,
) -> Binder {
    let name = lower_qualified_name(db, constructor.qualified_name());
    let fields = constructor
        .fields()
        .map(|fields| fields.children().map(|field| lower_binder(ctx, db, Some(field))).collect())
        .unwrap_or_default();
    Binder::Constructor { name, fields }
}

fn lower_binder_literal(
    ctx: &mut Ctx,
    db: &dyn SurfaceDatabase,
    literal: &ast::LiteralBinder,
) -> Binder {
    if let Some(literal) = literal.syntax().first_child_or_token() {
        Binder::Literal(lower_literal(ctx, db, literal, lower_binder))
    } else {
        Binder::NotImplemented
    }
}

fn lower_binder_negative(db: &dyn SurfaceDatabase, n: &ast::NegativeBinder) -> Binder {
    if let Some(token) = n.literal().and_then(|literal| literal.syntax().first_token()) {
        match token.kind() {
            SyntaxKind::LiteralInteger => {
                let value = token.text().parse().ok().unwrap();
                Binder::Negative(IntOrNumber::Int(value))
            }
            SyntaxKind::LiteralNumber => {
                let value = db.interner().intern(token.text());
                Binder::Negative(IntOrNumber::Number(value))
            }
            _ => Binder::NotImplemented,
        }
    } else {
        Binder::NotImplemented
    }
}

fn lower_binder_parenthesized(
    ctx: &mut Ctx,
    db: &dyn SurfaceDatabase,
    parenthesized: &ast::ParenthesizedBinder,
) -> Binder {
    Binder::Parenthesized(lower_binder(ctx, db, parenthesized.binder()))
}

fn lower_binder_variable(db: &dyn SurfaceDatabase, variable: &ast::VariableBinder) -> Binder {
    let name = lower_name(db, variable.name());

    Binder::Variable(name)
}

// endregion

// region: Type

fn lower_type(ctx: &mut Ctx, db: &dyn SurfaceDatabase, ty: Option<ast::Type>) -> TypeId {
    if let Some(ty) = ty {
        let lowered = match &ty {
            ast::Type::ApplicationType(a) => lower_type_application(ctx, db, a),
            ast::Type::ArrowType(a) => lower_type_arrow(ctx, db, a),
            ast::Type::ConstrainedType(c) => lower_type_constrained(ctx, db, c),
            ast::Type::ConstructorType(c) => lower_type_constructor(db, c),
            ast::Type::ForallType(f) => lower_type_forall(ctx, db, f),
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
        ctx.alloc_type(lowered, Some(&ty))
    } else {
        ctx.alloc_type(Type::NotImplemented, None)
    }
}

fn lower_type_application(
    ctx: &mut Ctx,
    db: &dyn SurfaceDatabase,
    application: &ast::ApplicationType,
) -> Type {
    let head = lower_type(ctx, db, application.head());
    let spine = application
        .spine()
        .map(|spine| spine.children().map(|argument| lower_type(ctx, db, Some(argument))).collect())
        .unwrap_or_default();

    Type::Application(head, spine)
}

fn lower_type_arrow(ctx: &mut Ctx, db: &dyn SurfaceDatabase, arrow: &ast::ArrowType) -> Type {
    let mut arguments = vec![lower_type(ctx, db, arrow.argument())];
    let mut result = arrow.result();

    while let Some(ast::Type::ArrowType(arrow)) = &result {
        arguments.push(lower_type(ctx, db, arrow.argument()));
        result = arrow.result();
    }

    let result = lower_type(ctx, db, result);

    Type::Arrow(arguments, result)
}

fn lower_type_constrained(
    ctx: &mut Ctx,
    db: &dyn SurfaceDatabase,
    constrained: &ast::ConstrainedType,
) -> Type {
    let constraint = lower_type(ctx, db, constrained.constraint());
    let constrained = lower_type(ctx, db, constrained.constrained());
    Type::Constrained(constraint, constrained)
}

fn lower_type_constructor(db: &dyn SurfaceDatabase, constructor: &ast::ConstructorType) -> Type {
    let name = lower_qualified_name(db, constructor.qualified_name());

    Type::Constructor(name)
}

fn lower_type_forall(ctx: &mut Ctx, db: &dyn SurfaceDatabase, forall: &ast::ForallType) -> Type {
    let variables = forall
        .variables()
        .map(|variables| lower_type_variable_binding(ctx, db, variables.children()))
        .unwrap_or_default();
    let inner = lower_type(ctx, db, forall.inner());
    Type::Forall(variables, inner)
}

fn lower_type_parenthesized(
    ctx: &mut Ctx,
    db: &dyn SurfaceDatabase,
    parenthesized: &ast::ParenthesizedType,
) -> Type {
    Type::Parenthesized(lower_type(ctx, db, parenthesized.ty()))
}

fn lower_type_variable(db: &dyn SurfaceDatabase, variable: &ast::VariableType) -> Type {
    let name = lower_name_ref(db, variable.name_ref());

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
                let kind = lower_type(ctx, db, k.kind());
                Some(TypeVariable::Kinded(name, kind))
            }
            ast::TypeVariableBinding::TypeVariableName(n) => {
                let name = Name::from_raw(n.name()?.in_db(db)?);
                Some(TypeVariable::Name(name))
            }
        })
        .collect()
}

// endregion

pub(super) fn file_surface_query(
    db: &dyn SurfaceDatabase,
    file_id: FileId,
) -> (Arc<Module>, Arc<SurfaceArena>) {
    let (surface, arena, _) = db.file_surface_map(file_id);
    (surface, arena)
}

pub(super) fn file_surface_map_query(
    db: &dyn SurfaceDatabase,
    file_id: FileId,
) -> (Arc<Module>, Arc<SurfaceArena>, Arc<SourceMap>) {
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
            Some((Arc::new(module), Arc::new(ctx.arena), Arc::new(ctx.source_map)))
        })
        .unwrap_or_else(|| {
            unreachable!("impossible: empty source files have a traversable CST");
        })
}
