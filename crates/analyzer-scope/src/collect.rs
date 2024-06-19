use std::sync::Arc;

use files::FileId;
use itertools::Itertools;
use la_arena::Arena;
use rustc_hash::FxHashMap;

use analyzer_surface::tree::*;

use super::{ScopeData, ScopeDatabase, ScopeId, ScopeInfo, ScopeKind, TypeVariableKind};

struct Ctx<'a> {
    arena: &'a SurfaceArena,
    scope_data: Arena<ScopeData>,
    per_expr: FxHashMap<ExprId, Vec<ScopeId>>,
    per_type: FxHashMap<TypeId, ScopeId>,
    current_scope: ScopeId,
    root_scope: ScopeId,
}

impl Ctx<'_> {
    fn new(arena: &SurfaceArena) -> Ctx<'_> {
        let mut scope_data = Arena::default();
        let per_expr = FxHashMap::default();
        let per_type = FxHashMap::default();

        let current_scope = scope_data.alloc(ScopeData { parent: None, kind: ScopeKind::Root });
        let root_scope = current_scope;

        Ctx { arena, scope_data, per_expr, per_type, current_scope, root_scope }
    }

    fn with_reverting_scope<T>(&mut self, f: impl FnOnce(&mut Ctx) -> T) -> T {
        let previous = self.current_scope;
        let result = f(self);
        self.current_scope = previous;
        result
    }
}

fn collect_class_declaration(ctx: &mut Ctx, class_declaration: &ClassDeclaration) {
    assert!(ctx.current_scope == ctx.root_scope);
    if let Some(signature) = class_declaration.signature {
        collect_type(ctx, signature);
    }

    let variables = class_declaration.variables.iter().map(TypeVariable::to_name).collect();
    let parent = Some(ctx.current_scope);
    let kind = ScopeKind::TypeVariable(variables, TypeVariableKind::Class(class_declaration.id));
    ctx.current_scope = ctx.scope_data.alloc(ScopeData { parent, kind });

    for class_member in class_declaration.members.values() {
        ctx.with_reverting_scope(|ctx| {
            collect_type(ctx, class_member.ty);
        });
    }
}

fn collect_data_declaration(ctx: &mut Ctx, data_declaration: &DataDeclaration) {
    assert!(ctx.current_scope == ctx.root_scope);
    if let Some(annotation) = data_declaration.annotation {
        collect_type(ctx, annotation);
    }

    let variables = data_declaration.variables.iter().map(TypeVariable::to_name).collect();
    let parent = Some(ctx.current_scope);
    let kind = ScopeKind::TypeVariable(variables, TypeVariableKind::Data(data_declaration.id));
    ctx.current_scope = ctx.scope_data.alloc(ScopeData { parent, kind });

    for data_constructor in data_declaration.constructors.values() {
        for field in &data_constructor.fields {
            ctx.with_reverting_scope(|ctx| {
                collect_type(ctx, *field);
            });
        }
    }
}

fn collect_value_declaration(ctx: &mut Ctx, value_declaration: &ValueDeclaration) {
    assert!(ctx.current_scope == ctx.root_scope);
    if let Some(annotation) = value_declaration.annotation {
        collect_type(ctx, annotation);
    }
    value_declaration.equations.iter().for_each(|value_equation| {
        collect_value_equation(ctx, value_equation);
    });
}

fn collect_value_equation(ctx: &mut Ctx, value_equation: &ValueEquation) {
    ctx.with_reverting_scope(|ctx| {
        collect_binders(ctx, &value_equation.binders);
        collect_binding(ctx, &value_equation.binding);
    });
}

fn collect_binders(ctx: &mut Ctx, binders: &[BinderId]) {
    let binders = if binders.is_empty() {
        None
    } else {
        let mut per_name = FxHashMap::default();
        binders.iter().for_each(|binder| {
            collect_binder(ctx, &mut per_name, *binder);
        });
        Some(per_name)
    };

    let parent = Some(ctx.current_scope);
    let kind = ScopeKind::Binders(binders);
    ctx.current_scope = ctx.scope_data.alloc(ScopeData { parent, kind });
}

fn collect_binding(ctx: &mut Ctx, binding: &Binding) {
    match binding {
        Binding::Unconditional { where_expr } => collect_where_expr(ctx, where_expr),
        Binding::Guarded { guarded_exprs } => {
            for guarded_expr in guarded_exprs {
                collect_guarded_expr(ctx, guarded_expr);
            }
        }
    }
}

fn collect_where_expr(ctx: &mut Ctx, where_expr: &WhereExpr) {
    ctx.with_reverting_scope(|ctx| {
        collect_let_bindings(ctx, &where_expr.let_bindings);
        collect_expr(ctx, where_expr.expr_id);
    })
}

fn collect_guarded_expr(ctx: &mut Ctx, guarded_expr: &GuardedExpr) {
    // This is a little tricky to visualize because of the ff.
    //
    // 1. The where block may not use values bound from the pattern guards.
    // 2. The pattern guards may not use values bound from the where block.
    // 3. The pattern guards form a chain of scope nodes linked sequentially.
    // 4. The expression must be able to use the where block and pattern guards.
    //
    // Guarded expressions effectively require branching in order to be modeled
    // correctly with the scope graph. By allowing multiple "entry points" for
    // each term, branching can be achieved simply through repeated traversal.
    // Note that traversal order also matters to preserve shadowing semantics.
    ctx.with_reverting_scope(|ctx| {
        for pattern_guard in &guarded_expr.pattern_guards {
            ctx.with_reverting_scope(|ctx| {
                collect_expr(ctx, pattern_guard.expr_id);
            });
            collect_binders(ctx, pattern_guard.binder_id.as_slice());
        }
        collect_expr(ctx, guarded_expr.where_expr.expr_id);
    });
    ctx.with_reverting_scope(|ctx| {
        collect_let_bindings(ctx, &guarded_expr.where_expr.let_bindings);
        collect_expr(ctx, guarded_expr.where_expr.expr_id);
    });
}

fn collect_let_bindings(ctx: &mut Ctx, let_bindings: &[LetBinding]) {
    #[derive(Debug, PartialEq, Eq)]
    enum GroupKind {
        Name,
        Pattern,
    }

    let groups = let_bindings.iter().chunk_by(|let_binding| match let_binding {
        LetBinding::Name { .. } => GroupKind::Name,
        LetBinding::Pattern { .. } => GroupKind::Pattern,
    });

    for (key, group) in groups.into_iter() {
        match key {
            GroupKind::Name => {
                let mut let_bound = FxHashMap::default();
                let mut equations = vec![];

                for let_binding in group {
                    let LetBinding::Name { id } = let_binding else {
                        unreachable!("Impossible");
                    };

                    let let_name = &ctx.arena[*id];
                    let_bound.insert(Name::clone(&let_name.name), *id);
                    equations.extend(&let_name.equations);
                }

                let parent = Some(ctx.current_scope);
                let kind = ScopeKind::LetBound(let_bound);
                ctx.current_scope = ctx.scope_data.alloc(ScopeData { parent, kind });

                for equation in equations {
                    ctx.with_reverting_scope(|ctx| {
                        collect_binders(ctx, &equation.binders);
                        collect_binding(ctx, &equation.binding);
                    });
                }
            }
            GroupKind::Pattern => {
                for let_binding in group {
                    let LetBinding::Pattern { binder, where_expr } = let_binding else {
                        unreachable!("Impossible");
                    };

                    ctx.with_reverting_scope(|ctx| {
                        collect_where_expr(ctx, where_expr);
                    });

                    let mut per_name = FxHashMap::default();
                    collect_binder(ctx, &mut per_name, *binder);

                    let parent = Some(ctx.current_scope);
                    let kind = ScopeKind::Binders(Some(per_name));
                    ctx.current_scope = ctx.scope_data.alloc(ScopeData { parent, kind });
                }
            }
        }
    }
}

fn collect_binder(ctx: &mut Ctx, per_name: &mut FxHashMap<Name, BinderId>, binder_id: BinderId) {
    match &ctx.arena[binder_id] {
        Binder::Constructor { fields, .. } => {
            for field in fields {
                collect_binder(ctx, per_name, *field);
            }
        }
        Binder::Literal(literal) => {
            collect_literal(ctx, literal, |ctx, binder_id| collect_binder(ctx, per_name, binder_id))
        }
        Binder::Negative(_) => (),
        Binder::Parenthesized(parenthesized) => {
            collect_binder(ctx, per_name, *parenthesized);
        }
        Binder::Variable(variable) => {
            per_name.insert(Name::clone(variable), binder_id);
        }
        Binder::Wildcard => (),
        Binder::NotImplemented => (),
    }
}

fn collect_expr(ctx: &mut Ctx, expr_id: ExprId) {
    ctx.per_expr.entry(expr_id).or_default().push(ctx.current_scope);
    match &ctx.arena[expr_id] {
        Expr::Application(head, spine) => {
            collect_expr(ctx, *head);
            for argument in spine {
                collect_expr(ctx, *argument);
            }
        }
        Expr::Constructor(_) => (),
        Expr::Lambda(binders, body) => {
            ctx.with_reverting_scope(|ctx| {
                collect_binders(ctx, binders);
                collect_expr(ctx, *body);
            });
        }
        Expr::LetIn(let_bindings, let_body) => {
            ctx.with_reverting_scope(|ctx| {
                collect_let_bindings(ctx, let_bindings);
                collect_expr(ctx, *let_body);
            });
        }
        Expr::Literal(literal) => collect_literal(ctx, literal, collect_expr),
        Expr::Variable(_) => (),
        Expr::NotImplemented => (),
    }
}

fn collect_type(ctx: &mut Ctx, type_id: TypeId) {
    ctx.per_type.insert(type_id, ctx.current_scope);
    match &ctx.arena[type_id] {
        Type::Arrow(arguments, result) => {
            for argument in arguments {
                collect_type(ctx, *argument);
            }
            collect_type(ctx, *result);
        }
        Type::Application(function, arguments) => {
            collect_type(ctx, *function);
            for argument in arguments {
                collect_type(ctx, *argument);
            }
        }
        Type::Constrained(constraint, constrained) => {
            collect_type(ctx, *constraint);
            collect_type(ctx, *constrained);
        }
        Type::Constructor(_) => (),
        Type::Forall(variables, inner) => {
            let variables = variables
                .iter()
                .map(|variable| {
                    Name::clone(match variable {
                        TypeVariable::Kinded(name, _) => name,
                        TypeVariable::Name(name) => name,
                    })
                })
                .collect();

            let parent = Some(ctx.current_scope);
            let kind = ScopeKind::TypeVariable(variables, TypeVariableKind::Type(type_id));
            ctx.current_scope = ctx.scope_data.alloc(ScopeData { parent, kind });

            collect_type(ctx, *inner);
        }
        Type::Parenthesized(parenthesized) => collect_type(ctx, *parenthesized),
        Type::Variable(_) => (),
        Type::NotImplemented => (),
    }
}

fn collect_literal<T>(
    ctx: &mut Ctx,
    literal: &Literal<T>,
    mut collect_inner: impl FnMut(&mut Ctx, T),
) where
    T: Copy,
{
    match literal {
        Literal::Array(items) => {
            for item in items {
                collect_inner(ctx, *item);
            }
        }
        Literal::Record(items) => {
            for item in items {
                match item {
                    RecordItem::RecordPun(_) => (),
                    RecordItem::RecordField(_, item) => {
                        collect_inner(ctx, *item);
                    }
                }
            }
        }
        Literal::Int(_) => (),
        Literal::Number(_) => (),
        Literal::String(_) => (),
        Literal::Char(_) => (),
        Literal::Boolean(_) => (),
    }
}

pub(super) fn file_scope_query(db: &dyn ScopeDatabase, file_id: FileId) -> Arc<ScopeInfo> {
    let (surface, arena) = db.file_surface(file_id);

    let mut ctx = Ctx::new(&arena);
    surface.body.declarations.iter().for_each(|declaration| {
        ctx.with_reverting_scope(|ctx| match declaration {
            Declaration::ClassDeclaration(class_declaration) => {
                collect_class_declaration(ctx, class_declaration);
            }
            Declaration::DataDeclaration(data_declaration) => {
                collect_data_declaration(ctx, data_declaration);
            }
            Declaration::ValueDeclaration(value_declaration) => {
                collect_value_declaration(ctx, value_declaration);
            }
        });
    });

    Arc::new(ScopeInfo {
        scope_data: ctx.scope_data,
        per_expr: ctx.per_expr,
        per_type: ctx.per_type,
    })
}
