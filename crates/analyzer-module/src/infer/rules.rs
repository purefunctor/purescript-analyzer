//! Implements inference rules.

use std::sync::Arc;

use files::FileId;
use petgraph::{algo::kosaraju_scc, graphmap::DiGraphMap};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
    id::InFile,
    index::nominal::DataGroupId,
    infer::pretty_print,
    scope::{ResolveInfo, TypeConstructorKind},
    surface::tree::*,
    InferenceDatabase,
};

use super::{CoreType, CoreTypeId, InferenceResult};

// region: Recursive Binding Groups

#[derive(Debug)]
struct RecursiveGroupBuilder<'a> {
    resolve_info: &'a ResolveInfo,
    type_graph: DiGraphMap<TypeConstructorKind, ()>,
}

impl<'a> RecursiveGroupBuilder<'a> {
    fn new(resolve_info: &'a ResolveInfo) -> RecursiveGroupBuilder<'a> {
        let type_graph = DiGraphMap::default();
        RecursiveGroupBuilder { resolve_info, type_graph }
    }

    fn analyze_type(&mut self, data_id: DataGroupId, arena: &SurfaceArena, type_id: TypeId) {
        match &arena[type_id] {
            Type::Arrow(arguments, result) => {
                for argument in arguments {
                    self.analyze_type(data_id, arena, *argument);
                }
                self.analyze_type(data_id, arena, *result);
            }
            Type::Application(function, arguments) => {
                self.analyze_type(data_id, arena, *function);
                for argument in arguments {
                    self.analyze_type(data_id, arena, *argument);
                }
            }
            Type::Constructor(_) => {
                if let Some(type_constructor) = self.resolve_info.per_type_type.get(&type_id) {
                    let dependent = TypeConstructorKind::Data(data_id);
                    let dependency = type_constructor.kind;
                    self.type_graph.add_edge(dependent, dependency, ());
                }
            }
            Type::Parenthesized(parenthesized) => {
                self.analyze_type(data_id, arena, *parenthesized);
            }
            Type::Variable(_) => (),
            Type::NotImplemented => (),
        }
    }
}

// endregion

// region: Type Inference Rules

#[derive(Default)]
struct InferenceState {
    count: u32,
}

struct InferContext<'a> {
    file_id: FileId,
    arena: &'a SurfaceArena,
    resolve: &'a ResolveInfo,
    state: InferenceState,
    result: InferenceResult,
}

impl<'a> InferContext<'a> {
    fn new(file_id: FileId, arena: &'a SurfaceArena, resolve: &'a ResolveInfo) -> InferContext<'a> {
        let state = InferenceState::default();
        let result = InferenceResult::default();
        InferContext { file_id, arena, resolve, state, result }
    }

    fn fresh_unification(&mut self, db: &dyn InferenceDatabase) -> CoreTypeId {
        let file_id = self.file_id;
        let value = self.state.count;
        self.state.count += 1;
        db.intern_type(CoreType::Unification(InFile { file_id, value }))
    }
}

fn infer_data_declaration(
    ctx: &mut InferContext,
    db: &dyn InferenceDatabase,
    file_id: FileId,
    data_declaration: &DataDeclaration,
) {
    let data_ty = {
        let data_id = InFile { file_id, value: data_declaration.id };
        let function_ty = db.intern_type(CoreType::Constructor(data_id));
        data_declaration.variables.iter().fold(function_ty, |function_ty, argument| {
            let argument_ty = match argument {
                TypeVariable::Kinded(name, _) => {
                    let name = Name::clone(name);
                    db.intern_type(CoreType::Variable(name))
                }
                TypeVariable::Name(name) => {
                    let name = Name::clone(name);
                    db.intern_type(CoreType::Variable(name))
                }
            };
            db.intern_type(CoreType::Application(function_ty, argument_ty))
        })
    };

    data_declaration.constructors.iter().for_each(|(constructor_id, data_constructor)| {
        let fields_ty = data_constructor.fields.iter().map(|field| lower_type(ctx, db, *field));

        let constructor_ty = fields_ty.rev().fold(data_ty, |data_ty, field_ty| {
            db.intern_type(CoreType::Function(field_ty, data_ty))
        });

        let qualified_ty = data_declaration.variables.iter().rev().fold(
            constructor_ty,
            |constructor_ty, variable| match variable {
                TypeVariable::Kinded(name, _) => {
                    let name = Name::clone(name);
                    db.intern_type(CoreType::Forall(name, constructor_ty))
                }
                TypeVariable::Name(name) => {
                    let name = Name::clone(name);
                    db.intern_type(CoreType::Forall(name, constructor_ty))
                }
            },
        );

        ctx.result.of_constructor.insert(*constructor_id, qualified_ty);
    });
}

fn lower_type(ctx: &mut InferContext, db: &dyn InferenceDatabase, type_id: TypeId) -> CoreTypeId {
    match &ctx.arena[type_id] {
        Type::Arrow(arguments, result) => {
            let result = lower_type(ctx, db, *result);
            let arguments = arguments.iter().map(|argument| lower_type(ctx, db, *argument));
            arguments.rev().fold(result, |result, argument| {
                db.intern_type(CoreType::Function(argument, result))
            })
        }
        Type::Application(function, arguments) => {
            let function = lower_type(ctx, db, *function);
            let arguments = arguments.iter().map(|argument| lower_type(ctx, db, *argument));
            arguments.fold(function, |function, argument| {
                db.intern_type(CoreType::Application(function, argument))
            })
        }
        Type::Constructor(_) => {
            let resolution = ctx.resolve.per_type_type.get(&type_id);
            db.intern_type(resolution.map_or(CoreType::NotImplemented, |resolution| {
                let file_id = resolution.file_id;
                match resolution.kind {
                    TypeConstructorKind::Data(data_id) => {
                        CoreType::Constructor(InFile { file_id, value: data_id })
                    }
                }
            }))
        }
        Type::Parenthesized(parenthesized) => lower_type(ctx, db, *parenthesized),
        Type::Variable(name) => {
            let name = Name::clone(name);
            db.intern_type(CoreType::Variable(name))
        }
        Type::NotImplemented => db.intern_type(CoreType::NotImplemented),
    }
}

fn infer_value_declaration(
    ctx: &mut InferContext,
    db: &dyn InferenceDatabase,
    value_declaration: &ValueDeclaration,
) {
    value_declaration.equations.iter().for_each(|value_equation| {
        value_equation.binders.iter().for_each(|binder| {
            infer_binder(ctx, db, *binder);
        });
        infer_binding(ctx, db, &value_equation.binding);
    });
}

fn infer_binding(ctx: &mut InferContext, db: &dyn InferenceDatabase, binding: &Binding) {}

fn infer_binder(
    ctx: &mut InferContext,
    db: &dyn InferenceDatabase,
    binder_id: BinderId,
) -> CoreTypeId {
    let binder_ty = match &ctx.arena[binder_id] {
        Binder::Constructor { fields, .. } => {
            if let Some(constructor_resolution) = ctx.resolve.per_constructor_binder.get(&binder_id)
            {
                if let Some(constructor_ty) =
                    ctx.result.of_constructor.get(&constructor_resolution.constructor_id)
                {
                    let constructor_ty = instantiate_type(ctx, db, *constructor_ty);
                    let arguments_ty = peel_arguments(db, constructor_ty);

                    for (field, argument_ty) in fields.iter().zip(arguments_ty) {
                        let field_ty = infer_binder(ctx, db, *field);
                        unify_types(ctx, db, field_ty, argument_ty);
                    }

                    constructor_ty
                } else {
                    db.intern_type(CoreType::NotImplemented)
                }
            } else {
                db.intern_type(CoreType::NotImplemented)
            }
        }
        Binder::Literal(literal) => match literal {
            Literal::Array(_) => db.intern_type(CoreType::NotImplemented),
            Literal::Record(_) => db.intern_type(CoreType::NotImplemented),
            Literal::Int(_) => {
                let name = db.interner().intern("Int");
                db.intern_type(CoreType::Primitive(Name::from_raw(name)))
            }
            Literal::Number(_) => {
                let name = db.interner().intern("Number");
                db.intern_type(CoreType::Primitive(Name::from_raw(name)))
            }
            Literal::String(_) => {
                let name = db.interner().intern("String");
                db.intern_type(CoreType::Primitive(Name::from_raw(name)))
            }
            Literal::Char(_) => {
                let name = db.interner().intern("Char");
                db.intern_type(CoreType::Primitive(Name::from_raw(name)))
            }
            Literal::Boolean(_) => {
                let name = db.interner().intern("Boolean");
                db.intern_type(CoreType::Primitive(Name::from_raw(name)))
            }
        },
        Binder::Negative(negative) => {
            let name = match negative {
                IntOrNumber::Int(_) => db.interner().intern("Int"),
                IntOrNumber::Number(_) => db.interner().intern("Number"),
            };
            db.intern_type(CoreType::Primitive(Name::from_raw(name)))
        }
        Binder::Parenthesized(parenthesized) => infer_binder(ctx, db, *parenthesized),
        Binder::Variable(_) => ctx.fresh_unification(db),
        Binder::Wildcard => ctx.fresh_unification(db),
        Binder::NotImplemented => db.intern_type(CoreType::NotImplemented),
    };
    ctx.result.of_binder.insert(binder_id, binder_ty);
    binder_ty
}

fn peel_arguments(db: &dyn InferenceDatabase, type_id: CoreTypeId) -> Vec<CoreTypeId> {
    let mut arguments = vec![];
    let mut current = type_id;
    while let CoreType::Function(argument, result) = db.lookup_intern_type(current) {
        arguments.push(argument);
        current = result;
    }
    arguments
}

fn unify_types(ctx: &mut InferContext, db: &dyn InferenceDatabase, x: CoreTypeId, y: CoreTypeId) {
    println!("{} ~ {}", pretty_print(db, x), pretty_print(db, y));
}

fn instantiate_type(
    ctx: &mut InferContext,
    db: &dyn InferenceDatabase,
    type_id: CoreTypeId,
) -> CoreTypeId {
    if let CoreType::Forall(initial_variable, initial_body) = db.lookup_intern_type(type_id) {
        let mut replacements = FxHashMap::default();

        let initial_unification = ctx.fresh_unification(db);
        replacements.insert(initial_variable, initial_unification);
        let mut current_body = initial_body;

        while let CoreType::Forall(variable, body) = db.lookup_intern_type(current_body) {
            let unification = ctx.fresh_unification(db);
            replacements.insert(variable, unification);
            current_body = body;
        }

        replace_type(db, &replacements, current_body)
    } else {
        type_id
    }
}

fn replace_type(
    db: &dyn InferenceDatabase,
    replacements: &FxHashMap<Name, CoreTypeId>,
    type_id: CoreTypeId,
) -> CoreTypeId {
    fn aux(
        db: &dyn InferenceDatabase,
        in_scope: &mut FxHashSet<Name>,
        replacements: &FxHashMap<Name, CoreTypeId>,
        type_id: CoreTypeId,
    ) -> CoreTypeId {
        match db.lookup_intern_type(type_id) {
            CoreType::Application(function, argument) => {
                let function = aux(db, in_scope, replacements, function);
                let argument = aux(db, in_scope, replacements, argument);
                db.intern_type(CoreType::Application(function, argument))
            }
            CoreType::Constructor(_) => type_id,
            CoreType::Forall(variable, body) => {
                in_scope.insert(Name::clone(&variable));
                let body = aux(db, in_scope, replacements, body);
                in_scope.remove(&variable);
                db.intern_type(CoreType::Forall(variable, body))
            }
            CoreType::Function(argument, result) => {
                let argument = aux(db, in_scope, replacements, argument);
                let result = aux(db, in_scope, replacements, result);
                db.intern_type(CoreType::Function(argument, result))
            }
            CoreType::Primitive(_) => type_id,
            CoreType::Unification(_) => type_id,
            CoreType::Variable(name) => {
                if in_scope.contains(&name) {
                    type_id
                } else {
                    replacements
                        .get(&name)
                        .copied()
                        .unwrap_or_else(|| db.intern_type(CoreType::NotImplemented))
                }
            }
            CoreType::NotImplemented => type_id,
        }
    }

    let mut in_scope = FxHashSet::default();
    aux(db, &mut in_scope, replacements, type_id)
}

// endregion

pub(super) fn file_infer_query(
    db: &dyn InferenceDatabase,
    file_id: FileId,
) -> Arc<InferenceResult> {
    let (surface, arena) = db.file_surface(file_id);
    let resolve = db.file_resolve(file_id);

    let mut builder = RecursiveGroupBuilder::new(&resolve);
    surface.body.iter_data_declarations().for_each(|data_declaration| {
        builder.type_graph.add_node(TypeConstructorKind::Data(data_declaration.id));
        data_declaration.constructors.values().for_each(|data_constructor| {
            data_constructor.fields.iter().for_each(|field| {
                builder.analyze_type(data_declaration.id, &arena, *field);
            });
        });
    });

    let mut ctx = InferContext::new(file_id, &arena, &resolve);

    for components in kosaraju_scc(&builder.type_graph) {
        for TypeConstructorKind::Data(data_group_id) in components {
            let index = surface.body.data_declarations.get(&data_group_id).unwrap_or_else(|| {
                unreachable!("impossible: data_group_id comes from iter_data_declarations");
            });
            let Declaration::DataDeclaration(data_declaration) = &surface.body.declarations[*index]
            else {
                unreachable!("impossible: an invalid index was set to data_declarations");
            };
            infer_data_declaration(&mut ctx, db, file_id, data_declaration);
        }
    }

    surface
        .body
        .declarations
        .iter()
        .filter_map(|declaration| {
            if let Declaration::ValueDeclaration(value_declaration) = declaration {
                Some(value_declaration)
            } else {
                None
            }
        })
        .for_each(|value_declaration| {
            infer_value_declaration(&mut ctx, db, &value_declaration);
        });

    Arc::new(ctx.result)
}
