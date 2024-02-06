//! Implements inference rules.

use std::sync::Arc;

use files::FileId;
use petgraph::{algo::kosaraju_scc, graphmap::DiGraphMap};

use crate::{
    id::InFile,
    index::nominal::DataGroupId,
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

struct InferContext<'a> {
    arena: &'a SurfaceArena,
    resolve: &'a ResolveInfo,
    result: InferenceResult,
}

impl<'a> InferContext<'a> {
    fn new(arena: &'a SurfaceArena, resolve: &'a ResolveInfo) -> InferContext<'a> {
        let result = InferenceResult::default();
        InferContext { arena, resolve, result }
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

    let mut ctx = InferContext::new(&arena, &resolve);
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

    Arc::new(ctx.result)
}
