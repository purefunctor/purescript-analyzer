//! Implements inference rules.

mod class;
mod common;
mod data;
mod recursive;
mod solve;
mod value;

use std::sync::Arc;

use files::FileId;
use itertools::Itertools;
use rustc_hash::FxHashMap;

use crate::{
    id::InFile,
    infer::pretty_print,
    scope::{ResolveInfo, TypeConstructorKind},
    surface::tree::*,
    InferenceDatabase,
};

use super::{Constraint, CoreType, CoreTypeId, Hint, InferError, InferMap, InferResult};

use recursive::{recursive_let_names, recursive_type_groups, recursive_value_groups};

#[derive(Default)]
struct InferState {
    count: u32,
    hints: Vec<Hint>,
    constraints: Vec<Constraint>,
    errors: Vec<InferError>,
    map: InferMap,
}

struct InferContext<'a> {
    file_id: FileId,
    arena: &'a SurfaceArena,
    resolve: &'a ResolveInfo,
    imported: &'a FxHashMap<FileId, Arc<InferResult>>,
    state: InferState,
}

impl<'a> InferContext<'a> {
    fn new(
        file_id: FileId,
        arena: &'a SurfaceArena,
        resolve: &'a ResolveInfo,
        imported: &'a FxHashMap<FileId, Arc<InferResult>>,
    ) -> InferContext<'a> {
        let state = InferState::default();
        InferContext { file_id, arena, resolve, state, imported }
    }

    fn add_hint(&mut self, hint: Hint) {
        self.state.hints.push(hint);
    }

    fn pop_hint(&mut self) {
        self.state.hints.pop();
    }

    fn current_hints(&self) -> Arc<[Hint]> {
        Arc::from(self.state.hints.as_slice())
    }

    fn add_constraint(&mut self, constraint: Constraint) {
        self.state.constraints.push(constraint)
    }

    fn add_error(&mut self, error: InferError) {
        self.state.errors.push(error)
    }
}

type UnificationDeferred = (Arc<[Hint]>, InFile<u32>, InFile<u32>);

#[derive(Default)]
struct SolveState {
    unification_solved: FxHashMap<InFile<u32>, CoreTypeId>,
    unification_deferred: Vec<UnificationDeferred>,
}

struct SolveContext<'i, 'a> {
    infer: &'i mut InferContext<'a>,
    state: SolveState,
}

impl<'i, 'a> SolveContext<'i, 'a> {
    fn new(infer: &'i mut InferContext<'a>) -> SolveContext<'i, 'a> {
        let state = SolveState::default();
        SolveContext { infer, state }
    }
}

pub(super) fn file_infer_query(db: &dyn InferenceDatabase, file_id: FileId) -> Arc<InferResult> {
    let (surface, arena) = db.file_surface(file_id);
    let resolve = db.file_resolve(file_id);

    let imported: FxHashMap<_, _> =
        resolve.imports.iter().map(|&file_id| (file_id, db.file_infer(file_id))).collect();

    let mut infer_ctx = InferContext::new(file_id, &arena, &resolve, &imported);

    let recursive_type = recursive_type_groups(
        &arena,
        &resolve,
        surface.body.iter_class_declarations(),
        surface.body.iter_data_declarations(),
    );
    for recursive_group in recursive_type {
        for constructor_id in recursive_group {
            match constructor_id {
                TypeConstructorKind::Class(class_id) => {
                    let Some(class_declaration) = surface.body.class_declaration(class_id) else {
                        unreachable!("impossible: unknown class_id");
                    };
                    infer_ctx.infer_class_declaration(db, class_declaration);
                }
                TypeConstructorKind::Data(data_id) => {
                    let Some(data_declaration) = surface.body.data_declaration(data_id) else {
                        unreachable!("impossible: unknown data_id");
                    };
                    infer_ctx.infer_data_declaration(db, data_declaration);
                }
            }
        }
    }

    let mut solve_ctx = SolveContext::new(&mut infer_ctx);

    let value_components =
        recursive_value_groups(&arena, &resolve, surface.body.iter_value_declarations());
    for value_component in value_components {
        let value_declarations = value_component
            .into_iter()
            .map(|value_group_id| {
                let Some(value_declaration) = surface.body.value_declaration(value_group_id) else {
                    unreachable!("impossible: unknown value_group_id");
                };
                (value_group_id, value_declaration)
            })
            .collect_vec();
        solve_ctx.infer.infer_value_scc(db, &value_declarations);
    }
    solve_ctx.solve(db);

    eprintln!("Solutions:");
    for (u, t) in solve_ctx.state.unification_solved {
        eprintln!("{} ~ {}", u.value, pretty_print(db, t));
    }

    Arc::new(InferResult {
        constraints: infer_ctx.state.constraints,
        errors: infer_ctx.state.errors,
        map: infer_ctx.state.map,
    })
}
