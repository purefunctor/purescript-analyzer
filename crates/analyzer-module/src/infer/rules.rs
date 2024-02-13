//! Implements inference rules.

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
    id::InFile, infer::pretty_print, scope::ResolveInfo, surface::tree::*, InferenceDatabase,
};

use super::{CoreType, CoreTypeId, InferenceResult};

use recursive::{recursive_data_groups, recursive_let_names, recursive_value_groups};

#[derive(Default)]
struct InferenceState {
    count: u32,
}

struct InferContext<'a> {
    file_id: FileId,
    arena: &'a SurfaceArena,
    resolve: &'a ResolveInfo,
    imported: &'a FxHashMap<FileId, Arc<InferenceResult>>,
    state: InferenceState,
    result: InferenceResult,
}

impl<'a> InferContext<'a> {
    fn new(
        file_id: FileId,
        arena: &'a SurfaceArena,
        resolve: &'a ResolveInfo,
        imported: &'a FxHashMap<FileId, Arc<InferenceResult>>,
    ) -> InferContext<'a> {
        let state = InferenceState::default();
        let result = InferenceResult::default();
        InferContext { file_id, arena, resolve, state, result, imported }
    }

    fn fresh_unification(&mut self, db: &dyn InferenceDatabase) -> CoreTypeId {
        let file_id = self.file_id;
        let value = self.state.count;
        self.state.count += 1;
        db.intern_type(CoreType::Unification(InFile { file_id, value }))
    }
}

#[derive(Default)]
struct SolveState {
    unification_solved: FxHashMap<InFile<u32>, CoreTypeId>,
    unification_deferred: Vec<(InFile<u32>, InFile<u32>)>,
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

pub(super) fn file_infer_query(
    db: &dyn InferenceDatabase,
    file_id: FileId,
) -> Arc<InferenceResult> {
    let (surface, arena) = db.file_surface(file_id);
    let resolve = db.file_resolve(file_id);

    let imported: FxHashMap<_, _> =
        resolve.imports.iter().map(|&file_id| (file_id, db.file_infer(file_id))).collect();

    let mut infer_ctx = InferContext::new(file_id, &arena, &resolve, &imported);

    let recursive_data =
        recursive_data_groups(&arena, &resolve, surface.body.iter_data_declarations());
    for recursive_group in recursive_data {
        for data_group_id in recursive_group {
            let Some(data_declaration) = surface.body.data_declaration(data_group_id) else {
                unreachable!("impossible: unknown data_group_id");
            };
            infer_ctx.infer_data_declaration(db, data_declaration);
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

    Arc::new(infer_ctx.result)
}
