//! Implements inference rules.

mod common;
mod data;
mod recursive;
mod value;

use std::sync::Arc;

use files::FileId;

use crate::{id::InFile, scope::ResolveInfo, surface::tree::*, InferenceDatabase};

use super::{CoreType, CoreTypeId, InferenceResult};

use recursive::{recursive_data_groups, recursive_value_groups};

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

pub(super) fn file_infer_query(
    db: &dyn InferenceDatabase,
    file_id: FileId,
) -> Arc<InferenceResult> {
    let (surface, arena) = db.file_surface(file_id);
    let resolve = db.file_resolve(file_id);

    let mut ctx = InferContext::new(file_id, &arena, &resolve);

    let recursive_data =
        recursive_data_groups(&arena, &resolve, surface.body.iter_data_declarations());
    for recursive_group in recursive_data {
        for data_group_id in recursive_group {
            let Some(data_declaration) = surface.body.data_declaration(data_group_id) else {
                unreachable!("impossible: unknown data_group_id");
            };
            ctx.infer_data_declaration(db, data_declaration);
        }
    }

    let recursive_value =
        recursive_value_groups(&arena, &resolve, surface.body.iter_value_declarations());
    for recursive_group in recursive_value {
        for value_group_id in recursive_group {
            let Some(value_declaration) = surface.body.value_declaration(value_group_id) else {
                unreachable!("impossible: unknown value_group_id");
            };
            ctx.infer_value_declaration(db, value_declaration);
        }
    }

    Arc::new(ctx.result)
}
