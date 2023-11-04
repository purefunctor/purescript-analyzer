//! Database for local scope information.
use std::sync::Arc;

mod collector;
mod data;

use crate::{id::InFile, resolver::ValueGroupId, surface::visitor::Visitor, SurfaceDatabase};

use collector::CollectorContext;
pub use data::{ScopeData, ScopeId, ScopeKind, ValueGroupScope};

#[salsa::query_group(ScopeStorage)]
pub trait ScopeDatabase: SurfaceDatabase {
    #[salsa::invoke(value_scope_query)]
    fn value_scope(&self, id: InFile<ValueGroupId>) -> Arc<ValueGroupScope>;
}

fn value_scope_query(db: &dyn ScopeDatabase, id: InFile<ValueGroupId>) -> Arc<ValueGroupScope> {
    let group_data = db.value_surface(id);

    let mut collector_context = CollectorContext::new(
        &group_data.binder_arena,
        &group_data.expr_arena,
        &group_data.type_arena,
    );

    let scope_per_equation = group_data
        .value
        .equations
        .iter()
        .map(|(equation_id, value_equation)| {
            collector_context.visit_value_equation(value_equation);
            (*equation_id, collector_context.take_equation())
        })
        .collect();

    Arc::new(ValueGroupScope::new(collector_context.scope_arena, scope_per_equation))
}
