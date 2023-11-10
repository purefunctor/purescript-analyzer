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
        &group_data.expr_arena,
        &group_data.let_binding_arena,
        &group_data.binder_arena,
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

/*

Describe the transaction that needs to occur:

In type inference, we want to be able to go from a name to
a type, specifically for the variable inference rule.

Currently, we're only doing module-local resolution using
the nominal map, which allows us to go from the `SmolStr`
representation of a name into the stable ID that represents it.

On the other hand for declaration-local resolution, we would
want to be able to go from `SmolStr` -> `LocalId` -> `Type`.
`LocalId` represents the various IDs used for the surface
representation, such as `ExprId`, `BinderId`, `LetBindingId`.

Likewise, we also want to be able to do resolution for
imported names--queries for both qualified and unqualified
imports expose this information.

Aside from scope information, we also want to encapsulate
the results of name resolution around a query. What we
effectively want is a mapping from names to where they
actually resolve.

This query is useful in a lot of scenarios, in particular,
with type inference, we can simply convert from an ExprId
into

Caching this information is useful in a lot of scenarios;
once computed, the type inference algorithm can resolve
variables into references pretty quickly. This is also
really useful for high-level IDE operations, in that
the flow from line/column position, to an AST pointer,
to an ExprId, and finally to whatever information is
desired is O(1) at best.

*/
