//! Implements context transfer for types.
//!
//! # Context transfer?
//!
//! In the type checker, [core] types are interned and allocated in an
//! arena. This is done mainly for locality, especially considering that
//! traversal is a very hot code path within in a type checker.
//!
//! While arena allocation greatly simplifies cleanup for allocated type
//! structures, the type checker creates many temporary types that would
//! be kept for longer than they should be.
//!
//! The type checker makes use of two distinct regions of memory for
//! the [core] types:
//!
//! * Global memory contains types that can be shared across modules;
//! * Local memory contains types that do not cross the module boundary;
//!
//! The type checker operates most of the time in local memory during
//! its execution. Types from other modules are [`localized`], while
//! the types that it produces are [`globalized`]. Local memory is
//! dropped at the end of type checking, improving memory efficiency.
//!
//! [core]: crate::core
//! [`localized`]: localize
//! [`globalized`]: globalize

use crate::algorithm::state::{CheckContext, CheckState};
use crate::{ExternalQueries, Type, TypeId};

/// Moves a type from local memory to global memory.
pub fn localize<Q>(state: &mut CheckState, context: &CheckContext<Q>, id: TypeId) -> TypeId
where
    Q: ExternalQueries,
{
    let queries = context.queries;
    let mut source = TraversalSource { state, queries, mode: TraversalMode::FromGlobal };
    traverse(&mut source, id)
}

/// Moves a type from global memory to local memory.
pub fn globalize<Q>(state: &mut CheckState, context: &CheckContext<Q>, id: TypeId) -> TypeId
where
    Q: ExternalQueries,
{
    let queries = context.queries;
    let mut source = TraversalSource { state, queries, mode: TraversalMode::FromLocal };
    traverse(&mut source, id)
}

struct TraversalSource<'a, Q: ExternalQueries> {
    state: &'a mut CheckState,
    queries: &'a Q,
    mode: TraversalMode,
}

enum TraversalMode {
    FromGlobal,
    FromLocal,
}

impl<'a, Q: ExternalQueries> TraversalSource<'a, Q> {
    fn intern(&mut self, ty: Type) -> TypeId {
        match self.mode {
            TraversalMode::FromGlobal => self.state.storage.intern(ty),
            TraversalMode::FromLocal => self.queries.intern_type(ty),
        }
    }

    fn lookup(&mut self, id: TypeId) -> Type {
        match self.mode {
            TraversalMode::FromGlobal => self.queries.lookup_type(id),
            TraversalMode::FromLocal => {
                let id = self.state.normalize_type(id);
                self.state.storage[id].clone()
            }
        }
    }
}

fn traverse<'a, Q: ExternalQueries>(source: &mut TraversalSource<'a, Q>, id: TypeId) -> TypeId {
    let ty = match source.lookup(id) {
        Type::Application(function, argument) => {
            let function = traverse(source, function);
            let argument = traverse(source, argument);
            Type::Application(function, argument)
        }

        Type::Constructor(file_id, item_id) => Type::Constructor(file_id, item_id),

        Type::Forall(binder, inner) => {
            let mut binder = binder.clone();

            binder.kind = traverse(source, binder.kind);
            let inner = traverse(source, inner);

            Type::Forall(binder, inner)
        }

        Type::Function(argument, result) => {
            let argument = traverse(source, argument);
            let result = traverse(source, result);
            Type::Function(argument, result)
        }

        Type::KindApplication(function, argument) => {
            let function = traverse(source, function);
            let argument = traverse(source, argument);
            Type::KindApplication(function, argument)
        }

        Type::Unification(_) => match source.mode {
            TraversalMode::FromGlobal => {
                unreachable!(
                    "invariant violated: forbidden unification variable transfer from Localize"
                );
            }
            TraversalMode::FromLocal => {
                unreachable!(
                    "invariant violated: forbidden unification variable transfer from Globalize"
                );
            }
        },

        Type::Variable(variable) => Type::Variable(variable),

        Type::Unknown => Type::Unknown,
    };
    source.intern(ty)
}
