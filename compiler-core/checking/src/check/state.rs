use building_types::QueryResult;
use indexing::IndexedModule;
use lowering::LoweredModule;

use crate::{
    External,
    core::{Type, TypeId, debruijn, storage::TypeStorage},
};

pub struct CheckState<'s, S>
where
    S: TypeStorage,
{
    pub storage: &'s mut S,
    pub bound: debruijn::Bound,
}

impl<'s, S> CheckState<'s, S>
where
    S: TypeStorage,
{
    pub fn new(storage: &'s mut S) -> CheckState<'s, S> {
        let bound = debruijn::Bound::default();
        CheckState { storage, bound }
    }
}

pub struct CheckContext<'e> {
    pub prim: PrimCore,
    pub indexed: &'e IndexedModule,
    pub lowered: &'e LoweredModule,
}

impl<'e> CheckContext<'e> {
    pub fn new(
        prim: PrimCore,
        indexed: &'e IndexedModule,
        lowered: &'e LoweredModule,
    ) -> CheckContext<'e> {
        CheckContext { prim, indexed, lowered }
    }
}

pub struct PrimCore {
    pub t: TypeId,
}

impl PrimCore {
    pub fn collect<S>(
        external: &impl External,
        state: &mut CheckState<S>,
    ) -> QueryResult<PrimCore>
    where
        S: TypeStorage,
    {
        let prim_id = external.prim_id();
        let prim_resolved = external.resolved(prim_id)?;

        let mut lookup_prim_type = |name| {
            let prim_type = prim_resolved.lookup_type(&prim_resolved, None, name);
            let (_, type_id) = prim_type.expect("invariant violated: missing Type");
            state.storage.intern(Type::Constructor(prim_id, type_id))
        };

        let t = lookup_prim_type("Type");

        Ok(PrimCore { t })
    }
}
