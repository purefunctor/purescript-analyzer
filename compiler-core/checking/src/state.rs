use indexing::IndexedModule;
use lowering::LoweredModule;

use crate::{core::TypeStorage, debruijn};

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
    pub indexed: &'e IndexedModule,
    pub lowered: &'e LoweredModule,
}

impl<'e> CheckContext<'e> {
    pub fn new(indexed: &'e IndexedModule, lowered: &'e LoweredModule) -> CheckContext<'e> {
        Self { indexed, lowered }
    }
}
