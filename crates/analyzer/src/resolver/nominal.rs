//! See documentation for [`NominalMap`].

use std::sync::Arc;

use files::FileId;

use crate::ResolverDatabase;

#[derive(Debug, Default, PartialEq, Eq)]
pub struct NominalMap {}

impl NominalMap {
    pub(crate) fn nominal_map_query(_: &dyn ResolverDatabase, _: FileId) -> Arc<NominalMap> {
        let nominal_map = NominalMap::default();
        Arc::new(nominal_map)
    }
}
