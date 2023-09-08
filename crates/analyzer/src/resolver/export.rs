//! See documentation for [`Exports`].

use std::sync::Arc;

use files::FileId;

use crate::ResolverDatabase;

#[derive(Debug, Default, PartialEq, Eq, Hash)]
pub struct Exports {}

impl Exports {
    pub(crate) fn exports_query(_db: &dyn ResolverDatabase, _file_id: FileId) -> Arc<Exports> {
        let exports = Exports::default();

        Arc::new(exports)
    }
}

/*

Idea:

The `Exports` struct takes a file and analyzes what exactly it exports.
If an export list is found, it uses exported items as a reference as to
what it should insert as entries. Otherwise, it traverses the module-level
scope to determine what can be exported.

*/
