mod filter;
mod prelude;
mod sources;

use building::QueryEngine;
use smol_str::SmolStr;

use filter::HasPrefix;
use prelude::{Context, Source};
use sources::QualifiedModules;

pub fn collect(engine: QueryEngine, context: Context, prefix: Option<SmolStr>) {
    if let Some(prefix) = prefix {
        let _ = QualifiedModules::candidates(&engine, &context, HasPrefix(prefix));
    }
}
