mod filter;
mod prelude;
mod sources;

use smol_str::SmolStr;

use filter::StartsWith;
use prelude::{Context, Source};
use sources::QualifiedModules;

pub fn collect(context: Context, prefix: Option<SmolStr>) {
    if let Some(prefix) = prefix {
        let _ = QualifiedModules::candidates(&context, StartsWith(prefix));
    }
}
