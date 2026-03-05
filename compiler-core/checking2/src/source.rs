//! Implements syntax-driven checking rules for source files.

pub mod binder;
pub mod operator;
pub mod roles;
pub mod synonym;
pub mod terms;
pub mod types;

mod derive;
mod term_items;
mod type_items;

pub use term_items::check_term_items;
pub use type_items::check_type_items;

use building_types::QueryResult;

use crate::ExternalQueries;
use crate::context::CheckContext;
use crate::core::{TypeId, signature};
use crate::state::CheckState;

fn is_binary_operator_type<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    kind: TypeId,
) -> QueryResult<bool>
where
    Q: ExternalQueries,
{
    let signature::DecomposedSignature { arguments, .. } = signature::decompose_signature(
        state,
        context,
        kind,
        signature::DecomposeSignatureMode::Full,
    )?;

    Ok(arguments.len() == 2)
}
