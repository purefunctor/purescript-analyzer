use building_types::QueryResult;

use crate::ExternalQueries;
use crate::context::CheckContext;
use crate::error::{ErrorCrumb, ErrorKind};
use crate::state::CheckState;

use super::{DeriveDispatch, DeriveHeadResult};

pub fn check_derive_members<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    derives: &[DeriveHeadResult],
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    for result in derives {
        state.with_error_crumb(ErrorCrumb::TermDeclaration(result.item_id), |state| {
            check_derive_member(state, context, result)
        })?;
    }
    Ok(())
}

fn check_derive_member<Q>(
    state: &mut CheckState,
    _context: &CheckContext<Q>,
    result: &DeriveHeadResult,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    match result.dispatch {
        DeriveDispatch::Eq
        | DeriveDispatch::Eq1
        | DeriveDispatch::Ord
        | DeriveDispatch::Ord1
        | DeriveDispatch::Functor
        | DeriveDispatch::Bifunctor
        | DeriveDispatch::Contravariant
        | DeriveDispatch::Profunctor
        | DeriveDispatch::Foldable
        | DeriveDispatch::Bifoldable
        | DeriveDispatch::Traversable
        | DeriveDispatch::Bitraversable
        | DeriveDispatch::Newtype
        | DeriveDispatch::Generic => {
            state.insert_error(ErrorKind::DeriveNotSupportedYet {
                class_file: result.class_file,
                class_id: result.class_id,
            });
        }
        DeriveDispatch::Unsupported => {
            state.insert_error(ErrorKind::CannotDeriveClass {
                class_file: result.class_file,
                class_id: result.class_id,
            });
        }
    }
    Ok(())
}
