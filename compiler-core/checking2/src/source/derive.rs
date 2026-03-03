pub mod head;
pub mod member;

pub mod contravariant;
pub mod eq1_ord1;
pub mod eq_ord;
pub mod field;
pub mod foldable;
pub mod functor;
pub mod generic;
pub mod newtype;
pub mod tools;
pub mod traversable;
pub mod variance;

use building_types::QueryResult;
use files::FileId;
use indexing::{TermItemId, TypeItemId};

use crate::core::TypeId;
use crate::ExternalQueries;
use crate::context::CheckContext;
use crate::state::CheckState;
use crate::source::derive::variance::VarianceConfig;

#[derive(Clone, Copy)]
enum DeriveDispatch {
    Eq,
    Eq1,
    Functor,
    Bifunctor,
    Contravariant,
    Ord,
    Ord1,
    SupportedButNotImplemented,
    Unsupported,
}

#[derive(Clone, Copy)]
pub(super) enum DeriveStrategy {
    FieldConstraints {
        data_file: FileId,
        data_id: TypeItemId,
        derived_type: TypeId,
        class: (FileId, TypeItemId),
    },
    DelegateConstraint {
        derived_type: TypeId,
        class: (FileId, TypeItemId),
    },
    VarianceConstraints {
        data_file: FileId,
        data_id: TypeItemId,
        derived_type: TypeId,
        config: VarianceConfig,
    },
    Unsupported,
}

pub struct DeriveHeadResult {
    item_id: TermItemId,
    constraints: Vec<TypeId>,
    class_file: FileId,
    class_id: TypeItemId,
    arguments: Vec<TypeId>,
    strategy: DeriveStrategy,
}

fn derive_dispatch<Q>(
    context: &CheckContext<Q>,
    class_file: FileId,
    class_id: TypeItemId,
) -> DeriveDispatch
where
    Q: ExternalQueries,
{
    let class = Some((class_file, class_id));
    if class == context.known_types.eq {
        DeriveDispatch::Eq
    } else if class == context.known_types.eq1 {
        DeriveDispatch::Eq1
    } else if class == context.known_types.functor {
        DeriveDispatch::Functor
    } else if class == context.known_types.bifunctor {
        DeriveDispatch::Bifunctor
    } else if class == context.known_types.contravariant {
        DeriveDispatch::Contravariant
    } else if class == context.known_types.ord {
        DeriveDispatch::Ord
    } else if class == context.known_types.ord1 {
        DeriveDispatch::Ord1
    } else if class == context.known_types.profunctor
        || class == context.known_types.foldable
        || class == context.known_types.bifoldable
        || class == context.known_types.traversable
        || class == context.known_types.bitraversable
        || class == context.known_types.newtype
        || class == context.known_types.generic
    {
        DeriveDispatch::SupportedButNotImplemented
    } else {
        DeriveDispatch::Unsupported
    }
}

pub fn check_derive_declarations<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
) -> QueryResult<Vec<DeriveHeadResult>>
where
    Q: ExternalQueries,
{
    head::check_derive_declarations(state, context)
}

pub fn check_derive_members<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    derives: &[DeriveHeadResult],
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    member::check_derive_members(state, context, derives)
}
