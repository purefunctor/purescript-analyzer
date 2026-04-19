//! Implements canonicalisation for constraints.

use std::ops::Index;
use std::sync::Arc;

use building_types::QueryResult;
use files::FileId;
use indexing::TypeItemId;
use interner::{Id, Interner};
use itertools::Itertools;
use rustc_hash::FxHashMap;

use crate::ExternalQueries;
use crate::context::CheckContext;
use crate::core::{KindOrType, Type, TypeId, normalise, toolkit, zonk};
use crate::state::CheckState;

/// The canonical structure of a constraint.
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct CanonicalConstraint {
    pub file_id: FileId,
    pub type_id: TypeItemId,
    pub arguments: Arc<[KindOrType]>,
}

impl CanonicalConstraint {
    pub fn expect_type_arguments<const N: usize>(&self) -> Option<[TypeId; N]> {
        self.arguments
            .iter()
            .filter_map(|argument| match argument {
                KindOrType::Type(argument) => Some(*argument),
                KindOrType::Kind(_) => None,
            })
            .collect_array()
    }
}

/// Stable identifier for a [`CanonicalConstraint`].
pub type CanonicalConstraintId = Id<CanonicalConstraint>;

/// Interner and cache for [`CanonicalConstraint`].
#[derive(Default)]
pub struct Canonicals {
    interner: Interner<CanonicalConstraint>,
    cache: FxHashMap<TypeId, CanonicalConstraintId>,
}

impl Canonicals {
    pub fn intern(&mut self, canonical: CanonicalConstraint) -> Id<CanonicalConstraint> {
        self.interner.intern(canonical)
    }

    pub fn type_id<Q>(&self, context: &CheckContext<Q>, id: CanonicalConstraintId) -> TypeId
    where
        Q: ExternalQueries,
    {
        let CanonicalConstraint { file_id, type_id, arguments } = &self[id];
        let mut constraint = context.queries.intern_type(Type::Constructor(*file_id, *type_id));

        for &argument in arguments.iter() {
            constraint = match argument {
                KindOrType::Kind(argument) => context.intern_kind_application(constraint, argument),
                KindOrType::Type(argument) => context.intern_application(constraint, argument),
            };
        }

        constraint
    }

    pub fn associate(
        &mut self,
        constraint: TypeId,
        canonical: CanonicalConstraint,
    ) -> CanonicalConstraintId {
        let id = self.intern(canonical);
        self.cache.insert(constraint, id);
        // TODO: This check was disabled as it does not consider normalisation.
        // A future version of this check must ensure that normalisation is
        // taken into account before checking that the cache is not overwritten.
        // debug_assert!(previous.is_none(), "critical violation: canonical cache overwrite");
        id
    }
}

impl Index<CanonicalConstraintId> for Canonicals {
    type Output = CanonicalConstraint;

    fn index(&self, index: CanonicalConstraintId) -> &CanonicalConstraint {
        &self.interner[index]
    }
}

/// Extracts a [`CanonicalConstraint`] from a [`Type`].
pub fn canonicalise<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    id: TypeId,
) -> QueryResult<Option<CanonicalConstraintId>>
where
    Q: ExternalQueries,
{
    let (class, arguments) = toolkit::extract_all_applications(state, context, id)?;

    let class = normalise::expand(state, context, class)?;

    let Type::Constructor(file_id, type_id) = context.lookup_type(class) else {
        return Ok(None);
    };

    let arguments = Arc::from(arguments); // TODO: extract_all_applications
    let canonical = CanonicalConstraint { file_id, type_id, arguments };
    let canonical_id = state.canonicals.associate(id, canonical);

    Ok(Some(canonical_id))
}

pub fn zonk_canonical<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    id: CanonicalConstraintId,
) -> QueryResult<CanonicalConstraintId>
where
    Q: ExternalQueries,
{
    let canonical = state.canonicals[id].clone();
    let arguments = canonical.arguments.iter().map(|&argument| match argument {
        KindOrType::Kind(argument) => zonk::zonk(state, context, argument).map(KindOrType::Kind),
        KindOrType::Type(argument) => zonk::zonk(state, context, argument).map(KindOrType::Type),
    });

    let arguments = arguments.collect::<QueryResult<Arc<[_]>>>()?;
    Ok(state.canonicals.intern(CanonicalConstraint { arguments, ..canonical }))
}
