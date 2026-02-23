//! Implements name-based type substitution for the core representation.

use rustc_hash::FxHashMap;

use building_types::QueryResult;

use crate::ExternalQueries;
use crate::context::CheckContext;
use crate::core::fold::{FoldAction, TypeFold, fold_type};
use crate::core::{Name, Type, TypeId};
use crate::state::CheckState;

pub type NameToType = FxHashMap<Name, TypeId>;

/// Implements [`Name`]-based substitution for [`Type::Rigid`] variables.
///
/// Names are globally unique, removing the need for scope tracking and
/// removing the need for capture-avoiding substitutions. This property
/// is extremely useful for for instantiation.
pub struct SubstituteName {
    bindings: NameToType,
}

impl SubstituteName {
    pub fn one<Q>(
        state: &mut CheckState,
        context: &CheckContext<Q>,
        name: Name,
        replacement: TypeId,
        in_type: TypeId,
    ) -> QueryResult<TypeId>
    where
        Q: ExternalQueries,
    {
        let bindings = NameToType::from_iter([(name, replacement)]);
        fold_type(state, context, in_type, &mut SubstituteName { bindings })
    }

    pub fn many<Q>(
        state: &mut CheckState,
        context: &CheckContext<Q>,
        bindings: NameToType,
        in_type: TypeId,
    ) -> QueryResult<TypeId>
    where
        Q: ExternalQueries,
    {
        fold_type(state, context, in_type, &mut SubstituteName { bindings })
    }
}

impl TypeFold for SubstituteName {
    fn transform<Q>(
        &mut self,
        _state: &mut CheckState,
        _context: &CheckContext<Q>,
        _id: TypeId,
        t: &Type,
    ) -> QueryResult<FoldAction>
    where
        Q: ExternalQueries,
    {
        if let Type::Rigid(name, _, _) = t
            && let Some(id) = self.bindings.get(name)
        {
            Ok(FoldAction::Replace(*id))
        } else {
            Ok(FoldAction::Continue)
        }
    }
}
