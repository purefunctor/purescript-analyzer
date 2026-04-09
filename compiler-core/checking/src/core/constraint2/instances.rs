//! Implements searching for instance chains.

use building_types::QueryResult;
use files::FileId;
use indexing::{IndexedModule, InstanceChainId, TypeItemId};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::context::CheckContext;
use crate::core::constraint2::CanonicalConstraintId;
use crate::core::walk::{TypeWalker, WalkAction, walk_type};
use crate::core::{CheckedInstance, KindOrType, Type, TypeId, normalise};
use crate::state::CheckState;
use crate::{CheckedModule, ExternalQueries};

/// A candidate found for a constraint.
pub struct InstanceCandidate {
    /// The syntactic ID for the instance chain.
    pub id: Option<InstanceChainId>,
    /// The position of the instance in the chain.
    pub position: u32,
    /// Type information about the instance.
    pub instance: CheckedInstance,
}

/// Collects [`InstanceCandidate`]s for a given constraint.
pub fn collect_instance_chains<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    constraint: CanonicalConstraintId,
) -> QueryResult<Vec<Vec<InstanceCandidate>>>
where
    Q: ExternalQueries,
{
    let constraint = state.canonicals[constraint].clone(); // TODO: FIXME

    let mut files_to_search = FxHashSet::from_iter([constraint.file_id]);
    for &argument in constraint.arguments.iter() {
        let argument = match argument {
            KindOrType::Kind(id) | KindOrType::Type(id) => id,
        };
        CollectFileReferences::collect(state, context, argument, &mut files_to_search)?;
    }

    let mut instances = vec![];

    for &file_id in &files_to_search {
        if file_id == context.id {
            collect_instances_from_checked(
                &mut instances,
                &state.checked,
                &context.indexed,
                constraint.file_id,
                constraint.type_id,
            );
        } else {
            let checked = context.queries.checked(file_id)?;
            let indexed = context.queries.indexed(file_id)?;
            collect_instances_from_checked(
                &mut instances,
                &checked,
                &indexed,
                constraint.file_id,
                constraint.type_id,
            );
        }
    }

    type Grouped = FxHashMap<InstanceChainId, Vec<InstanceCandidate>>;

    let mut grouped = Grouped::default();
    let mut chains = vec![];

    for instance in instances {
        if let Some(id) = instance.id {
            grouped.entry(id).or_default().push(instance);
        } else {
            chains.push(vec![instance]);
        }
    }

    for (_, mut chain) in grouped {
        chain.sort_by_key(|instance| instance.position);
        chains.push(chain);
    }

    Ok(chains)
}

fn collect_instances_from_checked(
    output: &mut Vec<InstanceCandidate>,
    checked: &CheckedModule,
    indexed: &IndexedModule,
    class_file: FileId,
    class_id: TypeItemId,
) {
    output.extend(
        checked
            .instances
            .iter()
            .filter(|(_, instance)| instance.resolution == (class_file, class_id))
            .map(|(&id, instance)| InstanceCandidate {
                id: indexed.pairs.instance_chain_id(id),
                position: indexed.pairs.instance_chain_position(id).unwrap_or(0),
                instance: CheckedInstance::clone(instance),
            }),
    );

    output.extend(
        checked
            .derived
            .values()
            .filter(|instance| instance.resolution == (class_file, class_id))
            .cloned()
            .map(|instance| InstanceCandidate { id: None, position: 0, instance }),
    );
}

struct CollectFileReferences<'a> {
    files: &'a mut FxHashSet<FileId>,
}

impl<'a> CollectFileReferences<'a> {
    fn collect<Q>(
        state: &mut CheckState,
        context: &CheckContext<Q>,
        id: TypeId,
        files: &'a mut FxHashSet<FileId>,
    ) -> QueryResult<()>
    where
        Q: ExternalQueries,
    {
        let id = normalise::expand(state, context, id)?;
        walk_type(state, context, id, &mut CollectFileReferences { files })
    }
}

impl TypeWalker for CollectFileReferences<'_> {
    fn visit<Q>(
        &mut self,
        _state: &mut CheckState,
        _context: &CheckContext<Q>,
        _id: TypeId,
        t: &Type,
    ) -> QueryResult<WalkAction>
    where
        Q: ExternalQueries,
    {
        if let Type::Constructor(file_id, _) = t {
            self.files.insert(*file_id);
        }
        Ok(WalkAction::Continue)
    }
}
