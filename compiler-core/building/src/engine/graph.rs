use std::iter;

use rustc_hash::FxHashMap;

use super::SnapshotId;

#[derive(Debug, Default)]
pub(super) struct SnapshotGraph {
    inner: FxHashMap<SnapshotId, SnapshotId>,
}

impl SnapshotGraph {
    pub(super) fn add_edge(&mut self, from_id: SnapshotId, to_id: SnapshotId) -> bool {
        if iter::successors(Some(to_id), |&id| self.inner.get(&id).copied()).any(|id| id == from_id)
        {
            return false;
        }

        self.inner.insert(from_id, to_id);

        true
    }
}

#[cfg(test)]
mod tests {
    use super::{SnapshotGraph, SnapshotId};

    #[test]
    fn test_basic_cycle() {
        let mut graph = SnapshotGraph::default();
        let id_a = SnapshotId(0);
        let id_b = SnapshotId(1);
        assert_eq!(graph.add_edge(id_a, id_b), true);
        assert_eq!(graph.add_edge(id_b, id_a), false);
    }

    #[test]
    fn test_triplet_cycle() {
        let mut graph = SnapshotGraph::default();
        let id_a = SnapshotId(0);
        let id_b = SnapshotId(1);
        let id_c = SnapshotId(2);
        assert_eq!(graph.add_edge(id_a, id_b), true);
        assert_eq!(graph.add_edge(id_b, id_c), true);
        assert_eq!(graph.add_edge(id_c, id_a), false);
    }

    #[test]
    fn test_self_cycle() {
        let mut graph = SnapshotGraph::default();
        let id_a = SnapshotId(0);
        assert_eq!(graph.add_edge(id_a, id_a), false);
    }
}
