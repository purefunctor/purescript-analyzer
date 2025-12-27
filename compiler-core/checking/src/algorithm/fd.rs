use std::collections::HashSet;

/// Represents a functional dependency: determiners -> determined
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunDep {
    pub determiners: HashSet<usize>,
    pub determined: HashSet<usize>,
}

impl FunDep {
    pub fn new(
        determiners: impl IntoIterator<Item = usize>,
        determined: impl IntoIterator<Item = usize>,
    ) -> FunDep {
        FunDep {
            determiners: determiners.into_iter().collect(),
            determined: determined.into_iter().collect(),
        }
    }
}

/// Compute the closure of positions determined by functional dependencies.
///
/// Starting from `initial` positions, iteratively applies fundeps:
/// if all determiner positions are in the set, add all determined positions.
pub fn compute_closure(
    functional_dependencies: &[FunDep],
    initial_positions: &HashSet<usize>,
) -> HashSet<usize> {
    let mut determined = initial_positions.clone();

    loop {
        let mut changed = false;

        for functional_dependency in functional_dependencies {
            if functional_dependency.determiners.is_subset(&determined) {
                for &position in &functional_dependency.determined {
                    if determined.insert(position) {
                        changed = true;
                    }
                }
            }
        }

        if !changed {
            break;
        }
    }

    determined
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_no_fundeps() {
        let initial: HashSet<usize> = [0, 1].into_iter().collect();
        let result = compute_closure(&[], &initial);
        assert_eq!(result, initial);
    }

    #[test]
    fn test_single_fundep() {
        // a -> b (position 0 determines position 1)
        let fundeps = vec![FunDep::new([0], [1])];
        let initial: HashSet<usize> = [0].into_iter().collect();
        let result = compute_closure(&fundeps, &initial);
        assert_eq!(result, [0, 1].into_iter().collect());
    }

    #[test]
    fn test_fundep_not_triggered() {
        // a -> b, but only b is in initial
        let fundeps = vec![FunDep::new([0], [1])];
        let initial: HashSet<usize> = [1].into_iter().collect();
        let result = compute_closure(&fundeps, &initial);
        assert_eq!(result, [1].into_iter().collect());
    }

    #[test]
    fn test_transitive_fundeps() {
        // a -> b, b -> c (positions 0 -> 1 -> 2)
        let fundeps = vec![FunDep::new([0], [1]), FunDep::new([1], [2])];
        let initial: HashSet<usize> = [0].into_iter().collect();
        let result = compute_closure(&fundeps, &initial);
        assert_eq!(result, [0, 1, 2].into_iter().collect());
    }

    #[test]
    fn test_multi_determiner() {
        // a b -> c (positions 0 and 1 together determine 2)
        let fundeps = vec![FunDep::new([0, 1], [2])];

        // Only position 0 present - fundep should not fire
        let initial: HashSet<usize> = [0].into_iter().collect();
        let result = compute_closure(&fundeps, &initial);
        assert_eq!(result, [0].into_iter().collect());

        // Both positions 0 and 1 present - fundep fires
        let initial: HashSet<usize> = [0, 1].into_iter().collect();
        let result = compute_closure(&fundeps, &initial);
        assert_eq!(result, [0, 1, 2].into_iter().collect());
    }

    #[test]
    fn test_multiple_determined() {
        // a -> b c (position 0 determines both 1 and 2)
        let fundeps = vec![FunDep::new([0], [1, 2])];
        let initial: HashSet<usize> = [0].into_iter().collect();
        let result = compute_closure(&fundeps, &initial);
        assert_eq!(result, [0, 1, 2].into_iter().collect());
    }

    #[test]
    fn test_empty_determiners() {
        // -> a (always determines position 0)
        let fundeps = vec![FunDep::new([], [0])];
        let initial: HashSet<usize> = HashSet::new();
        let result = compute_closure(&fundeps, &initial);
        assert_eq!(result, [0].into_iter().collect());
    }
}
