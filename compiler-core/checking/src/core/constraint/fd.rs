use std::collections::HashSet;

use crate::safe_loop;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Fd {
    pub determiners: HashSet<usize>,
    pub determined: HashSet<usize>,
}

impl Fd {
    pub fn new(
        determiners: impl IntoIterator<Item = usize>,
        determined: impl IntoIterator<Item = usize>,
    ) -> Fd {
        Fd {
            determiners: determiners.into_iter().collect(),
            determined: determined.into_iter().collect(),
        }
    }
}

pub fn get_all_determined(functional_dependencies: &[Fd]) -> HashSet<usize> {
    functional_dependencies.iter().flat_map(|fd| fd.determined.iter().copied()).collect()
}

pub fn compute_closure(
    functional_dependencies: &[Fd],
    initial_positions: &HashSet<usize>,
) -> HashSet<usize> {
    let mut determined = initial_positions.clone();
    safe_loop! {
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
            return determined;
        }
    }
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
        let fundeps = vec![Fd::new([0], [1])];
        let initial: HashSet<usize> = [0].into_iter().collect();
        let result = compute_closure(&fundeps, &initial);
        assert_eq!(result, [0, 1].into_iter().collect());
    }

    #[test]
    fn test_fundep_not_triggered() {
        let fundeps = vec![Fd::new([0], [1])];
        let initial: HashSet<usize> = [1].into_iter().collect();
        let result = compute_closure(&fundeps, &initial);
        assert_eq!(result, [1].into_iter().collect());
    }

    #[test]
    fn test_transitive_fundeps() {
        let fundeps = vec![Fd::new([0], [1]), Fd::new([1], [2])];
        let initial: HashSet<usize> = [0].into_iter().collect();
        let result = compute_closure(&fundeps, &initial);
        assert_eq!(result, [0, 1, 2].into_iter().collect());
    }

    #[test]
    fn test_multi_determiner() {
        let fundeps = vec![Fd::new([0, 1], [2])];

        let initial: HashSet<usize> = [0].into_iter().collect();
        let result = compute_closure(&fundeps, &initial);
        assert_eq!(result, [0].into_iter().collect());

        let initial: HashSet<usize> = [0, 1].into_iter().collect();
        let result = compute_closure(&fundeps, &initial);
        assert_eq!(result, [0, 1, 2].into_iter().collect());
    }

    #[test]
    fn test_multiple_determined() {
        let fundeps = vec![Fd::new([0], [1, 2])];
        let initial: HashSet<usize> = [0].into_iter().collect();
        let result = compute_closure(&fundeps, &initial);
        assert_eq!(result, [0, 1, 2].into_iter().collect());
    }

    #[test]
    fn test_empty_determiners() {
        let fundeps = vec![Fd::new([], [0])];
        let initial: HashSet<usize> = HashSet::new();
        let result = compute_closure(&fundeps, &initial);
        assert_eq!(result, [0].into_iter().collect());
    }

    #[test]
    fn test_all_determined_no_fundeps() {
        let result = get_all_determined(&[]);
        assert_eq!(result, HashSet::new());
    }

    #[test]
    fn test_all_determined_single_fundep() {
        let fundeps = vec![Fd::new([0], [1])];
        let result = get_all_determined(&fundeps);
        assert_eq!(result, [1].into_iter().collect());
    }

    #[test]
    fn test_all_determined_multiple_fundeps() {
        let fundeps = vec![Fd::new([0], [1]), Fd::new([1], [2])];
        let result = get_all_determined(&fundeps);
        assert_eq!(result, [1, 2].into_iter().collect());
    }

    #[test]
    fn test_all_determined_overlapping() {
        let fundeps = vec![Fd::new([0], [1, 2]), Fd::new([3], [1])];
        let result = get_all_determined(&fundeps);
        assert_eq!(result, [1, 2].into_iter().collect());
    }

    #[test]
    fn test_all_determined_empty_determiners() {
        let fundeps = vec![Fd::new([], [0])];
        let result = get_all_determined(&fundeps);
        assert_eq!(result, [0].into_iter().collect());
    }
}
