/// Decision logic for test runner output, separated from rendering for testability.

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DecisionInput {
    pub tests_passed: bool,
    pub pending_count: usize,
    pub total_lines_changed: usize,
    pub showed_diffs: bool,
    pub ran_all: bool,
    pub debug: bool,
    pub trace_count: usize,
    pub max_count: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Outcome {
    Clean,
    Failure(FailureDecision),
    Pending(PendingDecision),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FailureDecision {
    pub show_debug_hint: bool,
    pub show_trace_hint: bool,
    pub max_traces_to_show: usize,
    pub pending_note: Option<usize>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PendingDecision {
    pub show_lines_changed: bool,
    pub next_action: NextAction,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NextAction {
    AcceptOrReject,
    ReviewSubset,
    ShowDiff,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SnapshotDisplayLimits {
    pub max_shown: usize,
}

pub fn decide_outcome(input: &DecisionInput) -> Outcome {
    let has_pending = input.pending_count > 0;
    let has_traces = input.trace_count > 0;

    if input.tests_passed && !has_pending {
        return Outcome::Clean;
    }

    if !input.tests_passed {
        return Outcome::Failure(FailureDecision {
            show_debug_hint: !input.debug,
            show_trace_hint: input.debug && has_traces,
            max_traces_to_show: 3,
            pending_note: if has_pending { Some(input.pending_count) } else { None },
        });
    }

    let many_pending = input.ran_all && input.pending_count > 3;

    let next_action = if input.showed_diffs {
        NextAction::AcceptOrReject
    } else if many_pending {
        NextAction::ReviewSubset
    } else {
        NextAction::ShowDiff
    };

    Outcome::Pending(PendingDecision {
        show_lines_changed: input.total_lines_changed > 50,
        next_action,
    })
}

pub fn decide_snapshot_limits(input: &DecisionInput) -> SnapshotDisplayLimits {
    let max_shown = if input.showed_diffs { usize::MAX } else { input.max_count };

    SnapshotDisplayLimits { max_shown }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn base_input() -> DecisionInput {
        DecisionInput {
            tests_passed: true,
            pending_count: 0,
            total_lines_changed: 0,
            showed_diffs: false,
            ran_all: true,
            debug: false,
            trace_count: 0,
            max_count: 3,
        }
    }

    #[test]
    fn clean_when_passed_no_pending() {
        let input = base_input();
        assert_eq!(decide_outcome(&input), Outcome::Clean);
    }

    #[test]
    fn failure_suggests_debug_when_not_in_debug_mode() {
        let input = DecisionInput { tests_passed: false, ..base_input() };
        let Outcome::Failure(decision) = decide_outcome(&input) else {
            panic!("expected Failure");
        };
        assert!(decision.show_debug_hint);
        assert!(!decision.show_trace_hint);
    }

    #[test]
    fn failure_suggests_traces_when_in_debug_mode_with_traces() {
        let input =
            DecisionInput { tests_passed: false, debug: true, trace_count: 2, ..base_input() };
        let Outcome::Failure(decision) = decide_outcome(&input) else {
            panic!("expected Failure");
        };
        assert!(!decision.show_debug_hint);
        assert!(decision.show_trace_hint);
    }

    #[test]
    fn failure_notes_pending_count() {
        let input = DecisionInput { tests_passed: false, pending_count: 5, ..base_input() };
        let Outcome::Failure(decision) = decide_outcome(&input) else {
            panic!("expected Failure");
        };
        assert_eq!(decision.pending_note, Some(5));
    }

    #[test]
    fn pending_accept_reject_after_diff() {
        let input = DecisionInput { pending_count: 1, showed_diffs: true, ..base_input() };
        let Outcome::Pending(decision) = decide_outcome(&input) else {
            panic!("expected Pending");
        };
        assert_eq!(decision.next_action, NextAction::AcceptOrReject);
    }

    #[test]
    fn pending_review_subset_when_many() {
        let input = DecisionInput { pending_count: 10, ran_all: true, ..base_input() };
        let Outcome::Pending(decision) = decide_outcome(&input) else {
            panic!("expected Pending");
        };
        assert_eq!(decision.next_action, NextAction::ReviewSubset);
    }

    #[test]
    fn pending_show_diff_for_small_batch() {
        let input = DecisionInput { pending_count: 2, ran_all: true, ..base_input() };
        let Outcome::Pending(decision) = decide_outcome(&input) else {
            panic!("expected Pending");
        };
        assert_eq!(decision.next_action, NextAction::ShowDiff);
    }

    #[test]
    fn pending_shows_lines_changed_when_large() {
        let input = DecisionInput { pending_count: 1, total_lines_changed: 100, ..base_input() };
        let Outcome::Pending(decision) = decide_outcome(&input) else {
            panic!("expected Pending");
        };
        assert!(decision.show_lines_changed);
    }

    #[test]
    fn snapshot_limits_max_when_diff_enabled() {
        let input = DecisionInput { showed_diffs: true, ..base_input() };
        let limits = decide_snapshot_limits(&input);
        assert_eq!(limits.max_shown, usize::MAX);
    }

    #[test]
    fn snapshot_limits_uses_max_count() {
        let input = DecisionInput { max_count: 10, ..base_input() };
        let limits = decide_snapshot_limits(&input);
        assert_eq!(limits.max_shown, 10);
    }

    #[test]
    fn snapshot_limits_default_count() {
        let input = base_input();
        let limits = decide_snapshot_limits(&input);
        assert_eq!(limits.max_shown, 3);
    }

    #[test]
    fn filtered_run_not_considered_many() {
        let input = DecisionInput { pending_count: 10, ran_all: false, ..base_input() };
        let Outcome::Pending(decision) = decide_outcome(&input) else {
            panic!("expected Pending");
        };
        assert_eq!(decision.next_action, NextAction::ShowDiff);
    }
}
