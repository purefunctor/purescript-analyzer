use std::path::{Path, PathBuf};
use std::str::FromStr;

#[derive(Copy, Clone, Debug)]
pub enum TestCategory {
    Checking,
    Lowering,
    Resolving,
    Lsp,
}

impl TestCategory {
    pub fn as_str(&self) -> &'static str {
        match self {
            TestCategory::Checking => "checking",
            TestCategory::Lowering => "lowering",
            TestCategory::Resolving => "resolving",
            TestCategory::Lsp => "lsp",
        }
    }

    pub fn fixtures_subdir_fragment(&self) -> String {
        format!("tests-integration/fixtures/{}", self.as_str())
    }

    pub fn snapshot_path_fragments(&self) -> Vec<String> {
        vec![
            format!("tests-integration/fixtures/{}", self.as_str()),
            format!("tests-integration/tests/snapshots/{}__", self.as_str()),
        ]
    }

    pub fn extra_env(&self, debug: bool) -> Vec<(&'static str, String)> {
        if debug { vec![("TRACE_LEVEL", "debug".to_string())] } else { vec![] }
    }

    pub fn trace_for_snapshot(&self, snap_path: &Path, trace_paths: &[PathBuf]) -> Option<PathBuf> {
        match self {
            TestCategory::Checking => {
                crate::test_runner::traces::match_checking_trace(snap_path, trace_paths)
            }
            _ => None,
        }
    }
}

impl FromStr for TestCategory {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "checking" | "c" => Ok(TestCategory::Checking),
            "lowering" | "l" => Ok(TestCategory::Lowering),
            "resolving" | "r" => Ok(TestCategory::Resolving),
            "lsp" => Ok(TestCategory::Lsp),
            _ => Err(format!(
                "unknown test category '{}', expected: checking (c), lowering (l), resolving (r), lsp",
                s
            )),
        }
    }
}
