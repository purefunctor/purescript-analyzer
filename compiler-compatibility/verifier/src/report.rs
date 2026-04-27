use std::collections::BTreeSet;
use std::fs;
use std::path::PathBuf;

use serde::{Deserialize, Serialize};

use crate::error::VerifierError;
use crate::selection::{SelectedPackage, SelectionMode};

#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct PackageSetReport {
    pub version: String,
    pub compiler: String,
    pub published: String,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SelectionReport {
    pub mode: SelectionMode,
    pub requested_packages: Vec<String>,
    pub resolved_packages: Vec<SelectedPackage>,
}

#[derive(Debug, Clone, Default, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Summary {
    pub packages: usize,
    pub source_files: usize,
    pub verifier_errors: usize,
    pub compiler_errors: usize,
    pub compiler_warnings: usize,
    pub packages_with_errors: usize,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct SpanReport {
    pub start: u32,
    pub end: u32,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct CompilerDiagnostic {
    pub package: String,
    pub version: String,
    pub file: String,
    pub stage: String,
    pub severity: String,
    pub code: String,
    pub message: String,
    pub span: SpanReport,
    #[serde(skip)]
    pub human: String,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct VerifierIssue {
    pub kind: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub package: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub dependency: Option<String>,
    pub message: String,
}

impl VerifierIssue {
    pub fn missing_package(package: &str) -> VerifierIssue {
        VerifierIssue {
            kind: "MissingPackageSetPackage".to_string(),
            package: Some(package.to_string()),
            dependency: None,
            message: format!("package '{package}' is not present in the selected package set"),
        }
    }

    pub fn missing_manifest(package: &str, version: &str) -> VerifierIssue {
        VerifierIssue {
            kind: "MissingManifest".to_string(),
            package: Some(package.to_string()),
            dependency: None,
            message: format!(
                "package '{package}' has package-set version '{version}' but no matching registry-index manifest"
            ),
        }
    }

    pub fn missing_dependency(package: &str, dependency: &str) -> VerifierIssue {
        VerifierIssue {
            kind: "MissingPackageSetDependency".to_string(),
            package: Some(package.to_string()),
            dependency: Some(dependency.to_string()),
            message: format!(
                "package '{package}' depends on '{dependency}', which is absent from the selected package set"
            ),
        }
    }

    pub fn duplicate_module(module: &str, first: &str, second: &str) -> VerifierIssue {
        VerifierIssue {
            kind: "DuplicateModule".to_string(),
            package: None,
            dependency: None,
            message: format!("module '{module}' is provided by both '{first}' and '{second}'"),
        }
    }

    pub fn query_error(package: &str, file: &str, stage: &str, message: String) -> VerifierIssue {
        VerifierIssue {
            kind: "QueryError".to_string(),
            package: Some(package.to_string()),
            dependency: None,
            message: format!("{stage} failed for {file}: {message}"),
        }
    }
}

#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Report {
    pub package_set: PackageSetReport,
    pub selection: SelectionReport,
    pub summary: Summary,
    pub diagnostics: Vec<CompilerDiagnostic>,
    pub verifier_errors: Vec<VerifierIssue>,
}

impl Report {
    pub fn new(package_set: PackageSetReport, selection: SelectionReport) -> Report {
        let packages = selection.resolved_packages.len();
        Report {
            package_set,
            selection,
            summary: Summary { packages, ..Summary::default() },
            diagnostics: Vec::new(),
            verifier_errors: Vec::new(),
        }
    }

    pub fn recompute_summary(&mut self) {
        self.summary.packages = self.selection.resolved_packages.len();
        self.summary.verifier_errors = self.verifier_errors.len();
        self.summary.compiler_errors =
            self.diagnostics.iter().filter(|diag| diag.severity == "error").count();
        self.summary.compiler_warnings =
            self.diagnostics.iter().filter(|diag| diag.severity == "warning").count();
        self.summary.packages_with_errors = self
            .diagnostics
            .iter()
            .filter(|diag| diag.severity == "error")
            .map(|diag| diag.package.as_str())
            .collect::<BTreeSet<_>>()
            .len();
    }

    pub fn has_errors(&self) -> bool {
        self.summary.verifier_errors > 0 || self.summary.compiler_errors > 0
    }

    pub fn print_human(&self) {
        println!("Package set: {}", self.package_set.version);
        println!("Compiler: {}", self.package_set.compiler);
        println!("Selected packages: {}", self.summary.packages);
        println!("Source files: {}", self.summary.source_files);
        println!();

        for issue in &self.verifier_errors {
            println!("verifier[{}]: {}", issue.kind, issue.message);
        }
        if !self.verifier_errors.is_empty() {
            println!();
        }

        for diagnostic in &self.diagnostics {
            print!("{}", diagnostic.human);
            if !diagnostic.human.ends_with('\n') {
                println!();
            }
        }

        println!("Summary:");
        println!("  verifier errors: {}", self.summary.verifier_errors);
        println!("  parse errors: {}", self.stage_error_count("parse"));
        println!("  indexing errors: {}", self.stage_error_count("indexing"));
        println!("  resolving errors: {}", self.stage_error_count("resolving"));
        println!("  lowering errors: {}", self.stage_error_count("lowering"));
        println!("  checking errors: {}", self.stage_error_count("checking"));
        println!("  compiler errors: {}", self.summary.compiler_errors);
        println!("  compiler warnings: {}", self.summary.compiler_warnings);
        println!("  packages with errors: {}", self.summary.packages_with_errors);
    }

    pub fn write_json(&self, path: PathBuf) -> Result<(), VerifierError> {
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent)?;
        }
        let content = serde_json::to_string_pretty(self)?;
        fs::write(path, content).map_err(VerifierError::from)
    }

    fn stage_error_count(&self, stage: &str) -> usize {
        self.diagnostics
            .iter()
            .filter(|diagnostic| diagnostic.severity == "error" && diagnostic.stage == stage)
            .count()
    }
}

#[cfg(test)]
mod tests {
    use tempfile::tempdir;

    use crate::selection::{SelectedPackage, SelectionMode};

    use super::{
        CompilerDiagnostic, PackageSetReport, Report, SelectionReport, SpanReport, VerifierIssue,
    };

    #[test]
    fn json_serialization_includes_core_sections() {
        let mut report = Report::new(
            PackageSetReport {
                version: "64.7.1".to_string(),
                compiler: "0.15.15".to_string(),
                published: "2025-05-06".to_string(),
            },
            SelectionReport {
                mode: SelectionMode::Packages,
                requested_packages: vec!["effect".to_string()],
                resolved_packages: vec![SelectedPackage {
                    name: "effect".to_string(),
                    version: "4.0.0".to_string(),
                }],
            },
        );
        report.summary.source_files = 1;
        report.diagnostics.push(CompilerDiagnostic {
            package: "effect".to_string(),
            version: "4.0.0".to_string(),
            file: "src/Effect.purs".to_string(),
            stage: "checking".to_string(),
            severity: "error".to_string(),
            code: "UnknownType".to_string(),
            message: "unknown type".to_string(),
            span: SpanReport { start: 0, end: 10 },
            human: "hidden from JSON".to_string(),
        });
        report.verifier_errors.push(VerifierIssue::missing_dependency("effect", "prelude"));
        report.recompute_summary();

        let dir = tempdir().unwrap();
        let path = dir.path().join("report.json");
        report.write_json(path.clone()).unwrap();
        let json = std::fs::read_to_string(path).unwrap();

        assert!(json.contains("\"packageSet\""));
        assert!(json.contains("\"selection\""));
        assert!(json.contains("\"summary\""));
        assert!(json.contains("\"diagnostics\""));
        assert!(json.contains("\"verifierErrors\""));
        assert!(!json.contains("hidden from JSON"));
        assert!(report.has_errors());
    }
}
