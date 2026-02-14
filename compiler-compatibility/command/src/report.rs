use std::collections::BTreeMap;

use serde::Serialize;

#[derive(Debug, Serialize)]
pub struct CompatReport {
    pub timestamp: String,
    pub git_sha: String,
    pub package_set: String,
    pub summary: ReportSummary,
    pub root_causes: Vec<String>,
    pub packages: BTreeMap<String, PackageReport>,
}

#[derive(Debug, Serialize)]
pub struct ReportSummary {
    pub total: usize,
    pub ok: usize,
    pub warnings_only: usize,
    pub failed: usize,
    pub failed_root_cause: usize,
    pub failed_cascaded: usize,
}

#[derive(Debug, Serialize)]
pub struct PackageReport {
    pub version: String,
    pub topo_layer: usize,
    pub errors: usize,
    pub warnings: usize,
    pub classification: Classification,
}

#[derive(Debug, Serialize)]
pub struct Classification {
    pub root_cause: bool,
    pub cascaded_from: Vec<String>,
    pub cascaded_from_root_causes: Vec<String>,
}
