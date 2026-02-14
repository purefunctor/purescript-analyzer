#[derive(Debug, Clone, Default)]
pub struct ResolvedSet {
    pub packages: std::collections::BTreeMap<String, String>,
    /// Maps each package name to its direct dependency package names.
    pub dependencies: std::collections::BTreeMap<String, Vec<String>>,
}
