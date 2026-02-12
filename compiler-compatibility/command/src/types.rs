#[derive(Debug, Clone, Default)]
pub struct ResolvedSet {
    pub packages: std::collections::BTreeMap<String, String>,
}
