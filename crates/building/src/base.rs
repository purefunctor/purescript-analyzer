#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum QueryKey {
    Index(usize),
    Resolve(usize),
}
