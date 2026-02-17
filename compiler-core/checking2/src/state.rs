//! Implements the algorithm's core state structures.

use files::FileId;

use crate::CheckedModule;
use crate::core::Name;

/// Yields globally unique [`Name`] values.
pub struct Names {
    next: u32,
    file: FileId,
}

impl Names {
    pub fn new(file: FileId) -> Names {
        Names { next: 0, file }
    }

    pub fn fresh(&mut self) -> Name {
        let unique = self.next;
        self.next += 1;
        Name { file: self.file, unique }
    }
}

/// The core state structure threaded through the algorithm.
pub struct CheckState {
    /// The output being built, populated by checking rules.
    pub checked: CheckedModule,

    /// Produces fresh [`Name`] values for bound type variables.
    pub names: Names,
}

impl CheckState {
    pub fn new(file_id: FileId) -> CheckState {
        CheckState { checked: Default::default(), names: Names::new(file_id) }
    }
}
