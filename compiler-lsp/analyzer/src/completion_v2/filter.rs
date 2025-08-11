use smol_str::SmolStr;

use super::prelude::*;

pub struct StartsWith(pub SmolStr);

impl Filter for StartsWith {
    fn matches(&self, name: &str) -> bool {
        name.starts_with(self.0.as_str())
    }
}
