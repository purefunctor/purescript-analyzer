use smol_str::SmolStr;

use super::prelude::*;

pub struct HasPrefix(pub SmolStr);

impl Filter for HasPrefix {
    fn matches(&self, name: &str) -> bool {
        name.starts_with(self.0.as_str())
    }
}
