pub mod completion;
pub mod definition;
pub mod hover;
pub mod locate;

use building::QueryEngine;
use files::Files;

#[derive(Default)]
pub struct Compiler {
    pub runtime: QueryEngine,
    pub files: Files,
}
