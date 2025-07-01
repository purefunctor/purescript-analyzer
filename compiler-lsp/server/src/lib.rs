pub mod completion;
pub mod definition;
pub mod hover;
pub mod locate;

use building::Runtime;
use files::Files;

#[derive(Default)]
pub struct Compiler {
    pub runtime: Runtime,
    pub files: Files,
}
