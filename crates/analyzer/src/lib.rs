//! The core of the analyzer.

pub mod id;
pub mod infer;
pub mod lower;
pub mod names;
pub mod resolver;
pub mod scope;
pub mod source;

use std::hash::BuildHasherDefault;

use indexmap::{IndexMap, IndexSet};
pub use lower::LowerDatabase;
pub use resolver::ResolverDatabase;
use rustc_hash::FxHasher;
pub use scope::ScopeDatabase;
pub use source::SourceDatabase;

/// The analyzer's core database.
#[derive(Default)]
#[salsa::database(
    infer::InferStorage,
    resolver::ResolverStorage,
    lower::LowerStorage,
    scope::ScopeStorage,
    source::SourceStorage
)]
pub struct RootDatabase {
    storage: salsa::Storage<RootDatabase>,
}

impl salsa::Database for RootDatabase {}

pub trait Upcast<T: ?Sized> {
    fn upcast(&self) -> &T;
}

impl Upcast<dyn ResolverDatabase> for RootDatabase {
    fn upcast(&self) -> &(dyn ResolverDatabase + 'static) {
        self
    }
}

pub(crate) type FxIndexSet<T> = IndexSet<T, BuildHasherDefault<FxHasher>>;
pub(crate) type FxIndexMap<K, V> = IndexMap<K, V, BuildHasherDefault<FxHasher>>;

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use files::{ChangedFile, Files};
    use salsa::Durability;

    use crate::{
        infer::InferDatabase, LowerDatabase, ResolverDatabase, RootDatabase, SourceDatabase,
    };

    #[test]
    fn api() {
        // Initialization Code
        let mut db = RootDatabase::default();
        let mut files = Files::default();

        // Given the source file glob, we take all purs files and load them onto the file system.
        // files.set_file_contents(
        //     "./Main.purs".into(),
        //     Some("module Main where\n\nimport Hello\n\nmain = hello".into()),
        // );
        // files.set_file_contents(
        //     "./Hello.purs".into(),
        //     Some("module Hello (hello) where\n\nhello = 0".into()),
        // );
        files.set_file_contents(
            "./Main.purs".into(),
            Some(
                "
module Main where

hello =
  let
    int = 21
    number = 21.0
    char = 'f'
  in
    [int, number, char, string, boolean]
  where
    string = \"I am a string.\"
    boolean = false
"
                .into(),
            ),
        );
        // Then, we feed it to the database through the `take_changes` method.
        for ChangedFile { file_id, .. } in files.take_changes() {
            let contents = files.file_contents(file_id);
            db.set_file_contents(file_id, Arc::from(std::str::from_utf8(contents).unwrap()));
        }
        // Finally, we provide the file paths to the database for use by queries like `module_map`.
        // Note that we're setting the durability to medium as we don't expect file paths to change
        // as often as something like editing a file would with the file contents.
        db.set_file_paths_with_durability(files.iter().collect(), Durability::MEDIUM);

        let file_id = files.file_id("./Main.purs".into()).unwrap();
        let hello_id = db.nominal_map(file_id).get_value("hello").unwrap()[0];
        db.lower_value_declaration(hello_id);
        db.infer_value_declaration(hello_id);
    }
}

/*

Self-recursion:

Name bindings can be self-recursive, provided that they're thunked:

a = a       # invalid, fails at compile-time.
f _ = f {}  # valid, fails at runtime.

However, pattern bindings cannot be self-recursive:

{ a } = f a

Mutual recursion:

Name bindings can be self-recursive, provided that they're thunked:

# Invalid

a = b
b = a

# Valid

f _ = g {}
g _ = f {}

Pattern bindings cannot be mutually recursive.

Algorithm:

All let-bound names are available within the let-body,
however, the available let-bound names for a let-binding
is restricted by certain rules.

Due to purity, evaluation order for let-bindings may not
necessarily come from the order in which they appear in
source code. Let-bindings can be reordered or floated by
an optimizing compiler if it sees it fit.

Draft #1

Given a name binding, we always make it visible to itself,
at least initially. During the traversal, we can check if
a variable is a valid use of itself. For instance:

a = a           -- no!
a _ = a {}      -- yes!
a = \_ -> a {}  -- kinda?

The tricky part is the special casing for the "thunked"
contexts, but here's my idea for both: certain ScopeKinds
introduce "thunking", for example, the presence of `Binders`
implies that an expression is behind a function application.

The idea is for the scope linked list to look like this
for the following examples:

a = a
LetBound({ "a" })

a _ = a {}
LetBound({ "a" })
Binders({ })

a = \_ -> a {}
LetBound({ "a" })
Binders({ })

Notice how the `a` does not immediately resolve to `LetBound`
because of the `Binders` that were introduced by the function
abstraction. For self-recursion, this is enough information
to know if the use of a name is valid or not.

Moving on to mutually recursive bindings, take the following
examples:

f = g  -- No!
g = f

f _ = g {}  -- Yes!
g _ = f {}

f = g
g _ = f {}  -- No!

For the first example, their scopes would look like this:

f = g
LetBound({ "f", "g" })

g = f
LetBound({ "f", "g" })  -- Shared with `f`

Currently, `LetBound` does not encode information about binding
groups which can be used to determine if a name is valid within
the bound value for another name. If it had information about
which binding group a name belongs to, then the check would
look like this:

For the declaration for `f`, if a name like `g` resolves
immediately through a `LetBound`, and the binding group
for `g` is the same as `f`, then it's an invalid reference.
This check would allow for the following to still work:

f _ = g {}  -- f = \_ -> g {}
g _ = f {}  -- g = \_ -> f {}

For cases such as:

f = g
LetBound({ "f", "g" })

g _ = f {}
LetBound({ "f", "g" })
Binders({ })

`g` resolves immediately to `LetBound`, and it exists
within the same binding group as `f`, so it's an invalid
use.

`f` does not immediately resolve to `LetBound` because
of thunking with `Binders`, so even if it's in the same
binding group as `g`.

*/
