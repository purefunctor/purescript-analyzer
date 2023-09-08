//! See documentation for [`NominalMap`].

use std::sync::Arc;

use files::FileId;
use rowan::ast::AstNode;
use rustc_hash::FxHashMap;
use smol_str::SmolStr;
use syntax::ast;

use crate::{
    id::{AstId, InFile},
    ResolverDatabase,
};

#[derive(Debug, Default, PartialEq, Eq)]
pub struct NominalMap {
    annotation: FxHashMap<SmolStr, InFile<AstId<ast::AnnotationDeclaration>>>,
    value: FxHashMap<SmolStr, Vec<InFile<AstId<ast::ValueDeclaration>>>>,
}

impl NominalMap {
    pub(crate) fn nominal_map_query(db: &dyn ResolverDatabase, file_id: FileId) -> Arc<NominalMap> {
        let mut nominal_map = NominalMap::default();

        let node = db.parse_file(file_id);
        let declarations = ast::Source::<ast::Module>::cast(node)
            .and_then(|source| Some(source.child()?.body()?.declarations()?.children()));
        if let Some(declarations) = declarations {
            for declaration in declarations {
                nominal_map.collect_declaration(db, file_id, &declaration);
            }
        }

        Arc::new(nominal_map)
    }

    fn collect_declaration(
        &mut self,
        db: &dyn ResolverDatabase,
        file_id: FileId,
        declaration: &ast::Declaration,
    ) -> Option<()> {
        match declaration {
            ast::Declaration::AnnotationDeclaration(annotation) => {
                let name = annotation.name()?.as_str()?;
                let id = db.positional_map(file_id).ast_id(annotation).in_file(file_id);
                self.annotation.insert(name, id);
            }
            ast::Declaration::ValueDeclaration(value) => {
                let name = value.name()?.as_str()?;
                let id = db.positional_map(file_id).ast_id(value).in_file(file_id);
                self.value.entry(name).or_default().push(id);
            }
        }

        Some(())
    }

    pub fn get_annotation(&self, name: &str) -> Option<InFile<AstId<ast::AnnotationDeclaration>>> {
        self.annotation.get(name).copied()
    }

    pub fn get_value(&self, name: &str) -> Option<&[InFile<AstId<ast::ValueDeclaration>>]> {
        self.value.get(name).map(Vec::as_slice)
    }
}

/*

Idea: the nominal map is used to fasttrack the resolution of regular names
to AST nodes. In conjunction with the positional map, type inference can then
go from a name to an AST pointer, an AST ID, and finally, an inferred type.

At the moment, the nominal map only stores information about local declarations,
but what about imported ones, how do we deal with resolving them? For example,
if I had the following source file:

```
import Data.Maybe

a = Just 0
```

For only exposing local declarations, just using the name is perfectly fine
for queries, since it's a simple map lookup. But now, we have to take into
account the fact that we also have to resolve imports. For example:

```
a = M.Just 0
```

The intent here is simple:
1. Figure out what module `M` is in the context of the current source file.
2. Use the information obtained in 1. and the name `Just` to `get` on the FxHashMap

This requires that the nominal map:
1. Also read import declarations to know how to resolve `as` qualifications
2. Use data from other nominal maps to merge with

Tihs architecture assumes the following structure:

FxHashMap<ModuleName, ModuleName>,  // `as` to the actual name
FxHashMap<ModuleId, FxHashMap<Name, ...>>

Where if I had an import like:
```
import Data.Maybe (fromMaybe)
```

I would take `Data.Maybe`'s nominal map, take the items I want from it,
and then insert them to the current module's nominal map. This is inefficient,
however. We already cache the nominal maps, and taking entries from other
nominal maps and duplicating them to the local one is inefficient.

So going back, how would we resolve the following:

```
a = M.Just 0
```

The intent here is clear:
1. Figure out what module `M` is in the context of the current source file.
   The nominal map can provide this and can return a module ID or not.

2. Get the nominal map for the module ID, by first consulting the module
   map and then creating a nominal map.

3. Search the nominal map for the module for that specific declaration,
   which should finally yield an AST ID.

How about for the following:

```
a = Just 0
```

The intent here is less clear, but:
1. First, we want to search the local declarations, so we consult the
   nominal map if we can find the "Just" in the local scope.

   a. If we do, then break off.

2. Then, we want to search the current set of imports. But how can the
   nominal map provide this? As in, what information should we generate
   during the creation of the nominal map such that we can search unqualified,
   and imported names?

   One such option would just be to take note of all declarations that
   do not have qualification, like `Data.Maybe`. Then, those imports
   would be iterated through to find the "Just". This is a perfectly
   fine solution, as it does not make the nominal map more or less
   sensitive than it already is (it depends on the source file) after all.

   Another option would be to go through all imports and collect whatever names
   have been imported without qualification, then create a mapping that relates
   names to module IDs. Given the module ID for a name, we know that the nominal
   map for that module _has_ the name that we want.

   The second option is a bit too janky, in my opinion, in that it doesn't scale
   well for too many open imports. Suddenly, your re-export file starts chugging
   because it spends a lot of time iterating over each import for each keystroke.

    There's some extra computation in the first option, in the form of a effectively
    a linear search. The tricky part is that we have no caching mechanism for this
    process, so each occurence of this name is a recomputation. What else is missing?

Let's think about one part where we do extensive name resolution, value declarations.
Given the constraint that I've just mentioned for the first option, what do you think
the behaviour here should be?

```
a = [ Just 0, Just 1 ]
```

I think there's a chance for optimization here, specifically, it has to do with the
idea of scopes. It's more noticeable once you introduce let-bindings, like `where`:

```
a = let just = Just in [ just 0, just 1 ]
```

The point I'm trying to make here is that there's a concrete hierarcy in which names
gets introduced in a top-down manner. Think of it this way, the expression in the
first example does not introduce any new names, and so you could say that the expressions
within that declaration all share the same scope.

On the second value declaration, a local scope is introduced, but that local scope is
_also_ shared within the sub-expression. This sharing of the scopes means that there's
an opportunity for caching.

*/
