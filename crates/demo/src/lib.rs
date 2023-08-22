pub mod id;
pub mod source;
pub mod surface;

#[derive(Default)]
#[salsa::database(source::SourceStorage, surface::SurfaceStorage)]
pub struct RootDatabase {
    storage: salsa::Storage<RootDatabase>,
}

impl salsa::Database for RootDatabase {}

// mod declaration_map;
// mod module_map;
// mod surface_ast;

// mod source;

// use std::sync::Arc;

// use declaration_map::DeclarationMap;
// use files::FileId;
// use itertools::Itertools;
// use module_map::ModuleId;
// use parsing::{error::ParseError, parse_module};
// use rowan::ast::AstNode;
// use syntax::{ast, SyntaxNode};

// #[salsa::query_group(SourceDatabase)]
// trait Source {
//     #[salsa::input]
//     fn file_contents(&self, file_id: FileId) -> Arc<str>;

//     // NOTE: rather than associating FileIds to ModuleIds
//     // individually, we could pass a graph here that keeps
//     // track of this information.
//     #[salsa::input]
//     fn file_module_id(&self, file_id: FileId) -> ModuleId;

//     #[salsa::invoke(file_syntax_query)]
//     fn file_syntax(&self, file_id: FileId) -> (SyntaxNode, Arc<Vec<ParseError>>);

//     #[salsa::invoke(file_module_name_query)]
//     fn file_module_name(&self, file_id: FileId) -> Option<Arc<str>>;

//     #[salsa::invoke(file_declaration_map_query)]
//     fn file_declaration_map(&self, file_id: FileId) -> Arc<DeclarationMap>;
// }

// fn file_syntax_query(db: &dyn Source, file_id: FileId) -> (SyntaxNode, Arc<Vec<ParseError>>) {
//     let contents = db.file_contents(file_id);
//     let (node, errors) = parse_module(&contents);
//     (node, Arc::new(errors))
// }

// fn file_module_name_query(db: &dyn Source, file_id: FileId) -> Option<Arc<str>> {
//     let (node, _) = db.file_syntax(file_id);
//     let module: ast::Module = ast::Source::cast(node)?.child()?;
//     let name = module.header()?.name()?.children().flat_map(|name| name.as_str()).join(".");
//     Some(Arc::from(name))
// }

// fn file_declaration_map_query(db: &dyn Source, file_id: FileId) -> Arc<DeclarationMap> {
//     let (node, _) = db.file_syntax(file_id);
//     Arc::new(DeclarationMap::from_source(&node))
// }

// #[salsa::database(SourceDatabase)]
// #[derive(Default)]
// pub struct SourceImpl {
//     storage: salsa::Storage<SourceImpl>,
// }

// impl salsa::Database for SourceImpl {}

// #[cfg(test)]
// mod tests {
//     use std::sync::Arc;

//     use files::{ChangedFile, Files};
//     use rowan::{ast::AstNode, TextRange, TextSize};
//     use syntax::{ast, SyntaxKind};

//     use crate::{
//         module_map::ModuleMap,
//         surface_ast::{Surface, SurfaceImpl},
//         Source, SourceImpl,
//     };

//     #[test]
//     fn server_loop() {
//         let mut db = SourceImpl::default();

//         let mut files = Files::default();
//         let mut module_map = ModuleMap::default();

//         files.set_file_contents(
//             "./Main.purs".into(),
//             Some(
//                 "module Hello.World where
// a = [0, 1, 2]"
//                     .into(),
//             ),
//         );
//         if files.has_changes() {
//             for ChangedFile { file_id, .. } in files.take_changes() {
//                 let contents = std::str::from_utf8(files.file_contents(file_id)).unwrap();
//                 db.set_file_contents(file_id, Arc::from(contents));

//                 if let Some(module_name) = db.file_module_name(file_id) {
//                     let module_id = module_map.allocate(module_name);
//                     db.set_file_module_id(file_id, module_id);
//                 }
//             }
//         }

//         let file_id = files.file_id("./Main.purs".into()).unwrap();

//         let (node, _) = db.file_syntax(file_id);
//         let module: ast::Module = ast::Source::cast(node).unwrap().child().unwrap();

//         // To obtain a declaration ID, we can query for the file's declaration map
//         // and then lookup using the syntax pointer that we have. This syntax pointer
//         // may also come from a traversal e.g. given an offset.
//         let declaration_id = module
//             .body()
//             .unwrap()
//             .declarations()
//             .unwrap()
//             .children()
//             .find_map(|t| match t {
//                 ast::Declaration::AnnotationDeclaration(_) => None,
//                 ast::Declaration::ValueDeclaration(t) => {
//                     Some(db.file_declaration_map(file_id).find(&t))
//                 }
//             })
//             .unwrap();

//         // We then proceed to create an instance of `SurfaceImpl`, which we'll use
//         // for lowering alongside type checking. We can think of this as an ephemeral
//         // database that exists for each file.
//         let mut surface_db = SurfaceImpl::default();
//         let (node, _) = db.file_syntax(file_id);
//         let declaration_map = db.file_declaration_map(file_id);
//         surface_db.set_syntax_and_declaration_map((node, declaration_map));

//         // We can use the surface to perform operations such as lowering:
//         let declaration_data = surface_db.lower_declaration(declaration_id);
//         dbg!(&declaration_data);

//         // and type inference:
//         let inference_result = surface_db.infer_declaration(declaration_id);
//         dbg!(&inference_result);

//         // With this information, we can recover inference coming from a source offset:
//         let start = TextSize::new(30);
//         let end = TextSize::new(31);
//         let (node, _) = db.file_syntax(file_id);

//         let hover_element = node.covering_element(TextRange::new(start, end));

//         match hover_element.kind() {
//             SyntaxKind::LiteralInteger => {
//                 let expr_ptr = &hover_element.parent().unwrap();
//                 let expr_id = declaration_data.source_map.get_expr_id(expr_ptr).unwrap();
//                 let expr_ty = &inference_result[expr_id];
//                 println!("{:?} has type {:?}", expr_ptr, expr_ty);
//             }
//             _ => (),
//         }
//     }
// }

/*

TODO:

type_infer(declaration_id) -> InferenceResult

lower_plain(declaration_id) -> (Arena<Expr>, ExprId)
lower_source(declaration_id) -> (Arena<Expr>, ExprId, SourceMap)

DeclarationMap

*/

/*

Lowering:

Before performning any semantic analysis, lowering has to be performed first
on the syntax. One of the steps in lowering is name resolution, where we make
it so that name refs resolve to what they're actually talking about. Note that
this step has to be resilient to changes as well, such that we do not repeat
computations.

We usually do name resolution by performing a traversal of the syntax tree.
Let's say that we want our top-level value declarations to be assigned with
IDs:

a = ...  -- ID 0
b = ...  -- ID 1
c = ...  -- ID 2

More generally, when a name is introduced, we assign it an ever-incrementing
ID.

Recursing into the declaration bodies, we may see references to other names:

a = b + b
b = ...
c = ...

During name resolution, we add more information saying that `b` _actually_
points to ID 1.

We may also see introductions that perform shadowing:

a = let b = 0 in b + b
b = ...
c = ...

In this case, `b` in `b + b` should resolve to the inner `b` instead. One
way to accomplish this is to resume ID assignment when recursing into definitions,
which gives us ID 4 for the inner `b`.

Assuming that we're doing name resolution as a single pass, this approach works
quite well. However, since we're incrementally editing the file, declarations
may come and go, and in order to reuse information, we now treat the set of
declarations not as in input to the resolver but as part of its state.

Take for example adding a declaration:

a = ...
b = ...
c = ...
+ d = ...

We've already assigned IDs for `a`, `b`, and `c`. Since we're adding a new
declaration, `d`, we have to assign an ID for it as well. We also have to
think about this in an inverted manner, for example, let's think of some LSP
service that relies on the declaration body like type-on-hover.

Type-on-hover would want to query the syntax node which the cursor is pointing
to. Internally, a query to locate the corresponding HIR item is performed. Then,
a query to determine what it actually points to is performed. Then, type checking
is performed from the bottom-up.

Alternative, type-on-hover would want to query the syntax node which the cursor
is pointing to, then internally, a query is performed to locate the corresponding
declaration for that node, then a query is performed to collect type information
for the entire subtree, before a search is performed. This way, type checking
is still performed top-down despite starting from the bottom.

One thing that this setup entails is that name resolution can effectively become
lazy to a certain extent. Specifically, since we can do top-down type checking
starting from the declaration level, we only need an ephemeral local environment
for each, in that while we still have the whole setup with binding introductions
and usages, they're only performed if the entire declaration has to be rechecked.

*/

/*

Type Checking:

The idea w/ type checking is that we do it at the declaration-level, and we generate
information in such a way that we can refer back to it simply by using CST position
information. For instance, checking the type of:

a = 1

Would generate a map that says that the `1` in this declaration has a type of `Int`.
Internally of course, the type checking implementation remains the same, just that
alongside error reporting, we _also_ do information reporting.

An idea that we can use is a bidirectional source map, that associates CST pointers
and lowered nodes together. How we do this is by associating _each_ expression with
an identifier, and mapping CST pointers to those identifiers and vice versa. For
example, something like:

a = 1 + 2

Would generate the IDs 0, 1, 2 for `1`, `2`, and `1 + 2` respectively, which we can
then map to the CST pointers. When somewhere higher up in the hierarchy asks about
type information for a specific CST pointer, we can use this source map to then
get the expression identifier, and to use it to get type information.

*/

/*

Resilience:

Given that we save type information on the declaration level, the resilience for
type checking also sits at that point. What we mean by this is that as long the
declarations are not changed, the type checking results are cached. In the case
of another declaration being added at the end, we still retain type information
for all previous declarations since they point to the same IDs, and those same IDs
still evaluate to the same information, and those same information as still computed
from the same set of syntax nodes.

Likewise, separating position information from the expressions being operated on
by the type checking, as in using a lowered representation with associated IDs
means that if declarations get shifted with whitespace, the source map gets changed,
but the IDs being generated for those expressions remain the same. What this means,
effecitvely, is that we could extract the type checker as its own query group?

let
  a = 0
in
  a + a

0: let
1: 0
2: a + a
3: a
4: a

let
  a = 0
  b = 1
in
  a + a

0: let
1: 0
2: 1
3: a + a
4: a
5: a

lower(declaration_id) -> (Arena<Ts>, Lowered<Ts>)

lower_with_source_map(declaration_id) -> (Arena<Ts>, SourceMap<CST, T>, Lowered<Ts>)

-- Type inference doesn't care about positional information, so in this way, we can be
-- insensitive to positional changes such as adding whitespace before a declaration.
type_infer(declaration_id) -> InferenceResult
    lower(declaration_id)

Another problem with type inference is name resolution:

a = b  -- declaration_id: 0
       -- expression_id: b, 0

For local bindings, this is fine as they're entirely localized to the expression being
checked. Global bindings on the other hand would be checked a bit differently. Bindings
can be introduced in two different ways, local and imported declarations.

One idea here is to make it so that during lowering, we resolve names such that we only
perform lookups to the appropriate modules later. What this means is that if I had the
set of declarations:

a = b
b = 0

I would lower it into:

a = Module.b
b = 0

Where `Module` and `b` hold information about the module ID and declaration ID being
referred to. This can then be used to query the type information for checking. This
approach ensures that qualification information is inserted to all names.

The alternative approach is to perform resolution dynamically, during type checking.
If a local search fails, a global search entails. If the name is unqualified, declarations
in the current module are searched first, then declarations based on whatever is currently
imported. If the name is qualified, then it's a direct search to the source.

*/
