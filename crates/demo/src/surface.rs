//! Queries related to a file as a module.
use std::{ops::Index, sync::Arc};

use files::FileId;
use la_arena::Arena;
use rowan::ast::{AstNode, AstPtr};
use rustc_hash::FxHashMap;
use smol_str::SmolStr;
use syntax::{ast, PureScript, SyntaxNode, SyntaxNodePtr};

use crate::{
    id::{AstId, CstId, InFileAstId},
    source::SourceDatabase,
};

/// A mapping from AST pointers to stable IDs.
///
/// Stable IDs are derived from the declaration's relative position in
/// the source file, as in 0th, 1st, and 2nd. The idea is that while
/// [`DeclarationMap`] is still recomputed when the source code changes,
/// the values attached to the ID keys remain unaffected.
#[derive(Debug, Default, PartialEq, Eq)]
pub struct DeclarationMap {
    inner: Arena<SyntaxNodePtr>,
}

impl DeclarationMap {
    pub(crate) fn from_source(node: &SyntaxNode) -> DeclarationMap {
        let mut declaration_map = DeclarationMap::default();

        let module: ast::Module = ast::Source::cast(node.clone()).unwrap().child().unwrap();
        for declaration in module.body().unwrap().declarations().unwrap().children() {
            declaration_map.allocate(&declaration.syntax());
        }

        declaration_map
    }

    /// Returns the ID for an AST pointer.
    pub fn lookup<N: AstNode<Language = PureScript>>(&self, ast_ptr: &N) -> AstId<N> {
        let target = SyntaxNodePtr::new(&ast_ptr.syntax());
        self.inner
            .iter()
            .find_map(|(raw, inner)| if &target == inner { Some(AstId::new(raw)) } else { None })
            .unwrap()
    }

    /// Returns the AST pointer for an ID.
    pub fn get<N: AstNode<Language = PureScript>>(&self, ast_id: AstId<N>) -> AstPtr<N> {
        self.inner[ast_id.raw].clone().cast().unwrap()
    }

    fn allocate(&mut self, node: &SyntaxNode) -> CstId {
        self.inner.alloc(SyntaxNodePtr::new(node))
    }
}

#[derive(Debug, Default, PartialEq, Eq)]
pub struct NominalMap {
    annotation_declarations: FxHashMap<SmolStr, InFileAstId<ast::AnnotationDeclaration>>,
    value_declarations: FxHashMap<SmolStr, InFileAstId<ast::ValueDeclaration>>,
}

impl NominalMap {
    pub fn get_annotation(&self, k: &str) -> InFileAstId<ast::AnnotationDeclaration> {
        self.annotation_declarations.get(k).unwrap().clone()
    }

    pub fn get_value(&self, k: &str) -> InFileAstId<ast::ValueDeclaration> {
        self.value_declarations.get(k).unwrap().clone()
    }
}

#[salsa::query_group(SurfaceStorage)]
pub trait SurfaceDatabase: SourceDatabase {
    fn declaration_map(&self, file_id: FileId) -> Arc<DeclarationMap>;

    fn nominal_map(&self, file_id: FileId) -> Arc<NominalMap>;

    // fn lower_declaration(&self, ast_id: InFileAstId<ast::Declaration>) -> Arc<Declaration>;

    // fn type_infer_declaration(&self, ast_id: InFileAstId<ast::Declaration>) -> ();
}

fn declaration_map(db: &dyn SurfaceDatabase, file_id: FileId) -> Arc<DeclarationMap> {
    let node = db.parse_file(file_id).syntax.clone();
    let declaration_map = DeclarationMap::from_source(&node);
    Arc::new(declaration_map)
}

fn nominal_map(db: &dyn SurfaceDatabase, file_id: FileId) -> Arc<NominalMap> {
    let node = db.parse_file(file_id).syntax.clone();
    let module: ast::Module = ast::Source::cast(node).unwrap().child().unwrap();

    let mut nominal_map = NominalMap::default();

    for declaration in module.body().unwrap().declarations().unwrap().children() {
        match declaration {
            ast::Declaration::AnnotationDeclaration(declaration) => {
                let name = declaration.name().unwrap().as_str().unwrap();
                let value = db.declaration_map(file_id).lookup(&declaration).in_file(file_id);
                nominal_map.annotation_declarations.insert(name, value);
            }
            ast::Declaration::ValueDeclaration(declaration) => {
                let name = declaration.name().unwrap().as_str().unwrap();
                let value = db.declaration_map(file_id).lookup(&declaration).in_file(file_id);
                nominal_map.value_declarations.insert(name, value);
            }
        }
    }

    Arc::new(nominal_map)
}

// fn lower_declaration(
//     db: &dyn SurfaceDatabase,
//     ast_id: InFileAstId<ast::Declaration>,
// ) -> Arc<Declaration> {
//     dbg!("Called lower_declaration...");
//     let InFile { file_id, value: ast_id } = ast_id;
//     let ast_ptr = db.declaration_map(file_id).get(ast_id);

//     let root = db.parse_file(file_id).syntax.clone();

//     match ast_ptr.to_node(&root) {
//         ast::Declaration::AnnotationDeclaration(_) => todo!("AnnotationDeclaration"),
//         ast::Declaration::ValueDeclaration(t) => {
//             let name = t.name().unwrap().as_str().unwrap();
//             Arc::new(Declaration::ValueDeclaration { name })
//         }
//     }
// }

// fn type_infer_declaration(db: &dyn SurfaceDatabase, ast_id: InFileAstId<ast::Declaration>) -> () {
//     dbg!("Called type_infer_declaration...");
//     let _ = db.lower_declaration(ast_id);
//     ()
// }

/*

Question:

Should lowering include information about the body as well?
Similarly, what qualifies as the body? In rust-analyzer, the
body pertains to whatever is in the `{}` block for a function
where parameters and the name is treated as the data.

The idea for this seems to be to disconnect the interface
of some node to its implementation. For example, if we change
the signature of the function, anything that depends on it
won't have to repeat lowering for the expression body.

Likewise, other declarations also do not have function bodies,
like data declarations for instance, so they only have data.

For type checking, there's a few queries that we need:
1. type_infer_value

This is simple in that it simply infers the type of a value
declaration. It may call into other type inference declarations
for information.

2. type_infer_data

In the traditional compiler architecture, constructors are
inserted to the environment after they're type checked, but
for our purposes, type checking (and consequently, name resolution)
is performed on-demand.

Inference for constructor then would take the assigned ID for
a constructor, then the query will find the corresponding data
declaration for it among other information like parameters.

Speaking of name resolution, let's imagine a scheme where we're
trying to name-resolve the following declaration during lowering.

a = Just

The idea is that we want to be able to define a structure that
we can index nominally that will return indices to us. Where
DeclarationMap provides a mapping from syntax node pointers to
ast IDs, a NominalMap provides a mapping from names to IDs given
whatever is currently visible.

DeclarationMap is constructed from the source by traversing
the declarations, getting their pointers, and then assigning
them to arenas.

On the other hand, NominalMap is constructed from the source,
traversing the imports and declarations, and assigning the ID
for each name.

// Returns an ID that we can use for type inference, among other things...
db.nominal_map(file_id).lookup(None, Nominal::Constructor("Just".into()))

db.nominal_map(file_id).lookup(Some("Data.Maybe".into()), Nominal::Constructor("Just".into()))

Note that the NominalMap also does resolution based on the
current set of imports by analyzing them, calling into the
nominal maps of other files, and then performing an unqualified
lookup.

*/
