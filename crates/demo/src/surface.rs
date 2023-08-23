//! Queries related to a file as a module.
use std::sync::Arc;

use files::FileId;
use la_arena::Arena;
use rowan::ast::{AstNode, AstPtr};
use smol_str::SmolStr;
use syntax::{ast, PureScript, SyntaxNode, SyntaxNodePtr};

use crate::{
    id::{AstId, CstId, InFile, InFileAstId},
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

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Declaration {
    ValueDeclaration { name: SmolStr },
}

#[salsa::query_group(SurfaceStorage)]
pub trait SurfaceDatabase: SourceDatabase {
    fn declaration_map(&self, file_id: FileId) -> Arc<DeclarationMap>;

    fn lower_declaration(&self, ast_id: InFileAstId<ast::Declaration>) -> Arc<Declaration>;

    fn type_infer_declaration(&self, ast_id: InFileAstId<ast::Declaration>) -> ();
}

fn declaration_map(db: &dyn SurfaceDatabase, file_id: FileId) -> Arc<DeclarationMap> {
    dbg!("Called declaration_map...");
    let node = db.parse_file(file_id).syntax.clone();
    let declaration_map = DeclarationMap::from_source(&node);
    Arc::new(declaration_map)
}

fn lower_declaration(
    db: &dyn SurfaceDatabase,
    ast_id: InFileAstId<ast::Declaration>,
) -> Arc<Declaration> {
    dbg!("Called lower_declaration...");
    let InFile { file_id, value: ast_id } = ast_id;
    let ast_ptr = db.declaration_map(file_id).get(ast_id);

    let root = db.parse_file(file_id).syntax.clone();

    match ast_ptr.to_node(&root) {
        ast::Declaration::AnnotationDeclaration(_) => todo!("AnnotationDeclaration"),
        ast::Declaration::ValueDeclaration(t) => {
            let name = t.name().unwrap().as_str().unwrap();
            Arc::new(Declaration::ValueDeclaration { name })
        }
    }
}

fn type_infer_declaration(db: &dyn SurfaceDatabase, ast_id: InFileAstId<ast::Declaration>) -> () {
    dbg!("Called type_infer_declaration...");
    let _ = db.lower_declaration(ast_id);
    ()
}
