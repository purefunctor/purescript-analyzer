//! Queries related to a file as a module.

use std::sync::Arc;

use files::FileId;
use la_arena::Arena;
use rowan::ast::{AstNode, AstPtr};
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

#[salsa::query_group(SurfaceStorage)]
pub trait SurfaceDatabase: SourceDatabase {
    fn declaration_map(&self, file_id: FileId) -> Arc<DeclarationMap>;

    fn lower_declaration(&self, declaration_id: InFileAstId<ast::Declaration>) -> ();
}

fn declaration_map(db: &dyn SurfaceDatabase, file_id: FileId) -> Arc<DeclarationMap> {
    let parse_result = db.parse_file(file_id);
    Arc::new(DeclarationMap::from_source(&parse_result.syntax))
}

fn lower_declaration(
    db: &dyn SurfaceDatabase,
    declaration_id: InFileAstId<ast::Declaration>,
) -> () {
    let parse_result = db.parse_file(declaration_id.file_id);
    let ast_ptr = declaration_id.ast_ptr(db);
    let declaration = ast_ptr.to_node(&parse_result.syntax);

    match declaration {
        ast::Declaration::AnnotationDeclaration(declaration) => {
            dbg!(declaration);
        }
        ast::Declaration::ValueDeclaration(declaration) => {
            dbg!(declaration);
        }
    }
}
