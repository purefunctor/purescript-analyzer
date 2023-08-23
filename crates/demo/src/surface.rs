//! Queries related to a file as a module.
use std::sync::Arc;

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

/// A mapping from AST pointers to IDs.
///
/// IDs are derived from the declaration's relative position in the source
/// file, as in 0th, 1st, and 2nd. The idea is that while [`DeclarationMap`]
/// is rebuilt for each change in the source code, the IDs, which are used
/// as keys are not directly invalidated.
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

/// A mapping from names to AST IDs.
///
/// This is primarily used for name resolution within a file. It's sensitive
/// to changes in the source file similar to [`DeclarationMap`], but provides
/// a mapping from names to IDs that are used as keys.
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
}

fn declaration_map(db: &dyn SurfaceDatabase, file_id: FileId) -> Arc<DeclarationMap> {
    let node = db.parse_file(file_id).syntax.clone();
    let declaration_map = DeclarationMap::from_source(&node);
    Arc::new(declaration_map)
}

// FIXME: figure out how to do name resolution for imported values...
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
