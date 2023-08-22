//! Queries related to a file as a module.
use std::{ops::Index, sync::Arc};

use files::FileId;
use la_arena::{Arena, Idx};
use rowan::ast::{AstNode, AstPtr};
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

#[derive(Debug, Default, PartialEq, Eq, Hash)]
struct DeclarationData {
    value: Arena<ValueDeclarationData>,
}

#[derive(Debug, Default, PartialEq, Eq, Hash)]
pub struct ValueDeclarationData {
    name: SmolStr,
}

impl Index<Idx<ValueDeclarationData>> for ModuleSurface {
    type Output = ValueDeclarationData;

    fn index(&self, index: Idx<ValueDeclarationData>) -> &Self::Output {
        &self.inner.value[index]
    }
}

#[derive(Debug, Default, PartialEq, Eq)]
pub struct ModuleSurface {
    items: Vec<ValueDeclarationId>,
    inner: DeclarationData,
}

impl ModuleSurface {
    fn module_surface_query(db: &dyn SurfaceDatabase, file_id: FileId) -> Arc<ModuleSurface> {
        let node = db.parse_file(file_id).syntax.clone();
        let module: ast::Module = ast::Source::cast(node).unwrap().child().unwrap();

        let mut inner = DeclarationData::default();
        let mut items = vec![];

        for declaration in module.body().unwrap().declarations().unwrap().children() {
            match declaration {
                ast::Declaration::AnnotationDeclaration(_) => (),
                ast::Declaration::ValueDeclaration(t) => {
                    let data = ValueDeclarationData { name: t.name().unwrap().as_str().unwrap() };
                    let ast_id = db.declaration_map(file_id).lookup(&t).in_file(file_id);
                    let lower_id = inner.value.alloc(data);
                    items.push(
                        db.intern_value_declaration(ValueDeclarationLocation { ast_id, lower_id }),
                    );
                }
            }
        }

        Arc::new(ModuleSurface { inner, items })
    }

    pub fn value_declarations(&self) -> &[ValueDeclarationId] {
        &self.items
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ValueDeclarationLocation {
    pub ast_id: InFileAstId<ast::ValueDeclaration>,
    pub lower_id: Idx<ValueDeclarationData>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ValueDeclarationId(salsa::InternId);

impl salsa::InternKey for ValueDeclarationId {
    fn from_intern_id(v: salsa::InternId) -> Self {
        ValueDeclarationId(v)
    }

    fn as_intern_id(&self) -> salsa::InternId {
        self.0
    }
}

#[salsa::query_group(SurfaceStorage)]
pub trait SurfaceDatabase: SourceDatabase {
    fn declaration_map(&self, file_id: FileId) -> Arc<DeclarationMap>;

    #[salsa::invoke(ModuleSurface::module_surface_query)]
    fn module_surface(&self, file_id: FileId) -> Arc<ModuleSurface>;

    #[salsa::interned]
    fn intern_value_declaration(&self, location: ValueDeclarationLocation) -> ValueDeclarationId;
}

fn declaration_map(db: &dyn SurfaceDatabase, file_id: FileId) -> Arc<DeclarationMap> {
    let parse_result = db.parse_file(file_id);
    Arc::new(DeclarationMap::from_source(&parse_result.syntax))
}

/*

Lowering code is invoked as many times as there are changes
in the source file, much like with the declaration map. The
idea is that other operations should be insensitive to syntax
changes.

DeclarationMap encapsulates this sort of behaviour already,
in that it gets recomputed each time the syntax changes, but
the declaration IDs that it yields remain the same. This allows
us to use IDs to refer to syntax that changes all the time.

IDs are out of sync of the syntax, and the DeclarationMap is
in sync with it all the time.

What we want is out of sync IDs for stuff like declaration
information, while the declarations themselves stay in sync
with the syntax.

InFileAstId<N> -> ValueDeclarationId

*/
