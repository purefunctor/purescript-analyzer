mod id;
mod map;

pub use id::*;
pub use map::*;

use rowan::WalkEvent;
use syntax::{SyntaxKind, SyntaxNode};

pub fn stabilized(node: &SyntaxNode) -> AstPtrMap {
    let mut ast_ptr_map = AstPtrMap::default();

    for event in node.preorder() {
        if let WalkEvent::Enter(node) = event
            && let kind = node.kind()
            && !matches!(
                kind,
                SyntaxKind::Annotation | SyntaxKind::QualifiedName | SyntaxKind::LabelName
            )
        {
            ast_ptr_map.allocate(&node);
        }
    }

    ast_ptr_map
}
