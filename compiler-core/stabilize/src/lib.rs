use rowan::WalkEvent;
use syntax::{SyntaxKind, SyntaxNode, map_v2::AstPtrMap};

pub fn stabilize(node: &SyntaxNode) -> AstPtrMap {
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
