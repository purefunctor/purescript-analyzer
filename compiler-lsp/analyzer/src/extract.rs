use rowan::TextRange;
use syntax::{SyntaxKind, SyntaxNode, SyntaxNodePtr};

#[derive(Debug, Default)]
pub struct AnnotationSyntaxRange {
    pub annotation: Option<TextRange>,
    pub syntax: Option<TextRange>,
}

impl AnnotationSyntaxRange {
    pub fn from_ptr(root: &SyntaxNode, ptr: &SyntaxNodePtr) -> AnnotationSyntaxRange {
        ptr.try_to_node(root)
            .map(|node| AnnotationSyntaxRange::from_node(&node))
            .unwrap_or_default()
    }

    pub fn from_node(node: &SyntaxNode) -> AnnotationSyntaxRange {
        let mut children = node.children_with_tokens().peekable();

        let annotation = children.next_if(|child| {
            let kind = child.kind();
            matches!(kind, SyntaxKind::Annotation)
        });

        let start = children.peek().map(|child| child.text_range());
        let end = children.last().map(|child| child.text_range());

        let annotation = annotation.map(|child| child.text_range());
        let syntax = start.zip(end).map(|(start, end)| start.cover(end));

        AnnotationSyntaxRange { annotation, syntax }
    }
}

pub fn extract_annotation(root: &SyntaxNode, range: TextRange) -> String {
    let text = root.text().slice(range);

    let mut annotation = String::default();

    text.for_each_chunk(|chunk| {
        let lines = chunk.lines().filter_map(|line| {
            let trimmed = line.trim_start_matches("-- |");
            if line != trimmed { Some(trimmed.trim_matches(' ')) } else { None }
        });

        let mut lines = lines.peekable();
        if let Some(line) = lines.next() {
            annotation.push_str(line);
        }

        lines.for_each(|line| {
            annotation.push('\n');
            annotation.push_str(line);
        });
    });

    annotation
}

pub fn extract_syntax(root: &SyntaxNode, range: TextRange) -> String {
    root.text().slice(range).to_string()
}
