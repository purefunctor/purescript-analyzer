use lsp::server::Backend;
use test_each_file::test_each_file;
use tokio::runtime::Runtime; // 0.3.5
use tower_lsp::lsp_types::{
    GotoDefinitionParams, GotoDefinitionResponse, Location, Range, TextDocumentIdentifier,
    TextDocumentPositionParams, Url,
};
use tower_lsp::LanguageServer;
use tower_lsp::{lsp_types::Position, LspService};

#[derive(Debug)]
struct GotoDefitionTest {
    input: Position,
    output: Option<Range>,
}

fn parse_pos(content: &str) -> Position {
    match content.trim().split_once(":") {
        Some((line, character)) => {
            let line = line.parse().unwrap();
            let character = character.parse().unwrap();
            Position { line, character }
        }
        None => panic!("Invalid position format {}", content),
    }
}

fn parse_range(content: &str) -> Range {
    match content.trim().split_once("-") {
        Some((start, end)) => {
            let start = parse_pos(start);
            let end = parse_pos(end);
            Range { start, end }
        }
        None => panic!("Invalid range format {}", content),
    }
}

fn parse_tests(content: &str) -> Vec<GotoDefitionTest> {
    let mut tests = Vec::new();
    let mut lines = content.lines();
    while let Some(line) = lines.next() {
        match line.strip_prefix("-- TEST") {
            Some(test) => match test.split_once("=>") {
                Some((input, output)) => {
                    let input = parse_pos(input);
                    let output = if output.trim() == "None" {
                        None
                    } else {
                        let output = parse_range(output);
                        Some(output)
                    };
                    tests.push(GotoDefitionTest { input, output });
                }
                None => panic!("Invalid test format {}", test),
            },
            None => (),
        }
    }
    tests
}

test_each_file! { in "crates/lsp/tests/fixtures/goto_definition" as goto_definition => |content: &str|  {

  let tests = parse_tests(content);

  for test in tests {
    let uri : Url = "file:///test".try_into().unwrap();
    let params = GotoDefinitionParams {
        text_document_position_params: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier {
                uri: uri.clone(),
            },
            position: test.input,
        },
        work_done_progress_params: Default::default(),
        partial_result_params: Default::default(),
    };

    let (service, _socket) = LspService::build(Backend::new).finish();
    let backend = service.inner();
    let result = Runtime::new().unwrap().block_on(build_and_goto_def(backend, params, content));
    let expected = test.output.map(|range| GotoDefinitionResponse::Scalar(Location { uri, range }));

    assert_eq!(result, expected);
 };

}}

async fn build_and_goto_def(
    backend: &Backend,
    params: GotoDefinitionParams,
    content: &str,
) -> Option<GotoDefinitionResponse> {
    let uri = params.text_document_position_params.text_document.uri.clone();
    backend.analyze(1, uri, content).await;
    let result = backend.goto_definition(params);
    result.await.unwrap()
}
