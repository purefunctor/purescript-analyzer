use indexing::FullModuleIndex;
use lowering::{Graph, Intermediate, LoweringSource};
use rowan::ast::AstNode;
use syntax::cst;

pub fn lower_source(
    source: &str,
) -> (cst::Module, FullModuleIndex, Intermediate, LoweringSource, Graph) {
    let lexed = lexing::lex(source);
    let tokens = lexing::layout(&lexed);

    let (module, _) = parsing::parse(&lexed, &tokens);
    let module = cst::Module::cast(module).unwrap();

    let full_module_index = indexing::index_module(&module);
    let (ir, source, graph) = lowering::lower_module(
        &module,
        &full_module_index.index,
        &full_module_index.relational,
        &full_module_index.source,
    );

    (module, full_module_index, ir, source, graph)
}
