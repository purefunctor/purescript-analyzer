use indexing::FullIndexedModule;
use lowering::FullLoweredModule;
use rowan::ast::AstNode;
use syntax::cst;

pub fn lower_source(source: &str) -> (cst::Module, FullIndexedModule, FullLoweredModule) {
    let lexed = lexing::lex(source);
    let tokens = lexing::layout(&lexed);

    let (module, _) = parsing::parse(&lexed, &tokens);
    let module = cst::Module::cast(module).unwrap();

    let indexed = indexing::index_module(&module);
    let lowered = lowering::lower_module(&module, &indexed);

    (module, indexed, lowered)
}
