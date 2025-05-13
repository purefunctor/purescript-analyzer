use indexing::FullIndexedModule;
use lowering::FullLoweredModule;
use syntax::cst;

pub fn lower_source(source: &str) -> (cst::Module, FullIndexedModule, FullLoweredModule) {
    let lexed = lexing::lex(source);
    let tokens = lexing::layout(&lexed);

    let (parsed, _) = parsing::parse(&lexed, &tokens);
    let module = parsed.cst();

    let indexed = indexing::index_module(&module);
    let lowered = lowering::lower_module(&module, &indexed);

    (module, indexed, lowered)
}
