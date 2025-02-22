pub mod core;

use indexing::{FullModuleIndex, TermItem};
use lowering::FullModuleLower;

extern crate core as rust_core;

fn check_module(index: &FullModuleIndex, _: &FullModuleLower) {
    let foreign = index.index.iter_term_item().filter_map(|(item_id, item)| {
        if let TermItem::Foreign { id } = item { Some((item_id, id)) } else { None }
    });

    for (item_id, foreign_id) in foreign {
        dbg!((item_id, foreign_id));
    }
}

#[cfg(test)]
mod tests {
    use indexing::FullModuleIndex;
    use lowering::FullModuleLower;
    use rowan::ast::AstNode;
    use syntax::cst;

    fn check_source(source: &str) -> (cst::Module, FullModuleIndex, FullModuleLower) {
        let lexed = lexing::lex(source);
        let tokens = lexing::layout(&lexed);

        let (module, _) = parsing::parse(&lexed, &tokens);
        let module = cst::Module::cast(module).unwrap();

        let full_module_index = indexing::index_module(&module);
        let full_module_lower = lowering::lower_module(
            &module,
            &full_module_index.index,
            &full_module_index.relational,
            &full_module_index.source,
        );

        let _ = super::check_module(&full_module_index, &full_module_lower);

        (module, full_module_index, full_module_lower)
    }

    #[test]
    fn test_basic() {
        let (_, _, _) = check_source(
            r#"module Main where
foreign import unit :: Unit"#,
        );
    }
}
