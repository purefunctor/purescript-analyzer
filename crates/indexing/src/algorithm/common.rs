use smol_str::{SmolStr, SmolStrBuilder};
use syntax::cst;

pub(crate) fn extract_module_name(cst: &cst::ModuleName) -> Option<SmolStr> {
    let mut buffer = SmolStrBuilder::default();
    if let Some(token) = cst.qualifier().and_then(|cst| cst.text()) {
        buffer.push_str(token.text());
    }

    let token = cst.name_token()?;
    buffer.push_str(token.text());

    Some(buffer.finish())
}
