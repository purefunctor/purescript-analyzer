use smol_str::SmolStr;

_create_ast!(Name);

impl Name {
    pub fn as_str(&self) -> Option<SmolStr> {
        Some(self.node.first_token()?.text().into())
    }
}
