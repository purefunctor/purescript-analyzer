pub mod module_name_map;
pub mod runtime;

pub use module_name_map::*;
pub use runtime::*;

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use files::Files;

    use super::Runtime;

    #[test]
    fn test_basic() {
        let mut runtime = Runtime::default();
        let mut files = Files::default();

        let id = files.insert("./src/Main.purs", "module Main where\n\nlife = 42");
        let content = files.content(id);

        runtime.set_content(id, content);
        let index_a = runtime.indexed(id);
        let index_b = runtime.indexed(id);
        assert!(Arc::ptr_eq(&index_a, &index_b));

        let lower_a = runtime.lowered(id);
        let lower_b = runtime.lowered(id);
        assert!(Arc::ptr_eq(&lower_a, &lower_b));

        runtime.set_content(id, "module Main where\n\n\n\nlife   =   42".into());
        let index_a = runtime.indexed(id);
        let index_b = runtime.indexed(id);
        assert!(Arc::ptr_eq(&index_a, &index_b));

        let lower_a = runtime.lowered(id);
        let lower_b = runtime.lowered(id);
        assert!(Arc::ptr_eq(&lower_a, &lower_b));
    }
}
