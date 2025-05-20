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
    fn test_ptr_eq_same_revision() {
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

        let id = files.insert("./src/Main.purs", "module Main where\n\nlife = 42");
        let content = files.content(id);

        runtime.set_content(id, content);
        let index_a = runtime.indexed(id);
        let index_b = runtime.indexed(id);
        assert!(Arc::ptr_eq(&index_a, &index_b));

        let lower_a = runtime.lowered(id);
        let lower_b = runtime.lowered(id);
        assert!(Arc::ptr_eq(&lower_a, &lower_b));
    }

    #[test]
    fn test_ptr_eq_across_revision() {
        let mut runtime = Runtime::default();
        let mut files = Files::default();

        let id = files.insert("./src/Main.purs", "module Main where\n\nlife = 42");
        let content = files.content(id);

        runtime.set_content(id, content);
        let indexed_a = runtime.indexed(id);

        let id = files.insert("./src/Main.purs", "module Main where\n\nlife = 42\n\n");
        let content = files.content(id);

        runtime.set_content(id, content);
        let indexed_b = runtime.indexed(id);

        // indexed_b would have been recomputed, but since the existing value
        // is equivalent then we return it, thus making the pointers the same.
        assert!(indexed_a == indexed_b);
        assert!(Arc::ptr_eq(&indexed_a, &indexed_b));

        let id = files.insert("./src/Main.purs", "module Main where\n\n\n\nlife = 42\n\n");
        let content = files.content(id);

        runtime.set_content(id, content);
        let indexed_c = runtime.indexed(id);

        assert!(indexed_a != indexed_c);
        assert!(!Arc::ptr_eq(&indexed_a, &indexed_c));

        assert!(indexed_b != indexed_c);
        assert!(!Arc::ptr_eq(&indexed_b, &indexed_c));

        let id = files.insert("./src/Main.purs", "module Main where\n\nlife = 42\n\n");
        let content = files.content(id);

        runtime.set_content(id, content);
        let indexed_d = runtime.indexed(id);

        // indexed_b and indexed_d are equal, but they're different objects.
        assert!(indexed_b == indexed_d);
        assert!(!Arc::ptr_eq(&indexed_b, &indexed_d));
    }
}
