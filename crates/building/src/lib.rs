pub mod module_name_map;
pub mod runtime;

pub use module_name_map::*;
pub use runtime::*;

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    #[test]
    fn test_pointer_equality() {
        let mut runtime = super::Runtime::default();
        let mut files = files::Files::default();

        let id = files.insert("./src/Main.purs", "module Main where\n\nlife = 42");
        let content = files.content(id);

        runtime.set_content(id, content);
        let index_a = runtime.indexed(id);
        let index_b = runtime.indexed(id);
        assert!(Arc::ptr_eq(&index_a, &index_b));

        let resolve_a = runtime.resolved(id);
        let resolve_b = runtime.resolved(id);
        assert!(Arc::ptr_eq(&resolve_a, &resolve_b));

        let id = files.insert("./src/Main.purs", "module Main where\n\nlife = 42");
        let content = files.content(id);

        runtime.set_content(id, content);
        let index_a = runtime.indexed(id);
        let index_b = runtime.indexed(id);
        assert!(Arc::ptr_eq(&index_a, &index_b));

        let resolve_a = runtime.resolved(id);
        let resolve_b = runtime.resolved(id);
        assert!(Arc::ptr_eq(&resolve_a, &resolve_b));
    }

    #[test]
    fn test_verifying_step_traces() {
        let mut runtime = super::Runtime::default();
        let mut files = files::Files::default();

        macro_rules! assert_trace {
            ($key:ident($id:expr) => { built: $built:expr, changed: $changed:expr }) => {
                let key = crate::QueryKey::$key($id);
                let trace = runtime.trace(key);
                assert!(trace.is_some(), "Invalid key {:?}", key);
                let trace = trace.unwrap();
                assert_eq!(trace.built, $built, "Built is incorrect");
                assert_eq!(trace.changed, $changed, "Changed is incorrect");
            };
        }

        let id = files.insert("./src/Main.purs", "module Main where\n\nlife = 42");
        let content = files.content(id);

        runtime.set_content(id, content);
        let indexed_a = runtime.indexed(id);
        let lowered_a = runtime.lowered(id);
        let resolved_a = runtime.resolved(id);

        assert_trace!(FileContent(id) => { built: 1, changed: 1 });
        assert_trace!(Parsed(id) => { built: 1, changed: 1 });
        assert_trace!(Indexed(id) => { built: 1, changed: 1 });
        assert_trace!(Lowered(id) => { built: 1, changed: 1 });
        assert_trace!(Resolved(id) => { built: 1, changed: 1 });

        let id = files.insert("./src/Main.purs", "module Main where\n\n\n\nlife = 42");
        let content = files.content(id);

        runtime.set_content(id, content);
        let indexed_b = runtime.indexed(id);
        let lowered_b = runtime.lowered(id);
        let resolved_b = runtime.resolved(id);

        assert_trace!(FileContent(id) => { built: 2, changed: 2 });
        assert_trace!(Parsed(id) => { built: 2, changed: 2 });
        // Indexed/Lowered changed because they contain pointers
        // that also change when declarations are shifted down.
        assert_trace!(Indexed(id) => { built: 2, changed: 2 });
        assert_trace!(Lowered(id) => { built: 2, changed: 2 });
        // Meanwhile, Resolved is very stable because it uses IDs.
        assert_trace!(Resolved(id) => { built: 2, changed: 1 });

        let id = files.insert("./src/Main.purs", "module Main where\n\n\n\nlife = 42\n\n");
        let content = files.content(id);

        runtime.set_content(id, content);
        let indexed_c = runtime.indexed(id);
        let lowered_c = runtime.lowered(id);
        let resolved_c = runtime.resolved(id);

        // FileContent and Parsed will always change.
        assert_trace!(FileContent(id) => { built: 3, changed: 3 });
        assert_trace!(Parsed(id) => { built: 3, changed: 3 });
        // Indexed/Lowered did not change because the pointers it
        // contains were not affected by theappended whitespace.
        assert_trace!(Indexed(id) => { built: 3, changed: 2 });
        assert_trace!(Lowered(id) => { built: 3, changed: 2 });
        // Resolved is still very stable against non-semantic edits.
        assert_trace!(Resolved(id) => { built: 3, changed: 1 });

        assert!(Arc::ptr_eq(&indexed_b, &indexed_c));
        assert!(Arc::ptr_eq(&lowered_b, &lowered_c));
        assert!(Arc::ptr_eq(&resolved_a, &resolved_b));
        assert!(Arc::ptr_eq(&resolved_b, &resolved_c));
        assert!(Arc::ptr_eq(&resolved_a, &resolved_c));

        assert!(!Arc::ptr_eq(&indexed_a, &indexed_b));
        assert!(!Arc::ptr_eq(&indexed_a, &indexed_c));
        assert!(!Arc::ptr_eq(&lowered_a, &lowered_b));
        assert!(!Arc::ptr_eq(&lowered_a, &lowered_c));
    }
}
