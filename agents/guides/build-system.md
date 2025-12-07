# Build System

## Adding a New Query

To add a new query to the build system:

1. **building-types/src/lib.rs**: Add variant to `QueryKey` enum and add associated type + method to `QueryProxy` trait
2. **building/src/engine.rs**:
   - Add field to `DerivedStorage` struct
   - Add case to `derived_changed!` macro invocation in dependency tracking
   - Implement the query method on `QueryEngine`
   - Implement `QueryProxy` associated type and method
3. **Downstream crates**: Update `ExternalQueries` trait bounds to include the new associated type

## Query Implementation Pattern

```rust
pub fn my_query(&self, id: FileId) -> QueryResult<Arc<MyResult>> {
    self.query(
        QueryKey::MyQuery(id),
        |storage| storage.derived.my_query.get(&id),
        |storage| storage.derived.my_query.entry(id),
        |this| {
            let dependency = this.some_dependency(id)?;
            let result = compute_result(&dependency)?;
            Ok(Arc::new(result))
        },
    )
}
```
