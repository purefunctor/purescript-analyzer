/// Attempt fallible operations, return `Default` on early `?` exit.
///
/// The body is wrapped in an IIFE that returns `Some(body)`.
/// If any `?` inside the body returns `None`, the whole expression
/// evaluates to `T::default()`.
///
/// # Example
///
/// ```ignore
/// let args: Arc<[_]> = recover! {
///     cst.instance_head()?
///         .children()
///         .map(|c| lower_type(state, context, &c))
///         .collect()
/// };
/// ```
macro_rules! recover {
    ($($body:tt)*) => {
        (|| Some({ $($body)* }))().unwrap_or_default()
    };
}
