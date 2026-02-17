//! Safety utilities for implementing type checker rules.

/// Fuel constant for bounded loops to prevent infinite looping.
pub const FUEL: u32 = 1_000_000;

/// Executes a loop body with fuel, breaking when fuel runs out.
///
/// Use this for loops that traverse type structures which could
/// theoretically be infinite due to bugs or malformed input.
///
/// # Example
///
/// ```ignore
/// let mut current_id = type_id;
/// safe_loop! {
///     current_id = normalise(current_id);
///     if let Type::Application(function, _) = context.queries.lookup_type(current_id) {
///         current_id = function;
///     } else {
///         break;
///     }
/// }
/// ```
#[macro_export]
macro_rules! safe_loop {
    ($($body:tt)*) => {{
        let mut fuel = 0u32;
        loop {
            if fuel >= $crate::safety::FUEL {
                unreachable!("invariant violated: fuel exhausted");
            }
            fuel += 1;
            $($body)*
        }
    }};
}

pub use safe_loop;
