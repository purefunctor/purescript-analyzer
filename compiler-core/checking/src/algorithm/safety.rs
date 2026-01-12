//! Safety mechanisms for the type checker.

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
///     current_id = state.normalize_type(current_id);
///     match state.storage[current_id] {
///         Type::Application(function, _) => current_id = function,
///         _ => break,
///     }
/// }
/// ```
#[macro_export]
macro_rules! safe_loop {
    ($($body:tt)*) => {{
        let mut fuel = 0u32;
        loop {
            if fuel >= $crate::algorithm::safety::FUEL {
                unreachable!("invariant violated: fuel exhausted");
            }
            fuel += 1;
            $($body)*
        }
    }};
}

pub use safe_loop;
