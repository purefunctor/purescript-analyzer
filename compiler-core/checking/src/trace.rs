//! Tracing infrastructure for type checker debugging.
//!
//! This module provides macros and helpers for emitting structured trace events
//! with pretty-printed type information. All tracing is compiled out when the
//! `no-tracing` feature is enabled.

#[cfg(not(feature = "no-tracing"))]
mod enabled {
    use std::fmt;

    use crate::ExternalQueries;
    use crate::algorithm::state::{CheckContext, CheckState};
    use crate::core::TypeId;
    use crate::core::pretty::print_local;

    /// A rendered type for tracing output.
    ///
    /// Implements both [`fmt::Display`] for human-readable output and
    /// [`fmt::Debug`] for JSON serialisation via tracing subscribers.
    #[derive(Clone)]
    pub struct TypeTrace(String);

    impl fmt::Display for TypeTrace {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            f.write_str(&self.0)
        }
    }

    impl fmt::Debug for TypeTrace {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{:?}", self.0)
        }
    }

    /// Renders a type for tracing, normalising through unification chains.
    #[inline]
    pub fn render<Q: ExternalQueries>(
        state: &mut CheckState,
        context: &CheckContext<'_, Q>,
        type_id: TypeId,
    ) -> TypeTrace {
        let normalised = state.normalize_type(type_id);
        TypeTrace(print_local(state, context, normalised))
    }
}

#[cfg(not(feature = "no-tracing"))]
pub use enabled::*;

/// Core macro for emitting type fields at a configurable tracing level.
#[cfg(not(feature = "no-tracing"))]
#[macro_export]
macro_rules! type_fields {
    ($level:expr, $state:expr, $context:expr, { $($name:ident = $type_id:expr),* $(,)? }) => {
        if ::tracing::enabled!($level) {
            $(let $name = $crate::trace::render($state, $context, $type_id);)*
            ::tracing::event!($level, $($name = %$name),*);
        }
    };
    ($level:expr, $state:expr, $context:expr, { $($name:ident = $type_id:expr),* $(,)? }, $($msg:tt)+) => {
        if ::tracing::enabled!($level) {
            $(let $name = $crate::trace::render($state, $context, $type_id);)*
            ::tracing::event!($level, $($name = %$name,)* $($msg)+);
        }
    };
}

#[cfg(feature = "no-tracing")]
#[macro_export]
macro_rules! type_fields {
    ($($tt:tt)*) => {};
}

/// Emits an INFO-level event with type fields.
#[macro_export]
macro_rules! info_fields {
    ($($args:tt)*) => {
        $crate::type_fields!(::tracing::Level::INFO, $($args)*)
    };
}

/// Emits a DEBUG-level event with type fields.
#[macro_export]
macro_rules! debug_fields {
    ($($args:tt)*) => {
        $crate::type_fields!(::tracing::Level::DEBUG, $($args)*)
    };
}

/// Emits a TRACE-level event with type fields.
#[macro_export]
macro_rules! trace_fields {
    ($($args:tt)*) => {
        $crate::type_fields!(::tracing::Level::TRACE, $($args)*)
    };
}
