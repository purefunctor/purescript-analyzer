//! Tracing infrastructure for type checker debugging.
//!
//! This module provides macros and helpers for emitting structured trace events
//! with pretty-printed type information. All tracing is compiled out when the
//! `no-tracing` feature is enabled.

use building_types::QueryResult;

use crate::ExternalQueries;
use crate::algorithm::state::CheckContext;
use crate::error::ErrorStep;

/// Extracts the byte offset range from an error step.
pub fn step_byte_range<Q>(step: &ErrorStep, context: &CheckContext<Q>) -> Option<(u32, u32)>
where
    Q: ExternalQueries,
{
    let pointer = match step {
        ErrorStep::ConstructorArgument(id) => context.stabilized.syntax_ptr(*id)?,
        ErrorStep::InferringKind(id) | ErrorStep::CheckingKind(id) => {
            context.stabilized.syntax_ptr(*id)?
        }
        ErrorStep::InferringBinder(id) | ErrorStep::CheckingBinder(id) => {
            context.stabilized.syntax_ptr(*id)?
        }
        ErrorStep::InferringExpression(id) | ErrorStep::CheckingExpression(id) => {
            context.stabilized.syntax_ptr(*id)?
        }
        ErrorStep::TermDeclaration(id) => {
            context.indexed.term_item_ptr(&context.stabilized, *id).next()?
        }
        ErrorStep::TypeDeclaration(id) => {
            context.indexed.type_item_ptr(&context.stabilized, *id).next()?
        }
        ErrorStep::InferringDoBind(id) | ErrorStep::InferringDoDiscard(id) => {
            context.stabilized.syntax_ptr(*id)?
        }
        ErrorStep::InferringAdoMap(id) | ErrorStep::InferringAdoApply(id) => {
            context.stabilized.syntax_ptr(*id)?
        }
    };

    let range = pointer.text_range();

    let start = range.start().into();
    let end = range.end().into();

    Some((start, end))
}

/// Returns the byte offset range for the most specific (innermost) error step.
pub fn current_offset<Q>(check_steps: &[ErrorStep], context: &CheckContext<Q>) -> Option<(u32, u32)>
where
    Q: ExternalQueries,
{
    check_steps.last().and_then(|step| step_byte_range(step, context))
}

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

    /// A trace value that can be either pretty-printed or debug-formatted.
    pub enum TraceValue {
        Pretty(TypeTrace),
        Debug(String),
    }

    impl fmt::Display for TraceValue {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                TraceValue::Pretty(trace) => fmt::Display::fmt(trace, f),
                TraceValue::Debug(s) => f.write_str(s),
            }
        }
    }

    impl fmt::Debug for TraceValue {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                TraceValue::Pretty(trace) => fmt::Debug::fmt(trace, f),
                TraceValue::Debug(s) => write!(f, "{:?}", s),
            }
        }
    }

    /// Trait for types that can be traced with type context.
    pub trait Traceable<Q>
    where
        Q: ExternalQueries,
    {
        fn trace(self, state: &mut CheckState, context: &CheckContext<Q>) -> TraceValue;
    }

    impl<Q> Traceable<Q> for TypeId
    where
        Q: ExternalQueries,
    {
        fn trace(self, state: &mut CheckState, context: &CheckContext<Q>) -> TraceValue {
            TraceValue::Pretty(render(state, context, self))
        }
    }

    impl<Q> Traceable<Q> for Option<TypeId>
    where
        Q: ExternalQueries,
    {
        fn trace(self, state: &mut CheckState, context: &CheckContext<Q>) -> TraceValue {
            if let Some(value) = self {
                value.trace(state, context)
            } else {
                TraceValue::Debug("None".to_string())
            }
        }
    }

    /// Wrapper for tracing arbitrary `Debug` types.
    pub struct TraceDebug<T>(pub T);

    impl<T, Q> Traceable<Q> for TraceDebug<T>
    where
        T: fmt::Debug,
        Q: ExternalQueries,
    {
        fn trace(self, _state: &mut CheckState, _context: &CheckContext<Q>) -> TraceValue {
            TraceValue::Debug(format!("{:?}", self.0))
        }
    }

    /// Renders a type for tracing, normalising through unification chains.
    #[inline]
    pub fn render<Q>(
        state: &mut CheckState,
        context: &CheckContext<Q>,
        type_id: TypeId,
    ) -> TypeTrace
    where
        Q: ExternalQueries,
    {
        let normalised = state.normalize_type(type_id);
        TypeTrace(print_local(state, context, normalised))
    }
}

#[cfg(not(feature = "no-tracing"))]
pub use enabled::*;

/// Core macro for emitting type fields at a configurable tracing level.
///
/// Supports both pretty-printed types and debug-formatted values:
/// - `name = expr` - pretty-prints TypeId values via Traceable
/// - `?name = expr` - debug-formats arbitrary values via Dbg wrapper
///
/// # Examples
///
/// ```ignore
/// // Pretty-print TypeIds
/// trace_fields!(state, context, { t1 = t1, t2 = t2 });
///
/// // Debug-format primitives with ? prefix
/// trace_fields!(state, context, { ?count = count });
///
/// // Mix both with a message
/// trace_fields!(state, context, { t1 = t1, ?level = level }, "solving");
/// ```
#[cfg(not(feature = "no-tracing"))]
#[macro_export]
macro_rules! type_fields {
    // Entry point: fields only
    ($level:expr, $state:expr, $context:expr, { $($fields:tt)* }) => {
        $crate::type_fields!(@impl $level, $state, $context, [] { $($fields)* })
    };
    // Entry point: fields + message
    ($level:expr, $state:expr, $context:expr, { $($fields:tt)* }, $($msg:tt)+) => {
        $crate::type_fields!(@impl_msg $level, $state, $context, [] { $($fields)* }, $($msg)+)
    };

    // === No-message variant ===

    // Done processing fields
    (@impl $level:expr, $state:expr, $context:expr, [$(($name:ident, $bind:expr))*] { $(,)? }) => {
        if ::tracing::enabled!($level) {
            $(let $name = $bind;)*
            if let Some((offset_start, offset_end)) = $crate::trace::current_offset(&$state.check_steps, $context) {
                ::tracing::event!($level, offset_start, offset_end, $($name = %$name),*);
            } else {
                ::tracing::event!($level, $($name = %$name),*);
            }
        }
    };

    // Match ?name = expr,
    (@impl $level:expr, $state:expr, $context:expr, [$($acc:tt)*] { ? $name:ident = $value:expr, $($rest:tt)* }) => {
        $crate::type_fields!(@impl $level, $state, $context, [$($acc)* ($name, $crate::trace::Traceable::trace($crate::trace::TraceDebug($value), $state, $context))] { $($rest)* })
    };

    // Match ?name = expr (last, no comma)
    (@impl $level:expr, $state:expr, $context:expr, [$($acc:tt)*] { ? $name:ident = $value:expr }) => {
        $crate::type_fields!(@impl $level, $state, $context, [$($acc)* ($name, $crate::trace::Traceable::trace($crate::trace::TraceDebug($value), $state, $context))] { })
    };

    // Match name = expr,
    (@impl $level:expr, $state:expr, $context:expr, [$($acc:tt)*] { $name:ident = $value:expr, $($rest:tt)* }) => {
        $crate::type_fields!(@impl $level, $state, $context, [$($acc)* ($name, $crate::trace::Traceable::trace($value, $state, $context))] { $($rest)* })
    };

    // Match name = expr (last, no comma)
    (@impl $level:expr, $state:expr, $context:expr, [$($acc:tt)*] { $name:ident = $value:expr }) => {
        $crate::type_fields!(@impl $level, $state, $context, [$($acc)* ($name, $crate::trace::Traceable::trace($value, $state, $context))] { })
    };

    // === Message variant ===

    // Done processing fields
    (@impl_msg $level:expr, $state:expr, $context:expr, [$(($name:ident, $bind:expr))*] { $(,)? }, $($msg:tt)+) => {
        if ::tracing::enabled!($level) {
            $(let $name = $bind;)*
            if let Some((offset_start, offset_end)) = $crate::trace::current_offset(&$state.check_steps, $context) {
                ::tracing::event!($level, offset_start, offset_end, $($name = %$name,)* $($msg)+);
            } else {
                ::tracing::event!($level, $($name = %$name,)* $($msg)+);
            }
        }
    };

    // Match ?name = expr,
    (@impl_msg $level:expr, $state:expr, $context:expr, [$($acc:tt)*] { ? $name:ident = $value:expr, $($rest:tt)* }, $($msg:tt)+) => {
        $crate::type_fields!(@impl_msg $level, $state, $context, [$($acc)* ($name, $crate::trace::Traceable::trace($crate::trace::TraceDebug($value), $state, $context))] { $($rest)* }, $($msg)+)
    };

    // Match ?name = expr (last, no comma)
    (@impl_msg $level:expr, $state:expr, $context:expr, [$($acc:tt)*] { ? $name:ident = $value:expr }, $($msg:tt)+) => {
        $crate::type_fields!(@impl_msg $level, $state, $context, [$($acc)* ($name, $crate::trace::Traceable::trace($crate::trace::TraceDebug($value), $state, $context))] { }, $($msg)+)
    };

    // Match name = expr,
    (@impl_msg $level:expr, $state:expr, $context:expr, [$($acc:tt)*] { $name:ident = $value:expr, $($rest:tt)* }, $($msg:tt)+) => {
        $crate::type_fields!(@impl_msg $level, $state, $context, [$($acc)* ($name, $crate::trace::Traceable::trace($value, $state, $context))] { $($rest)* }, $($msg)+)
    };

    // Match name = expr (last, no comma)
    (@impl_msg $level:expr, $state:expr, $context:expr, [$($acc:tt)*] { $name:ident = $value:expr }, $($msg:tt)+) => {
        $crate::type_fields!(@impl_msg $level, $state, $context, [$($acc)* ($name, $crate::trace::Traceable::trace($value, $state, $context))] { }, $($msg)+)
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

/// Creates an info-level span for module checking.
#[cfg(not(feature = "no-tracing"))]
#[inline]
pub fn check_module(
    queries: &impl ExternalQueries,
    file_id: files::FileId,
) -> QueryResult<tracing::span::EnteredSpan> {
    const UNKNOWN: smol_str::SmolStr = smol_str::SmolStr::new_static("<Unknown>");

    let (parsed, _) = queries.parsed(file_id)?;
    let module_name = parsed.module_name();
    let module_name = module_name.unwrap_or(UNKNOWN);

    Ok(tracing::info_span!("check_module", ?file_id, %module_name).entered())
}

#[cfg(feature = "no-tracing")]
#[inline]
pub fn module_span(_queries: &impl ExternalQueries, _file_id: files::FileId) -> QueryResult<()> {
    Ok(())
}
