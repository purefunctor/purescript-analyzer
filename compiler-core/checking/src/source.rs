//! Implements syntax-driven checking rules for source files.

pub mod binder;
pub mod operator;
pub mod roles;
pub mod synonym;
pub mod terms;
pub mod types;

mod derive;
mod term_items;
mod type_items;

pub use term_items::check_term_items;
pub use type_items::check_type_items;
