//! The core of the analyzer.

pub mod id;
pub mod infer;
pub mod names;
pub mod resolver;
pub mod scope;
pub mod source;
pub mod surface;

use indexmap::{IndexMap, IndexSet};
use rustc_hash::FxHasher;
use std::hash::BuildHasherDefault;

pub use infer::InferDatabase;
pub use resolver::ResolverDatabase;
pub use scope::ScopeDatabase;
pub use source::SourceDatabase;
pub use surface::SurfaceDatabase;

/// The analyzer's core database.
#[derive(Default)]
#[salsa::database(
    infer::InferStorage,
    resolver::ResolverStorage,
    scope::ScopeStorage,
    source::SourceStorage,
    surface::LowerStorage
)]
pub struct RootDatabase {
    storage: salsa::Storage<RootDatabase>,
}

impl salsa::Database for RootDatabase {}

pub trait Upcast<T: ?Sized> {
    fn upcast(&self) -> &T;
}

impl Upcast<dyn ResolverDatabase> for RootDatabase {
    fn upcast(&self) -> &(dyn ResolverDatabase + 'static) {
        self
    }
}

pub(crate) type FxIndexSet<T> = IndexSet<T, BuildHasherDefault<FxHasher>>;
pub(crate) type FxIndexMap<K, V> = IndexMap<K, V, BuildHasherDefault<FxHasher>>;

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use files::{ChangedFile, Files};
    use salsa::Durability;

    use crate::{InferDatabase, ResolverDatabase, RootDatabase, SourceDatabase};

    #[test]
    fn api() {
        // Initialization Code
        let mut db = RootDatabase::default();
        let mut files = Files::default();

        // Given the source file glob, we take all purs files and load them onto the file system.
        files.set_file_contents(
            "./Main.purs".into(),
            Some(
                "
module Main where

a = 0

b = a
"
                .into(),
            ),
        );
        // Then, we feed it to the database through the `take_changes` method.
        for ChangedFile { file_id, .. } in files.take_changes() {
            let contents = files.file_contents(file_id);
            db.set_file_contents(file_id, Arc::from(std::str::from_utf8(contents).unwrap()));
        }
        // Finally, we provide the file paths to the database for use by queries like `module_map`.
        // Note that we're setting the durability to medium as we don't expect file paths to change
        // as often as something like editing a file would with the file contents.
        db.set_file_paths_with_durability(files.iter().collect(), Durability::MEDIUM);

        let file_id = files.file_id("./Main.purs".into()).unwrap();
        let a_id = db.nominal_map(file_id).get_value("a").unwrap()[0];
        let b_id = db.nominal_map(file_id).get_value("b").unwrap()[0];

        db.infer_value_declaration(a_id);
        db.infer_value_declaration(b_id);

        // let cons_id = db.nominal_map(file_id).get_constructor("Cons").unwrap();
        // let list_id = db.nominal_map(file_id).get_data("List").unwrap();

        // let list_dt = db.surface_data(list_id);
        // let cons_dt = list_dt.constructors.get(&cons_id).unwrap();

        // let expr_arena = Default::default();
        // let binder_arena = Default::default();
        // let pretty_printer = PrettyPrinter::new(&expr_arena, &binder_arena, &list_dt.type_arena);

        // let mut out = String::new();
        // for field in cons_dt.fields.iter() {
        //     pretty_printer.ty(*field).render_fmt(80, &mut out).unwrap();
        //     out.push('\n');
        // }
        // println!("{}", out);

        // let list_id = db.nominal_map(file_id).get_data("List").unwrap();
        // // let cons_id = db.nominal_map(file_id).get_constructor("Cons").unwrap();
        // let val_id = db.nominal_map(file_id).get_value("hello").unwrap()[0];
        // let val_data = db.surface_value_declaration(val_id);

        // let type_arena = la_arena::Arena::default();
        // let pretty_printer =
        //     PrettyPrinter::new(&val_data.expr_arena, &val_data.binder_arena, &type_arena);

        // let mut out = String::default();
        // for binder_id in val_data.binders.iter() {
        //     pretty_printer.binder(*binder_id).render_fmt(80, &mut out).unwrap();
        //     out.push('\n');
        // }
        // println!("{}", out);

        // let mut out = String::default();
        // match &val_data.binding {
        //     crate::surface::Binding::Unconditional { where_expr } => {
        //         pretty_printer.expr(where_expr.expr_id).render_fmt(80, &mut out).unwrap();
        //     }
        // }
        // println!("{}", out);
    }
}
