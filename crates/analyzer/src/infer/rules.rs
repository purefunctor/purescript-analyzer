//! Implements the type inference routines for PureScript.
mod data;
mod instantiate;
mod lower;
mod replace;
mod solve;
mod substitute;
mod unify;
mod value;

use rustc_hash::FxHashMap;
use syntax::ast;

use crate::{id::AstId, resolver::ValueGroupId, surface, InferDatabase};

use super::{constraint::Constraint, Provenance, Type, TypeId, Unification};

pub(crate) use data::infer_data_group_query;
pub(crate) use value::infer_binding_group_query;

#[derive(Debug, Default)]
struct InferState {
    fresh_index: usize,
    constraints: Vec<Constraint>,
}

impl InferState {
    fn fresh_unification(&mut self, db: &dyn InferDatabase, provenance: Provenance) -> TypeId {
        let index = self.fresh_index as u32;
        self.fresh_index += 1;
        db.intern_type(Type::Unification(Unification { index, provenance }))
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct BindingGroupTypes {
    of_value_group: FxHashMap<ValueGroupId, ValueGroupTypes>,
    constraints: Vec<Constraint>,
}

impl BindingGroupTypes {
    fn new(
        of_value_group: FxHashMap<ValueGroupId, ValueGroupTypes>,
        constraints: Vec<Constraint>,
    ) -> BindingGroupTypes {
        BindingGroupTypes { of_value_group, constraints }
    }

    pub fn get(&self, id: ValueGroupId) -> &ValueGroupTypes {
        self.of_value_group.get(&id).unwrap()
    }

    pub fn iter(&self) -> impl Iterator<Item = (ValueGroupId, &ValueGroupTypes)> {
        self.of_value_group.iter().map(|(id, infer)| (*id, infer))
    }

    pub fn constraints(&self) -> &[Constraint] {
        &self.constraints
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct DataGroupTypes {
    of_constructor: FxHashMap<AstId<ast::DataConstructor>, TypeId>,
}

impl DataGroupTypes {
    fn new(of_constructor: FxHashMap<AstId<ast::DataConstructor>, TypeId>) -> DataGroupTypes {
        DataGroupTypes { of_constructor }
    }

    pub fn get_constructor(&self, constructor_id: AstId<ast::DataConstructor>) -> TypeId {
        *self.of_constructor.get(&constructor_id).unwrap()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ValueGroupTypes {
    of_value_group: TypeId,
    of_expr: FxHashMap<surface::ExprId, TypeId>,
    of_let_name: FxHashMap<surface::LetNameId, TypeId>,
    of_binder: FxHashMap<surface::BinderId, TypeId>,
}

impl ValueGroupTypes {
    fn new(
        of_value_group: TypeId,
        of_expr: FxHashMap<surface::ExprId, TypeId>,
        of_let_name: FxHashMap<surface::LetNameId, TypeId>,
        of_binder: FxHashMap<surface::BinderId, TypeId>,
    ) -> ValueGroupTypes {
        ValueGroupTypes { of_value_group, of_expr, of_let_name, of_binder }
    }

    pub fn as_type(&self) -> TypeId {
        self.of_value_group
    }

    pub fn get_expr(&self, expr_id: surface::ExprId) -> TypeId {
        *self.of_expr.get(&expr_id).unwrap()
    }

    pub fn get_let_name(&self, let_name_id: surface::LetNameId) -> TypeId {
        *self.of_let_name.get(&let_name_id).unwrap()
    }

    pub fn get_binder(&self, binder_id: surface::BinderId) -> TypeId {
        *self.of_binder.get(&binder_id).unwrap()
    }
}
