use std::sync::Arc;

use la_arena::Arena;
use rustc_hash::FxHashMap;
use syntax::ast;

use crate::{
    id::{AstId, InFile},
    infer::{rules::lower::lower_type, Type, TypeId},
    resolver::DataGroupId,
    scope::Resolutions,
    surface, InferDatabase,
};

use super::DataGroupTypes;

struct InferDataGroupContext<'env> {
    db: &'env dyn InferDatabase,
    type_arena: &'env Arena<surface::Type>,
    resolutions: &'env Resolutions,
    of_constructor: FxHashMap<AstId<ast::DataConstructor>, TypeId>,
}

impl<'env> InferDataGroupContext<'env> {
    fn new(
        db: &'env dyn InferDatabase,
        type_arena: &'env Arena<surface::Type>,
        resolutions: &'env Resolutions,
    ) -> InferDataGroupContext<'env> {
        let of_constructor = FxHashMap::default();
        InferDataGroupContext { db, type_arena, resolutions, of_constructor }
    }
}

impl<'env> InferDataGroupContext<'env> {
    fn infer(&mut self, id: InFile<DataGroupId>, data_group: &surface::DataGroup) {
        let constructors = &data_group.declaration.constructors;
        let variables = &data_group.declaration.variables;

        let data_ty = variables.iter().fold(
            self.db.intern_type(Type::Constructor(id)),
            |function_ty, argument_ty_var| {
                let argument_ty = match argument_ty_var {
                    surface::TypeVariable::Kinded(name, _) => {
                        self.db.intern_type(Type::Variable(name.clone()))
                    }
                    surface::TypeVariable::Name(name) => {
                        self.db.intern_type(Type::Variable(name.clone()))
                    }
                };
                self.db.intern_type(Type::Application(function_ty, argument_ty))
            },
        );

        for (id, constructor) in constructors {
            let fields = constructor
                .fields
                .iter()
                .map(|field| lower_type(self.db, self.type_arena, self.resolutions, *field));

            let constructor_ty = fields.rev().fold(data_ty, |result, argument| {
                self.db.intern_type(Type::Function(argument, result))
            });

            let constructor_ty = variables.iter().rev().fold(constructor_ty, |ty, ty_variable| {
                let name = match ty_variable {
                    surface::TypeVariable::Kinded(name, _) => name.clone(),
                    surface::TypeVariable::Name(name) => name.clone(),
                };
                self.db.intern_type(Type::Forall(name, ty))
            });

            self.of_constructor.insert(*id, constructor_ty);
        }
    }
}

pub(crate) fn infer_data_group_query(
    db: &dyn InferDatabase,
    id: InFile<DataGroupId>,
) -> Arc<DataGroupTypes> {
    let data_group = db.data_surface(id);
    let resolutions = db.data_resolutions(id);

    let mut context = InferDataGroupContext::new(db, &data_group.type_arena, &resolutions);

    context.infer(id, &data_group.value);

    Arc::new(DataGroupTypes::new(context.of_constructor))
}
