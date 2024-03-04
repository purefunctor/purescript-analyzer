use crate::{id::InFile, infer::ConstructorId, surface::tree::*, InferenceDatabase};

use super::{CoreType, InferContext};

impl InferContext<'_> {
    pub(super) fn infer_data_declaration(
        &mut self,
        db: &dyn InferenceDatabase,
        data_declaration: &DataDeclaration,
    ) {
        let file_id = self.file_id;
        let data_ty = {
            let data_id = InFile { file_id, value: ConstructorId::Data(data_declaration.id) };
            let function_ty = db.intern_type(CoreType::Constructor(data_id));
            data_declaration.variables.iter().fold(function_ty, |function_ty, argument| {
                let name = argument.to_name();
                let argument_ty = db.intern_type(CoreType::Variable(name));
                db.intern_type(CoreType::Application(function_ty, argument_ty))
            })
        };

        data_declaration.constructors.iter().for_each(|(constructor_id, data_constructor)| {
            let fields_ty = data_constructor.fields.iter().map(|field| self.lower_type(db, *field));
            let constructor_ty = fields_ty.rev().fold(data_ty, |data_ty, field_ty| {
                db.intern_type(CoreType::Function(field_ty, data_ty))
            });
            let qualified_ty = self.qualify_type(db, &data_declaration.variables, constructor_ty);
            self.state.map.of_constructor.insert(*constructor_id, qualified_ty);
        });
    }
}
