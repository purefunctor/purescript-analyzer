use crate::{
    id::InFile,
    infer::{ConstructorId, CoreType},
    InferenceDatabase,
};

use super::{ClassDeclaration, InferContext};

impl InferContext<'_> {
    pub(super) fn infer_class_declaration(
        &mut self,
        db: &dyn InferenceDatabase,
        class_declaration: &ClassDeclaration,
    ) {
        let file_id = self.file_id;
        let constraint_ty = {
            let class_id = InFile { file_id, value: ConstructorId::Class(class_declaration.id) };
            let function_ty = db.intern_type(CoreType::Constructor(class_id));
            class_declaration.variables.iter().fold(function_ty, |function_ty, variable| {
                let name = variable.to_name();
                let argument_ty = db.intern_type(CoreType::Variable(name));
                db.intern_type(CoreType::Application(function_ty, argument_ty))
            })
        };

        class_declaration.members.iter().for_each(|(member_id, class_member)| {
            let member_ty = self.lower_type(db, class_member.ty);
            let constrained_ty = db.intern_type(CoreType::Constrained(constraint_ty, member_ty));
            let qualified_ty = self.qualify_type(db, &class_declaration.variables, constrained_ty);
            self.state.map.of_member.insert(*member_id, qualified_ty);
        });
    }
}
