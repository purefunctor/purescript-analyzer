pub mod check;
pub mod core;

pub use core::{Type, TypeId, TypeInterner};

use std::sync::Arc;

use building_types::{QueryProxy, QueryResult};
use files::FileId;
use indexing::{IndexedModule, TermItemId, TypeItemId};
use itertools::Itertools;
use lowering::{DataIr, LoweredModule, Scc, TermItemIr, TypeItemIr};
use resolving::ResolvedModule;
use rustc_hash::FxHashMap;
use smol_str::SmolStr;

use crate::check::{CheckContext, CheckState, convert, kind, transfer, unification};
use crate::core::{ForallBinder, Variable, debruijn};

pub trait ExternalQueries:
    QueryProxy<
        Indexed = Arc<IndexedModule>,
        Lowered = Arc<LoweredModule>,
        Resolved = Arc<ResolvedModule>,
        Checked = Arc<CheckedModule>,
    >
{
    fn intern_type(&self, t: Type) -> TypeId;

    fn lookup_type(&self, id: TypeId) -> Type;
}

#[derive(Debug, Default, PartialEq, Eq)]
pub struct CheckedModule {
    terms: FxHashMap<TermItemId, TypeId>,
    types: FxHashMap<TypeItemId, TypeId>,
}

impl CheckedModule {
    pub fn lookup_term(&self, id: TermItemId) -> Option<TypeId> {
        self.terms.get(&id).copied()
    }

    pub fn lookup_type(&self, id: TypeItemId) -> Option<TypeId> {
        self.types.get(&id).copied()
    }
}

pub fn check_module(queries: &impl ExternalQueries, file_id: FileId) -> QueryResult<CheckedModule> {
    let prim_id = queries.prim_id();
    if file_id == prim_id {
        prim_check_module(queries, prim_id)
    } else {
        source_check_module(queries, file_id)
    }
}

fn source_check_module(
    queries: &impl ExternalQueries,
    file_id: FileId,
) -> QueryResult<CheckedModule> {
    let mut state = CheckState::default();
    let context = CheckContext::new(queries, &mut state, file_id)?;

    for scc in &context.lowered.type_scc {
        match scc {
            Scc::Base(id) => {
                check_type_item(&mut state, &context, *id);
            }
            Scc::Recursive(id) => {
                state.type_binding_group(&context, [*id]);
                check_type_item(&mut state, &context, *id);
                state.commit_binding_group(&context);
            }
            Scc::Mutual(mutual) => {
                state.type_binding_group(&context, mutual);
                for id in mutual {
                    check_type_item(&mut state, &context, *id);
                }
                state.commit_binding_group(&context);
            }
        }
    }

    Ok(state.checked)
}

const MISSING_NAME: SmolStr = SmolStr::new_static("<MissingName>");

fn check_type_item<Q>(state: &mut CheckState, context: &CheckContext<Q>, item_id: TypeItemId)
where
    Q: ExternalQueries,
{
    let Some(item) = context.lowered.info.get_type_item(item_id) else { return };
    match item {
        TypeItemIr::DataGroup { signature, data, .. } => {
            let Some(DataIr { variables }) = data else { return };

            let signature = signature.map(|id| convert::inspect_signature(state, context, id));

            let (kind_variables, type_variables, result_kind) = if let Some(signature) = signature {
                if variables.len() != signature.arguments.len() {
                    todo!("proper arity checking errors innit")
                };

                let variables = variables.iter();
                let arguments = signature.arguments.iter();

                let kinds = variables.zip(arguments).map(|(variable, &argument)| {
                    // Use contravariant subtyping for type variables:
                    //
                    // data Example :: Argument -> Type
                    // data Example (a :: Variable) = Example
                    //
                    // Signature: Argument -> Type
                    // Inferred: Variable -> Type
                    //
                    // Given
                    //   Variable -> Type <: Argument -> Type
                    //
                    // Therefore
                    //   [Argument <: Variable, Type <: Type]
                    let kind = variable.kind.map_or(argument, |kind| {
                        let kind = convert::type_to_core(state, context, kind);
                        let valid = unification::subsumes(state, context, argument, kind);
                        if valid { kind } else { context.prim.unknown }
                    });

                    let name = variable.name.clone().unwrap_or(MISSING_NAME);
                    (variable.id, variable.visible, name, kind)
                });

                let kinds = kinds.collect_vec();

                let kind_variables = signature.variables;
                let result_kind = signature.result;
                let type_variables = kinds.into_iter().map(|(id, visible, name, kind)| {
                    let level = state.bind_forall(id, kind);
                    ForallBinder { visible, name, level, kind }
                });

                (kind_variables, type_variables.collect_vec(), result_kind)
            } else {
                let kind_variables = vec![];
                let result_kind = context.prim.t;
                let type_variables = variables.iter().map(|variable| {
                    let kind = match variable.kind {
                        Some(id) => convert::type_to_core(state, context, id),
                        None => state.fresh_unification_type(context),
                    };

                    let visible = variable.visible;
                    let name = variable.name.clone().unwrap_or(MISSING_NAME);
                    let level = state.bind_forall(variable.id, kind);
                    ForallBinder { visible, name, level, kind }
                });

                (kind_variables, type_variables.collect_vec(), result_kind)
            };

            let data_reference = {
                let size = state.bound.size();
                let reference_type = state.storage.intern(Type::Constructor(context.id, item_id));
                type_variables.iter().cloned().fold(reference_type, |reference_type, variable| {
                    let Some(index) = variable.level.to_index(size) else {
                        let level = variable.level;
                        unreachable!("invariant violated: invalid {level} for {size}");
                    };

                    let variable = Variable::Bound(index);
                    let variable = state.storage.intern(Type::Variable(variable));

                    state.storage.intern(Type::Application(reference_type, variable))
                })
            };

            for item_id in context.indexed.pairs.data_constructors(item_id) {
                let Some(TermItemIr::Constructor { arguments }) =
                    context.lowered.info.get_term_item(item_id)
                else {
                    continue;
                };

                let arguments = arguments.iter().map(|&argument| {
                    let (inferred_type, _) =
                        kind::check_surface_kind(state, context, argument, context.prim.t);
                    inferred_type
                });

                let arguments = arguments.collect_vec();

                let constructor_type =
                    arguments.into_iter().rfold(data_reference, |result, argument| {
                        state.storage.intern(Type::Function(argument, result))
                    });

                let all_variables = {
                    let from_kind = kind_variables.iter();
                    let from_type = type_variables.iter();
                    from_kind.chain(from_type).cloned()
                };

                let constructor_type = all_variables.rfold(constructor_type, |inner, variable| {
                    state.storage.intern(Type::Forall(variable, inner))
                });

                if let Some(pending_type) = state.binding_group.terms.get(&item_id) {
                    unification::unify(state, context, *pending_type, constructor_type);
                } else {
                    state.binding_group.terms.insert(item_id, constructor_type);
                }
            }

            let type_kind = {
                let type_kind = type_variables.iter().rfold(result_kind, |result, variable| {
                    state.storage.intern(Type::Function(variable.kind, result))
                });
                kind_variables.iter().cloned().rfold(type_kind, |inner, binder| {
                    state.storage.intern(Type::Forall(binder, inner))
                })
            };

            if let Some(pending_kind) = state.binding_group.types.get(&item_id) {
                unification::unify(state, context, *pending_kind, type_kind);
            } else {
                state.binding_group.types.insert(item_id, type_kind);
            };
        }

        TypeItemIr::NewtypeGroup { .. } => (),

        TypeItemIr::SynonymGroup { .. } => (),

        TypeItemIr::ClassGroup { .. } => (),

        TypeItemIr::Foreign { signature, .. } => {
            let Some(signature_id) = signature else { return };
            let (inferred_type, _) =
                kind::check_surface_kind(state, context, *signature_id, context.prim.t);
            let inferred_type = transfer::globalize(state, context, inferred_type);
            state.checked.types.insert(item_id, inferred_type);
        }

        TypeItemIr::Operator { .. } => (),
    }
}

fn prim_check_module(
    queries: &impl ExternalQueries,
    file_id: FileId,
) -> QueryResult<CheckedModule> {
    let mut checked_module = CheckedModule::default();
    let resolved = queries.resolved(file_id)?;

    let lookup_type = |name: &str| {
        let prim_type = resolved.exports.lookup_type(name);
        prim_type.unwrap_or_else(|| unreachable!("invariant violated: {name} not in Prim"))
    };

    let type_core = {
        let (file_id, item_id) = lookup_type("Type");
        queries.intern_type(Type::Constructor(file_id, item_id))
    };

    let row_core = {
        let (file_id, item_id) = lookup_type("Row");
        queries.intern_type(Type::Constructor(file_id, item_id))
    };

    let constraint_core = {
        let (file_id, item_id) = lookup_type("Constraint");
        queries.intern_type(Type::Constructor(file_id, item_id))
    };

    let type_to_type_core = queries.intern_type(Type::Function(type_core, type_core));
    let function_core = queries.intern_type(Type::Function(type_core, type_to_type_core));

    let row_type_core = queries.intern_type(Type::Application(row_core, type_core));
    let record_core = queries.intern_type(Type::Function(row_type_core, type_core));

    let mut insert_type = |name: &str, id: TypeId| {
        let (_, item_id) = lookup_type(name);
        checked_module.types.insert(item_id, id)
    };

    insert_type("Type", type_core);
    insert_type("Function", function_core);
    insert_type("Array", type_to_type_core);
    insert_type("Record", record_core);
    insert_type("Number", type_core);
    insert_type("Int", type_core);
    insert_type("String", type_core);
    insert_type("Char", type_core);
    insert_type("Boolean", type_core);
    insert_type("Partial", constraint_core);
    insert_type("Constraint", type_core);
    insert_type("Symbol", type_core);
    insert_type("Row", type_to_type_core);

    let proxy_core = {
        let variable = queries.intern_type(Type::Variable(Variable::Bound(debruijn::Index(0))));
        let function = queries.intern_type(Type::Function(variable, type_core));

        let forall = queries.intern_type(Type::Forall(
            ForallBinder {
                visible: false,
                name: "t".into(),
                level: debruijn::Level(1),
                kind: variable,
            },
            function,
        ));

        queries.intern_type(Type::Forall(
            ForallBinder {
                visible: false,
                name: "k".into(),
                level: debruijn::Level(0),
                kind: type_core,
            },
            forall,
        ))
    };

    insert_type("Proxy", proxy_core);

    Ok(checked_module)
}
