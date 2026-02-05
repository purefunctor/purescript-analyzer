use std::sync::Arc;

use building_types::QueryResult;
use files::FileId;
use indexing::TypeItemId;
use itertools::izip;

use crate::algorithm::constraint::{self, MatchInstance};
use crate::algorithm::safety::safe_loop;
use crate::algorithm::state::{CheckContext, CheckState};
use crate::algorithm::{derive, kind, substitute, toolkit};
use crate::core::Role;
use crate::{ExternalQueries, Type, TypeId};

enum NewtypeCoercionResult {
    Success(MatchInstance),
    ConstructorNotInScope { file_id: FileId, item_id: TypeItemId },
    NotApplicable,
}

pub fn prim_coercible<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    arguments: &[TypeId],
) -> QueryResult<Option<MatchInstance>>
where
    Q: ExternalQueries,
{
    let &[left, right] = arguments else {
        return Ok(None);
    };

    let left = state.normalize_type(left);
    let right = state.normalize_type(right);

    if left == right {
        return Ok(Some(MatchInstance::Match { constraints: vec![], equalities: vec![] }));
    }

    if is_unification_head(state, left) || is_unification_head(state, right) {
        return Ok(Some(MatchInstance::Stuck));
    }

    let newtype_result = try_newtype_coercion(state, context, left, right)?;
    if let NewtypeCoercionResult::Success(result) = newtype_result {
        return Ok(Some(result));
    }

    if let Some(result) = try_application_coercion(state, context, left, right)? {
        return Ok(Some(result));
    }

    if let Some(result) = try_higher_kinded_coercion(state, context, left, right)? {
        return Ok(Some(result));
    }

    if let Some(result) = try_row_coercion(state, context, left, right) {
        return Ok(Some(result));
    }

    if let NewtypeCoercionResult::ConstructorNotInScope { file_id, item_id } = newtype_result {
        state.insert_error(crate::error::ErrorKind::CoercibleConstructorNotInScope {
            file_id,
            item_id,
        });
    }

    Ok(Some(MatchInstance::Apart))
}

fn is_unification_head(state: &mut CheckState, mut type_id: TypeId) -> bool {
    loop {
        type_id = state.normalize_type(type_id);
        match state.storage[type_id] {
            Type::Unification(_) => return true,
            Type::Application(function, _) | Type::KindApplication(function, _) => {
                type_id = function;
            }
            _ => return false,
        }
    }
}

fn try_newtype_coercion<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    left: TypeId,
    right: TypeId,
) -> QueryResult<NewtypeCoercionResult>
where
    Q: ExternalQueries,
{
    let mut hidden_newtype: Option<(FileId, TypeItemId)> = None;

    if has_type_kind(state, context, left)?
        && let Some((file_id, type_id)) = derive::extract_type_constructor(state, left)
        && is_newtype(context, file_id, type_id)?
    {
        if is_constructor_in_scope(context, file_id, type_id)? {
            let (inner, _) = derive::get_newtype_inner(state, context, file_id, type_id, left)?;
            let constraint = make_coercible_constraint(state, context, inner, right);
            return Ok(NewtypeCoercionResult::Success(MatchInstance::Match {
                constraints: vec![constraint],
                equalities: vec![],
            }));
        } else {
            hidden_newtype = Some((file_id, type_id));
        }
    }

    if has_type_kind(state, context, right)?
        && let Some((file_id, type_id)) = derive::extract_type_constructor(state, right)
        && is_newtype(context, file_id, type_id)?
    {
        if is_constructor_in_scope(context, file_id, type_id)? {
            let (inner, _) = derive::get_newtype_inner(state, context, file_id, type_id, right)?;
            let constraint = make_coercible_constraint(state, context, left, inner);
            return Ok(NewtypeCoercionResult::Success(MatchInstance::Match {
                constraints: vec![constraint],
                equalities: vec![],
            }));
        } else if hidden_newtype.is_none() {
            hidden_newtype = Some((file_id, type_id));
        }
    }

    if let Some((file_id, item_id)) = hidden_newtype {
        return Ok(NewtypeCoercionResult::ConstructorNotInScope { file_id, item_id });
    }

    Ok(NewtypeCoercionResult::NotApplicable)
}

fn has_type_kind<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    type_id: TypeId,
) -> QueryResult<bool>
where
    Q: ExternalQueries,
{
    let kind = kind::elaborate_kind(state, context, type_id)?;
    let kind = state.normalize_type(kind);
    Ok(kind == context.prim.t)
}

fn is_newtype<Q>(
    context: &CheckContext<Q>,
    file_id: FileId,
    type_id: TypeItemId,
) -> QueryResult<bool>
where
    Q: ExternalQueries,
{
    let is_newtype = if file_id == context.id {
        matches!(context.indexed.items[type_id].kind, indexing::TypeItemKind::Newtype { .. })
    } else {
        let indexed = context.queries.indexed(file_id)?;
        matches!(indexed.items[type_id].kind, indexing::TypeItemKind::Newtype { .. })
    };
    Ok(is_newtype)
}

fn is_constructor_in_scope<Q>(
    context: &CheckContext<Q>,
    file_id: FileId,
    item_id: TypeItemId,
) -> QueryResult<bool>
where
    Q: ExternalQueries,
{
    let constructor_term_id = if file_id == context.id {
        context.indexed.pairs.data_constructors(item_id).next()
    } else {
        let indexed = context.queries.indexed(file_id)?;
        indexed.pairs.data_constructors(item_id).next()
    };

    let Some(constructor_term_id) = constructor_term_id else {
        return Ok(false);
    };

    Ok(context.resolved.is_term_in_scope(&context.prim_resolved, file_id, constructor_term_id))
}

fn try_application_coercion<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    left: TypeId,
    right: TypeId,
) -> QueryResult<Option<MatchInstance>>
where
    Q: ExternalQueries,
{
    let Some((left_file, left_id)) = derive::extract_type_constructor(state, left) else {
        return Ok(None);
    };
    let Some((right_file, right_id)) = derive::extract_type_constructor(state, right) else {
        return Ok(None);
    };

    if left_file != right_file || left_id != right_id {
        return Ok(None);
    }

    let (_, left) = toolkit::extract_type_application(state, left);
    let (_, right) = toolkit::extract_type_application(state, right);

    if left.len() != right.len() {
        return Ok(Some(MatchInstance::Apart));
    }

    let Some(roles) = lookup_roles_for_type(state, context, left_file, left_id)? else {
        return Ok(Some(MatchInstance::Stuck));
    };

    debug_assert_eq!(roles.len(), left.len(), "critical failure: mismatched lengths");
    debug_assert_eq!(roles.len(), right.len(), "critical failure: mismatched lengths");

    let mut constraints = vec![];
    let mut equalities = vec![];

    for (role, &left, &right) in izip!(&*roles, &left, &right) {
        match role {
            Role::Phantom => (),
            Role::Representational => {
                let constraint = make_coercible_constraint(state, context, left, right);
                constraints.push(constraint);
            }
            Role::Nominal => {
                if left != right {
                    if constraint::can_unify(state, left, right).is_apart() {
                        return Ok(Some(MatchInstance::Apart));
                    }
                    equalities.push((left, right));
                }
            }
        }
    }

    Ok(Some(MatchInstance::Match { constraints, equalities }))
}

fn lookup_roles_for_type<Q>(
    state: &CheckState,
    context: &CheckContext<Q>,
    file_id: FileId,
    type_id: TypeItemId,
) -> QueryResult<Option<Arc<[Role]>>>
where
    Q: ExternalQueries,
{
    if file_id == context.id {
        Ok(state.checked.lookup_roles(type_id))
    } else {
        let checked = context.queries.checked(file_id)?;
        Ok(checked.lookup_roles(type_id))
    }
}

fn try_row_coercion<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    left: TypeId,
    right: TypeId,
) -> Option<MatchInstance>
where
    Q: ExternalQueries,
{
    let Type::Row(left_row) = &state.storage[left] else { return None };
    let Type::Row(right_row) = &state.storage[right] else { return None };

    let left_row = left_row.clone();
    let right_row = right_row.clone();

    if left_row.fields.len() != right_row.fields.len() {
        return Some(MatchInstance::Apart);
    }

    let mut constraints = vec![];

    for (left_field, right_field) in izip!(&*left_row.fields, &*right_row.fields) {
        if left_field.label != right_field.label {
            return Some(MatchInstance::Apart);
        }
        let constraint = make_coercible_constraint(state, context, left_field.id, right_field.id);
        constraints.push(constraint);
    }

    match (left_row.tail, right_row.tail) {
        (None, None) => (),
        (Some(left_tail), Some(right_tail)) => {
            let constraint = make_coercible_constraint(state, context, left_tail, right_tail);
            constraints.push(constraint);
        }
        (None, Some(_)) | (Some(_), None) => {
            return Some(MatchInstance::Apart);
        }
    }

    Some(MatchInstance::Match { constraints, equalities: vec![] })
}

fn try_higher_kinded_coercion<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    left: TypeId,
    right: TypeId,
) -> QueryResult<Option<MatchInstance>>
where
    Q: ExternalQueries,
{
    // Let's say we're attempting to coerce the following types:
    //
    // data Maybe :: forall k. k -> Type -> Type
    // data Maybe n a = Just a | Nothing
    //
    // newtype MaybeAlias :: forall k. k -> Type -> Type
    // newtype MaybeAlias n a = MaybeAlias (Maybe n a)
    //
    // solve[Coercible Maybe MaybeAlias]
    //
    // In order to solve coercion for higher-kinded types like
    // this, we need to be able to solve the following coercion.
    //
    // solve[Coercible (Maybe ~a) (MaybeAlias ~a)]
    //
    // To begin, we get the kinds of these types
    //
    // left_kind  := forall k. k -> Type -> Type
    // right_kind := forall k. k -> Type -> Type
    let left_kind = kind::elaborate_kind(state, context, left)?;
    let right_kind = kind::elaborate_kind(state, context, right)?;

    // decompose_kind_for_coercion instantiates the variables into
    // skolem variables, then returns the first argument, which in
    // this case is the already-skolemized `~k`
    //
    // left_kind_applied := Maybe @~k
    // left_domain       := ~k
    let Some((left_kind_applied, left_domain)) =
        decompose_kind_for_coercion(state, left, left_kind)
    else {
        return Ok(None);
    };

    // right_kind_applied := MaybeAlias @~k
    // right_domain       := ~k
    let Some((right_kind_applied, right_domain)) =
        decompose_kind_for_coercion(state, right, right_kind)
    else {
        return Ok(None);
    };

    if constraint::can_unify(state, left_domain, right_domain).is_apart() {
        return Ok(Some(MatchInstance::Apart));
    }

    // Given left_domain ~ right_domain, create a skolem kinded by `~k`
    let argument = state.fresh_skolem_kinded(left_domain);

    // Finally, we can saturated left_kind_applied and right_kind_applied
    //
    // left  := Maybe      @~k (~a :: ~k)
    // right := MaybeAlias @~k (~a :: ~k)
    //
    // Finally, we emit `left <~> right` as a constraint and rely on the
    // remaining rules to solve this for us, particularly newtype coercion.
    let left = state.storage.intern(Type::Application(left_kind_applied, argument));
    let right = state.storage.intern(Type::Application(right_kind_applied, argument));
    let constraint = make_coercible_constraint(state, context, left, right);

    Ok(Some(MatchInstance::Match { constraints: vec![constraint], equalities: vec![] }))
}

fn decompose_kind_for_coercion(
    state: &mut CheckState,
    mut type_id: TypeId,
    mut kind_id: TypeId,
) -> Option<(TypeId, TypeId)> {
    safe_loop! {
        kind_id = state.normalize_type(kind_id);

        let forall = match &state.storage[kind_id] {
            Type::Forall(binder, inner) => Some((binder.kind, binder.level, *inner)),
            Type::Function(domain, _) => return Some((type_id, *domain)),
            _ => return None,
        };

        if let Some((binder_kind, binder_level, inner_kind)) = forall {
            let fresh_kind = state.fresh_skolem_kinded(binder_kind);
            type_id = state.storage.intern(Type::KindApplication(type_id, fresh_kind));
            kind_id = substitute::SubstituteBound::on(state, binder_level, fresh_kind, inner_kind);
        }
    }
}

fn make_coercible_constraint<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    left: TypeId,
    right: TypeId,
) -> TypeId
where
    Q: ExternalQueries,
{
    let coerce = &context.prim_coerce;
    let coercible = state.storage.intern(Type::Constructor(coerce.file_id, coerce.coercible));

    let coercible = state.storage.intern(Type::Application(coercible, left));
    state.storage.intern(Type::Application(coercible, right))
}
