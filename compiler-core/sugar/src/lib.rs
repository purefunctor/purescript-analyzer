use std::{iter::Peekable, sync::Arc};

use building_types::QueryResult;
use files::FileId;
use indexing::FullIndexedModule;
use lowering::{
    Associativity, DeferredResolutionId, Domain, ExpressionId, ExpressionKind, FullLoweredModule,
    OperatorPair, TermItemIr, TypeItemIr,
};
use resolving::FullResolvedModule;
use rustc_hash::FxHashMap;

pub trait External {
    fn indexed(&self, id: FileId) -> QueryResult<Arc<FullIndexedModule>>;

    fn resolved(&self, id: FileId) -> QueryResult<Arc<FullResolvedModule>>;

    fn lowered(&self, id: FileId) -> QueryResult<Arc<FullLoweredModule>>;

    fn prim_id(&self) -> FileId;
}

pub fn bracketed<E>(
    external: &E,
    id: FileId,
) -> QueryResult<FxHashMap<ExpressionId, Result<Tree, BracketError>>>
where
    E: External,
{
    let prim = {
        let id = external.prim_id();
        external.resolved(id)?
    };

    let resolved = external.resolved(id)?;
    let lowered = external.lowered(id)?;

    let operators = resolve_operators(external, &prim, &resolved, &lowered)?;
    let mut expressions = FxHashMap::default();

    for (id, expression) in lowered.intermediate.iter_expression() {
        if let ExpressionKind::OperatorChain { head, tail } = expression {
            expressions.insert(id, rebracket(&operators, *head, tail));
        }
    }

    Ok(expressions)
}

type OperatorMap = FxHashMap<DeferredResolutionId, (DeferredResolutionId, Associativity, u8)>;

fn resolve_operators<E>(
    external: &E,
    prim: &FullResolvedModule,
    resolved: &FullResolvedModule,
    lowered: &FullLoweredModule,
) -> QueryResult<OperatorMap>
where
    E: External,
{
    let mut operators = FxHashMap::default();

    for (id, deferred) in lowered.graph.deferred() {
        let prefix = deferred.qualifier.as_deref();
        let Some(name) = deferred.name.as_deref() else { continue };
        match deferred.domain {
            Domain::Term => {
                let Some((file_id, term_id)) = resolved.lookup_term(prim, prefix, name) else {
                    continue;
                };

                let lowered = external.lowered(file_id)?;

                let Some(TermItemIr::Operator { resolution, associativity, precedence }) =
                    lowered.intermediate.index_term_item(term_id)
                else {
                    continue;
                };

                if let Some((associativity, precedence)) = associativity.zip(*precedence) {
                    operators.insert(id, (*resolution, associativity, precedence));
                }
            }
            Domain::Type => {
                let Some((file_id, type_id)) = resolved.lookup_type(prim, prefix, name) else {
                    continue;
                };

                let lowered = external.lowered(file_id)?;

                let Some(TypeItemIr::Operator { resolution, associativity, precedence }) =
                    lowered.intermediate.index_type_item(type_id)
                else {
                    continue;
                };

                if let Some((associativity, precedence)) = associativity.zip(*precedence) {
                    operators.insert(id, (*resolution, associativity, precedence));
                }
            }
        }
    }

    Ok(operators)
}

fn binding_power(associativity: Associativity, precedence: u8) -> (u8, u8) {
    let bp_0 = precedence.saturating_add(1);
    let bp_1 = precedence.saturating_add(2);
    match associativity {
        Associativity::None => (bp_0, bp_0),
        Associativity::Left => (bp_0, bp_1),
        Associativity::Right => (bp_1, bp_0),
    }
}

#[derive(Debug)]
pub enum BracketError {
    FailedToResolve(DeferredResolutionId),
    NonAssociative,
    MixedAssociativity,
}

fn rebracket(
    operators: &OperatorMap,
    head: Option<ExpressionId>,
    tail: &[OperatorPair<ExpressionId>],
) -> Result<Tree, BracketError> {
    match tail {
        [OperatorPair { resolution, element }] => {
            let (resolution, _, _) =
                operators.get(resolution).ok_or(BracketError::FailedToResolve(*resolution))?;
            Ok(Tree::Branch(*resolution, vec![Tree::Leaf(head), Tree::Leaf(*element)]))
        }
        _ => {
            let mut tail = tail.iter().copied().peekable();
            rebracket_core(operators, head, &mut tail, 0, None)
        }
    }
}

fn rebracket_core<I>(
    operators: &OperatorMap,
    head: Option<ExpressionId>,
    tail: &mut Peekable<I>,
    minimum_binding_power: u8,
    previous_op: Option<(Associativity, u8)>,
) -> Result<Tree, BracketError>
where
    I: Iterator<Item = OperatorPair<ExpressionId>>,
{
    let mut left = Tree::Leaf(head);

    loop {
        let Some(OperatorPair { resolution, element }) = tail.peek().copied() else {
            break;
        };

        let (resolution, associativity, precedence) =
            operators.get(&resolution).copied().ok_or(BracketError::FailedToResolve(resolution))?;

        let (left_binding_power, right_binding_power) = binding_power(associativity, precedence);
        if left_binding_power < minimum_binding_power {
            break;
        }

        if previous_op.is_some_and(|(previous_associativity, _)| {
            previous_associativity == Associativity::None && associativity == Associativity::None
        }) {
            return Err(BracketError::NonAssociative);
        } else if previous_op.is_some_and(|(previous_associativity, previous_precedence)| {
            previous_associativity != associativity && previous_precedence == precedence
        }) {
            return Err(BracketError::MixedAssociativity);
        }

        tail.next();

        let right = rebracket_core(
            operators,
            element,
            tail,
            right_binding_power,
            Some((associativity, precedence)),
        )?;

        left = Tree::Branch(resolution, vec![left, right]);
    }

    Ok(left)
}

#[derive(Debug)]
pub enum Tree {
    Branch(DeferredResolutionId, Vec<Tree>),
    Leaf(Option<ExpressionId>),
}
