use std::{iter::Peekable, sync::Arc};

use building_types::QueryResult;
use files::FileId;
use indexing::FullIndexedModule;
use lowering::{
    Associativity, ExpressionId, ExpressionKind, FullLoweredModule, OperatorPair, TermItemIr,
    TermOperatorId,
};
use resolving::FullResolvedModule;
use rustc_hash::FxHashMap;

pub trait External {
    fn indexed(&self, id: FileId) -> QueryResult<Arc<FullIndexedModule>>;

    fn resolved(&self, id: FileId) -> QueryResult<Arc<FullResolvedModule>>;

    fn lowered(&self, id: FileId) -> QueryResult<Arc<FullLoweredModule>>;

    fn prim_id(&self) -> FileId;
}

struct Context<'c> {
    lowered: &'c FullLoweredModule,
}

pub fn bracketed<E>(
    external: &E,
    id: FileId,
) -> QueryResult<FxHashMap<ExpressionId, Result<Tree, BracketError>>>
where
    E: External,
{
    let lowered = external.lowered(id)?;
    let context = Context { lowered: &lowered };

    let mut expressions = FxHashMap::default();
    for (id, expression) in lowered.intermediate.iter_expression() {
        if let ExpressionKind::OperatorChain { head, tail } = expression {
            expressions.insert(id, rebracket(external, &context, *head, tail));
        }
    }

    Ok(expressions)
}

fn resolve_operator<E>(
    external: &E,
    context: &Context,
    id: TermOperatorId,
) -> Option<(Associativity, u8)>
where
    E: External,
{
    let (file_id, term_id) = context.lowered.intermediate.index_term_operator(id)?;

    let lowered = external.lowered(*file_id).ok()?;
    let Some(TermItemIr::Operator { associativity, precedence, .. }) =
        lowered.intermediate.index_term_item(*term_id)
    else {
        return None;
    };

    associativity.zip(*precedence)
}

fn rebracket<E>(
    external: &E,
    context: &Context,
    head: Option<ExpressionId>,
    tail: &[OperatorPair<TermOperatorId, ExpressionId>],
) -> Result<Tree, BracketError>
where
    E: External,
{
    match tail {
        [OperatorPair { id, element }] => {
            let id = id.ok_or(BracketError::InvalidOperatorPair)?;
            Ok(Tree::Branch(id, vec![Tree::Leaf(head), Tree::Leaf(*element)]))
        }
        _ => {
            let mut tail = tail.iter().copied().peekable();
            rebracket_core(external, context, head, &mut tail, 0, None)
        }
    }
}

fn rebracket_core<E, I>(
    external: &E,
    context: &Context,
    head: Option<ExpressionId>,
    tail: &mut Peekable<I>,
    minimum_binding_power: u8,
    previous_operator: Option<Operator>,
) -> Result<Tree, BracketError>
where
    E: External,
    I: Iterator<Item = OperatorPair<TermOperatorId, ExpressionId>>,
{
    let mut left = Tree::Leaf(head);

    loop {
        let Some(OperatorPair { id, element }) = tail.peek().copied() else {
            break;
        };

        let id = id.ok_or(BracketError::InvalidOperatorPair)?;

        let (associativity, precedence) =
            resolve_operator(external, context, id).ok_or(BracketError::FailedToResolve(id))?;

        let operator = Operator { id, associativity, precedence };

        let (left_binding_power, right_binding_power) = binding_power(associativity, precedence);
        if left_binding_power < minimum_binding_power {
            break;
        }

        if let Some(previous) = previous_operator
            && associativity == Associativity::None
            && previous.associativity == Associativity::None
        {
            return Err(BracketError::NonAssociative { previous, operator });
        } else if let Some(previous) = previous_operator
            && previous.associativity != associativity
            && previous.precedence == precedence
        {
            return Err(BracketError::MixedAssociativity { previous, operator });
        }

        tail.next();

        let right = rebracket_core(
            external,
            context,
            element,
            tail,
            right_binding_power,
            Some(Operator { id, associativity, precedence }),
        )?;

        left = Tree::Branch(id, vec![left, right]);
    }

    Ok(left)
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

#[derive(Debug, Clone, Copy)]
pub struct Operator {
    pub id: TermOperatorId,
    pub associativity: Associativity,
    pub precedence: u8,
}

#[derive(Debug, Clone, Copy)]
pub enum BracketError {
    InvalidOperatorPair,
    FailedToResolve(TermOperatorId),
    NonAssociative { previous: Operator, operator: Operator },
    MixedAssociativity { previous: Operator, operator: Operator },
}

#[derive(Debug)]
pub enum Tree {
    Branch(TermOperatorId, Vec<Tree>),
    Leaf(Option<ExpressionId>),
}
