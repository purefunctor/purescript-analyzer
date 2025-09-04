//! Creates binary operator trees from operator chains.
//!
//! There are no built-in binary operators in PureScript, as they are defined
//! in-source for existing binary functions. Consequently, this feature makes
//! it nearly impossible for a PureScript parser to determine associativity
//! and precedence for operators.
//!
//! This module implements the 'bracketing' algorithm, which turns operator
//! chains into proper binary operator trees. The algorithm is implemented
//! as a [pratt parser], and it's surprisingly quite simple too!
//!
//! ```text
//! 1 + 2 * 3 + 4
//!
//! 1, [(+ 2), (* 3), (+ 4)]
//!
//! ((1 + (2 * 3)) + 4)
//! ```
//!
//! [pratt parser]: https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html

use std::{iter::Peekable, sync::Arc};

use building_types::QueryResult;
use files::FileId;
use indexing::{TermItemId, TypeItemId};
use lowering::{
    Associativity, BinderId, BinderKind, ExpressionId, ExpressionKind, FullLoweredModule,
    IsElement, OperatorPair, TermItemIr, TermOperatorId, TypeId, TypeItemIr, TypeKind,
    TypeOperatorId,
};
use rustc_hash::FxHashMap;

// section: utilities

/// Helper trait for [`TermOperatorId`]  and [`TypeOperatorId`].
trait ForOperatorId {
    type ItemId;

    fn resolve_operator(lowered: &FullLoweredModule, id: Self) -> Option<(FileId, Self::ItemId)>;

    fn operator_info(lowered: &FullLoweredModule, id: Self::ItemId) -> Option<(Associativity, u8)>;
}

impl ForOperatorId for TermOperatorId {
    type ItemId = TermItemId;

    fn resolve_operator(lowered: &FullLoweredModule, id: Self) -> Option<(FileId, Self::ItemId)> {
        lowered.intermediate.index_term_operator(id).copied()
    }

    fn operator_info(lowered: &FullLoweredModule, id: Self::ItemId) -> Option<(Associativity, u8)> {
        let Some(TermItemIr::Operator { associativity, precedence }) =
            lowered.intermediate.index_term_item(id)
        else {
            return None;
        };
        associativity.zip(*precedence)
    }
}

impl ForOperatorId for TypeOperatorId {
    type ItemId = TypeItemId;

    fn resolve_operator(lowered: &FullLoweredModule, id: Self) -> Option<(FileId, Self::ItemId)> {
        lowered.intermediate.index_type_operator(id).copied()
    }

    fn operator_info(lowered: &FullLoweredModule, id: Self::ItemId) -> Option<(Associativity, u8)> {
        let Some(TypeItemIr::Operator { associativity, precedence }) =
            lowered.intermediate.index_type_item(id)
        else {
            return None;
        };
        associativity.zip(*precedence)
    }
}

/// Resolves an operator and its associativity and precedence.
fn operator_info<OperatorId>(
    external: &impl crate::External,
    lowered: &FullLoweredModule,
    id: OperatorId,
) -> Option<(Associativity, u8)>
where
    OperatorId: ForOperatorId,
{
    let (file_id, term_id) = OperatorId::resolve_operator(lowered, id)?;
    let lowered = external.lowered(file_id).ok()?;
    OperatorId::operator_info(&lowered, term_id)
}

/// Translates [`Associativity`] and precedence into binding power.
fn binding_power(associativity: Associativity, precedence: u8) -> (u8, u8) {
    let bp_0 = precedence.saturating_add(1);
    let bp_1 = precedence.saturating_add(2);
    match associativity {
        Associativity::None => (bp_0, bp_0),
        Associativity::Left => (bp_0, bp_1),
        Associativity::Right => (bp_1, bp_0),
    }
}

// section: algorithm

/// Common entry point for bracketing.
fn bracket<Id>(
    external: &impl crate::External,
    lowered: &FullLoweredModule,
    item: Option<Id>,
    items: &[OperatorPair<Id>],
) -> BracketingResult<Id>
where
    Id: IsElement,
    Id::OperatorId: ForOperatorId,
{
    match items {
        [OperatorPair { id, element }] => {
            let id = id.ok_or(BracketingError::InvalidOperator)?;
            Ok(OperatorTree::Branch(
                id,
                [OperatorTree::Leaf(item), OperatorTree::Leaf(*element)].into(),
            ))
        }
        _ => {
            let mut items = items.iter().copied().peekable();
            bracket_loop(external, lowered, item, &mut items, 0, None)
        }
    }
}

/// Core pratt parsing loop for bracketing.
fn bracket_loop<Id>(
    external: &impl crate::External,
    lowered: &FullLoweredModule,
    item: Option<Id>,
    items: &mut Peekable<impl Iterator<Item = OperatorPair<Id>>>,
    minimum_binding_power: u8,
    previous_operator: Option<OperatorInfo<Id::OperatorId>>,
) -> BracketingResult<Id>
where
    Id: IsElement,
    Id::OperatorId: ForOperatorId,
{
    let mut left = OperatorTree::Leaf(item);

    loop {
        let Some(OperatorPair { id, element }) = items.peek().copied() else {
            break;
        };

        let id = id.ok_or(BracketingError::InvalidOperator)?;

        let (associativity, precedence) =
            operator_info(external, lowered, id).ok_or(BracketingError::FailedToResolve(id))?;

        let operator = OperatorInfo { id, associativity, precedence };

        let (left_binding_power, right_binding_power) = binding_power(associativity, precedence);
        if left_binding_power < minimum_binding_power {
            break;
        }

        if let Some(previous) = previous_operator
            && associativity == Associativity::None
            && previous.associativity == Associativity::None
        {
            return Err(BracketingError::NonAssociative { previous, operator });
        } else if let Some(previous) = previous_operator
            && previous.associativity != associativity
            && previous.precedence == precedence
        {
            return Err(BracketingError::MixedAssociativity { previous, operator });
        }

        items.next();

        let right =
            bracket_loop(external, lowered, element, items, right_binding_power, Some(operator))?;

        left = OperatorTree::Branch(id, [left, right].into());
    }

    Ok(left)
}

// section: types

/// An operator tree, the result of bracketing.
#[derive(Debug, PartialEq, Eq)]
pub enum OperatorTree<Id: IsElement> {
    Branch(Id::OperatorId, Arc<[OperatorTree<Id>]>),
    Leaf(Option<Id>),
}

/// Errors that can occur during bracketing.
#[derive(Debug, PartialEq, Eq)]
pub enum BracketingError<T> {
    /// Likely a malformed operator.
    InvalidOperator,
    /// The operator does not exist.
    FailedToResolve(T),
    /// Non-associative operators used consecutively.
    NonAssociative { previous: OperatorInfo<T>, operator: OperatorInfo<T> },
    /// Operators with the same precedence but different associativities.
    MixedAssociativity { previous: OperatorInfo<T>, operator: OperatorInfo<T> },
}

/// Utility structure for bundling operator information.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct OperatorInfo<T> {
    pub id: T,
    pub associativity: Associativity,
    pub precedence: u8,
}

pub type BracketingResult<Id> =
    Result<OperatorTree<Id>, BracketingError<<Id as IsElement>::OperatorId>>;

pub type BracketingMap<Id> = FxHashMap<Id, BracketingResult<Id>>;

// section: top-level

/// The result of bracketing across all operator chains in a module.
#[derive(Debug, PartialEq, Eq)]
pub struct Bracketed {
    pub binders: BracketingMap<BinderId>,
    pub expressions: BracketingMap<ExpressionId>,
    pub types: BracketingMap<TypeId>,
}

/// Performs bracketing across all operator chains in a module.
pub fn bracketed(
    external: &impl crate::External,
    lowered: &FullLoweredModule,
) -> QueryResult<Bracketed> {
    let mut binders = FxHashMap::default();
    for (id, kind) in lowered.intermediate.iter_binder() {
        if let BinderKind::OperatorChain { head, tail } = kind {
            binders.insert(id, bracket(external, lowered, *head, tail));
        }
    }

    let mut expressions = FxHashMap::default();
    for (id, kind) in lowered.intermediate.iter_expression() {
        if let ExpressionKind::OperatorChain { head, tail } = kind {
            expressions.insert(id, bracket(external, lowered, *head, tail));
        }
    }

    let mut types = FxHashMap::default();
    for (id, kind) in lowered.intermediate.iter_type() {
        if let TypeKind::OperatorChain { head, tail } = kind {
            types.insert(id, bracket(external, lowered, *head, tail));
        }
    }

    Ok(Bracketed { binders, expressions, types })
}
