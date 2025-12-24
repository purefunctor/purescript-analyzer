use std::collections::VecDeque;

use building_types::QueryResult;

use crate::algorithm::state::{CheckContext, CheckState};
use crate::core::pretty;
use crate::{ExternalQueries, TypeId};

pub fn solve_constraints<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    wanted: VecDeque<TypeId>,
    given: Vec<TypeId>,
) -> QueryResult<Vec<TypeId>>
where
    Q: ExternalQueries,
{
    println!("=== Wanted ===");
    for wanted in wanted {
        let wanted = pretty::print_local(state, context, wanted);
        println!("{wanted}");
    }

    println!("=== Given ===");
    for given in given {
        let given = pretty::print_local(state, context, given);
        println!("{given}");
    }

    Ok(vec![])
}
