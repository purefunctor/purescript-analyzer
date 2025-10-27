use std::marker::PhantomData;
use std::num::NonZeroU32;
use std::{any, fmt, hash};

use rowan::ast::{AstNode, AstPtr};
use syntax::PureScript;

pub struct AstId<N: AstNode<Language = PureScript>> {
    pub(crate) id: NonZeroU32,
    phantom: PhantomData<fn() -> AstPtr<N>>,
}

impl<N: AstNode<Language = PureScript>> AstId<N> {
    pub const fn new(id: NonZeroU32) -> AstId<N> {
        AstId { id, phantom: PhantomData }
    }
}

impl<N: AstNode<Language = PureScript>> fmt::Debug for AstId<N> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("AstId<{}>({})", any::type_name::<N>(), self.id))
    }
}

impl<N: AstNode<Language = PureScript>> Clone for AstId<N> {
    fn clone(&self) -> AstId<N> {
        *self
    }
}

impl<N: AstNode<Language = PureScript>> Copy for AstId<N> {}

impl<N: AstNode<Language = PureScript>> PartialEq for AstId<N> {
    fn eq(&self, other: &AstId<N>) -> bool {
        self.id == other.id
    }
}

impl<N: AstNode<Language = PureScript>> Eq for AstId<N> {}

impl<N: AstNode<Language = PureScript>> hash::Hash for AstId<N> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}
