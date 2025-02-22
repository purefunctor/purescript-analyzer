use la_arena::Idx;

pub enum Type {
    Constructor {},
}

pub type TypeId = Idx<Type>;
