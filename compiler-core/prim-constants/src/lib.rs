macro_rules! define {
    ($($const:ident, $path:expr, $name:expr $(;)?)+) => {
        $(
            pub const $const: &str = include_str!($path);
        )+

        pub const MODULE_MAP: &[(&str, &str)] = &[
            $(
                ($name, $const),
            )+
        ];
    };
}

define!(
    PRIM, "prim/Prim.purs", "Prim";
    PRIM_BOOLEAN, "prim/Prim.Boolean.purs", "Prim.Boolean";
    PRIM_COERCE, "prim/Prim.Coerce.purs", "Prim.Coerce";
    PRIM_INT, "prim/Prim.Int.purs", "Prim.Int";
    PRIM_ORDERING, "prim/Prim.Ordering.purs", "Prim.Ordering";
    PRIM_ROW, "prim/Prim.Row.purs", "Prim.Row";
    PRIM_ROW_LIST, "prim/Prim.RowList.purs", "Prim.RowList";
    PRIM_SYMBOL, "prim/Prim.Symbol.purs", "Prim.Symbol";
    PRIM_TYPE_ERROR, "prim/Prim.TypeError.purs", "Prim.TypeError";
);
