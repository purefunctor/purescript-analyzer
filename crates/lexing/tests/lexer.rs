macro_rules! lexer_tests {
    ($($name:ident => $source:expr),+ $(,)?) => {
        $(
            #[test]
            fn $name() {
                let lexed = lexing::lex($source);
                let tokens = lexed.kinds();
                insta::assert_debug_snapshot!(tokens);
            }
        )+
    };
}

lexer_tests!(
    keyword => "ado as case class data derive do else false foreign hiding if import in infix infixl infixr instance let module newtype of then true type where",
    prefixed => "Hooks.do Main.main List.Cons",
    operator_purs => "<- ← -> → => ⇒ :: ∷ ∀ = . \\ | @",
    operator_source => "=>> >>= >=> && || : ++",
    integer_leading_zero => "0",
    integer_underscores => "1_000_000",
    integer_hex => "0xFF",
    integer_hex_detached => "0xFF_FF",
    number => "42.0",
    number_exponent => "42e10",
    number_exponent_negative => "42e-10",
    number_exponent_positive => "42e+10",
    number_fraction_exponent => "42.0e10",
    number_fraction_exponent_negative => "42.0e-10",
    number_fraction_exponent_positive => "42.0e+10",
    leading_zero_consumes_token => "000_000.000_000e+42 module",
    bracket => "()[]{}",
    reserved => "` ,",
    line_comment => "-- this is a comment\n-- this is another comment",
    block_comment => "{- this is a comment -}\n{- {- this is\nanother comment -} -}",
    whitespace => " \n \t",
    end_of_file => "",
);

lexer_tests!(
    lenient_char_single => "'a'",
    lenient_char_double => "'ab'",
    lenient_char_escape => "'\\'",
    lenient_char_hex => "'\\xABC'"
);

lexer_tests!(
    lenient_string_empty => "\"\"",
    lenient_string_escape => "\"\\t \\r \\n \\xABC \\z \\x \\c\"",
    lenient_string_gap => "\"hello \\ valid \\ world\"",
    lenient_raw_string_empty => "\"\"\"\"\"\"",
    lenient_raw_string_single_start => "\"\"\"\" \"\"\"",
    lenient_raw_string_double_start => "\"\"\"\"\" \"\"\"",
    lenient_raw_string_single_end => "\"\"\" \"\"\"\"",
    lenient_raw_string_double_end => "\"\"\" \"\"\"\"\"",
    lenient_raw_string_then_string => "\"\"\"\"\"\"\"\"\"\"",
);

lexer_tests!(
    operator_name_glued => "(->) (=>) (::)",
    operator_name_non_glue_type_var => "(@a :: Type)",
    operator_name_non_glue_type_int => "(-1)",
    double_period_operator_name => ".. (..)"
);

lexer_tests!(
    hole_glued => "?hole ?Hole ?_hole ?_Hole ?'hole ?'Hole ?hole' ?Hole' ?_hole' ?_Hole'",
    hole_unglued => "? hole",
    hole_operator => "?-> ?=> ?~",
);
