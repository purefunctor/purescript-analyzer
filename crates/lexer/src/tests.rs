macro_rules! grammar_tests {
    ($($name:ident => $source:expr),* $(,)?) => {
        $(
            #[test]
            fn $name() {
                insta::with_settings!({omit_expression => true}, {
                    let mut input = winnow::Located::new($source);
                    insta::assert_debug_snapshot!(crate::grammar::tokens(&mut input));
                })
            }
        )*
    }
}

grammar_tests!(
    hole_source => "?help ?Help",
    upper_source => "Works Works123 Works'123 Works_123",
    lower_source => "works works123 works'123 works_123 _123 _'123",
    lower_keywords => "ado as case class data derive do else false foreign hiding if import in infix infixl infixr instance let module newtype of then true type where",
    qualified_upper => "Module.Name Qualified.Type",
    operator_purs => "<- ← -> → => ⇒ :: ∷ ∀ = . \\ | @",
    operator_source => "=>> >>= >=> && || : ++",
    string_regular => "\"this is a string\"",
    string_escape => "\"\\t \\r \\n \\\" \\' \\\\ \\xFFFFFF\"",
    string_space_escape => r#"
        " hello \
        \ world " 
    "#.trim(),
    raw_string => "\"\"\"this is a string\"\"\"",
    raw_string_empty => "\"\"\"\"\"\"",
    raw_string_single_start => "\"\"\"\" \"\"\"",
    raw_string_double_start => "\"\"\"\"\" \"\"\"",
    raw_string_single_end => "\"\"\" \"\"\"\"",
    raw_string_double_end => "\"\"\" \"\"\"\"\"",
    raw_string_then_string => "\"\"\"\"\"\"\"\"\"\"",
    integer_leading_zero => "0",
    integer_underscores => "1_000_000",
    number => "42.0",
    number_exponent => "42e10",
    number_exponent_negative => "42e-10",
    number_exponent_positive => "42e+10",
    number_fraction_exponent => "42.0e10",
    number_fraction_exponent_negative => "42.0e-10",
    number_fraction_exponent_positive => "42.0e+10",
    character => "'c'",
    character_escape => "'\\t' '\\r' '\\n' '\\\"' '\\'' '\\\\' '\\x00'",
    bracket => "()[]{}",
    reserved => "` ,",
    symbol => "(+) (-) (*) (/)",
    line_comment => "-- this is a comment\n-- this is another comment",
    block_comment => "{- this is a comment -}\n{- {- this is\nanother comment -} -}",
    whitespace => " \n \t",
    end_of_file => "",
);
