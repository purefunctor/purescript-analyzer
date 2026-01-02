import * as monaco from "monaco-editor";
import { catppuccinMacchiato, catppuccinLatte } from "../../themes/catppuccin";

let registered = false;

export function registerPureScript(): void {
  if (registered) return;
  registered = true;

  monaco.languages.register({ id: "purescript" });

  // Monarch tokenizer based on compiler-core/lexing/src/lexer.rs
  monaco.languages.setMonarchTokensProvider("purescript", {
    // Keywords from lexer.rs take_lower()
    keywords: [
      "ado",
      "as",
      "case",
      "class",
      "data",
      "derive",
      "do",
      "else",
      "false",
      "forall",
      "foreign",
      "hiding",
      "if",
      "import",
      "in",
      "infix",
      "infixl",
      "infixr",
      "instance",
      "let",
      "module",
      "newtype",
      "nominal",
      "of",
      "phantom",
      "representational",
      "role",
      "then",
      "true",
      "type",
      "where",
    ],

    // Operators from lexer.rs take_operator_kind()
    // Includes both ASCII and Unicode variants
    operators: [
      "=",
      ":",
      "::",
      ".",
      "..",
      "<-",
      "->",
      "<=",
      "=>",
      "|",
      "@",
      "-",
      "\\",
      // Unicode variants
      "\u2237", // ∷ DOUBLE_COLON
      "\u2190", // ← LEFT_ARROW
      "\u2192", // → RIGHT_ARROW
      "\u21D0", // ⇐ LEFT_THICK_ARROW
      "\u21D2", // ⇒ RIGHT_THICK_ARROW
      "\u2200", // ∀ FORALL
    ],

    // Operator characters from lexer.rs is_operator trait
    // PureScript operators: ! # $ % & * + - . / < = > ? @ \ ^ | ~ :
    // Plus Unicode category Symbol
    symbols: /[!#$%&*+\-./<=>?@\\^|~:]+/,

    // Character/string escapes - matches lexer escape handling
    escapes: /\\(?:[abfnrtv\\"'0]|x[0-9A-Fa-f]{1,4}|u[0-9A-Fa-f]{4}|U[0-9A-Fa-f]{8})/,

    // Name characters (after first char): alphanumeric, _, '
    nameChars: /[a-zA-Z0-9_']/,

    tokenizer: {
      root: [
        // Holes: ?holeName (must come before operators due to ?)
        [/\?[a-z_][a-zA-Z0-9_']*/, "variable.other"],

        // Qualified identifiers: Module.Name.identifier
        // Handle qualified upper (types/constructors)
        [/([A-Z][a-zA-Z0-9_']*\.)+[A-Z][a-zA-Z0-9_']*/, "type.identifier"],
        // Handle qualified lower (values)
        [
          /([A-Z][a-zA-Z0-9_']*\.)+[a-z_][a-zA-Z0-9_']*/,
          {
            cases: {
              "@keywords": "keyword",
              "@default": "identifier",
            },
          },
        ],
        // Handle qualified operator: Module.Operator.(+)
        [/([A-Z][a-zA-Z0-9_']*\.)+\([!#$%&*+\-./<=>?@\\^|~:]+\)/, "operator"],

        // Operator names in parentheses: (+), (..)
        [/\(\.\.\)/, "operator"], // special double period operator name
        [/\([!#$%&*+\-./<=>?@\\^|~:]+\)/, "operator"],

        // Type identifiers (start with uppercase)
        [/[A-Z][a-zA-Z0-9_']*/, "type.identifier"],

        // Identifiers and keywords (start with lowercase or _)
        [
          /[a-z_][a-zA-Z0-9_']*/,
          {
            cases: {
              "@keywords": "keyword",
              "@default": "identifier",
            },
          },
        ],

        // Wildcard underscore (standalone)
        [/(?<![a-zA-Z0-9_'])_(?![a-zA-Z0-9_'])/, "keyword"],

        // Whitespace
        { include: "@whitespace" },

        // Brackets and delimiters
        [/[{}()[\]]/, "@brackets"],
        [/[,`]/, "delimiter"],

        // Unicode operators (standalone)
        [/[\u2237\u2190\u2192\u21D0\u21D2\u2200]/, "operator"],

        // Operators and symbols
        [
          /@symbols/,
          {
            cases: {
              "@operators": "operator",
              "@default": "operator",
            },
          },
        ],

        // Numbers - order matters!
        // Hex integers: 0x1234abcd
        [/0[xX][0-9a-fA-F][0-9a-fA-F_]*/, "number.hex"],
        // Floats with exponent: 1.0e10, 1e10
        [/\d[\d_]*\.\d[\d_]*[eE][+-]?\d[\d_]*/, "number.float"],
        [/\d[\d_]*[eE][+-]?\d[\d_]*/, "number.float"],
        // Floats without exponent: 1.0
        [/\d[\d_]*\.\d[\d_]*/, "number.float"],
        // Integers (may contain underscores)
        [/\d[\d_]*/, "number"],

        // Raw strings: """...""" (3-5 quotes to open/close)
        [/"{3,5}/, "string", "@rawString"],
        // Normal strings
        [/"/, "string", "@string"],

        // Characters
        [/'[^\\']'/, "string"],
        [/'/, "string", "@charLiteral"],
      ],

      whitespace: [
        [/[ \t\r\n]+/, "white"],
        // Block comments (nested): {- ... -}
        [/\{-/, "comment", "@comment"],
        // Line comments: -- ...
        [/--.*$/, "comment"],
      ],

      // Nested block comments
      comment: [
        [/[^{-]+/, "comment"],
        [/-\}/, "comment", "@pop"],
        [/\{-/, "comment", "@push"], // nested comment
        [/[-{]/, "comment"],
      ],

      // Normal string (single-quoted with escapes)
      string: [
        [/[^\\"]+/, "string"],
        [/@escapes/, "string.escape"],
        [/\\./, "string.escape.invalid"],
        [/"/, "string", "@pop"],
      ],

      // Raw string (triple-quoted, no escapes)
      rawString: [
        [/[^"]+/, "string.raw"],
        [/"{3,5}/, "string.raw", "@pop"],
        [/"/, "string.raw"],
      ],

      // Character literal
      charLiteral: [
        [/@escapes/, "string.escape"],
        [/[^\\']/, "string"],
        [/'/, "string", "@pop"],
      ],
    },
  });

  // Register Catppuccin themes
  monaco.editor.defineTheme("catppuccin-macchiato", catppuccinMacchiato);
  monaco.editor.defineTheme("catppuccin-latte", catppuccinLatte);
}
