open Parser_raw

  let string_of_INT = function
    | (s, None) -> Printf.sprintf "INT(%s)" s
    | (s, Some c) -> Printf.sprintf "INT(%s%c)" s c

  let string_of_FLOAT = function
    | (s, None) -> Printf.sprintf "FLOAT(%s)" s
    | (s, Some c) -> Printf.sprintf "FLOAT(%s%c)" s c

  let string_of_STRING = function
    | s, Some s' -> Printf.sprintf "STRING(%S,%S)" s s'
    | s, None -> Printf.sprintf "STRING(%S)" s


let print_symbol = function
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_error) -> "error"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_WITH) -> "with"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_WHILE_LWT) -> "while_lwt"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_WHILE) -> "while"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_WHEN) -> "when"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_VIRTUAL) -> "virtual"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_VAL) -> "val"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_UNDERSCORE) -> "_"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_UIDENT) -> "UIDENT"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_TYPE) -> "type"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_TRY_LWT) -> "try_lwt"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_TRY) -> "try"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_TRUE) -> "true"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_TO) -> "to"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_TILDE) -> "~"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_THEN) -> "then"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_STRUCT) -> "struct"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_STRING) -> "STRING"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_STAR) -> "*"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_SIG) -> "sig"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_SHARP) -> "#"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_SEMISEMI) -> ";;"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_SEMI) -> ";"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_RPAREN) -> ")"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_REC) -> "rec"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_RBRACKET) -> "]"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_RBRACE) -> "}"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_QUOTE) -> "'"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_QUESTIONQUESTION) -> "??"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_QUESTION) -> "?"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_PRIVATE) -> "private"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_PREFIXOP) -> "!"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_PLUSEQ) -> "+="
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_PLUSDOT) -> "+."
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_PLUS) -> "+"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_PERCENT) -> "%"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_OR) -> "or"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_OPTLABEL) -> "?<label>"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_OPEN) -> "open"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_OF) -> "of"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_OBJECT) -> "object"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_NONREC) -> "nonrec"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_NEW) -> "new"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_MUTABLE) -> "mutable"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_MODULE) -> "module"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_MINUSGREATER) -> "->"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_MINUSDOT) -> "-."
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_MINUS) -> "-"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_METHOD) -> "method"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_MATCH_LWT) -> "match_lwt"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_MATCH) -> "match"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_LPAREN) -> ")"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_LIDENT) -> "LIDENT"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_LET_LWT) -> "lwt"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_LET) -> "let"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_LESSMINUS) -> "<-"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_LESS) -> "<"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_LBRACKETPERCENTPERCENT) -> "[%%"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_LBRACKETPERCENT) -> "[%"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_LBRACKETLESS) -> "[<"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_LBRACKETGREATER) -> "[>"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_LBRACKETBAR) -> "[|"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_LBRACKETATATAT) -> "[@@@"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_LBRACKETATAT) -> "[@@"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_LBRACKETAT) -> "[@"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_LBRACKET) -> "["
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_LBRACELESS) -> "{<"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_LBRACE) -> "{"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_LAZY) -> "lazy"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_LABEL) -> "label"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_INT) -> "INT"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_INITIALIZER) -> "initializer"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_INHERIT) -> "inherit"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_INFIXOP4) -> "INFIXOP4"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_INFIXOP3) -> "INFIXOP3"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_INFIXOP2) -> "INFIXOP2"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_INFIXOP1) -> "INFIXOP1"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_INFIXOP0) -> "INFIXOP0"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_INCLUDE) -> "include"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_IN) -> "in"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_IF) -> "if"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_HASHOP) -> "#<op>"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_GREATERRBRACKET) -> ">]"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_GREATERRBRACE) -> ">}"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_GREATERDOT) -> ">."
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_GREATER) -> ">"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_FUNCTOR) -> "functor"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_FUNCTION) -> "function"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_FUN) -> "fun"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_FOR_LWT) -> "for_lwt"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_FOR) -> "for"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_FLOAT) -> "FLOAT"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_FINALLY_LWT) -> "finally"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_FALSE) -> "false"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_EXTERNAL) -> "external"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_EXCEPTION) -> "exception"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_EQUAL) -> "="
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_EOL) -> "EOL"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_EOF) -> "EOF"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_END) -> "end"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_ELSE) -> "else"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_DOWNTO) -> "downto"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_DOTTILDE) -> ".~"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_DOTOP) -> "DOTOP"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_DOTLESS) -> ".<"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_DOTDOT) -> ".."
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_DOT) -> "."
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_DONE) -> "done"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_DOCSTRING) -> "DOCSTRING"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_DO) -> "do"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_CONSTRAINT) -> "constraint"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_COMMENT) -> "COMMENT"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_COMMA) -> ","
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_COLONGREATER) -> ":>"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_COLONEQUAL) -> ":="
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_COLONCOLON) -> "::"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_COLON) -> ":"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_CLASS) -> "class"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_CHAR) -> "CHAR"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_BEGIN) -> "begin"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_BARRBRACKET) -> "|]"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_BARBAR) -> "||"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_BAR) -> "|"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_BANG) -> "!"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_BACKQUOTE) -> "`"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_ASSERT) -> "assert"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_AS) -> "as"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_AND) -> "and"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_AMPERSAND) -> "&"
  | MenhirInterpreter.X (MenhirInterpreter.T MenhirInterpreter.T_AMPERAMPER) -> "&&"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_with_type_binder) -> "with_type_binder"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_with_constraints) -> "with_constraints"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_with_constraint) -> "with_constraint"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_virtual_flag) -> "virtual_flag"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_value_type) -> "value_type"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_value_description) -> "value_description"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_value) -> "value"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_val_longident) -> "val_longident"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_val_ident) -> "val_ident"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_typevar_list) -> "typevar_list"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_type_variance) -> "type_variance"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_type_variable) -> "type_variable"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_type_parameter_list) -> "type_parameter_list"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_type_parameter) -> "type_parameter"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_type_longident) -> "type_longident"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_type_kind) -> "type_kind"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_type_declarations) -> "type_declarations"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_type_declaration) -> "type_declaration"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_type_constraint) -> "type_constraint"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_toplevel_directive) -> "toplevel_directive"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_tag_field) -> "tag_field"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_subtractive) -> "subtractive"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_structure_tail) -> "structure_tail"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_structure_item) -> "structure_item"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_structure) -> "structure"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_strict_binding) -> "strict_binding"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_str_type_extension) -> "str_type_extension"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_str_include_statement) -> "str_include_statement"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_str_extension_constructors) -> "str_extension_constructors"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_str_exception_declaration) -> "str_exception_declaration"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_single_attr_id) -> "single_attr_id"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_simple_pattern_not_ident) -> "simple_pattern_not_ident"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_simple_pattern) -> "simple_pattern"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_simple_labeled_expr_list) -> "simple_labeled_expr_list"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_simple_expr) -> "simple_expr"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_simple_delimited_pattern) -> "simple_delimited_pattern"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_simple_core_type_or_tuple) -> "simple_core_type_or_tuple"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_simple_core_type2) -> "simple_core_type2"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_simple_core_type) -> "simple_core_type"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_signed_constant) -> "signed_constant"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_signature_item) -> "signature_item"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_signature) -> "signature"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_sig_type_extension) -> "sig_type_extension"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_sig_include_statement) -> "sig_include_statement"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_sig_extension_constructors) -> "sig_extension_constructors"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_sig_exception_declaration) -> "sig_exception_declaration"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_seq_expr) -> "seq_expr"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_row_field_list) -> "row_field_list"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_row_field) -> "row_field"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_record_expr) -> "record_expr"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rec_module_declarations) -> "rec_module_declarations"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rec_module_declaration) -> "rec_module_declaration"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rec_module_bindings) -> "rec_module_bindings"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rec_module_binding) -> "rec_module_binding"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_rec_flag) -> "rec_flag"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_private_virtual_flags) -> "private_virtual_flags"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_private_flag) -> "private_flag"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_primitive_declaration_body) -> "primitive_declaration_body"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_primitive_declaration) -> "primitive_declaration"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_post_item_attributes) -> "post_item_attributes"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_post_item_attribute) -> "post_item_attribute"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_poly_type_no_attr) -> "poly_type_no_attr"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_poly_type) -> "poly_type"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_payload) -> "payload"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_pattern_var) -> "pattern_var"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_pattern_semi_list) -> "pattern_semi_list"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_pattern_no_exn_comma_list) -> "pattern_no_exn_comma_list"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_pattern_no_exn) -> "pattern_no_exn"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_pattern_gen) -> "pattern_gen"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_pattern_comma_list) -> "pattern_comma_list"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_pattern) -> "pattern"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_parse_expression) -> "parse_expression"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_parent_binder) -> "parent_binder"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_paren_module_expr) -> "paren_module_expr"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_package_type) -> "package_type"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_override_flag) -> "override_flag"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_optional_type_variable) -> "optional_type_variable"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_optional_type_parameters) -> "optional_type_parameters"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_optional_type_parameter_list) -> "optional_type_parameter_list"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_optional_type_parameter) -> "optional_type_parameter"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_opt_type_constraint) -> "opt_type_constraint"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_opt_semi) -> "opt_semi"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_opt_pattern_type_constraint) -> "opt_pattern_type_constraint"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_opt_default) -> "opt_default"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_opt_bar) -> "opt_bar"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_opt_ampersand) -> "opt_ampersand"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_operator) -> "operator"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_open_statement) -> "open_statement"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_nonrec_flag) -> "nonrec_flag"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_name_tag_list) -> "name_tag_list"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_name_tag) -> "name_tag"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_mutable_flag) -> "mutable_flag"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_mty_longident) -> "mty_longident"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_module_type_declaration_body) -> "module_type_declaration_body"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_module_type_declaration) -> "module_type_declaration"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_module_type) -> "module_type"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_module_expr) -> "module_expr"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_module_declaration_body) -> "module_declaration_body"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_module_declaration) -> "module_declaration"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_module_binding_body) -> "module_binding_body"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_module_binding) -> "module_binding"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_module_alias) -> "module_alias"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_mod_longident) -> "mod_longident"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_mod_ext_longident) -> "mod_ext_longident"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_method_) -> "method_"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_meth_list) -> "meth_list"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_match_cases) -> "match_cases"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_match_case) -> "match_case"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_lwt_bindings) -> "lwt_bindings"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_lwt_binding) -> "lwt_binding"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_lident_list) -> "lident_list"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_let_pattern) -> "let_pattern"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_let_exception_declaration) -> "let_exception_declaration"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_let_bindings) -> "let_bindings"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_let_binding_body) -> "let_binding_body"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_let_binding) -> "let_binding"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_lbl_pattern_list) -> "lbl_pattern_list"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_lbl_pattern) -> "lbl_pattern"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_lbl_expr_list) -> "lbl_expr_list"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_lbl_expr) -> "lbl_expr"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_labeled_simple_pattern) -> "labeled_simple_pattern"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_labeled_simple_expr) -> "labeled_simple_expr"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_label_var) -> "label_var"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_label_longident) -> "label_longident"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_label_let_pattern) -> "label_let_pattern"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_label_ident) -> "label_ident"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_label_expr) -> "label_expr"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_label_declarations) -> "label_declarations"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_label_declaration_semi) -> "label_declaration_semi"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_label_declaration) -> "label_declaration"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_label) -> "label"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_item_extension) -> "item_extension"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_interface) -> "interface"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_inherit_field_semi) -> "inherit_field_semi"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_implementation) -> "implementation"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ident) -> "ident"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_generalized_constructor_arguments) -> "generalized_constructor_arguments"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_functor_args) -> "functor_args"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_functor_arg_name) -> "functor_arg_name"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_functor_arg) -> "functor_arg"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_fun_def) -> "fun_def"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_fun_binding) -> "fun_binding"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_floating_attribute) -> "floating_attribute"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_field_semi) -> "field_semi"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_field_expr_list) -> "field_expr_list"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_field_expr) -> "field_expr"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_field) -> "field"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_extension_constructor_rebind) -> "extension_constructor_rebind"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_extension_constructor_declaration) -> "extension_constructor_declaration"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_extension) -> "extension"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_ext_attributes) -> "ext_attributes"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_expr_semi_list) -> "expr_semi_list"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_expr_comma_list) -> "expr_comma_list"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_expr) -> "expr"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_direction_flag) -> "direction_flag"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_core_type_no_attr) -> "core_type_no_attr"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_core_type_list) -> "core_type_list"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_core_type_comma_list) -> "core_type_comma_list"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_core_type2) -> "core_type2"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_core_type) -> "core_type"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_constructor_declarations) -> "constructor_declarations"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_constructor_declaration) -> "constructor_declaration"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_constructor_arguments) -> "constructor_arguments"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_constraints) -> "constraints"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_constrain_field) -> "constrain_field"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_constrain) -> "constrain"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_constr_longident) -> "constr_longident"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_constr_ident) -> "constr_ident"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_constant) -> "constant"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_clty_longident) -> "clty_longident"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_class_type_parameters) -> "class_type_parameters"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_class_type_declarations) -> "class_type_declarations"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_class_type_declaration) -> "class_type_declaration"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_class_type) -> "class_type"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_class_structure) -> "class_structure"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_class_simple_expr) -> "class_simple_expr"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_class_signature) -> "class_signature"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_class_sig_fields) -> "class_sig_fields"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_class_sig_field) -> "class_sig_field"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_class_sig_body) -> "class_sig_body"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_class_self_type) -> "class_self_type"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_class_self_pattern) -> "class_self_pattern"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_class_longident) -> "class_longident"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_class_fun_def) -> "class_fun_def"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_class_fun_binding) -> "class_fun_binding"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_class_fields) -> "class_fields"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_class_field) -> "class_field"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_class_expr) -> "class_expr"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_class_descriptions) -> "class_descriptions"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_class_description) -> "class_description"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_class_declarations) -> "class_declarations"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_class_declaration) -> "class_declaration"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_bar_extension_constructor_rebind) -> "bar_extension_constructor_rebind"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_bar_extension_constructor_declaration) -> "bar_extension_constructor_declaration"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_bar_constructor_declaration) -> "bar_constructor_declaration"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_attributes) -> "attributes"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_attribute) -> "attribute"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_attr_id) -> "attr_id"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_and_type_declaration) -> "and_type_declaration"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_and_module_declaration) -> "and_module_declaration"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_and_module_binding) -> "and_module_binding"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_and_let_binding) -> "and_let_binding"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_and_class_type_declaration) -> "and_class_type_declaration"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_and_class_description) -> "and_class_description"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_and_class_declaration) -> "and_class_declaration"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_amper_type_list) -> "amper_type_list"
  | MenhirInterpreter.X (MenhirInterpreter.N MenhirInterpreter.N_additive) -> "additive"

let print_value (type a) : a MenhirInterpreter.symbol -> a -> string = function
  | MenhirInterpreter.T MenhirInterpreter.T_error -> (fun _ -> "error")
  | MenhirInterpreter.T MenhirInterpreter.T_WITH -> (fun _ -> "with")
  | MenhirInterpreter.T MenhirInterpreter.T_WHILE_LWT -> (fun _ -> "while_lwt")
  | MenhirInterpreter.T MenhirInterpreter.T_WHILE -> (fun _ -> "while")
  | MenhirInterpreter.T MenhirInterpreter.T_WHEN -> (fun _ -> "when")
  | MenhirInterpreter.T MenhirInterpreter.T_VIRTUAL -> (fun _ -> "virtual")
  | MenhirInterpreter.T MenhirInterpreter.T_VAL -> (fun _ -> "val")
  | MenhirInterpreter.T MenhirInterpreter.T_UNDERSCORE -> (fun _ -> "_")
  | MenhirInterpreter.T MenhirInterpreter.T_UIDENT -> (Printf.sprintf "UIDENT(%S)")
  | MenhirInterpreter.T MenhirInterpreter.T_TYPE -> (fun _ -> "type")
  | MenhirInterpreter.T MenhirInterpreter.T_TRY_LWT -> (fun _ -> "try_lwt")
  | MenhirInterpreter.T MenhirInterpreter.T_TRY -> (fun _ -> "try")
  | MenhirInterpreter.T MenhirInterpreter.T_TRUE -> (fun _ -> "true")
  | MenhirInterpreter.T MenhirInterpreter.T_TO -> (fun _ -> "to")
  | MenhirInterpreter.T MenhirInterpreter.T_TILDE -> (fun _ -> "~")
  | MenhirInterpreter.T MenhirInterpreter.T_THEN -> (fun _ -> "then")
  | MenhirInterpreter.T MenhirInterpreter.T_STRUCT -> (fun _ -> "struct")
  | MenhirInterpreter.T MenhirInterpreter.T_STRING -> (string_of_STRING)
  | MenhirInterpreter.T MenhirInterpreter.T_STAR -> (fun _ -> "*")
  | MenhirInterpreter.T MenhirInterpreter.T_SIG -> (fun _ -> "sig")
  | MenhirInterpreter.T MenhirInterpreter.T_SHARP -> (fun _ -> "#")
  | MenhirInterpreter.T MenhirInterpreter.T_SEMISEMI -> (fun _ -> ";;")
  | MenhirInterpreter.T MenhirInterpreter.T_SEMI -> (fun _ -> ";")
  | MenhirInterpreter.T MenhirInterpreter.T_RPAREN -> (fun _ -> ")")
  | MenhirInterpreter.T MenhirInterpreter.T_REC -> (fun _ -> "rec")
  | MenhirInterpreter.T MenhirInterpreter.T_RBRACKET -> (fun _ -> "]")
  | MenhirInterpreter.T MenhirInterpreter.T_RBRACE -> (fun _ -> "}")
  | MenhirInterpreter.T MenhirInterpreter.T_QUOTE -> (fun _ -> "'")
  | MenhirInterpreter.T MenhirInterpreter.T_QUESTIONQUESTION -> (fun _ -> "??")
  | MenhirInterpreter.T MenhirInterpreter.T_QUESTION -> (fun _ -> "?")
  | MenhirInterpreter.T MenhirInterpreter.T_PRIVATE -> (fun _ -> "private")
  | MenhirInterpreter.T MenhirInterpreter.T_PREFIXOP -> (Printf.sprintf "PREFIXOP(%S)")
  | MenhirInterpreter.T MenhirInterpreter.T_PLUSEQ -> (fun _ -> "+=")
  | MenhirInterpreter.T MenhirInterpreter.T_PLUSDOT -> (fun _ -> "+.")
  | MenhirInterpreter.T MenhirInterpreter.T_PLUS -> (fun _ -> "+")
  | MenhirInterpreter.T MenhirInterpreter.T_PERCENT -> (fun _ -> "%")
  | MenhirInterpreter.T MenhirInterpreter.T_OR -> (fun _ -> "or")
  | MenhirInterpreter.T MenhirInterpreter.T_OPTLABEL -> (Printf.sprintf "OPTLABEL(%S)")
  | MenhirInterpreter.T MenhirInterpreter.T_OPEN -> (fun _ -> "open")
  | MenhirInterpreter.T MenhirInterpreter.T_OF -> (fun _ -> "of")
  | MenhirInterpreter.T MenhirInterpreter.T_OBJECT -> (fun _ -> "object")
  | MenhirInterpreter.T MenhirInterpreter.T_NONREC -> (fun _ -> "nonrec")
  | MenhirInterpreter.T MenhirInterpreter.T_NEW -> (fun _ -> "new")
  | MenhirInterpreter.T MenhirInterpreter.T_MUTABLE -> (fun _ -> "mutable")
  | MenhirInterpreter.T MenhirInterpreter.T_MODULE -> (fun _ -> "module")
  | MenhirInterpreter.T MenhirInterpreter.T_MINUSGREATER -> (fun _ -> "->")
  | MenhirInterpreter.T MenhirInterpreter.T_MINUSDOT -> (fun _ -> "-.")
  | MenhirInterpreter.T MenhirInterpreter.T_MINUS -> (fun _ -> "-")
  | MenhirInterpreter.T MenhirInterpreter.T_METHOD -> (fun _ -> "method")
  | MenhirInterpreter.T MenhirInterpreter.T_MATCH_LWT -> (fun _ -> "match_lwt")
  | MenhirInterpreter.T MenhirInterpreter.T_MATCH -> (fun _ -> "match")
  | MenhirInterpreter.T MenhirInterpreter.T_LPAREN -> (fun _ -> ")")
  | MenhirInterpreter.T MenhirInterpreter.T_LIDENT -> (Printf.sprintf "LIDENT(%S)")
  | MenhirInterpreter.T MenhirInterpreter.T_LET_LWT -> (fun _ -> "lwt")
  | MenhirInterpreter.T MenhirInterpreter.T_LET -> (fun _ -> "let")
  | MenhirInterpreter.T MenhirInterpreter.T_LESSMINUS -> (fun _ -> "<-")
  | MenhirInterpreter.T MenhirInterpreter.T_LESS -> (fun _ -> "<")
  | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETPERCENTPERCENT -> (fun _ -> "[%%")
  | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETPERCENT -> (fun _ -> "[%")
  | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETLESS -> (fun _ -> "[<")
  | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETGREATER -> (fun _ -> "[>")
  | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETBAR -> (fun _ -> "[|")
  | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETATATAT -> (fun _ -> "[@@@")
  | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETATAT -> (fun _ -> "[@@")
  | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETAT -> (fun _ -> "[@")
  | MenhirInterpreter.T MenhirInterpreter.T_LBRACKET -> (fun _ -> "[")
  | MenhirInterpreter.T MenhirInterpreter.T_LBRACELESS -> (fun _ -> "{<")
  | MenhirInterpreter.T MenhirInterpreter.T_LBRACE -> (fun _ -> "{")
  | MenhirInterpreter.T MenhirInterpreter.T_LAZY -> (fun _ -> "lazy")
  | MenhirInterpreter.T MenhirInterpreter.T_LABEL -> (Printf.sprintf "LABEL(%S)")
  | MenhirInterpreter.T MenhirInterpreter.T_INT -> (string_of_INT)
  | MenhirInterpreter.T MenhirInterpreter.T_INITIALIZER -> (fun _ -> "initializer")
  | MenhirInterpreter.T MenhirInterpreter.T_INHERIT -> (fun _ -> "inherit")
  | MenhirInterpreter.T MenhirInterpreter.T_INFIXOP4 -> (Printf.sprintf "INFIXOP4(%S)")
  | MenhirInterpreter.T MenhirInterpreter.T_INFIXOP3 -> (Printf.sprintf "INFIXOP3(%S)")
  | MenhirInterpreter.T MenhirInterpreter.T_INFIXOP2 -> (Printf.sprintf "INFIXOP2(%S)")
  | MenhirInterpreter.T MenhirInterpreter.T_INFIXOP1 -> (Printf.sprintf "INFIXOP1(%S)")
  | MenhirInterpreter.T MenhirInterpreter.T_INFIXOP0 -> (Printf.sprintf "INFIXOP0(%S)")
  | MenhirInterpreter.T MenhirInterpreter.T_INCLUDE -> (fun _ -> "include")
  | MenhirInterpreter.T MenhirInterpreter.T_IN -> (fun _ -> "in")
  | MenhirInterpreter.T MenhirInterpreter.T_IF -> (fun _ -> "if")
  | MenhirInterpreter.T MenhirInterpreter.T_HASHOP -> (Printf.sprintf "HASHOP(%S)")
  | MenhirInterpreter.T MenhirInterpreter.T_GREATERRBRACKET -> (fun _ -> ">]")
  | MenhirInterpreter.T MenhirInterpreter.T_GREATERRBRACE -> (fun _ -> ">}")
  | MenhirInterpreter.T MenhirInterpreter.T_GREATERDOT -> (fun _ -> ">.")
  | MenhirInterpreter.T MenhirInterpreter.T_GREATER -> (fun _ -> ">")
  | MenhirInterpreter.T MenhirInterpreter.T_FUNCTOR -> (fun _ -> "functor")
  | MenhirInterpreter.T MenhirInterpreter.T_FUNCTION -> (fun _ -> "function")
  | MenhirInterpreter.T MenhirInterpreter.T_FUN -> (fun _ -> "fun")
  | MenhirInterpreter.T MenhirInterpreter.T_FOR_LWT -> (fun _ -> "for_lwt")
  | MenhirInterpreter.T MenhirInterpreter.T_FOR -> (fun _ -> "for")
  | MenhirInterpreter.T MenhirInterpreter.T_FLOAT -> (string_of_FLOAT)
  | MenhirInterpreter.T MenhirInterpreter.T_FINALLY_LWT -> (fun _ -> "finally")
  | MenhirInterpreter.T MenhirInterpreter.T_FALSE -> (fun _ -> "false")
  | MenhirInterpreter.T MenhirInterpreter.T_EXTERNAL -> (fun _ -> "external")
  | MenhirInterpreter.T MenhirInterpreter.T_EXCEPTION -> (fun _ -> "exception")
  | MenhirInterpreter.T MenhirInterpreter.T_EQUAL -> (fun _ -> "=")
  | MenhirInterpreter.T MenhirInterpreter.T_EOL -> (fun _ -> "EOL")
  | MenhirInterpreter.T MenhirInterpreter.T_EOF -> (fun _ -> "EOF")
  | MenhirInterpreter.T MenhirInterpreter.T_END -> (fun _ -> "end")
  | MenhirInterpreter.T MenhirInterpreter.T_ELSE -> (fun _ -> "else")
  | MenhirInterpreter.T MenhirInterpreter.T_DOWNTO -> (fun _ -> "downto")
  | MenhirInterpreter.T MenhirInterpreter.T_DOTTILDE -> (fun _ -> ".~")
  | MenhirInterpreter.T MenhirInterpreter.T_DOTOP -> (fun _ -> "DOTOP")
  | MenhirInterpreter.T MenhirInterpreter.T_DOTLESS -> (fun _ -> ".<")
  | MenhirInterpreter.T MenhirInterpreter.T_DOTDOT -> (fun _ -> "..")
  | MenhirInterpreter.T MenhirInterpreter.T_DOT -> (fun _ -> ".")
  | MenhirInterpreter.T MenhirInterpreter.T_DONE -> (fun _ -> "done")
  | MenhirInterpreter.T MenhirInterpreter.T_DOCSTRING -> (fun _ -> "DOCSTRING")
  | MenhirInterpreter.T MenhirInterpreter.T_DO -> (fun _ -> "do")
  | MenhirInterpreter.T MenhirInterpreter.T_CONSTRAINT -> (fun _ -> "constraint")
  | MenhirInterpreter.T MenhirInterpreter.T_COMMENT -> (fun _ -> "COMMENT")
  | MenhirInterpreter.T MenhirInterpreter.T_COMMA -> (fun _ -> ",")
  | MenhirInterpreter.T MenhirInterpreter.T_COLONGREATER -> (fun _ -> ":>")
  | MenhirInterpreter.T MenhirInterpreter.T_COLONEQUAL -> (fun _ -> ":=")
  | MenhirInterpreter.T MenhirInterpreter.T_COLONCOLON -> (fun _ -> "::")
  | MenhirInterpreter.T MenhirInterpreter.T_COLON -> (fun _ -> ":")
  | MenhirInterpreter.T MenhirInterpreter.T_CLASS -> (fun _ -> "class")
  | MenhirInterpreter.T MenhirInterpreter.T_CHAR -> (fun _ -> "CHAR")
  | MenhirInterpreter.T MenhirInterpreter.T_BEGIN -> (fun _ -> "begin")
  | MenhirInterpreter.T MenhirInterpreter.T_BARRBRACKET -> (fun _ -> "|]")
  | MenhirInterpreter.T MenhirInterpreter.T_BARBAR -> (fun _ -> "||")
  | MenhirInterpreter.T MenhirInterpreter.T_BAR -> (fun _ -> "|")
  | MenhirInterpreter.T MenhirInterpreter.T_BANG -> (fun _ -> "!")
  | MenhirInterpreter.T MenhirInterpreter.T_BACKQUOTE -> (fun _ -> "`")
  | MenhirInterpreter.T MenhirInterpreter.T_ASSERT -> (fun _ -> "assert")
  | MenhirInterpreter.T MenhirInterpreter.T_AS -> (fun _ -> "as")
  | MenhirInterpreter.T MenhirInterpreter.T_AND -> (fun _ -> "and")
  | MenhirInterpreter.T MenhirInterpreter.T_AMPERSAND -> (fun _ -> "&")
  | MenhirInterpreter.T MenhirInterpreter.T_AMPERAMPER -> (fun _ -> "&&")
  | MenhirInterpreter.N MenhirInterpreter.N_with_type_binder -> (fun _ -> "with_type_binder")
  | MenhirInterpreter.N MenhirInterpreter.N_with_constraints -> (fun _ -> "with_constraints")
  | MenhirInterpreter.N MenhirInterpreter.N_with_constraint -> (fun _ -> "with_constraint")
  | MenhirInterpreter.N MenhirInterpreter.N_virtual_flag -> (fun _ -> "virtual_flag")
  | MenhirInterpreter.N MenhirInterpreter.N_value_type -> (fun _ -> "value_type")
  | MenhirInterpreter.N MenhirInterpreter.N_value_description -> (fun _ -> "value_description")
  | MenhirInterpreter.N MenhirInterpreter.N_value -> (fun _ -> "value")
  | MenhirInterpreter.N MenhirInterpreter.N_val_longident -> (fun _ -> "val_longident")
  | MenhirInterpreter.N MenhirInterpreter.N_val_ident -> (fun _ -> "val_ident")
  | MenhirInterpreter.N MenhirInterpreter.N_typevar_list -> (fun _ -> "typevar_list")
  | MenhirInterpreter.N MenhirInterpreter.N_type_variance -> (fun _ -> "type_variance")
  | MenhirInterpreter.N MenhirInterpreter.N_type_variable -> (fun _ -> "type_variable")
  | MenhirInterpreter.N MenhirInterpreter.N_type_parameter_list -> (fun _ -> "type_parameter_list")
  | MenhirInterpreter.N MenhirInterpreter.N_type_parameter -> (fun _ -> "type_parameter")
  | MenhirInterpreter.N MenhirInterpreter.N_type_longident -> (fun _ -> "type_longident")
  | MenhirInterpreter.N MenhirInterpreter.N_type_kind -> (fun _ -> "type_kind")
  | MenhirInterpreter.N MenhirInterpreter.N_type_declarations -> (fun _ -> "type_declarations")
  | MenhirInterpreter.N MenhirInterpreter.N_type_declaration -> (fun _ -> "type_declaration")
  | MenhirInterpreter.N MenhirInterpreter.N_type_constraint -> (fun _ -> "type_constraint")
  | MenhirInterpreter.N MenhirInterpreter.N_toplevel_directive -> (fun _ -> "toplevel_directive")
  | MenhirInterpreter.N MenhirInterpreter.N_tag_field -> (fun _ -> "tag_field")
  | MenhirInterpreter.N MenhirInterpreter.N_subtractive -> (fun _ -> "subtractive")
  | MenhirInterpreter.N MenhirInterpreter.N_structure_tail -> (fun _ -> "structure_tail")
  | MenhirInterpreter.N MenhirInterpreter.N_structure_item -> (fun _ -> "structure_item")
  | MenhirInterpreter.N MenhirInterpreter.N_structure -> (fun _ -> "structure")
  | MenhirInterpreter.N MenhirInterpreter.N_strict_binding -> (fun _ -> "strict_binding")
  | MenhirInterpreter.N MenhirInterpreter.N_str_type_extension -> (fun _ -> "str_type_extension")
  | MenhirInterpreter.N MenhirInterpreter.N_str_include_statement -> (fun _ -> "str_include_statement")
  | MenhirInterpreter.N MenhirInterpreter.N_str_extension_constructors -> (fun _ -> "str_extension_constructors")
  | MenhirInterpreter.N MenhirInterpreter.N_str_exception_declaration -> (fun _ -> "str_exception_declaration")
  | MenhirInterpreter.N MenhirInterpreter.N_single_attr_id -> (fun _ -> "single_attr_id")
  | MenhirInterpreter.N MenhirInterpreter.N_simple_pattern_not_ident -> (fun _ -> "simple_pattern_not_ident")
  | MenhirInterpreter.N MenhirInterpreter.N_simple_pattern -> (fun _ -> "simple_pattern")
  | MenhirInterpreter.N MenhirInterpreter.N_simple_labeled_expr_list -> (fun _ -> "simple_labeled_expr_list")
  | MenhirInterpreter.N MenhirInterpreter.N_simple_expr -> (fun _ -> "simple_expr")
  | MenhirInterpreter.N MenhirInterpreter.N_simple_delimited_pattern -> (fun _ -> "simple_delimited_pattern")
  | MenhirInterpreter.N MenhirInterpreter.N_simple_core_type_or_tuple -> (fun _ -> "simple_core_type_or_tuple")
  | MenhirInterpreter.N MenhirInterpreter.N_simple_core_type2 -> (fun _ -> "simple_core_type2")
  | MenhirInterpreter.N MenhirInterpreter.N_simple_core_type -> (fun _ -> "simple_core_type")
  | MenhirInterpreter.N MenhirInterpreter.N_signed_constant -> (fun _ -> "signed_constant")
  | MenhirInterpreter.N MenhirInterpreter.N_signature_item -> (fun _ -> "signature_item")
  | MenhirInterpreter.N MenhirInterpreter.N_signature -> (fun _ -> "signature")
  | MenhirInterpreter.N MenhirInterpreter.N_sig_type_extension -> (fun _ -> "sig_type_extension")
  | MenhirInterpreter.N MenhirInterpreter.N_sig_include_statement -> (fun _ -> "sig_include_statement")
  | MenhirInterpreter.N MenhirInterpreter.N_sig_extension_constructors -> (fun _ -> "sig_extension_constructors")
  | MenhirInterpreter.N MenhirInterpreter.N_sig_exception_declaration -> (fun _ -> "sig_exception_declaration")
  | MenhirInterpreter.N MenhirInterpreter.N_seq_expr -> (fun _ -> "seq_expr")
  | MenhirInterpreter.N MenhirInterpreter.N_row_field_list -> (fun _ -> "row_field_list")
  | MenhirInterpreter.N MenhirInterpreter.N_row_field -> (fun _ -> "row_field")
  | MenhirInterpreter.N MenhirInterpreter.N_record_expr -> (fun _ -> "record_expr")
  | MenhirInterpreter.N MenhirInterpreter.N_rec_module_declarations -> (fun _ -> "rec_module_declarations")
  | MenhirInterpreter.N MenhirInterpreter.N_rec_module_declaration -> (fun _ -> "rec_module_declaration")
  | MenhirInterpreter.N MenhirInterpreter.N_rec_module_bindings -> (fun _ -> "rec_module_bindings")
  | MenhirInterpreter.N MenhirInterpreter.N_rec_module_binding -> (fun _ -> "rec_module_binding")
  | MenhirInterpreter.N MenhirInterpreter.N_rec_flag -> (fun _ -> "rec_flag")
  | MenhirInterpreter.N MenhirInterpreter.N_private_virtual_flags -> (fun _ -> "private_virtual_flags")
  | MenhirInterpreter.N MenhirInterpreter.N_private_flag -> (fun _ -> "private_flag")
  | MenhirInterpreter.N MenhirInterpreter.N_primitive_declaration_body -> (fun _ -> "primitive_declaration_body")
  | MenhirInterpreter.N MenhirInterpreter.N_primitive_declaration -> (fun _ -> "primitive_declaration")
  | MenhirInterpreter.N MenhirInterpreter.N_post_item_attributes -> (fun _ -> "post_item_attributes")
  | MenhirInterpreter.N MenhirInterpreter.N_post_item_attribute -> (fun _ -> "post_item_attribute")
  | MenhirInterpreter.N MenhirInterpreter.N_poly_type_no_attr -> (fun _ -> "poly_type_no_attr")
  | MenhirInterpreter.N MenhirInterpreter.N_poly_type -> (fun _ -> "poly_type")
  | MenhirInterpreter.N MenhirInterpreter.N_payload -> (fun _ -> "payload")
  | MenhirInterpreter.N MenhirInterpreter.N_pattern_var -> (fun _ -> "pattern_var")
  | MenhirInterpreter.N MenhirInterpreter.N_pattern_semi_list -> (fun _ -> "pattern_semi_list")
  | MenhirInterpreter.N MenhirInterpreter.N_pattern_no_exn_comma_list -> (fun _ -> "pattern_no_exn_comma_list")
  | MenhirInterpreter.N MenhirInterpreter.N_pattern_no_exn -> (fun _ -> "pattern_no_exn")
  | MenhirInterpreter.N MenhirInterpreter.N_pattern_gen -> (fun _ -> "pattern_gen")
  | MenhirInterpreter.N MenhirInterpreter.N_pattern_comma_list -> (fun _ -> "pattern_comma_list")
  | MenhirInterpreter.N MenhirInterpreter.N_pattern -> (fun _ -> "pattern")
  | MenhirInterpreter.N MenhirInterpreter.N_parse_expression -> (fun _ -> "parse_expression")
  | MenhirInterpreter.N MenhirInterpreter.N_parent_binder -> (fun _ -> "parent_binder")
  | MenhirInterpreter.N MenhirInterpreter.N_paren_module_expr -> (fun _ -> "paren_module_expr")
  | MenhirInterpreter.N MenhirInterpreter.N_package_type -> (fun _ -> "package_type")
  | MenhirInterpreter.N MenhirInterpreter.N_override_flag -> (fun _ -> "override_flag")
  | MenhirInterpreter.N MenhirInterpreter.N_optional_type_variable -> (fun _ -> "optional_type_variable")
  | MenhirInterpreter.N MenhirInterpreter.N_optional_type_parameters -> (fun _ -> "optional_type_parameters")
  | MenhirInterpreter.N MenhirInterpreter.N_optional_type_parameter_list -> (fun _ -> "optional_type_parameter_list")
  | MenhirInterpreter.N MenhirInterpreter.N_optional_type_parameter -> (fun _ -> "optional_type_parameter")
  | MenhirInterpreter.N MenhirInterpreter.N_opt_type_constraint -> (fun _ -> "opt_type_constraint")
  | MenhirInterpreter.N MenhirInterpreter.N_opt_semi -> (fun _ -> "opt_semi")
  | MenhirInterpreter.N MenhirInterpreter.N_opt_pattern_type_constraint -> (fun _ -> "opt_pattern_type_constraint")
  | MenhirInterpreter.N MenhirInterpreter.N_opt_default -> (fun _ -> "opt_default")
  | MenhirInterpreter.N MenhirInterpreter.N_opt_bar -> (fun _ -> "opt_bar")
  | MenhirInterpreter.N MenhirInterpreter.N_opt_ampersand -> (fun _ -> "opt_ampersand")
  | MenhirInterpreter.N MenhirInterpreter.N_operator -> (fun _ -> "operator")
  | MenhirInterpreter.N MenhirInterpreter.N_open_statement -> (fun _ -> "open_statement")
  | MenhirInterpreter.N MenhirInterpreter.N_nonrec_flag -> (fun _ -> "nonrec_flag")
  | MenhirInterpreter.N MenhirInterpreter.N_name_tag_list -> (fun _ -> "name_tag_list")
  | MenhirInterpreter.N MenhirInterpreter.N_name_tag -> (fun _ -> "name_tag")
  | MenhirInterpreter.N MenhirInterpreter.N_mutable_flag -> (fun _ -> "mutable_flag")
  | MenhirInterpreter.N MenhirInterpreter.N_mty_longident -> (fun _ -> "mty_longident")
  | MenhirInterpreter.N MenhirInterpreter.N_module_type_declaration_body -> (fun _ -> "module_type_declaration_body")
  | MenhirInterpreter.N MenhirInterpreter.N_module_type_declaration -> (fun _ -> "module_type_declaration")
  | MenhirInterpreter.N MenhirInterpreter.N_module_type -> (fun _ -> "module_type")
  | MenhirInterpreter.N MenhirInterpreter.N_module_expr -> (fun _ -> "module_expr")
  | MenhirInterpreter.N MenhirInterpreter.N_module_declaration_body -> (fun _ -> "module_declaration_body")
  | MenhirInterpreter.N MenhirInterpreter.N_module_declaration -> (fun _ -> "module_declaration")
  | MenhirInterpreter.N MenhirInterpreter.N_module_binding_body -> (fun _ -> "module_binding_body")
  | MenhirInterpreter.N MenhirInterpreter.N_module_binding -> (fun _ -> "module_binding")
  | MenhirInterpreter.N MenhirInterpreter.N_module_alias -> (fun _ -> "module_alias")
  | MenhirInterpreter.N MenhirInterpreter.N_mod_longident -> (fun _ -> "mod_longident")
  | MenhirInterpreter.N MenhirInterpreter.N_mod_ext_longident -> (fun _ -> "mod_ext_longident")
  | MenhirInterpreter.N MenhirInterpreter.N_method_ -> (fun _ -> "method_")
  | MenhirInterpreter.N MenhirInterpreter.N_meth_list -> (fun _ -> "meth_list")
  | MenhirInterpreter.N MenhirInterpreter.N_match_cases -> (fun _ -> "match_cases")
  | MenhirInterpreter.N MenhirInterpreter.N_match_case -> (fun _ -> "match_case")
  | MenhirInterpreter.N MenhirInterpreter.N_lwt_bindings -> (fun _ -> "lwt_bindings")
  | MenhirInterpreter.N MenhirInterpreter.N_lwt_binding -> (fun _ -> "lwt_binding")
  | MenhirInterpreter.N MenhirInterpreter.N_lident_list -> (fun _ -> "lident_list")
  | MenhirInterpreter.N MenhirInterpreter.N_let_pattern -> (fun _ -> "let_pattern")
  | MenhirInterpreter.N MenhirInterpreter.N_let_exception_declaration -> (fun _ -> "let_exception_declaration")
  | MenhirInterpreter.N MenhirInterpreter.N_let_bindings -> (fun _ -> "let_bindings")
  | MenhirInterpreter.N MenhirInterpreter.N_let_binding_body -> (fun _ -> "let_binding_body")
  | MenhirInterpreter.N MenhirInterpreter.N_let_binding -> (fun _ -> "let_binding")
  | MenhirInterpreter.N MenhirInterpreter.N_lbl_pattern_list -> (fun _ -> "lbl_pattern_list")
  | MenhirInterpreter.N MenhirInterpreter.N_lbl_pattern -> (fun _ -> "lbl_pattern")
  | MenhirInterpreter.N MenhirInterpreter.N_lbl_expr_list -> (fun _ -> "lbl_expr_list")
  | MenhirInterpreter.N MenhirInterpreter.N_lbl_expr -> (fun _ -> "lbl_expr")
  | MenhirInterpreter.N MenhirInterpreter.N_labeled_simple_pattern -> (fun _ -> "labeled_simple_pattern")
  | MenhirInterpreter.N MenhirInterpreter.N_labeled_simple_expr -> (fun _ -> "labeled_simple_expr")
  | MenhirInterpreter.N MenhirInterpreter.N_label_var -> (fun _ -> "label_var")
  | MenhirInterpreter.N MenhirInterpreter.N_label_longident -> (fun _ -> "label_longident")
  | MenhirInterpreter.N MenhirInterpreter.N_label_let_pattern -> (fun _ -> "label_let_pattern")
  | MenhirInterpreter.N MenhirInterpreter.N_label_ident -> (fun _ -> "label_ident")
  | MenhirInterpreter.N MenhirInterpreter.N_label_expr -> (fun _ -> "label_expr")
  | MenhirInterpreter.N MenhirInterpreter.N_label_declarations -> (fun _ -> "label_declarations")
  | MenhirInterpreter.N MenhirInterpreter.N_label_declaration_semi -> (fun _ -> "label_declaration_semi")
  | MenhirInterpreter.N MenhirInterpreter.N_label_declaration -> (fun _ -> "label_declaration")
  | MenhirInterpreter.N MenhirInterpreter.N_label -> (fun _ -> "label")
  | MenhirInterpreter.N MenhirInterpreter.N_item_extension -> (fun _ -> "item_extension")
  | MenhirInterpreter.N MenhirInterpreter.N_interface -> (fun _ -> "interface")
  | MenhirInterpreter.N MenhirInterpreter.N_inherit_field_semi -> (fun _ -> "inherit_field_semi")
  | MenhirInterpreter.N MenhirInterpreter.N_implementation -> (fun _ -> "implementation")
  | MenhirInterpreter.N MenhirInterpreter.N_ident -> (fun _ -> "ident")
  | MenhirInterpreter.N MenhirInterpreter.N_generalized_constructor_arguments -> (fun _ -> "generalized_constructor_arguments")
  | MenhirInterpreter.N MenhirInterpreter.N_functor_args -> (fun _ -> "functor_args")
  | MenhirInterpreter.N MenhirInterpreter.N_functor_arg_name -> (fun _ -> "functor_arg_name")
  | MenhirInterpreter.N MenhirInterpreter.N_functor_arg -> (fun _ -> "functor_arg")
  | MenhirInterpreter.N MenhirInterpreter.N_fun_def -> (fun _ -> "fun_def")
  | MenhirInterpreter.N MenhirInterpreter.N_fun_binding -> (fun _ -> "fun_binding")
  | MenhirInterpreter.N MenhirInterpreter.N_floating_attribute -> (fun _ -> "floating_attribute")
  | MenhirInterpreter.N MenhirInterpreter.N_field_semi -> (fun _ -> "field_semi")
  | MenhirInterpreter.N MenhirInterpreter.N_field_expr_list -> (fun _ -> "field_expr_list")
  | MenhirInterpreter.N MenhirInterpreter.N_field_expr -> (fun _ -> "field_expr")
  | MenhirInterpreter.N MenhirInterpreter.N_field -> (fun _ -> "field")
  | MenhirInterpreter.N MenhirInterpreter.N_extension_constructor_rebind -> (fun _ -> "extension_constructor_rebind")
  | MenhirInterpreter.N MenhirInterpreter.N_extension_constructor_declaration -> (fun _ -> "extension_constructor_declaration")
  | MenhirInterpreter.N MenhirInterpreter.N_extension -> (fun _ -> "extension")
  | MenhirInterpreter.N MenhirInterpreter.N_ext_attributes -> (fun _ -> "ext_attributes")
  | MenhirInterpreter.N MenhirInterpreter.N_expr_semi_list -> (fun _ -> "expr_semi_list")
  | MenhirInterpreter.N MenhirInterpreter.N_expr_comma_list -> (fun _ -> "expr_comma_list")
  | MenhirInterpreter.N MenhirInterpreter.N_expr -> (fun _ -> "expr")
  | MenhirInterpreter.N MenhirInterpreter.N_direction_flag -> (fun _ -> "direction_flag")
  | MenhirInterpreter.N MenhirInterpreter.N_core_type_no_attr -> (fun _ -> "core_type_no_attr")
  | MenhirInterpreter.N MenhirInterpreter.N_core_type_list -> (fun _ -> "core_type_list")
  | MenhirInterpreter.N MenhirInterpreter.N_core_type_comma_list -> (fun _ -> "core_type_comma_list")
  | MenhirInterpreter.N MenhirInterpreter.N_core_type2 -> (fun _ -> "core_type2")
  | MenhirInterpreter.N MenhirInterpreter.N_core_type -> (fun _ -> "core_type")
  | MenhirInterpreter.N MenhirInterpreter.N_constructor_declarations -> (fun _ -> "constructor_declarations")
  | MenhirInterpreter.N MenhirInterpreter.N_constructor_declaration -> (fun _ -> "constructor_declaration")
  | MenhirInterpreter.N MenhirInterpreter.N_constructor_arguments -> (fun _ -> "constructor_arguments")
  | MenhirInterpreter.N MenhirInterpreter.N_constraints -> (fun _ -> "constraints")
  | MenhirInterpreter.N MenhirInterpreter.N_constrain_field -> (fun _ -> "constrain_field")
  | MenhirInterpreter.N MenhirInterpreter.N_constrain -> (fun _ -> "constrain")
  | MenhirInterpreter.N MenhirInterpreter.N_constr_longident -> (fun _ -> "constr_longident")
  | MenhirInterpreter.N MenhirInterpreter.N_constr_ident -> (fun _ -> "constr_ident")
  | MenhirInterpreter.N MenhirInterpreter.N_constant -> (fun _ -> "constant")
  | MenhirInterpreter.N MenhirInterpreter.N_clty_longident -> (fun _ -> "clty_longident")
  | MenhirInterpreter.N MenhirInterpreter.N_class_type_parameters -> (fun _ -> "class_type_parameters")
  | MenhirInterpreter.N MenhirInterpreter.N_class_type_declarations -> (fun _ -> "class_type_declarations")
  | MenhirInterpreter.N MenhirInterpreter.N_class_type_declaration -> (fun _ -> "class_type_declaration")
  | MenhirInterpreter.N MenhirInterpreter.N_class_type -> (fun _ -> "class_type")
  | MenhirInterpreter.N MenhirInterpreter.N_class_structure -> (fun _ -> "class_structure")
  | MenhirInterpreter.N MenhirInterpreter.N_class_simple_expr -> (fun _ -> "class_simple_expr")
  | MenhirInterpreter.N MenhirInterpreter.N_class_signature -> (fun _ -> "class_signature")
  | MenhirInterpreter.N MenhirInterpreter.N_class_sig_fields -> (fun _ -> "class_sig_fields")
  | MenhirInterpreter.N MenhirInterpreter.N_class_sig_field -> (fun _ -> "class_sig_field")
  | MenhirInterpreter.N MenhirInterpreter.N_class_sig_body -> (fun _ -> "class_sig_body")
  | MenhirInterpreter.N MenhirInterpreter.N_class_self_type -> (fun _ -> "class_self_type")
  | MenhirInterpreter.N MenhirInterpreter.N_class_self_pattern -> (fun _ -> "class_self_pattern")
  | MenhirInterpreter.N MenhirInterpreter.N_class_longident -> (fun _ -> "class_longident")
  | MenhirInterpreter.N MenhirInterpreter.N_class_fun_def -> (fun _ -> "class_fun_def")
  | MenhirInterpreter.N MenhirInterpreter.N_class_fun_binding -> (fun _ -> "class_fun_binding")
  | MenhirInterpreter.N MenhirInterpreter.N_class_fields -> (fun _ -> "class_fields")
  | MenhirInterpreter.N MenhirInterpreter.N_class_field -> (fun _ -> "class_field")
  | MenhirInterpreter.N MenhirInterpreter.N_class_expr -> (fun _ -> "class_expr")
  | MenhirInterpreter.N MenhirInterpreter.N_class_descriptions -> (fun _ -> "class_descriptions")
  | MenhirInterpreter.N MenhirInterpreter.N_class_description -> (fun _ -> "class_description")
  | MenhirInterpreter.N MenhirInterpreter.N_class_declarations -> (fun _ -> "class_declarations")
  | MenhirInterpreter.N MenhirInterpreter.N_class_declaration -> (fun _ -> "class_declaration")
  | MenhirInterpreter.N MenhirInterpreter.N_bar_extension_constructor_rebind -> (fun _ -> "bar_extension_constructor_rebind")
  | MenhirInterpreter.N MenhirInterpreter.N_bar_extension_constructor_declaration -> (fun _ -> "bar_extension_constructor_declaration")
  | MenhirInterpreter.N MenhirInterpreter.N_bar_constructor_declaration -> (fun _ -> "bar_constructor_declaration")
  | MenhirInterpreter.N MenhirInterpreter.N_attributes -> (fun _ -> "attributes")
  | MenhirInterpreter.N MenhirInterpreter.N_attribute -> (fun _ -> "attribute")
  | MenhirInterpreter.N MenhirInterpreter.N_attr_id -> (fun _ -> "attr_id")
  | MenhirInterpreter.N MenhirInterpreter.N_and_type_declaration -> (fun _ -> "and_type_declaration")
  | MenhirInterpreter.N MenhirInterpreter.N_and_module_declaration -> (fun _ -> "and_module_declaration")
  | MenhirInterpreter.N MenhirInterpreter.N_and_module_binding -> (fun _ -> "and_module_binding")
  | MenhirInterpreter.N MenhirInterpreter.N_and_let_binding -> (fun _ -> "and_let_binding")
  | MenhirInterpreter.N MenhirInterpreter.N_and_class_type_declaration -> (fun _ -> "and_class_type_declaration")
  | MenhirInterpreter.N MenhirInterpreter.N_and_class_description -> (fun _ -> "and_class_description")
  | MenhirInterpreter.N MenhirInterpreter.N_and_class_declaration -> (fun _ -> "and_class_declaration")
  | MenhirInterpreter.N MenhirInterpreter.N_amper_type_list -> (fun _ -> "amper_type_list")
  | MenhirInterpreter.N MenhirInterpreter.N_additive -> (fun _ -> "additive")

let print_token = function
  | WITH -> print_value (MenhirInterpreter.T MenhirInterpreter.T_WITH) ()
  | WHILE_LWT -> print_value (MenhirInterpreter.T MenhirInterpreter.T_WHILE_LWT) ()
  | WHILE -> print_value (MenhirInterpreter.T MenhirInterpreter.T_WHILE) ()
  | WHEN -> print_value (MenhirInterpreter.T MenhirInterpreter.T_WHEN) ()
  | VIRTUAL -> print_value (MenhirInterpreter.T MenhirInterpreter.T_VIRTUAL) ()
  | VAL -> print_value (MenhirInterpreter.T MenhirInterpreter.T_VAL) ()
  | UNDERSCORE -> print_value (MenhirInterpreter.T MenhirInterpreter.T_UNDERSCORE) ()
  | UIDENT v -> print_value (MenhirInterpreter.T MenhirInterpreter.T_UIDENT) v
  | TYPE -> print_value (MenhirInterpreter.T MenhirInterpreter.T_TYPE) ()
  | TRY_LWT -> print_value (MenhirInterpreter.T MenhirInterpreter.T_TRY_LWT) ()
  | TRY -> print_value (MenhirInterpreter.T MenhirInterpreter.T_TRY) ()
  | TRUE -> print_value (MenhirInterpreter.T MenhirInterpreter.T_TRUE) ()
  | TO -> print_value (MenhirInterpreter.T MenhirInterpreter.T_TO) ()
  | TILDE -> print_value (MenhirInterpreter.T MenhirInterpreter.T_TILDE) ()
  | THEN -> print_value (MenhirInterpreter.T MenhirInterpreter.T_THEN) ()
  | STRUCT -> print_value (MenhirInterpreter.T MenhirInterpreter.T_STRUCT) ()
  | STRING v -> print_value (MenhirInterpreter.T MenhirInterpreter.T_STRING) v
  | STAR -> print_value (MenhirInterpreter.T MenhirInterpreter.T_STAR) ()
  | SIG -> print_value (MenhirInterpreter.T MenhirInterpreter.T_SIG) ()
  | SHARP -> print_value (MenhirInterpreter.T MenhirInterpreter.T_SHARP) ()
  | SEMISEMI -> print_value (MenhirInterpreter.T MenhirInterpreter.T_SEMISEMI) ()
  | SEMI -> print_value (MenhirInterpreter.T MenhirInterpreter.T_SEMI) ()
  | RPAREN -> print_value (MenhirInterpreter.T MenhirInterpreter.T_RPAREN) ()
  | REC -> print_value (MenhirInterpreter.T MenhirInterpreter.T_REC) ()
  | RBRACKET -> print_value (MenhirInterpreter.T MenhirInterpreter.T_RBRACKET) ()
  | RBRACE -> print_value (MenhirInterpreter.T MenhirInterpreter.T_RBRACE) ()
  | QUOTE -> print_value (MenhirInterpreter.T MenhirInterpreter.T_QUOTE) ()
  | QUESTIONQUESTION -> print_value (MenhirInterpreter.T MenhirInterpreter.T_QUESTIONQUESTION) ()
  | QUESTION -> print_value (MenhirInterpreter.T MenhirInterpreter.T_QUESTION) ()
  | PRIVATE -> print_value (MenhirInterpreter.T MenhirInterpreter.T_PRIVATE) ()
  | PREFIXOP v -> print_value (MenhirInterpreter.T MenhirInterpreter.T_PREFIXOP) v
  | PLUSEQ -> print_value (MenhirInterpreter.T MenhirInterpreter.T_PLUSEQ) ()
  | PLUSDOT -> print_value (MenhirInterpreter.T MenhirInterpreter.T_PLUSDOT) ()
  | PLUS -> print_value (MenhirInterpreter.T MenhirInterpreter.T_PLUS) ()
  | PERCENT -> print_value (MenhirInterpreter.T MenhirInterpreter.T_PERCENT) ()
  | OR -> print_value (MenhirInterpreter.T MenhirInterpreter.T_OR) ()
  | OPTLABEL v -> print_value (MenhirInterpreter.T MenhirInterpreter.T_OPTLABEL) v
  | OPEN -> print_value (MenhirInterpreter.T MenhirInterpreter.T_OPEN) ()
  | OF -> print_value (MenhirInterpreter.T MenhirInterpreter.T_OF) ()
  | OBJECT -> print_value (MenhirInterpreter.T MenhirInterpreter.T_OBJECT) ()
  | NONREC -> print_value (MenhirInterpreter.T MenhirInterpreter.T_NONREC) ()
  | NEW -> print_value (MenhirInterpreter.T MenhirInterpreter.T_NEW) ()
  | MUTABLE -> print_value (MenhirInterpreter.T MenhirInterpreter.T_MUTABLE) ()
  | MODULE -> print_value (MenhirInterpreter.T MenhirInterpreter.T_MODULE) ()
  | MINUSGREATER -> print_value (MenhirInterpreter.T MenhirInterpreter.T_MINUSGREATER) ()
  | MINUSDOT -> print_value (MenhirInterpreter.T MenhirInterpreter.T_MINUSDOT) ()
  | MINUS -> print_value (MenhirInterpreter.T MenhirInterpreter.T_MINUS) ()
  | METHOD -> print_value (MenhirInterpreter.T MenhirInterpreter.T_METHOD) ()
  | MATCH_LWT -> print_value (MenhirInterpreter.T MenhirInterpreter.T_MATCH_LWT) ()
  | MATCH -> print_value (MenhirInterpreter.T MenhirInterpreter.T_MATCH) ()
  | LPAREN -> print_value (MenhirInterpreter.T MenhirInterpreter.T_LPAREN) ()
  | LIDENT v -> print_value (MenhirInterpreter.T MenhirInterpreter.T_LIDENT) v
  | LET_LWT -> print_value (MenhirInterpreter.T MenhirInterpreter.T_LET_LWT) ()
  | LET -> print_value (MenhirInterpreter.T MenhirInterpreter.T_LET) ()
  | LESSMINUS -> print_value (MenhirInterpreter.T MenhirInterpreter.T_LESSMINUS) ()
  | LESS -> print_value (MenhirInterpreter.T MenhirInterpreter.T_LESS) ()
  | LBRACKETPERCENTPERCENT -> print_value (MenhirInterpreter.T MenhirInterpreter.T_LBRACKETPERCENTPERCENT) ()
  | LBRACKETPERCENT -> print_value (MenhirInterpreter.T MenhirInterpreter.T_LBRACKETPERCENT) ()
  | LBRACKETLESS -> print_value (MenhirInterpreter.T MenhirInterpreter.T_LBRACKETLESS) ()
  | LBRACKETGREATER -> print_value (MenhirInterpreter.T MenhirInterpreter.T_LBRACKETGREATER) ()
  | LBRACKETBAR -> print_value (MenhirInterpreter.T MenhirInterpreter.T_LBRACKETBAR) ()
  | LBRACKETATATAT -> print_value (MenhirInterpreter.T MenhirInterpreter.T_LBRACKETATATAT) ()
  | LBRACKETATAT -> print_value (MenhirInterpreter.T MenhirInterpreter.T_LBRACKETATAT) ()
  | LBRACKETAT -> print_value (MenhirInterpreter.T MenhirInterpreter.T_LBRACKETAT) ()
  | LBRACKET -> print_value (MenhirInterpreter.T MenhirInterpreter.T_LBRACKET) ()
  | LBRACELESS -> print_value (MenhirInterpreter.T MenhirInterpreter.T_LBRACELESS) ()
  | LBRACE -> print_value (MenhirInterpreter.T MenhirInterpreter.T_LBRACE) ()
  | LAZY -> print_value (MenhirInterpreter.T MenhirInterpreter.T_LAZY) ()
  | LABEL v -> print_value (MenhirInterpreter.T MenhirInterpreter.T_LABEL) v
  | INT v -> print_value (MenhirInterpreter.T MenhirInterpreter.T_INT) v
  | INITIALIZER -> print_value (MenhirInterpreter.T MenhirInterpreter.T_INITIALIZER) ()
  | INHERIT -> print_value (MenhirInterpreter.T MenhirInterpreter.T_INHERIT) ()
  | INFIXOP4 v -> print_value (MenhirInterpreter.T MenhirInterpreter.T_INFIXOP4) v
  | INFIXOP3 v -> print_value (MenhirInterpreter.T MenhirInterpreter.T_INFIXOP3) v
  | INFIXOP2 v -> print_value (MenhirInterpreter.T MenhirInterpreter.T_INFIXOP2) v
  | INFIXOP1 v -> print_value (MenhirInterpreter.T MenhirInterpreter.T_INFIXOP1) v
  | INFIXOP0 v -> print_value (MenhirInterpreter.T MenhirInterpreter.T_INFIXOP0) v
  | INCLUDE -> print_value (MenhirInterpreter.T MenhirInterpreter.T_INCLUDE) ()
  | IN -> print_value (MenhirInterpreter.T MenhirInterpreter.T_IN) ()
  | IF -> print_value (MenhirInterpreter.T MenhirInterpreter.T_IF) ()
  | HASHOP v -> print_value (MenhirInterpreter.T MenhirInterpreter.T_HASHOP) v
  | GREATERRBRACKET -> print_value (MenhirInterpreter.T MenhirInterpreter.T_GREATERRBRACKET) ()
  | GREATERRBRACE -> print_value (MenhirInterpreter.T MenhirInterpreter.T_GREATERRBRACE) ()
  | GREATERDOT -> print_value (MenhirInterpreter.T MenhirInterpreter.T_GREATERDOT) ()
  | GREATER -> print_value (MenhirInterpreter.T MenhirInterpreter.T_GREATER) ()
  | FUNCTOR -> print_value (MenhirInterpreter.T MenhirInterpreter.T_FUNCTOR) ()
  | FUNCTION -> print_value (MenhirInterpreter.T MenhirInterpreter.T_FUNCTION) ()
  | FUN -> print_value (MenhirInterpreter.T MenhirInterpreter.T_FUN) ()
  | FOR_LWT -> print_value (MenhirInterpreter.T MenhirInterpreter.T_FOR_LWT) ()
  | FOR -> print_value (MenhirInterpreter.T MenhirInterpreter.T_FOR) ()
  | FLOAT v -> print_value (MenhirInterpreter.T MenhirInterpreter.T_FLOAT) v
  | FINALLY_LWT -> print_value (MenhirInterpreter.T MenhirInterpreter.T_FINALLY_LWT) ()
  | FALSE -> print_value (MenhirInterpreter.T MenhirInterpreter.T_FALSE) ()
  | EXTERNAL -> print_value (MenhirInterpreter.T MenhirInterpreter.T_EXTERNAL) ()
  | EXCEPTION -> print_value (MenhirInterpreter.T MenhirInterpreter.T_EXCEPTION) ()
  | EQUAL -> print_value (MenhirInterpreter.T MenhirInterpreter.T_EQUAL) ()
  | EOL -> print_value (MenhirInterpreter.T MenhirInterpreter.T_EOL) ()
  | EOF -> print_value (MenhirInterpreter.T MenhirInterpreter.T_EOF) ()
  | END -> print_value (MenhirInterpreter.T MenhirInterpreter.T_END) ()
  | ELSE -> print_value (MenhirInterpreter.T MenhirInterpreter.T_ELSE) ()
  | DOWNTO -> print_value (MenhirInterpreter.T MenhirInterpreter.T_DOWNTO) ()
  | DOTTILDE -> print_value (MenhirInterpreter.T MenhirInterpreter.T_DOTTILDE) ()
  | DOTOP v -> print_value (MenhirInterpreter.T MenhirInterpreter.T_DOTOP) v
  | DOTLESS -> print_value (MenhirInterpreter.T MenhirInterpreter.T_DOTLESS) ()
  | DOTDOT -> print_value (MenhirInterpreter.T MenhirInterpreter.T_DOTDOT) ()
  | DOT -> print_value (MenhirInterpreter.T MenhirInterpreter.T_DOT) ()
  | DONE -> print_value (MenhirInterpreter.T MenhirInterpreter.T_DONE) ()
  | DOCSTRING v -> print_value (MenhirInterpreter.T MenhirInterpreter.T_DOCSTRING) v
  | DO -> print_value (MenhirInterpreter.T MenhirInterpreter.T_DO) ()
  | CONSTRAINT -> print_value (MenhirInterpreter.T MenhirInterpreter.T_CONSTRAINT) ()
  | COMMENT v -> print_value (MenhirInterpreter.T MenhirInterpreter.T_COMMENT) v
  | COMMA -> print_value (MenhirInterpreter.T MenhirInterpreter.T_COMMA) ()
  | COLONGREATER -> print_value (MenhirInterpreter.T MenhirInterpreter.T_COLONGREATER) ()
  | COLONEQUAL -> print_value (MenhirInterpreter.T MenhirInterpreter.T_COLONEQUAL) ()
  | COLONCOLON -> print_value (MenhirInterpreter.T MenhirInterpreter.T_COLONCOLON) ()
  | COLON -> print_value (MenhirInterpreter.T MenhirInterpreter.T_COLON) ()
  | CLASS -> print_value (MenhirInterpreter.T MenhirInterpreter.T_CLASS) ()
  | CHAR v -> print_value (MenhirInterpreter.T MenhirInterpreter.T_CHAR) v
  | BEGIN -> print_value (MenhirInterpreter.T MenhirInterpreter.T_BEGIN) ()
  | BARRBRACKET -> print_value (MenhirInterpreter.T MenhirInterpreter.T_BARRBRACKET) ()
  | BARBAR -> print_value (MenhirInterpreter.T MenhirInterpreter.T_BARBAR) ()
  | BAR -> print_value (MenhirInterpreter.T MenhirInterpreter.T_BAR) ()
  | BANG -> print_value (MenhirInterpreter.T MenhirInterpreter.T_BANG) ()
  | BACKQUOTE -> print_value (MenhirInterpreter.T MenhirInterpreter.T_BACKQUOTE) ()
  | ASSERT -> print_value (MenhirInterpreter.T MenhirInterpreter.T_ASSERT) ()
  | AS -> print_value (MenhirInterpreter.T MenhirInterpreter.T_AS) ()
  | AND -> print_value (MenhirInterpreter.T MenhirInterpreter.T_AND) ()
  | AMPERSAND -> print_value (MenhirInterpreter.T MenhirInterpreter.T_AMPERSAND) ()
  | AMPERAMPER -> print_value (MenhirInterpreter.T MenhirInterpreter.T_AMPERAMPER) ()

let token_of_terminal (type a) (t : a MenhirInterpreter.terminal) (v : a) : token =
  match t with
  | MenhirInterpreter.T_error -> assert false
  | MenhirInterpreter.T_WITH -> WITH
  | MenhirInterpreter.T_WHILE_LWT -> WHILE_LWT
  | MenhirInterpreter.T_WHILE -> WHILE
  | MenhirInterpreter.T_WHEN -> WHEN
  | MenhirInterpreter.T_VIRTUAL -> VIRTUAL
  | MenhirInterpreter.T_VAL -> VAL
  | MenhirInterpreter.T_UNDERSCORE -> UNDERSCORE
  | MenhirInterpreter.T_UIDENT -> UIDENT v
  | MenhirInterpreter.T_TYPE -> TYPE
  | MenhirInterpreter.T_TRY_LWT -> TRY_LWT
  | MenhirInterpreter.T_TRY -> TRY
  | MenhirInterpreter.T_TRUE -> TRUE
  | MenhirInterpreter.T_TO -> TO
  | MenhirInterpreter.T_TILDE -> TILDE
  | MenhirInterpreter.T_THEN -> THEN
  | MenhirInterpreter.T_STRUCT -> STRUCT
  | MenhirInterpreter.T_STRING -> STRING v
  | MenhirInterpreter.T_STAR -> STAR
  | MenhirInterpreter.T_SIG -> SIG
  | MenhirInterpreter.T_SHARP -> SHARP
  | MenhirInterpreter.T_SEMISEMI -> SEMISEMI
  | MenhirInterpreter.T_SEMI -> SEMI
  | MenhirInterpreter.T_RPAREN -> RPAREN
  | MenhirInterpreter.T_REC -> REC
  | MenhirInterpreter.T_RBRACKET -> RBRACKET
  | MenhirInterpreter.T_RBRACE -> RBRACE
  | MenhirInterpreter.T_QUOTE -> QUOTE
  | MenhirInterpreter.T_QUESTIONQUESTION -> QUESTIONQUESTION
  | MenhirInterpreter.T_QUESTION -> QUESTION
  | MenhirInterpreter.T_PRIVATE -> PRIVATE
  | MenhirInterpreter.T_PREFIXOP -> PREFIXOP v
  | MenhirInterpreter.T_PLUSEQ -> PLUSEQ
  | MenhirInterpreter.T_PLUSDOT -> PLUSDOT
  | MenhirInterpreter.T_PLUS -> PLUS
  | MenhirInterpreter.T_PERCENT -> PERCENT
  | MenhirInterpreter.T_OR -> OR
  | MenhirInterpreter.T_OPTLABEL -> OPTLABEL v
  | MenhirInterpreter.T_OPEN -> OPEN
  | MenhirInterpreter.T_OF -> OF
  | MenhirInterpreter.T_OBJECT -> OBJECT
  | MenhirInterpreter.T_NONREC -> NONREC
  | MenhirInterpreter.T_NEW -> NEW
  | MenhirInterpreter.T_MUTABLE -> MUTABLE
  | MenhirInterpreter.T_MODULE -> MODULE
  | MenhirInterpreter.T_MINUSGREATER -> MINUSGREATER
  | MenhirInterpreter.T_MINUSDOT -> MINUSDOT
  | MenhirInterpreter.T_MINUS -> MINUS
  | MenhirInterpreter.T_METHOD -> METHOD
  | MenhirInterpreter.T_MATCH_LWT -> MATCH_LWT
  | MenhirInterpreter.T_MATCH -> MATCH
  | MenhirInterpreter.T_LPAREN -> LPAREN
  | MenhirInterpreter.T_LIDENT -> LIDENT v
  | MenhirInterpreter.T_LET_LWT -> LET_LWT
  | MenhirInterpreter.T_LET -> LET
  | MenhirInterpreter.T_LESSMINUS -> LESSMINUS
  | MenhirInterpreter.T_LESS -> LESS
  | MenhirInterpreter.T_LBRACKETPERCENTPERCENT -> LBRACKETPERCENTPERCENT
  | MenhirInterpreter.T_LBRACKETPERCENT -> LBRACKETPERCENT
  | MenhirInterpreter.T_LBRACKETLESS -> LBRACKETLESS
  | MenhirInterpreter.T_LBRACKETGREATER -> LBRACKETGREATER
  | MenhirInterpreter.T_LBRACKETBAR -> LBRACKETBAR
  | MenhirInterpreter.T_LBRACKETATATAT -> LBRACKETATATAT
  | MenhirInterpreter.T_LBRACKETATAT -> LBRACKETATAT
  | MenhirInterpreter.T_LBRACKETAT -> LBRACKETAT
  | MenhirInterpreter.T_LBRACKET -> LBRACKET
  | MenhirInterpreter.T_LBRACELESS -> LBRACELESS
  | MenhirInterpreter.T_LBRACE -> LBRACE
  | MenhirInterpreter.T_LAZY -> LAZY
  | MenhirInterpreter.T_LABEL -> LABEL v
  | MenhirInterpreter.T_INT -> INT v
  | MenhirInterpreter.T_INITIALIZER -> INITIALIZER
  | MenhirInterpreter.T_INHERIT -> INHERIT
  | MenhirInterpreter.T_INFIXOP4 -> INFIXOP4 v
  | MenhirInterpreter.T_INFIXOP3 -> INFIXOP3 v
  | MenhirInterpreter.T_INFIXOP2 -> INFIXOP2 v
  | MenhirInterpreter.T_INFIXOP1 -> INFIXOP1 v
  | MenhirInterpreter.T_INFIXOP0 -> INFIXOP0 v
  | MenhirInterpreter.T_INCLUDE -> INCLUDE
  | MenhirInterpreter.T_IN -> IN
  | MenhirInterpreter.T_IF -> IF
  | MenhirInterpreter.T_HASHOP -> HASHOP v
  | MenhirInterpreter.T_GREATERRBRACKET -> GREATERRBRACKET
  | MenhirInterpreter.T_GREATERRBRACE -> GREATERRBRACE
  | MenhirInterpreter.T_GREATERDOT -> GREATERDOT
  | MenhirInterpreter.T_GREATER -> GREATER
  | MenhirInterpreter.T_FUNCTOR -> FUNCTOR
  | MenhirInterpreter.T_FUNCTION -> FUNCTION
  | MenhirInterpreter.T_FUN -> FUN
  | MenhirInterpreter.T_FOR_LWT -> FOR_LWT
  | MenhirInterpreter.T_FOR -> FOR
  | MenhirInterpreter.T_FLOAT -> FLOAT v
  | MenhirInterpreter.T_FINALLY_LWT -> FINALLY_LWT
  | MenhirInterpreter.T_FALSE -> FALSE
  | MenhirInterpreter.T_EXTERNAL -> EXTERNAL
  | MenhirInterpreter.T_EXCEPTION -> EXCEPTION
  | MenhirInterpreter.T_EQUAL -> EQUAL
  | MenhirInterpreter.T_EOL -> EOL
  | MenhirInterpreter.T_EOF -> EOF
  | MenhirInterpreter.T_END -> END
  | MenhirInterpreter.T_ELSE -> ELSE
  | MenhirInterpreter.T_DOWNTO -> DOWNTO
  | MenhirInterpreter.T_DOTTILDE -> DOTTILDE
  | MenhirInterpreter.T_DOTOP -> DOTOP v
  | MenhirInterpreter.T_DOTLESS -> DOTLESS
  | MenhirInterpreter.T_DOTDOT -> DOTDOT
  | MenhirInterpreter.T_DOT -> DOT
  | MenhirInterpreter.T_DONE -> DONE
  | MenhirInterpreter.T_DOCSTRING -> DOCSTRING v
  | MenhirInterpreter.T_DO -> DO
  | MenhirInterpreter.T_CONSTRAINT -> CONSTRAINT
  | MenhirInterpreter.T_COMMENT -> COMMENT v
  | MenhirInterpreter.T_COMMA -> COMMA
  | MenhirInterpreter.T_COLONGREATER -> COLONGREATER
  | MenhirInterpreter.T_COLONEQUAL -> COLONEQUAL
  | MenhirInterpreter.T_COLONCOLON -> COLONCOLON
  | MenhirInterpreter.T_COLON -> COLON
  | MenhirInterpreter.T_CLASS -> CLASS
  | MenhirInterpreter.T_CHAR -> CHAR v
  | MenhirInterpreter.T_BEGIN -> BEGIN
  | MenhirInterpreter.T_BARRBRACKET -> BARRBRACKET
  | MenhirInterpreter.T_BARBAR -> BARBAR
  | MenhirInterpreter.T_BAR -> BAR
  | MenhirInterpreter.T_BANG -> BANG
  | MenhirInterpreter.T_BACKQUOTE -> BACKQUOTE
  | MenhirInterpreter.T_ASSERT -> ASSERT
  | MenhirInterpreter.T_AS -> AS
  | MenhirInterpreter.T_AND -> AND
  | MenhirInterpreter.T_AMPERSAND -> AMPERSAND
  | MenhirInterpreter.T_AMPERAMPER -> AMPERAMPER
