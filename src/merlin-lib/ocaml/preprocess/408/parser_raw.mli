
(* The type of tokens. *)

type token = 
  | WITH
  | WHILE_LWT
  | WHILE
  | WHEN
  | VIRTUAL
  | VAL
  | UNDERSCORE
  | UIDENT of (string)
  | TYPE
  | TRY_LWT
  | TRY
  | TRUE
  | TO
  | TILDE
  | THEN
  | STRUCT
  | STRING of (string * string option)
  | STAR
  | SIG
  | SEMISEMI
  | SEMI
  | RPAREN
  | REC
  | RBRACKET
  | RBRACE
  | QUOTE
  | QUESTIONQUESTION
  | QUESTION
  | PRIVATE
  | PREFIXOP of (string)
  | PLUSEQ
  | PLUSDOT
  | PLUS
  | PERCENT
  | OR
  | OPTLABEL of (string)
  | OPEN
  | OF
  | OBJECT
  | NONREC
  | NEW
  | MUTABLE
  | MODULE
  | MINUSGREATER
  | MINUSDOT
  | MINUS
  | METHOD
  | MATCH_LWT
  | MATCH
  | LPAREN
  | LIDENT of (string)
  | LET_LWT
  | LETOP of (string)
  | LET
  | LESSMINUS
  | LESS
  | LBRACKETPERCENTPERCENT
  | LBRACKETPERCENT
  | LBRACKETLESS
  | LBRACKETGREATER
  | LBRACKETBAR
  | LBRACKETATATAT
  | LBRACKETATAT
  | LBRACKETAT
  | LBRACKET
  | LBRACELESS
  | LBRACE
  | LAZY
  | LABEL of (string)
  | INT of (string * char option)
  | INITIALIZER
  | INHERIT
  | INFIXOP4 of (string)
  | INFIXOP3 of (string)
  | INFIXOP2 of (string)
  | INFIXOP1 of (string)
  | INFIXOP0 of (string)
  | INCLUDE
  | IN
  | IF
  | HASHOP of (string)
  | HASH
  | GREATERRBRACKET
  | GREATERRBRACE
  | GREATERDOT
  | GREATER
  | FUNCTOR
  | FUNCTION
  | FUN
  | FOR_LWT
  | FOR
  | FLOAT of (string * char option)
  | FINALLY_LWT
  | FALSE
  | EXTERNAL
  | EXCEPTION
  | EQUAL
  | EOL
  | EOF
  | END
  | ELSE
  | DOWNTO
  | DOTTILDE
  | DOTOP of (string)
  | DOTLESS
  | DOTDOT
  | DOT
  | DONE
  | DOCSTRING of (Docstrings.docstring)
  | DO
  | CONSTRAINT
  | COMMENT of (string * Location.t)
  | COMMA
  | COLONGREATER
  | COLONEQUAL
  | COLONCOLON
  | COLON
  | CLASS
  | CHAR of (char)
  | BEGIN
  | BARRBRACKET
  | BARBAR
  | BAR
  | BANG
  | BACKQUOTE
  | ASSERT
  | AS
  | ANDOP of (string)
  | AND
  | AMPERSAND
  | AMPERAMPER

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val use_file: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Parsetree.toplevel_phrase list)

val toplevel_phrase: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Parsetree.toplevel_phrase)

val parse_pattern: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Parsetree.pattern)

val parse_expression: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Parsetree.expression)

val parse_core_type: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Parsetree.core_type)

val interface: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Parsetree.signature)

val implementation: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Parsetree.structure)

module MenhirInterpreter : sig
  
  (* The incremental API. *)
  
  include MenhirLib.IncrementalEngine.INCREMENTAL_ENGINE
    with type token = token
  
  (* The indexed type of terminal symbols. *)
  
  type _ terminal = 
    | T_error : unit terminal
    | T_WITH : unit terminal
    | T_WHILE_LWT : unit terminal
    | T_WHILE : unit terminal
    | T_WHEN : unit terminal
    | T_VIRTUAL : unit terminal
    | T_VAL : unit terminal
    | T_UNDERSCORE : unit terminal
    | T_UIDENT : (string) terminal
    | T_TYPE : unit terminal
    | T_TRY_LWT : unit terminal
    | T_TRY : unit terminal
    | T_TRUE : unit terminal
    | T_TO : unit terminal
    | T_TILDE : unit terminal
    | T_THEN : unit terminal
    | T_STRUCT : unit terminal
    | T_STRING : (string * string option) terminal
    | T_STAR : unit terminal
    | T_SIG : unit terminal
    | T_SEMISEMI : unit terminal
    | T_SEMI : unit terminal
    | T_RPAREN : unit terminal
    | T_REC : unit terminal
    | T_RBRACKET : unit terminal
    | T_RBRACE : unit terminal
    | T_QUOTE : unit terminal
    | T_QUESTIONQUESTION : unit terminal
    | T_QUESTION : unit terminal
    | T_PRIVATE : unit terminal
    | T_PREFIXOP : (string) terminal
    | T_PLUSEQ : unit terminal
    | T_PLUSDOT : unit terminal
    | T_PLUS : unit terminal
    | T_PERCENT : unit terminal
    | T_OR : unit terminal
    | T_OPTLABEL : (string) terminal
    | T_OPEN : unit terminal
    | T_OF : unit terminal
    | T_OBJECT : unit terminal
    | T_NONREC : unit terminal
    | T_NEW : unit terminal
    | T_MUTABLE : unit terminal
    | T_MODULE : unit terminal
    | T_MINUSGREATER : unit terminal
    | T_MINUSDOT : unit terminal
    | T_MINUS : unit terminal
    | T_METHOD : unit terminal
    | T_MATCH_LWT : unit terminal
    | T_MATCH : unit terminal
    | T_LPAREN : unit terminal
    | T_LIDENT : (string) terminal
    | T_LET_LWT : unit terminal
    | T_LETOP : (string) terminal
    | T_LET : unit terminal
    | T_LESSMINUS : unit terminal
    | T_LESS : unit terminal
    | T_LBRACKETPERCENTPERCENT : unit terminal
    | T_LBRACKETPERCENT : unit terminal
    | T_LBRACKETLESS : unit terminal
    | T_LBRACKETGREATER : unit terminal
    | T_LBRACKETBAR : unit terminal
    | T_LBRACKETATATAT : unit terminal
    | T_LBRACKETATAT : unit terminal
    | T_LBRACKETAT : unit terminal
    | T_LBRACKET : unit terminal
    | T_LBRACELESS : unit terminal
    | T_LBRACE : unit terminal
    | T_LAZY : unit terminal
    | T_LABEL : (string) terminal
    | T_INT : (string * char option) terminal
    | T_INITIALIZER : unit terminal
    | T_INHERIT : unit terminal
    | T_INFIXOP4 : (string) terminal
    | T_INFIXOP3 : (string) terminal
    | T_INFIXOP2 : (string) terminal
    | T_INFIXOP1 : (string) terminal
    | T_INFIXOP0 : (string) terminal
    | T_INCLUDE : unit terminal
    | T_IN : unit terminal
    | T_IF : unit terminal
    | T_HASHOP : (string) terminal
    | T_HASH : unit terminal
    | T_GREATERRBRACKET : unit terminal
    | T_GREATERRBRACE : unit terminal
    | T_GREATERDOT : unit terminal
    | T_GREATER : unit terminal
    | T_FUNCTOR : unit terminal
    | T_FUNCTION : unit terminal
    | T_FUN : unit terminal
    | T_FOR_LWT : unit terminal
    | T_FOR : unit terminal
    | T_FLOAT : (string * char option) terminal
    | T_FINALLY_LWT : unit terminal
    | T_FALSE : unit terminal
    | T_EXTERNAL : unit terminal
    | T_EXCEPTION : unit terminal
    | T_EQUAL : unit terminal
    | T_EOL : unit terminal
    | T_EOF : unit terminal
    | T_END : unit terminal
    | T_ELSE : unit terminal
    | T_DOWNTO : unit terminal
    | T_DOTTILDE : unit terminal
    | T_DOTOP : (string) terminal
    | T_DOTLESS : unit terminal
    | T_DOTDOT : unit terminal
    | T_DOT : unit terminal
    | T_DONE : unit terminal
    | T_DOCSTRING : (Docstrings.docstring) terminal
    | T_DO : unit terminal
    | T_CONSTRAINT : unit terminal
    | T_COMMENT : (string * Location.t) terminal
    | T_COMMA : unit terminal
    | T_COLONGREATER : unit terminal
    | T_COLONEQUAL : unit terminal
    | T_COLONCOLON : unit terminal
    | T_COLON : unit terminal
    | T_CLASS : unit terminal
    | T_CHAR : (char) terminal
    | T_BEGIN : unit terminal
    | T_BARRBRACKET : unit terminal
    | T_BARBAR : unit terminal
    | T_BAR : unit terminal
    | T_BANG : unit terminal
    | T_BACKQUOTE : unit terminal
    | T_ASSERT : unit terminal
    | T_AS : unit terminal
    | T_ANDOP : (string) terminal
    | T_AND : unit terminal
    | T_AMPERSAND : unit terminal
    | T_AMPERAMPER : unit terminal
  
  (* The indexed type of nonterminal symbols. *)
  
  type _ nonterminal = 
    | N_with_type_binder : (Asttypes.private_flag) nonterminal
    | N_with_constraint : (Parsetree.with_constraint) nonterminal
    | N_virtual_with_private_flag : (Asttypes.private_flag) nonterminal
    | N_virtual_with_mutable_flag : (Asttypes.mutable_flag) nonterminal
    | N_virtual_flag : (Asttypes.virtual_flag) nonterminal
    | N_value_description : (Parsetree.value_description * string Location.loc option) nonterminal
    | N_value : ((string Location.loc * Asttypes.mutable_flag * Parsetree.class_field_kind) *
  Parsetree.attributes) nonterminal
    | N_val_longident : (Longident.t) nonterminal
    | N_val_ident : (string) nonterminal
    | N_use_file : (Parsetree.toplevel_phrase list) nonterminal
    | N_type_variance : (Asttypes.variance) nonterminal
    | N_type_variable : (Parsetree.core_type) nonterminal
    | N_type_parameters : ((Parsetree.core_type * Asttypes.variance) list) nonterminal
    | N_type_parameter : (Parsetree.core_type * Asttypes.variance) nonterminal
    | N_type_longident : (Longident.t) nonterminal
    | N_type_kind : (Parsetree.type_kind * Asttypes.private_flag * Parsetree.core_type option) nonterminal
    | N_type_constraint : (Parsetree.core_type option * Parsetree.core_type option) nonterminal
    | N_tuple_type : (Parsetree.core_type) nonterminal
    | N_toplevel_phrase : (Parsetree.toplevel_phrase) nonterminal
    | N_toplevel_directive : (Parsetree.toplevel_phrase) nonterminal
    | N_tag_field : (Parsetree.row_field) nonterminal
    | N_subtractive : (string) nonterminal
    | N_structure_item : (Parsetree.structure_item) nonterminal
    | N_structure : (Parsetree.structure) nonterminal
    | N_strict_binding : (Parsetree.expression) nonterminal
    | N_str_exception_declaration : (Parsetree.type_exception * string Location.loc option) nonterminal
    | N_single_attr_id : (string) nonterminal
    | N_simple_pattern_not_ident : (Parsetree.pattern) nonterminal
    | N_simple_pattern : (Parsetree.pattern) nonterminal
    | N_simple_expr : (Parsetree.expression) nonterminal
    | N_simple_delimited_pattern : (Parsetree.pattern) nonterminal
    | N_signed_constant : (Parsetree.constant) nonterminal
    | N_signature_item : (Parsetree.signature_item) nonterminal
    | N_signature : (Parsetree.signature) nonterminal
    | N_sig_exception_declaration : (Parsetree.type_exception * string Location.loc option) nonterminal
    | N_seq_expr : (Parsetree.expression) nonterminal
    | N_separated_or_terminated_nonempty_list_SEMI_record_expr_field_ : ((Longident.t Location.loc * Parsetree.expression) list) nonterminal
    | N_separated_or_terminated_nonempty_list_SEMI_pattern_ : (Parsetree.pattern list) nonterminal
    | N_separated_or_terminated_nonempty_list_SEMI_object_expr_field_ : ((string Location.loc * Parsetree.expression) list) nonterminal
    | N_separated_or_terminated_nonempty_list_SEMI_expr_ : (Parsetree.expression list) nonterminal
    | N_row_field : (Parsetree.row_field) nonterminal
    | N_reversed_separated_nontrivial_llist_STAR_atomic_type_ : (Parsetree.core_type list) nonterminal
    | N_reversed_separated_nontrivial_llist_COMMA_expr_ : (Parsetree.expression list) nonterminal
    | N_reversed_separated_nontrivial_llist_COMMA_core_type_ : (Parsetree.core_type list) nonterminal
    | N_reversed_separated_nonempty_llist_STAR_atomic_type_ : (Parsetree.core_type list) nonterminal
    | N_reversed_separated_nonempty_llist_COMMA_type_parameter_ : ((Parsetree.core_type * Asttypes.variance) list) nonterminal
    | N_reversed_separated_nonempty_llist_COMMA_core_type_ : (Parsetree.core_type list) nonterminal
    | N_reversed_separated_nonempty_llist_BAR_row_field_ : (Parsetree.row_field list) nonterminal
    | N_reversed_separated_nonempty_llist_AND_with_constraint_ : (Parsetree.with_constraint list) nonterminal
    | N_reversed_separated_nonempty_llist_AMPERSAND_core_type_no_attr_ : (Parsetree.core_type list) nonterminal
    | N_reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ : (Parsetree.case list) nonterminal
    | N_reversed_nonempty_llist_typevar_ : (string Location.loc list) nonterminal
    | N_reversed_nonempty_llist_name_tag_ : (string list) nonterminal
    | N_reversed_nonempty_llist_labeled_simple_expr_ : ((Asttypes.arg_label * Parsetree.expression) list) nonterminal
    | N_reversed_nonempty_llist_functor_arg_ : ((string Location.loc * Parsetree.module_type option) list) nonterminal
    | N_reversed_llist_preceded_CONSTRAINT_constrain__ : ((Parsetree.core_type * Parsetree.core_type * Warnings.loc) list) nonterminal
    | N_reversed_bar_llist_extension_constructor_declaration_ : (Parsetree.extension_constructor list) nonterminal
    | N_reversed_bar_llist_extension_constructor_ : (Parsetree.extension_constructor list) nonterminal
    | N_reversed_bar_llist_constructor_declaration_ : (Parsetree.constructor_declaration list) nonterminal
    | N_record_expr_content : (Parsetree.expression option *
  (Longident.t Location.loc * Parsetree.expression) list) nonterminal
    | N_rec_flag : (Asttypes.rec_flag) nonterminal
    | N_private_virtual_flags : (Asttypes.private_flag * Asttypes.virtual_flag) nonterminal
    | N_private_flag : (Asttypes.private_flag) nonterminal
    | N_primitive_declaration : (Parsetree.value_description * string Location.loc option) nonterminal
    | N_post_item_attribute : (Parsetree.attribute) nonterminal
    | N_possibly_poly_core_type_no_attr_ : (Parsetree.core_type) nonterminal
    | N_possibly_poly_core_type_ : (Parsetree.core_type) nonterminal
    | N_payload : (Parsetree.payload) nonterminal
    | N_pattern_var : (Parsetree.pattern) nonterminal
    | N_pattern_no_exn : (Parsetree.pattern) nonterminal
    | N_pattern_gen : (Parsetree.pattern) nonterminal
    | N_pattern_comma_list_pattern_no_exn_ : (Parsetree.pattern list) nonterminal
    | N_pattern_comma_list_pattern_ : (Parsetree.pattern list) nonterminal
    | N_pattern : (Parsetree.pattern) nonterminal
    | N_parse_pattern : (Parsetree.pattern) nonterminal
    | N_parse_expression : (Parsetree.expression) nonterminal
    | N_parse_core_type : (Parsetree.core_type) nonterminal
    | N_paren_module_expr : (Parsetree.module_expr) nonterminal
    | N_optlabel : (string) nonterminal
    | N_option_type_constraint_ : ((Parsetree.core_type option * Parsetree.core_type option) option) nonterminal
    | N_option_preceded_EQUAL_seq_expr__ : (Parsetree.expression option) nonterminal
    | N_option_preceded_EQUAL_pattern__ : (Parsetree.pattern option) nonterminal
    | N_option_preceded_EQUAL_module_type__ : (Parsetree.module_type option) nonterminal
    | N_option_preceded_EQUAL_expr__ : (Parsetree.expression option) nonterminal
    | N_option_preceded_COLON_core_type__ : (Parsetree.core_type option) nonterminal
    | N_option_preceded_AS_mkrhs_LIDENT___ : (string Location.loc option) nonterminal
    | N_option_SEMI_ : (unit option) nonterminal
    | N_option_BAR_ : (unit option) nonterminal
    | N_opt_ampersand : (bool) nonterminal
    | N_operator : (string) nonterminal
    | N_open_description : (Parsetree.open_description * string Location.loc option) nonterminal
    | N_open_declaration : (Parsetree.open_declaration * string Location.loc option) nonterminal
    | N_nonempty_type_kind : (Parsetree.type_kind * Asttypes.private_flag * Parsetree.core_type option) nonterminal
    | N_nonempty_list_raw_string_ : (string list) nonterminal
    | N_nonempty_list_mkrhs_LIDENT__ : (string Location.loc list) nonterminal
    | N_name_tag : (string) nonterminal
    | N_mutable_virtual_flags : (Asttypes.mutable_flag * Asttypes.virtual_flag) nonterminal
    | N_mutable_flag : (Asttypes.mutable_flag) nonterminal
    | N_mty_longident : (Longident.t) nonterminal
    | N_module_type_declaration : (Parsetree.module_type_declaration * string Location.loc option) nonterminal
    | N_module_type : (Parsetree.module_type) nonterminal
    | N_module_subst : (Parsetree.module_substitution * string Location.loc option) nonterminal
    | N_module_expr : (Parsetree.module_expr) nonterminal
    | N_module_declaration_body : (Parsetree.module_type) nonterminal
    | N_module_binding_body : (Parsetree.module_expr) nonterminal
    | N_mod_longident : (Longident.t) nonterminal
    | N_mod_ext_longident : (Longident.t) nonterminal
    | N_method_ : ((string Location.loc * Asttypes.private_flag * Parsetree.class_field_kind) *
  Parsetree.attributes) nonterminal
    | N_meth_list : (Parsetree.object_field list * Asttypes.closed_flag) nonterminal
    | N_match_case : (Parsetree.case) nonterminal
    | N_lwt_bindings : (Ast_helper.let_bindings) nonterminal
    | N_lwt_binding : (Ast_helper.let_bindings) nonterminal
    | N_listx_SEMI_record_pat_field_UNDERSCORE_ : ((Longident.t Location.loc * Parsetree.pattern) list * unit option) nonterminal
    | N_list_use_file_element_ : (Parsetree.toplevel_phrase list list) nonterminal
    | N_list_text_str_structure_item__ : (Parsetree.structure_item list list) nonterminal
    | N_list_text_cstr_class_field__ : (Parsetree.class_field list list) nonterminal
    | N_list_text_csig_class_sig_field__ : (Parsetree.class_type_field list list) nonterminal
    | N_list_structure_element_ : (Parsetree.structure_item list list) nonterminal
    | N_list_signature_element_ : (Parsetree.signature_item list list) nonterminal
    | N_list_post_item_attribute_ : (Parsetree.attributes) nonterminal
    | N_list_generic_and_type_declaration_type_subst_kind__ : (Parsetree.type_declaration list) nonterminal
    | N_list_generic_and_type_declaration_type_kind__ : (Parsetree.type_declaration list) nonterminal
    | N_list_attribute_ : (Parsetree.attributes) nonterminal
    | N_list_and_module_declaration_ : (Parsetree.module_declaration list) nonterminal
    | N_list_and_module_binding_ : (Parsetree.module_binding list) nonterminal
    | N_list_and_class_type_declaration_ : (Parsetree.class_type_declaration list) nonterminal
    | N_list_and_class_description_ : (Parsetree.class_description list) nonterminal
    | N_list_and_class_declaration_ : (Parsetree.class_declaration list) nonterminal
    | N_letop_bindings : (Parsetree.pattern * Parsetree.expression * Parsetree.binding_op list) nonterminal
    | N_letop_binding_body : (Parsetree.pattern * Parsetree.expression) nonterminal
    | N_let_pattern : (Parsetree.pattern) nonterminal
    | N_let_bindings_no_ext_ : (Ast_helper.let_bindings) nonterminal
    | N_let_bindings_ext_ : (Ast_helper.let_bindings) nonterminal
    | N_let_binding_body : (Parsetree.pattern * Parsetree.expression) nonterminal
    | N_labeled_simple_pattern : (Asttypes.arg_label * Parsetree.expression option * Parsetree.pattern) nonterminal
    | N_labeled_simple_expr : (Asttypes.arg_label * Parsetree.expression) nonterminal
    | N_label_longident : (Longident.t) nonterminal
    | N_label_let_pattern : (string * Parsetree.pattern) nonterminal
    | N_label_declarations : (Parsetree.label_declaration list) nonterminal
    | N_label_declaration_semi : (Parsetree.label_declaration) nonterminal
    | N_label_declaration : (Parsetree.label_declaration) nonterminal
    | N_item_extension : (Parsetree.extension) nonterminal
    | N_interface : (Parsetree.signature) nonterminal
    | N_implementation : (Parsetree.structure) nonterminal
    | N_ident : (string) nonterminal
    | N_generic_type_declaration_nonrec_flag_type_kind_ : ((Asttypes.rec_flag * string Location.loc option) *
  Parsetree.type_declaration) nonterminal
    | N_generic_type_declaration_no_nonrec_flag_type_subst_kind_ : ((Asttypes.rec_flag * string Location.loc option) *
  Parsetree.type_declaration) nonterminal
    | N_generic_constructor_declaration_epsilon_ : (Ast_helper.str * Parsetree.constructor_arguments *
  Parsetree.core_type option * Parsetree.attributes * Warnings.loc *
  Docstrings.info) nonterminal
    | N_generic_constructor_declaration_BAR_ : (Ast_helper.str * Parsetree.constructor_arguments *
  Parsetree.core_type option * Parsetree.attributes * Warnings.loc *
  Docstrings.info) nonterminal
    | N_generalized_constructor_arguments : (Parsetree.constructor_arguments * Parsetree.core_type option) nonterminal
    | N_functor_args : ((string Location.loc * Parsetree.module_type option) list) nonterminal
    | N_functor_arg_name : (string) nonterminal
    | N_functor_arg : (string Location.loc * Parsetree.module_type option) nonterminal
    | N_function_type : (Parsetree.core_type) nonterminal
    | N_fun_def : (Parsetree.expression) nonterminal
    | N_fun_binding : (Parsetree.expression) nonterminal
    | N_formal_class_parameters : ((Parsetree.core_type * Asttypes.variance) list) nonterminal
    | N_floating_attribute : (Parsetree.attribute) nonterminal
    | N_extension_constructor_rebind_epsilon_ : (Parsetree.extension_constructor) nonterminal
    | N_extension_constructor_rebind_BAR_ : (Parsetree.extension_constructor) nonterminal
    | N_extension : (Parsetree.extension) nonterminal
    | N_ext : (string Location.loc option) nonterminal
    | N_expr : (Parsetree.expression) nonterminal
    | N_direction_flag : (Asttypes.direction_flag) nonterminal
    | N_core_type : (Parsetree.core_type) nonterminal
    | N_constructor_declarations : (Parsetree.constructor_declaration list) nonterminal
    | N_constructor_arguments : (Parsetree.constructor_arguments) nonterminal
    | N_constrain_field : (Parsetree.core_type * Parsetree.core_type) nonterminal
    | N_constr_longident : (Longident.t) nonterminal
    | N_constr_ident : (string) nonterminal
    | N_constant : (Parsetree.constant) nonterminal
    | N_clty_longident : (Longident.t) nonterminal
    | N_class_type_declarations : (string Location.loc option * Parsetree.class_type_declaration list) nonterminal
    | N_class_type : (Parsetree.class_type) nonterminal
    | N_class_simple_expr : (Parsetree.class_expr) nonterminal
    | N_class_signature : (Parsetree.class_type) nonterminal
    | N_class_sig_field : (Parsetree.class_type_field) nonterminal
    | N_class_self_type : (Parsetree.core_type) nonterminal
    | N_class_self_pattern : (Parsetree.pattern) nonterminal
    | N_class_longident : (Longident.t) nonterminal
    | N_class_fun_def : (Parsetree.class_expr) nonterminal
    | N_class_fun_binding : (Parsetree.class_expr) nonterminal
    | N_class_field : (Parsetree.class_field) nonterminal
    | N_class_expr : (Parsetree.class_expr) nonterminal
    | N_attribute : (Parsetree.attribute) nonterminal
    | N_attr_id : (string Location.loc) nonterminal
    | N_atomic_type : (Parsetree.core_type) nonterminal
    | N_and_let_binding : (Ast_helper.let_binding) nonterminal
    | N_alias_type : (Parsetree.core_type) nonterminal
    | N_additive : (string) nonterminal
  
  (* The inspection API. *)
  
  include MenhirLib.IncrementalEngine.INSPECTION
    with type 'a lr1state := 'a lr1state
    with type production := production
    with type 'a terminal := 'a terminal
    with type 'a nonterminal := 'a nonterminal
    with type 'a env := 'a env
  
end

(* The entry point(s) to the incremental API. *)

module Incremental : sig
  
  val use_file: Lexing.position -> (Parsetree.toplevel_phrase list) MenhirInterpreter.checkpoint
  
  val toplevel_phrase: Lexing.position -> (Parsetree.toplevel_phrase) MenhirInterpreter.checkpoint
  
  val parse_pattern: Lexing.position -> (Parsetree.pattern) MenhirInterpreter.checkpoint
  
  val parse_expression: Lexing.position -> (Parsetree.expression) MenhirInterpreter.checkpoint
  
  val parse_core_type: Lexing.position -> (Parsetree.core_type) MenhirInterpreter.checkpoint
  
  val interface: Lexing.position -> (Parsetree.signature) MenhirInterpreter.checkpoint
  
  val implementation: Lexing.position -> (Parsetree.structure) MenhirInterpreter.checkpoint
  
end
