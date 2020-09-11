
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
  | SHARP
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
  | AND
  | AMPERSAND
  | AMPERAMPER

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val parse_expression: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Parsetree.expression)

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
    | T_SHARP : unit terminal
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
    | T_AND : unit terminal
    | T_AMPERSAND : unit terminal
    | T_AMPERAMPER : unit terminal
  
  (* The indexed type of nonterminal symbols. *)
  
  type _ nonterminal = 
    | N_with_type_binder : (Asttypes.private_flag) nonterminal
    | N_with_constraints : (Parsetree.with_constraint list) nonterminal
    | N_with_constraint : (Parsetree.with_constraint) nonterminal
    | N_virtual_flag : (Asttypes.virtual_flag) nonterminal
    | N_value_type : (string * Asttypes.mutable_flag * Asttypes.virtual_flag *
  Parsetree.core_type) nonterminal
    | N_value_description : (Parsetree.value_description * string Location.loc option) nonterminal
    | N_value : ((string Location.loc * Asttypes.mutable_flag * Parsetree.class_field_kind) *
  Parsetree.attributes) nonterminal
    | N_val_longident : (Longident.t) nonterminal
    | N_val_ident : (string) nonterminal
    | N_typevar_list : (string list) nonterminal
    | N_type_variance : (Asttypes.variance) nonterminal
    | N_type_variable : (Parsetree.core_type) nonterminal
    | N_type_parameters : ((Parsetree.core_type * Asttypes.variance) list) nonterminal
    | N_type_parameter_list : ((Parsetree.core_type * Asttypes.variance) list) nonterminal
    | N_type_parameter : (Parsetree.core_type * Asttypes.variance) nonterminal
    | N_type_longident : (Longident.t) nonterminal
    | N_type_kind : (Parsetree.type_kind * Asttypes.private_flag * Parsetree.core_type option) nonterminal
    | N_type_declarations : (Asttypes.rec_flag * Parsetree.type_declaration list *
  string Location.loc option) nonterminal
    | N_type_declaration : (Asttypes.rec_flag * Parsetree.type_declaration * string Location.loc option) nonterminal
    | N_type_constraint : (Parsetree.core_type option * Parsetree.core_type option) nonterminal
    | N_toplevel_directive : (Parsetree.structure_item) nonterminal
    | N_tag_field : (Parsetree.row_field) nonterminal
    | N_subtractive : (string) nonterminal
    | N_structure_tail : (Parsetree.structure) nonterminal
    | N_structure_item : (Parsetree.structure_item) nonterminal
    | N_structure : (Parsetree.structure) nonterminal
    | N_strict_binding : (Parsetree.expression) nonterminal
    | N_str_type_extension : (Parsetree.type_extension * string Location.loc option) nonterminal
    | N_str_include_statement : (Parsetree.include_declaration * string Location.loc option) nonterminal
    | N_str_extension_constructors : (Parsetree.extension_constructor list) nonterminal
    | N_str_exception_declaration : (Parsetree.extension_constructor * string Location.loc option) nonterminal
    | N_single_attr_id : (string) nonterminal
    | N_simple_pattern_not_ident : (Parsetree.pattern) nonterminal
    | N_simple_pattern : (Parsetree.pattern) nonterminal
    | N_simple_labeled_expr_list : ((Asttypes.arg_label * Parsetree.expression) list) nonterminal
    | N_simple_expr : (Parsetree.expression) nonterminal
    | N_simple_delimited_pattern : (Parsetree.pattern) nonterminal
    | N_simple_core_type_or_tuple : (Parsetree.core_type) nonterminal
    | N_simple_core_type2 : (Parsetree.core_type) nonterminal
    | N_simple_core_type : (Parsetree.core_type) nonterminal
    | N_signed_constant : (Parsetree.constant) nonterminal
    | N_signature_item : (Parsetree.signature_item) nonterminal
    | N_signature : (Parsetree.signature) nonterminal
    | N_sig_type_extension : (Parsetree.type_extension * string Location.loc option) nonterminal
    | N_sig_include_statement : (Parsetree.include_description * string Location.loc option) nonterminal
    | N_sig_extension_constructors : (Parsetree.extension_constructor list) nonterminal
    | N_sig_exception_declaration : (Parsetree.extension_constructor * string Location.loc option) nonterminal
    | N_seq_expr : (Parsetree.expression) nonterminal
    | N_row_field_list : (Parsetree.row_field list) nonterminal
    | N_row_field : (Parsetree.row_field) nonterminal
    | N_record_expr : (Parsetree.expression option *
  (Longident.t Location.loc * Parsetree.expression) list) nonterminal
    | N_rec_module_declarations : (Parsetree.module_declaration list * string Location.loc option) nonterminal
    | N_rec_module_declaration : (Parsetree.module_declaration * string Location.loc option) nonterminal
    | N_rec_module_bindings : (Parsetree.module_binding list * string Location.loc option) nonterminal
    | N_rec_module_binding : (Parsetree.module_binding * string Location.loc option) nonterminal
    | N_rec_flag : (Asttypes.rec_flag) nonterminal
    | N_private_virtual_flags : (Asttypes.private_flag * Asttypes.virtual_flag) nonterminal
    | N_private_flag : (Asttypes.private_flag) nonterminal
    | N_primitive_declaration_body : (string list) nonterminal
    | N_primitive_declaration : (Parsetree.value_description * string Location.loc option) nonterminal
    | N_post_item_attributes : (Parsetree.attributes) nonterminal
    | N_post_item_attribute : (Parsetree.attribute) nonterminal
    | N_poly_type_no_attr : (Parsetree.core_type) nonterminal
    | N_poly_type : (Parsetree.core_type) nonterminal
    | N_payload : (Parsetree.payload) nonterminal
    | N_pattern_var : (Parsetree.pattern) nonterminal
    | N_pattern_semi_list : (Parsetree.pattern list) nonterminal
    | N_pattern_no_exn_comma_list : (Parsetree.pattern list) nonterminal
    | N_pattern_no_exn : (Parsetree.pattern) nonterminal
    | N_pattern_gen : (Parsetree.pattern) nonterminal
    | N_pattern_comma_list : (Parsetree.pattern list) nonterminal
    | N_pattern : (Parsetree.pattern) nonterminal
    | N_parse_expression : (Parsetree.expression) nonterminal
    | N_parent_binder : (string option) nonterminal
    | N_package_type : (Parsetree.package_type) nonterminal
    | N_override_flag : (Asttypes.override_flag) nonterminal
    | N_optional_type_variable : (Parsetree.core_type) nonterminal
    | N_optional_type_parameters : ((Parsetree.core_type * Asttypes.variance) list) nonterminal
    | N_optional_type_parameter_list : ((Parsetree.core_type * Asttypes.variance) list) nonterminal
    | N_optional_type_parameter : (Parsetree.core_type * Asttypes.variance) nonterminal
    | N_opt_type_constraint : ((Parsetree.core_type option * Parsetree.core_type option) option) nonterminal
    | N_opt_semi : (unit) nonterminal
    | N_opt_pattern_type_constraint : (Parsetree.core_type option) nonterminal
    | N_opt_default : (Parsetree.expression option) nonterminal
    | N_opt_bar : (unit) nonterminal
    | N_opt_ampersand : (bool) nonterminal
    | N_operator : (string) nonterminal
    | N_open_statement : (Parsetree.open_description * string Location.loc option) nonterminal
    | N_nonrec_flag : (Asttypes.rec_flag) nonterminal
    | N_name_tag_list : (string list) nonterminal
    | N_name_tag : (string) nonterminal
    | N_mutable_flag : (Asttypes.mutable_flag) nonterminal
    | N_mty_longident : (Longident.t) nonterminal
    | N_module_type_declaration_body : (Parsetree.module_type option) nonterminal
    | N_module_type_declaration : (Parsetree.module_type_declaration * string Location.loc option) nonterminal
    | N_module_type : (Parsetree.module_type) nonterminal
    | N_module_expr : (Parsetree.module_expr) nonterminal
    | N_module_declaration_body : (Parsetree.module_type) nonterminal
    | N_module_declaration : (Parsetree.module_declaration * string Location.loc option) nonterminal
    | N_module_binding_body : (Parsetree.module_expr) nonterminal
    | N_module_binding : (Parsetree.module_binding * string Location.loc option) nonterminal
    | N_module_alias : (Parsetree.module_declaration * string Location.loc option) nonterminal
    | N_mod_longident : (Longident.t) nonterminal
    | N_mod_ext_longident : (Longident.t) nonterminal
    | N_method_ : ((string Location.loc * Asttypes.private_flag * Parsetree.class_field_kind) *
  Parsetree.attributes) nonterminal
    | N_meth_list : ((string * Parsetree.attributes * Parsetree.core_type) list *
  Asttypes.closed_flag) nonterminal
    | N_match_cases : (Parsetree.case list) nonterminal
    | N_match_case : (Parsetree.case) nonterminal
    | N_lwt_bindings : (Ast_helper.let_bindings) nonterminal
    | N_lwt_binding : (Ast_helper.let_bindings) nonterminal
    | N_lident_list : (string list) nonterminal
    | N_let_pattern : (Parsetree.pattern) nonterminal
    | N_let_exception_declaration : (Parsetree.extension_constructor) nonterminal
    | N_let_bindings : (Ast_helper.let_bindings) nonterminal
    | N_let_binding_body : (Parsetree.pattern * Parsetree.expression) nonterminal
    | N_let_binding : (Ast_helper.let_bindings) nonterminal
    | N_lbl_pattern_list : ((Longident.t Location.loc * Parsetree.pattern) list * Asttypes.closed_flag) nonterminal
    | N_lbl_pattern : (Longident.t Location.loc * Parsetree.pattern) nonterminal
    | N_lbl_expr_list : ((Longident.t Location.loc * Parsetree.expression) list) nonterminal
    | N_lbl_expr : (Longident.t Location.loc * Parsetree.expression) nonterminal
    | N_labeled_simple_pattern : (Asttypes.arg_label * Parsetree.expression option * Parsetree.pattern) nonterminal
    | N_labeled_simple_expr : (Asttypes.arg_label * Parsetree.expression) nonterminal
    | N_label_var : (string * Parsetree.pattern) nonterminal
    | N_label_longident : (Longident.t) nonterminal
    | N_label_let_pattern : (string * Parsetree.pattern) nonterminal
    | N_label_ident : (string * Parsetree.expression) nonterminal
    | N_label_expr : (Asttypes.arg_label * Parsetree.expression) nonterminal
    | N_label_declarations : (Parsetree.label_declaration list) nonterminal
    | N_label_declaration_semi : (Parsetree.label_declaration) nonterminal
    | N_label_declaration : (Parsetree.label_declaration) nonterminal
    | N_label : (string) nonterminal
    | N_item_extension : (Parsetree.extension) nonterminal
    | N_interface : (Parsetree.signature) nonterminal
    | N_implementation : (Parsetree.structure) nonterminal
    | N_ident : (string) nonterminal
    | N_generalized_constructor_arguments : (Parsetree.constructor_arguments * Parsetree.core_type option) nonterminal
    | N_functor_args : ((string Location.loc * Parsetree.module_type option) list) nonterminal
    | N_functor_arg_name : (string) nonterminal
    | N_functor_arg : (string Location.loc * Parsetree.module_type option) nonterminal
    | N_fun_def : (Parsetree.expression) nonterminal
    | N_fun_binding : (Parsetree.expression) nonterminal
    | N_floating_attribute : (Parsetree.attribute) nonterminal
    | N_field_semi : (string * Parsetree.attributes * Parsetree.core_type) nonterminal
    | N_field_expr_list : ((string Location.loc * Parsetree.expression) list) nonterminal
    | N_field_expr : (string Location.loc * Parsetree.expression) nonterminal
    | N_field : (string * Parsetree.attributes * Parsetree.core_type) nonterminal
    | N_extension_constructor_rebind : (Parsetree.extension_constructor) nonterminal
    | N_extension_constructor_declaration : (Parsetree.extension_constructor) nonterminal
    | N_extension : (Parsetree.extension) nonterminal
    | N_ext_attributes : (string Location.loc option * Parsetree.attributes) nonterminal
    | N_expr_semi_list : (Parsetree.expression list) nonterminal
    | N_expr_comma_list : (Parsetree.expression list) nonterminal
    | N_expr : (Parsetree.expression) nonterminal
    | N_direction_flag : (Asttypes.direction_flag) nonterminal
    | N_core_type_no_attr : (Parsetree.core_type) nonterminal
    | N_core_type_list : (Parsetree.core_type list) nonterminal
    | N_core_type_comma_list : (Parsetree.core_type list) nonterminal
    | N_core_type2 : (Parsetree.core_type) nonterminal
    | N_core_type : (Parsetree.core_type) nonterminal
    | N_constructor_declarations : (Parsetree.constructor_declaration list) nonterminal
    | N_constructor_declaration : (Parsetree.constructor_declaration) nonterminal
    | N_constructor_arguments : (Parsetree.constructor_arguments) nonterminal
    | N_constraints : ((Parsetree.core_type * Parsetree.core_type * Location.t) list) nonterminal
    | N_constrain_field : (Parsetree.core_type * Parsetree.core_type) nonterminal
    | N_constrain : (Parsetree.core_type * Parsetree.core_type * Location.t) nonterminal
    | N_constr_longident : (Longident.t) nonterminal
    | N_constr_ident : (string) nonterminal
    | N_constant : (Parsetree.constant) nonterminal
    | N_clty_longident : (Longident.t) nonterminal
    | N_class_type_parameters : ((Parsetree.core_type * Asttypes.variance) list) nonterminal
    | N_class_type_declarations : (Parsetree.class_type_declaration list * string Location.loc option) nonterminal
    | N_class_type_declaration : (Parsetree.class_type_declaration * string Location.loc option) nonterminal
    | N_class_type : (Parsetree.class_type) nonterminal
    | N_class_structure : (Parsetree.class_structure) nonterminal
    | N_class_simple_expr : (Parsetree.class_expr) nonterminal
    | N_class_signature : (Parsetree.class_type) nonterminal
    | N_class_sig_fields : (Parsetree.class_type_field list) nonterminal
    | N_class_sig_field : (Parsetree.class_type_field) nonterminal
    | N_class_sig_body : (Parsetree.class_signature) nonterminal
    | N_class_self_type : (Parsetree.core_type) nonterminal
    | N_class_self_pattern : (Parsetree.pattern) nonterminal
    | N_class_longident : (Longident.t) nonterminal
    | N_class_fun_def : (Parsetree.class_expr) nonterminal
    | N_class_fun_binding : (Parsetree.class_expr) nonterminal
    | N_class_fields : (Parsetree.class_field list) nonterminal
    | N_class_field : (Parsetree.class_field) nonterminal
    | N_class_expr : (Parsetree.class_expr) nonterminal
    | N_class_descriptions : (Parsetree.class_description list * string Location.loc option) nonterminal
    | N_class_description : (Parsetree.class_description * string Location.loc option) nonterminal
    | N_class_declarations : (Parsetree.class_declaration list * string Location.loc option) nonterminal
    | N_class_declaration : (Parsetree.class_declaration * string Location.loc option) nonterminal
    | N_bar_extension_constructor_rebind : (Parsetree.extension_constructor) nonterminal
    | N_bar_extension_constructor_declaration : (Parsetree.extension_constructor) nonterminal
    | N_bar_constructor_declaration : (Parsetree.constructor_declaration) nonterminal
    | N_attributes : (Parsetree.attributes) nonterminal
    | N_attribute : (Parsetree.attribute) nonterminal
    | N_attr_id : (string Location.loc) nonterminal
    | N_and_type_declaration : (Parsetree.type_declaration) nonterminal
    | N_and_module_declaration : (Parsetree.module_declaration) nonterminal
    | N_and_module_binding : (Parsetree.module_binding) nonterminal
    | N_and_let_binding : (Ast_helper.let_binding) nonterminal
    | N_and_class_type_declaration : (Parsetree.class_type_declaration) nonterminal
    | N_and_class_description : (Parsetree.class_description) nonterminal
    | N_and_class_declaration : (Parsetree.class_declaration) nonterminal
    | N_amper_type_list : (Parsetree.core_type list) nonterminal
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
  
  val parse_expression: Lexing.position -> (Parsetree.expression) MenhirInterpreter.checkpoint
  
  val interface: Lexing.position -> (Parsetree.signature) MenhirInterpreter.checkpoint
  
  val implementation: Lexing.position -> (Parsetree.structure) MenhirInterpreter.checkpoint
  
end
