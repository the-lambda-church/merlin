open Parser_raw

module Default = struct

  open Parsetree
  open Ast_helper

  let default_loc = ref Location.none

  let default_expr () =
    let id = Location.mkloc "merlin.hole" !default_loc in
    Exp.mk ~loc:!default_loc (Pexp_extension (id, PStr []))

  let default_pattern () = Pat.any ~loc:!default_loc ()

  let default_module_expr () = Mod.structure ~loc:!default_loc[]
  let default_module_type () = Mty.signature ~loc:!default_loc[]

  let value (type a) : a MenhirInterpreter.symbol -> a = function
    | MenhirInterpreter.T MenhirInterpreter.T_error -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_WITH -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_WHILE_LWT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_WHILE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_WHEN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_VIRTUAL -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_VAL -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_UNDERSCORE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_UIDENT -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_TYPE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_TRY_LWT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_TRY -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_TRUE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_TO -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_TILDE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_THEN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_STRUCT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_STRING -> ("", None)
    | MenhirInterpreter.T MenhirInterpreter.T_STAR -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_SIG -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_SEMISEMI -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_SEMI -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_RPAREN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_REC -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_RBRACKET -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_RBRACE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_QUOTE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_QUESTIONQUESTION -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_QUESTION -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_PRIVATE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_PREFIXOP -> "!"
    | MenhirInterpreter.T MenhirInterpreter.T_PLUSEQ -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_PLUSDOT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_PLUS -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_PERCENT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_OR -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_OPTLABEL -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_OPEN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_OF -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_OBJECT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_NONREC -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_NEW -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_MUTABLE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_MODULE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_MINUSGREATER -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_MINUSDOT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_MINUS -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_METHOD -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_MATCH_LWT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_MATCH -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LPAREN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LIDENT -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_LET_LWT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LETOP -> raise Not_found
    | MenhirInterpreter.T MenhirInterpreter.T_LET -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LESSMINUS -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LESS -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETPERCENTPERCENT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETPERCENT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETLESS -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETGREATER -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETBAR -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETATATAT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETATAT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETAT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACKET -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACELESS -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LAZY -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LABEL -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_INT -> ("0",None)
    | MenhirInterpreter.T MenhirInterpreter.T_INITIALIZER -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_INHERIT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_INFIXOP4 -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_INFIXOP3 -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_INFIXOP2 -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_INFIXOP1 -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_INFIXOP0 -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_INCLUDE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_IN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_IF -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_HASHOP -> ""
    | MenhirInterpreter.T MenhirInterpreter.T_HASH -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_GREATERRBRACKET -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_GREATERRBRACE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_GREATERDOT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_GREATER -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_FUNCTOR -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_FUNCTION -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_FUN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_FOR_LWT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_FOR -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_FLOAT -> ("0.",None)
    | MenhirInterpreter.T MenhirInterpreter.T_FINALLY_LWT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_FALSE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_EXTERNAL -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_EXCEPTION -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_EQUAL -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_EOL -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_EOF -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_END -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_ELSE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_DOWNTO -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_DOTTILDE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_DOTOP -> raise Not_found
    | MenhirInterpreter.T MenhirInterpreter.T_DOTLESS -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_DOTDOT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_DOT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_DONE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_DOCSTRING -> raise Not_found
    | MenhirInterpreter.T MenhirInterpreter.T_DO -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_CONSTRAINT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_COMMENT -> ("", Location.none)
    | MenhirInterpreter.T MenhirInterpreter.T_COMMA -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_COLONGREATER -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_COLONEQUAL -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_COLONCOLON -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_COLON -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_CLASS -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_CHAR -> '_'
    | MenhirInterpreter.T MenhirInterpreter.T_BEGIN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_BARRBRACKET -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_BARBAR -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_BAR -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_BANG -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_BACKQUOTE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_ASSERT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_AS -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_ANDOP -> raise Not_found
    | MenhirInterpreter.T MenhirInterpreter.T_AND -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_AMPERSAND -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_AMPERAMPER -> ()
    | MenhirInterpreter.N MenhirInterpreter.N_with_type_binder -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_with_constraint -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_virtual_with_private_flag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_virtual_with_mutable_flag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_virtual_flag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_value_description -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_value -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_val_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_val_ident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_use_file -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_variance -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_variable -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_parameters -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_parameter -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_kind -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_constraint -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_tuple_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_toplevel_phrase -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_toplevel_directive -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_tag_field -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_subtractive -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_structure_item -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_structure -> []
    | MenhirInterpreter.N MenhirInterpreter.N_strict_binding -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_str_exception_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_single_attr_id -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_simple_pattern_not_ident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_simple_pattern -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_simple_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_simple_delimited_pattern -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_signed_constant -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_signature_item -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_signature -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_sig_exception_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_seq_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_separated_or_terminated_nonempty_list_SEMI_record_expr_field_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_separated_or_terminated_nonempty_list_SEMI_pattern_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_separated_or_terminated_nonempty_list_SEMI_object_expr_field_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_separated_or_terminated_nonempty_list_SEMI_expr_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_row_field -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nontrivial_llist_STAR_atomic_type_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nontrivial_llist_COMMA_expr_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nontrivial_llist_COMMA_core_type_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_STAR_atomic_type_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_COMMA_type_parameter_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_COMMA_core_type_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_BAR_row_field_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_AND_with_constraint_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_AMPERSAND_core_type_no_attr_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_nonempty_llist_typevar_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_nonempty_llist_name_tag_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_nonempty_llist_labeled_simple_expr_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_nonempty_llist_functor_arg_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_llist_preceded_CONSTRAINT_constrain__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_bar_llist_extension_constructor_declaration_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_bar_llist_extension_constructor_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_bar_llist_constructor_declaration_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_record_expr_content -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_rec_flag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_private_virtual_flags -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_private_flag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_primitive_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_post_item_attribute -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_possibly_poly_core_type_no_attr_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_possibly_poly_core_type_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_payload -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_pattern_var -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_pattern_no_exn -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_pattern_gen -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_pattern_comma_list_pattern_no_exn_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_pattern_comma_list_pattern_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_pattern -> default_pattern ()
    | MenhirInterpreter.N MenhirInterpreter.N_parse_pattern -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_parse_expression -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_parse_core_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_paren_module_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_optlabel -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_type_constraint_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_preceded_EQUAL_seq_expr__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_preceded_EQUAL_pattern__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_preceded_EQUAL_module_type__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_preceded_EQUAL_expr__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_preceded_COLON_core_type__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_preceded_AS_mkrhs_LIDENT___ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_SEMI_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_BAR_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_opt_ampersand -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_operator -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_open_description -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_open_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_nonempty_type_kind -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_nonempty_list_raw_string_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_nonempty_list_mkrhs_LIDENT__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_name_tag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mutable_virtual_flags -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mutable_flag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mty_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_module_type_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_module_type -> default_module_type ()
    | MenhirInterpreter.N MenhirInterpreter.N_module_subst -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_module_expr -> default_module_expr ()
    | MenhirInterpreter.N MenhirInterpreter.N_module_declaration_body -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_module_binding_body -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mod_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mod_ext_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_method_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_meth_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_match_case -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_lwt_bindings -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_lwt_binding -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_listx_SEMI_record_pat_field_UNDERSCORE_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_use_file_element_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_text_str_structure_item__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_text_cstr_class_field__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_text_csig_class_sig_field__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_structure_element_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_signature_element_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_post_item_attribute_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_generic_and_type_declaration_type_subst_kind__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_generic_and_type_declaration_type_kind__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_attribute_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_and_module_declaration_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_and_module_binding_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_and_class_type_declaration_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_and_class_description_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_and_class_declaration_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_letop_bindings -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_letop_binding_body -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_let_pattern -> default_pattern ()
    | MenhirInterpreter.N MenhirInterpreter.N_let_bindings_no_ext_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_let_bindings_ext_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_let_binding_body -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_labeled_simple_pattern -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_labeled_simple_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_label_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_label_let_pattern -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_label_declarations -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_label_declaration_semi -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_label_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_item_extension -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_interface -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_implementation -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_ident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_generic_type_declaration_nonrec_flag_type_kind_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_generic_type_declaration_no_nonrec_flag_type_subst_kind_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_generic_constructor_declaration_epsilon_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_generic_constructor_declaration_BAR_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_generalized_constructor_arguments -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_functor_args -> []
    | MenhirInterpreter.N MenhirInterpreter.N_functor_arg_name -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_functor_arg -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_function_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_fun_def -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_fun_binding -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_formal_class_parameters -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_floating_attribute -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_extension_constructor_rebind_epsilon_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_extension_constructor_rebind_BAR_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_extension -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_ext -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_expr -> default_expr ()
    | MenhirInterpreter.N MenhirInterpreter.N_direction_flag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_core_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constructor_declarations -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constructor_arguments -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constrain_field -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constr_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constr_ident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constant -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_clty_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_type_declarations -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_simple_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_signature -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_sig_field -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_self_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_self_pattern -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_fun_def -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_fun_binding -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_field -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_attribute -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_attr_id -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_atomic_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_and_let_binding -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_alias_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_additive -> raise Not_found
end

let default_value = Default.value

open MenhirInterpreter

type action =
  | Abort
  | R of int
  | S : 'a symbol -> action
  | Sub of action list

type decision =
  | Nothing
  | One of action list
  | Select of (int -> action list)

let depth =
  [|0;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;2;3;2;2;1;2;1;2;3;1;1;2;3;1;2;3;1;1;1;1;1;2;3;1;1;2;3;3;4;1;2;1;1;2;1;1;1;2;1;2;3;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;2;3;4;2;3;4;2;3;4;1;1;1;1;1;1;2;3;4;5;1;1;1;1;1;2;1;2;3;1;1;2;3;4;1;1;2;1;2;3;1;4;1;1;1;1;1;2;2;1;2;3;2;3;5;6;1;1;1;1;1;2;1;2;1;1;1;2;1;2;1;1;2;1;2;2;1;1;2;2;1;2;1;1;2;1;2;1;2;3;4;2;3;2;3;1;3;2;3;2;1;2;3;4;1;2;3;3;1;1;3;4;2;3;1;2;1;3;4;2;1;3;2;3;4;5;1;2;1;2;1;2;3;2;3;4;5;3;4;3;4;4;5;6;2;1;1;2;3;2;3;3;4;5;6;1;7;1;2;3;1;2;2;3;3;4;5;2;3;2;3;4;5;4;2;3;2;3;2;3;1;2;2;1;1;2;3;4;5;6;7;3;4;1;2;1;1;2;1;1;1;1;2;1;1;2;3;1;2;3;2;1;1;2;3;4;2;3;4;1;1;2;1;1;2;2;1;2;3;1;2;3;1;2;1;2;3;4;5;6;4;4;3;4;5;3;3;1;7;8;9;1;2;1;2;3;4;5;6;7;8;2;3;4;5;1;2;9;6;7;1;8;1;2;3;1;2;3;1;2;1;1;2;3;4;5;4;5;9;10;2;2;1;1;1;1;1;2;3;4;1;4;5;6;7;8;5;6;7;8;9;1;1;1;1;2;3;4;1;2;1;2;3;1;1;1;2;2;1;2;2;1;1;2;3;4;5;6;6;1;2;3;4;1;2;3;1;1;1;2;3;1;2;3;1;2;1;2;3;1;4;1;1;1;1;1;2;3;2;3;1;1;2;2;1;1;2;3;1;1;2;1;1;1;1;4;1;1;2;3;1;1;1;2;3;4;1;2;3;1;1;1;2;3;2;3;2;1;2;1;1;2;4;4;5;2;3;2;3;2;3;3;4;2;3;1;2;3;3;1;2;3;4;5;1;6;5;2;2;3;1;1;2;1;2;3;3;4;2;1;2;3;1;1;1;1;1;2;1;2;3;3;4;5;1;2;1;2;1;2;3;4;1;2;1;1;2;3;4;5;1;2;1;1;1;2;2;3;1;1;2;1;2;3;4;1;5;2;1;2;3;1;2;4;5;4;5;6;1;2;3;4;2;3;4;1;3;2;3;2;3;2;1;2;3;3;1;1;1;2;3;4;5;3;4;1;5;2;3;2;3;3;4;5;2;2;1;1;6;7;1;1;1;1;1;1;1;1;1;1;2;3;1;2;3;1;2;3;1;2;3;1;1;2;1;2;3;4;5;6;7;1;1;2;3;4;5;1;2;3;4;5;1;1;1;2;1;1;2;3;4;1;1;4;5;6;7;8;9;10;1;1;1;1;2;3;4;1;2;3;4;2;3;2;3;1;1;1;2;1;2;1;2;2;3;2;3;4;5;1;2;1;2;1;1;1;1;2;3;1;1;2;3;1;2;3;2;3;2;1;2;1;2;2;3;4;5;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;1;2;1;2;3;4;5;1;2;3;2;3;2;3;2;3;2;3;2;1;1;2;3;3;4;2;2;3;3;4;5;3;4;5;3;4;5;6;7;5;6;7;5;6;7;3;1;2;2;3;4;5;6;7;3;4;5;6;7;3;4;5;6;7;2;3;4;5;6;7;3;4;5;6;7;3;4;5;6;7;3;4;5;6;7;8;9;5;6;7;8;9;5;6;7;8;9;3;4;5;1;2;2;1;2;4;5;3;4;5;3;4;5;5;1;2;3;2;3;4;2;3;1;1;4;5;3;4;4;5;1;2;3;3;4;4;5;3;4;5;3;1;2;3;1;1;2;1;2;3;4;1;2;3;4;5;1;4;5;1;2;3;3;6;1;1;7;8;9;10;11;6;7;8;9;5;6;7;8;9;10;11;2;1;2;3;4;1;2;3;4;1;1;2;5;1;2;3;3;4;5;8;4;5;3;4;5;2;3;3;2;4;2;3;1;4;5;6;7;8;4;4;5;4;2;3;2;2;3;2;2;3;4;2;2;3;2;3;2;2;3;8;3;4;5;6;7;2;3;4;5;1;2;1;2;3;4;6;7;8;1;2;2;3;4;5;6;7;8;9;2;3;4;5;6;2;1;2;3;1;1;2;5;2;2;4;5;2;2;3;4;5;6;7;8;3;4;5;6;7;2;3;4;2;5;6;3;4;5;6;4;5;6;4;5;5;6;7;5;6;7;7;8;9;5;7;8;2;3;3;4;5;4;1;1;2;3;4;5;6;5;6;7;2;3;1;1;2;1;2;2;3;4;5;2;3;4;5;4;5;6;1;1;2;3;4;5;6;7;8;9;10;11;1;2;3;6;7;8;1;5;2;3;1;1;2;1;2;2;3;4;5;2;3;4;5;6;7;8;9;10;5;6;7;4;1;2;1;2;3;4;1;2;3;4;5;1;2;6;7;2;3;4;5;6;4;5;6;7;1;2;3;4;5;6;8;1;2;1;2;3;4;5;1;2;3;4;5;6;7;1;2;8;9;1;2;3;4;5;6;7;8;5;6;7;1;1;1;2;3;4;5;6;2;3;4;5;1;2;3;4;5;6;7;5;6;7;1;2;5;6;1;2;4;5;6;7;8;1;2;3;4;5;6;7;9;1;2;3;4;5;1;2;3;4;1;2;3;4;1;5;1;2;3;6;7;8;1;2;1;2;3;3;1;2;1;2;1;2;3;4;5;6;7;1;2;1;2;1;2;3;4;5;6;7;1;2;1;1;2;3;4;5;6;1;2;3;1;2;3;4;2;3;1;1;1;7;2;3;4;5;6;3;4;1;2;1;2;3;3;4;4;5;1;2;1;1;2;9;10;1;2;3;4;5;6;7;8;9;11;2;3;4;5;6;7;1;2;3;4;1;1;1;2;1;2;3;1;2;3;1;4;1;3;5;8;9;1;2;3;4;5;6;7;8;9;10;1;1;1;1;1;1;1;2;1;2;1;1;2;3;4;5;6;7;8;2;1;1;2;3;4;5;6;7;8;9;2;1;1;2;2;1;2;1;2;3;4;5;6;1;1;2;3;1;2;3;4;1;2;3;1;1;2;3;4;5;6;7;2;3;4;5;6;1;2;3;4;1;2;1;2;1;2;1;1;2;1;3;2;2;3;2;3;7;3;4;5;6;2;3;4;5;2;3;3;4;5;4;1;2;5;6;2;3;4;5;1;2;3;4;4;5;1;2;1;1;2;2;1;2;3;4;1;2;7;8;1;2;3;4;5;6;7;8;9;1;1;1;1;1;1;1;1;1;2;1;1;2;1;2;1;1;1;1;2;3;3;4;1;1;1;3;4;5;6;3;4;5;6;2;3;4;5;2;3;4;2;3;4;10;6;7;8;9;10;2;1;1;4;5;6;7;8;9;5;6;7;8;9;3;4;5;6;6;7;3;4;2;2;3;4;5;6;6;7;8;2;3;3;4;4;5;6;4;5;6;7;8;5;6;4;5;6;7;3;4;3;4;5;6;7;1;2;1;0;1;2;1;0;1;1;2;0;1;2;1;0;1;2;1;0;1;2;3;3;3;3;3;3;2;1;1;1;2;1;2;1;2;3;1;2;0;1;1;1;2;2;2;3;4;2;1;1;2;3;4;1;2;|]

let can_pop (type a) : a terminal -> bool = function
  | T_WITH -> true
  | T_WHILE_LWT -> true
  | T_WHILE -> true
  | T_WHEN -> true
  | T_VIRTUAL -> true
  | T_VAL -> true
  | T_UNDERSCORE -> true
  | T_TYPE -> true
  | T_TRY_LWT -> true
  | T_TRY -> true
  | T_TRUE -> true
  | T_TO -> true
  | T_TILDE -> true
  | T_THEN -> true
  | T_STRUCT -> true
  | T_STAR -> true
  | T_SIG -> true
  | T_SEMISEMI -> true
  | T_SEMI -> true
  | T_RPAREN -> true
  | T_REC -> true
  | T_RBRACKET -> true
  | T_RBRACE -> true
  | T_QUOTE -> true
  | T_QUESTIONQUESTION -> true
  | T_QUESTION -> true
  | T_PRIVATE -> true
  | T_PLUSEQ -> true
  | T_PLUSDOT -> true
  | T_PLUS -> true
  | T_PERCENT -> true
  | T_OR -> true
  | T_OPEN -> true
  | T_OF -> true
  | T_OBJECT -> true
  | T_NONREC -> true
  | T_NEW -> true
  | T_MUTABLE -> true
  | T_MODULE -> true
  | T_MINUSGREATER -> true
  | T_MINUSDOT -> true
  | T_MINUS -> true
  | T_METHOD -> true
  | T_MATCH_LWT -> true
  | T_MATCH -> true
  | T_LPAREN -> true
  | T_LET_LWT -> true
  | T_LET -> true
  | T_LESSMINUS -> true
  | T_LESS -> true
  | T_LBRACKETPERCENTPERCENT -> true
  | T_LBRACKETPERCENT -> true
  | T_LBRACKETLESS -> true
  | T_LBRACKETGREATER -> true
  | T_LBRACKETBAR -> true
  | T_LBRACKETATATAT -> true
  | T_LBRACKETATAT -> true
  | T_LBRACKETAT -> true
  | T_LBRACKET -> true
  | T_LBRACELESS -> true
  | T_LBRACE -> true
  | T_LAZY -> true
  | T_INITIALIZER -> true
  | T_INHERIT -> true
  | T_INCLUDE -> true
  | T_IN -> true
  | T_IF -> true
  | T_HASH -> true
  | T_GREATERRBRACKET -> true
  | T_GREATERRBRACE -> true
  | T_GREATERDOT -> true
  | T_GREATER -> true
  | T_FUNCTOR -> true
  | T_FUNCTION -> true
  | T_FUN -> true
  | T_FOR_LWT -> true
  | T_FOR -> true
  | T_FINALLY_LWT -> true
  | T_FALSE -> true
  | T_EXTERNAL -> true
  | T_EXCEPTION -> true
  | T_EQUAL -> true
  | T_EOL -> true
  | T_END -> true
  | T_ELSE -> true
  | T_DOWNTO -> true
  | T_DOTTILDE -> true
  | T_DOTLESS -> true
  | T_DOTDOT -> true
  | T_DOT -> true
  | T_DONE -> true
  | T_DO -> true
  | T_CONSTRAINT -> true
  | T_COMMA -> true
  | T_COLONGREATER -> true
  | T_COLONEQUAL -> true
  | T_COLONCOLON -> true
  | T_COLON -> true
  | T_CLASS -> true
  | T_BEGIN -> true
  | T_BARRBRACKET -> true
  | T_BARBAR -> true
  | T_BAR -> true
  | T_BANG -> true
  | T_BACKQUOTE -> true
  | T_ASSERT -> true
  | T_AS -> true
  | T_AND -> true
  | T_AMPERSAND -> true
  | T_AMPERAMPER -> true
  | _ -> false

let recover =
  let r0 = [R 545] in
  let r1 = S (N N_expr) :: r0 in
  let r2 = [R 121] in
  let r3 = S (T T_DONE) :: r2 in
  let r4 = Sub (r1) :: r3 in
  let r5 = S (T T_DO) :: r4 in
  let r6 = Sub (r1) :: r5 in
  let r7 = R 270 :: r6 in
  let r8 = [R 642] in
  let r9 = S (T T_AND) :: r8 in
  let r10 = [R 32] in
  let r11 = Sub (r9) :: r10 in
  let r12 = [R 183] in
  let r13 = [R 33] in
  let r14 = [R 466] in
  let r15 = S (N N_structure) :: r14 in
  let r16 = [R 34] in
  let r17 = S (T T_RBRACKET) :: r16 in
  let r18 = Sub (r15) :: r17 in
  let r19 = [R 136] in
  let r20 = S (T T_DONE) :: r19 in
  let r21 = Sub (r1) :: r20 in
  let r22 = S (T T_DO) :: r21 in
  let r23 = Sub (r1) :: r22 in
  let r24 = R 270 :: r23 in
  let r25 = [R 323] in
  let r26 = [R 117] in
  let r27 = Sub (r1) :: r26 in
  let r28 = R 270 :: r27 in
  let r29 = [R 303] in
  let r30 = Sub (r1) :: r29 in
  let r31 = S (T T_MINUSGREATER) :: r30 in
  let r32 = S (N N_pattern) :: r31 in
  let r33 = [R 510] in
  let r34 = Sub (r32) :: r33 in
  let r35 = [R 133] in
  let r36 = Sub (r34) :: r35 in
  let r37 = S (T T_WITH) :: r36 in
  let r38 = Sub (r1) :: r37 in
  let r39 = R 270 :: r38 in
  let r40 = [R 610] in
  let r41 = S (T T_QUESTIONQUESTION) :: r40 in
  let r42 = [R 600] in
  let r43 = [R 56] in
  let r44 = S (T T_LIDENT) :: r43 in
  let r45 = [R 593] in
  let r46 = Sub (r44) :: r45 in
  let r47 = R 270 :: r46 in
  let r48 = [R 57] in
  let r49 = S (T T_LIDENT) :: r48 in
  let r50 = [R 324] in
  let r51 = [R 271] in
  let r52 = [R 580] in
  let r53 = S (T T_RPAREN) :: r52 in
  let r54 = [R 101] in
  let r55 = [R 750] in
  let r56 = [R 184] in
  let r57 = S (T T_RBRACKET) :: r56 in
  let r58 = Sub (r15) :: r57 in
  let r59 = S (T T_LIDENT) :: r55 in
  let r60 = [R 15] in
  let r61 = S (T T_UNDERSCORE) :: r60 in
  let r62 = [R 730] in
  let r63 = Sub (r61) :: r62 in
  let r64 = [R 196] in
  let r65 = Sub (r63) :: r64 in
  let r66 = [R 9] in
  let r67 = Sub (r65) :: r66 in
  let r68 = [R 111] in
  let r69 = Sub (r67) :: r68 in
  let r70 = [R 759] in
  let r71 = R 276 :: r70 in
  let r72 = Sub (r69) :: r71 in
  let r73 = S (T T_COLON) :: r72 in
  let r74 = Sub (r59) :: r73 in
  let r75 = R 270 :: r74 in
  let r76 = [R 410] in
  let r77 = S (T T_AMPERAMPER) :: r76 in
  let r78 = [R 751] in
  let r79 = S (T T_RPAREN) :: r78 in
  let r80 = Sub (r77) :: r79 in
  let r81 = [R 384] in
  let r82 = S (T T_RPAREN) :: r81 in
  let r83 = [R 386] in
  let r84 = [R 388] in
  let r85 = [R 320] in
  let r86 = [R 216] in
  let r87 = S (T T_LIDENT) :: r86 in
  let r88 = [R 14] in
  let r89 = Sub (r87) :: r88 in
  let r90 = [R 433] in
  let r91 = S (T T_COLON) :: r90 in
  let r92 = [R 13] in
  let r93 = S (T T_RPAREN) :: r92 in
  let r94 = S (N N_module_type) :: r93 in
  let r95 = R 270 :: r94 in
  let r96 = R 182 :: r95 in
  let r97 = [R 550] in
  let r98 = R 278 :: r97 in
  let r99 = [R 339] in
  let r100 = S (T T_END) :: r99 in
  let r101 = Sub (r98) :: r100 in
  let r102 = [R 213] in
  let r103 = R 276 :: r102 in
  let r104 = R 500 :: r103 in
  let r105 = R 735 :: r104 in
  let r106 = S (T T_LIDENT) :: r105 in
  let r107 = R 740 :: r106 in
  let r108 = R 270 :: r107 in
  let r109 = R 182 :: r108 in
  let r110 = [R 737] in
  let r111 = S (T T_LIDENT) :: r110 in
  let r112 = [R 96] in
  let r113 = S (T T_FALSE) :: r112 in
  let r114 = [R 210] in
  let r115 = R 270 :: r114 in
  let r116 = R 205 :: r115 in
  let r117 = Sub (r113) :: r116 in
  let r118 = [R 497] in
  let r119 = Sub (r117) :: r118 in
  let r120 = [R 557] in
  let r121 = R 276 :: r120 in
  let r122 = Sub (r119) :: r121 in
  let r123 = R 477 :: r122 in
  let r124 = S (T T_PLUSEQ) :: r123 in
  let r125 = Sub (r111) :: r124 in
  let r126 = R 740 :: r125 in
  let r127 = R 270 :: r126 in
  let r128 = [R 214] in
  let r129 = R 276 :: r128 in
  let r130 = R 500 :: r129 in
  let r131 = R 735 :: r130 in
  let r132 = S (T T_LIDENT) :: r131 in
  let r133 = R 740 :: r132 in
  let r134 = [R 558] in
  let r135 = R 276 :: r134 in
  let r136 = Sub (r119) :: r135 in
  let r137 = R 477 :: r136 in
  let r138 = S (T T_PLUSEQ) :: r137 in
  let r139 = Sub (r111) :: r138 in
  let r140 = [R 744] in
  let r141 = S (T T_UNDERSCORE) :: r140 in
  let r142 = [R 739] in
  let r143 = Sub (r141) :: r142 in
  let r144 = R 745 :: r143 in
  let r145 = [R 521] in
  let r146 = Sub (r144) :: r145 in
  let r147 = [R 742] in
  let r148 = S (T T_RPAREN) :: r147 in
  let r149 = [R 743] in
  let r150 = [R 522] in
  let r151 = [R 369] in
  let r152 = S (T T_DOTDOT) :: r151 in
  let r153 = [R 736] in
  let r154 = [R 370] in
  let r155 = [R 94] in
  let r156 = [R 198] in
  let r157 = Sub (r65) :: r156 in
  let r158 = S (T T_MINUSGREATER) :: r157 in
  let r159 = Sub (r63) :: r158 in
  let r160 = [R 20] in
  let r161 = [R 473] in
  let r162 = Sub (r67) :: r161 in
  let r163 = [R 310] in
  let r164 = R 270 :: r163 in
  let r165 = Sub (r162) :: r164 in
  let r166 = [R 508] in
  let r167 = [R 532] in
  let r168 = Sub (r69) :: r167 in
  let r169 = [R 517] in
  let r170 = Sub (r168) :: r169 in
  let r171 = [R 29] in
  let r172 = S (T T_RBRACKET) :: r171 in
  let r173 = Sub (r170) :: r172 in
  let r174 = [R 28] in
  let r175 = [R 27] in
  let r176 = S (T T_RBRACKET) :: r175 in
  let r177 = [R 358] in
  let r178 = Sub (r87) :: r177 in
  let r179 = S (T T_BACKQUOTE) :: r178 in
  let r180 = [R 718] in
  let r181 = R 270 :: r180 in
  let r182 = Sub (r179) :: r181 in
  let r183 = [R 24] in
  let r184 = S (T T_RBRACKET) :: r183 in
  let r185 = [R 21] in
  let r186 = [R 25] in
  let r187 = S (T T_RBRACKET) :: r186 in
  let r188 = [R 199] in
  let r189 = [R 529] in
  let r190 = [R 738] in
  let r191 = S (T T_LIDENT) :: r190 in
  let r192 = S (T T_UIDENT) :: r85 in
  let r193 = [R 322] in
  let r194 = S (T T_RPAREN) :: r193 in
  let r195 = [R 321] in
  let r196 = [R 22] in
  let r197 = [R 197] in
  let r198 = Sub (r65) :: r197 in
  let r199 = S (T T_MINUSGREATER) :: r198 in
  let r200 = [R 530] in
  let r201 = [R 518] in
  let r202 = [R 513] in
  let r203 = Sub (r67) :: r202 in
  let r204 = [R 717] in
  let r205 = R 270 :: r204 in
  let r206 = Sub (r203) :: r205 in
  let r207 = [R 514] in
  let r208 = [R 10] in
  let r209 = Sub (r87) :: r208 in
  let r210 = [R 26] in
  let r211 = S (T T_RBRACKET) :: r210 in
  let r212 = Sub (r170) :: r211 in
  let r213 = [R 506] in
  let r214 = Sub (r179) :: r213 in
  let r215 = [R 30] in
  let r216 = S (T T_RBRACKET) :: r215 in
  let r217 = [R 474] in
  let r218 = Sub (r67) :: r217 in
  let r219 = [R 509] in
  let r220 = [R 308] in
  let r221 = [R 19] in
  let r222 = [R 95] in
  let r223 = [R 18] in
  let r224 = Sub (r111) :: r223 in
  let r225 = [R 23] in
  let r226 = [R 525] in
  let r227 = [R 12] in
  let r228 = [R 526] in
  let r229 = [R 93] in
  let r230 = [R 220] in
  let r231 = R 270 :: r230 in
  let r232 = Sub (r162) :: r231 in
  let r233 = S (T T_COLON) :: r232 in
  let r234 = S (T T_LIDENT) :: r233 in
  let r235 = R 351 :: r234 in
  let r236 = [R 222] in
  let r237 = Sub (r235) :: r236 in
  let r238 = [R 374] in
  let r239 = S (T T_RBRACE) :: r238 in
  let r240 = [R 221] in
  let r241 = R 270 :: r240 in
  let r242 = S (T T_SEMI) :: r241 in
  let r243 = R 270 :: r242 in
  let r244 = Sub (r162) :: r243 in
  let r245 = S (T T_COLON) :: r244 in
  let r246 = [R 209] in
  let r247 = R 270 :: r246 in
  let r248 = R 205 :: r247 in
  let r249 = [R 106] in
  let r250 = Sub (r61) :: r249 in
  let r251 = [R 206] in
  let r252 = [R 108] in
  let r253 = S (T T_RBRACE) :: r252 in
  let r254 = [R 107] in
  let r255 = Sub (r61) :: r254 in
  let r256 = [R 208] in
  let r257 = [R 207] in
  let r258 = Sub (r61) :: r257 in
  let r259 = Sub (r113) :: r248 in
  let r260 = [R 373] in
  let r261 = S (T T_RBRACE) :: r260 in
  let r262 = [R 371] in
  let r263 = [R 372] in
  let r264 = [R 376] in
  let r265 = S (T T_RBRACE) :: r264 in
  let r266 = [R 375] in
  let r267 = S (T T_RBRACE) :: r266 in
  let r268 = [R 212] in
  let r269 = R 276 :: r268 in
  let r270 = R 500 :: r269 in
  let r271 = [R 475] in
  let r272 = S (T T_RBRACKET) :: r271 in
  let r273 = Sub (r15) :: r272 in
  let r274 = [R 491] in
  let r275 = Sub (r117) :: r274 in
  let r276 = [R 705] in
  let r277 = R 276 :: r276 in
  let r278 = Sub (r275) :: r277 in
  let r279 = R 477 :: r278 in
  let r280 = S (T T_PLUSEQ) :: r279 in
  let r281 = Sub (r111) :: r280 in
  let r282 = R 740 :: r281 in
  let r283 = R 270 :: r282 in
  let r284 = [R 706] in
  let r285 = R 276 :: r284 in
  let r286 = Sub (r275) :: r285 in
  let r287 = R 477 :: r286 in
  let r288 = S (T T_PLUSEQ) :: r287 in
  let r289 = Sub (r111) :: r288 in
  let r290 = [R 501] in
  let r291 = Sub (r69) :: r290 in
  let r292 = S (T T_EQUAL) :: r291 in
  let r293 = [R 277] in
  let r294 = [R 103] in
  let r295 = S (T T_FALSE) :: r294 in
  let r296 = [R 185] in
  let r297 = R 270 :: r296 in
  let r298 = [R 102] in
  let r299 = [R 100] in
  let r300 = [R 99] in
  let r301 = S (T T_RPAREN) :: r300 in
  let r302 = S (T T_COLONCOLON) :: r301 in
  let r303 = [R 186] in
  let r304 = R 270 :: r303 in
  let r305 = [R 282] in
  let r306 = [R 377] in
  let r307 = R 276 :: r306 in
  let r308 = S (N N_module_expr) :: r307 in
  let r309 = R 270 :: r308 in
  let r310 = [R 378] in
  let r311 = R 276 :: r310 in
  let r312 = S (N N_module_expr) :: r311 in
  let r313 = R 270 :: r312 in
  let r314 = [R 330] in
  let r315 = S (T T_END) :: r314 in
  let r316 = S (N N_structure) :: r315 in
  let r317 = [R 140] in
  let r318 = S (T T_END) :: r317 in
  let r319 = R 287 :: r318 in
  let r320 = R 60 :: r319 in
  let r321 = R 270 :: r320 in
  let r322 = [R 58] in
  let r323 = S (T T_RPAREN) :: r322 in
  let r324 = [R 628] in
  let r325 = [R 572] in
  let r326 = [R 570] in
  let r327 = [R 624] in
  let r328 = S (T T_RPAREN) :: r327 in
  let r329 = [R 626] in
  let r330 = S (T T_RPAREN) :: r329 in
  let r331 = S (T T_UIDENT) :: r330 in
  let r332 = R 270 :: r331 in
  let r333 = [R 627] in
  let r334 = S (T T_RPAREN) :: r333 in
  let r335 = [R 341] in
  let r336 = S (N N_module_expr) :: r335 in
  let r337 = R 270 :: r336 in
  let r338 = S (T T_OF) :: r337 in
  let r339 = [R 435] in
  let r340 = S (T T_RPAREN) :: r339 in
  let r341 = [R 436] in
  let r342 = S (T T_RPAREN) :: r341 in
  let r343 = S (N N_expr) :: r342 in
  let r344 = [R 116] in
  let r345 = Sub (r34) :: r344 in
  let r346 = S (T T_WITH) :: r345 in
  let r347 = Sub (r1) :: r346 in
  let r348 = R 270 :: r347 in
  let r349 = [R 132] in
  let r350 = Sub (r34) :: r349 in
  let r351 = S (T T_WITH) :: r350 in
  let r352 = Sub (r1) :: r351 in
  let r353 = R 270 :: r352 in
  let r354 = [R 170] in
  let r355 = [R 455] in
  let r356 = S (N N_pattern) :: r355 in
  let r357 = Sub (r295) :: r356 in
  let r358 = [R 460] in
  let r359 = Sub (r357) :: r358 in
  let r360 = [R 246] in
  let r361 = Sub (r1) :: r360 in
  let r362 = S (T T_EQUAL) :: r361 in
  let r363 = Sub (r359) :: r362 in
  let r364 = [R 300] in
  let r365 = R 276 :: r364 in
  let r366 = Sub (r363) :: r365 in
  let r367 = R 484 :: r366 in
  let r368 = R 270 :: r367 in
  let r369 = [R 577] in
  let r370 = [R 539] in
  let r371 = S (N N_pattern) :: r370 in
  let r372 = [R 575] in
  let r373 = S (T T_RBRACKET) :: r372 in
  let r374 = [R 227] in
  let r375 = S (T T_LIDENT) :: r374 in
  let r376 = [R 296] in
  let r377 = R 426 :: r376 in
  let r378 = R 420 :: r377 in
  let r379 = Sub (r375) :: r378 in
  let r380 = [R 574] in
  let r381 = S (T T_RBRACE) :: r380 in
  let r382 = [R 228] in
  let r383 = S (T T_LIDENT) :: r382 in
  let r384 = [R 421] in
  let r385 = [R 427] in
  let r386 = S (T T_UNDERSCORE) :: r324 in
  let r387 = [R 623] in
  let r388 = Sub (r386) :: r387 in
  let r389 = [R 457] in
  let r390 = Sub (r388) :: r389 in
  let r391 = R 270 :: r390 in
  let r392 = [R 88] in
  let r393 = [R 633] in
  let r394 = S (T T_INT) :: r392 in
  let r395 = [R 569] in
  let r396 = Sub (r394) :: r395 in
  let r397 = [R 630] in
  let r398 = [R 635] in
  let r399 = S (T T_RBRACKET) :: r398 in
  let r400 = S (T T_LBRACKET) :: r399 in
  let r401 = [R 636] in
  let r402 = [R 449] in
  let r403 = S (N N_pattern) :: r402 in
  let r404 = R 270 :: r403 in
  let r405 = [R 450] in
  let r406 = [R 443] in
  let r407 = [R 456] in
  let r408 = [R 637] in
  let r409 = [R 451] in
  let r410 = [R 448] in
  let r411 = [R 446] in
  let r412 = [R 298] in
  let r413 = [R 576] in
  let r414 = [R 693] in
  let r415 = Sub (r1) :: r414 in
  let r416 = S (T T_EQUAL) :: r415 in
  let r417 = [R 242] in
  let r418 = [R 239] in
  let r419 = [R 225] in
  let r420 = S (T T_LIDENT) :: r419 in
  let r421 = [R 238] in
  let r422 = S (T T_RPAREN) :: r421 in
  let r423 = [R 226] in
  let r424 = [R 235] in
  let r425 = [R 234] in
  let r426 = S (T T_RPAREN) :: r425 in
  let r427 = R 428 :: r426 in
  let r428 = [R 429] in
  let r429 = [R 257] in
  let r430 = Sub (r1) :: r429 in
  let r431 = S (T T_EQUAL) :: r430 in
  let r432 = Sub (r359) :: r431 in
  let r433 = [R 258] in
  let r434 = Sub (r432) :: r433 in
  let r435 = [R 168] in
  let r436 = Sub (r1) :: r435 in
  let r437 = S (T T_IN) :: r436 in
  let r438 = [R 255] in
  let r439 = [R 465] in
  let r440 = S (T T_UNDERSCORE) :: r439 in
  let r441 = [R 237] in
  let r442 = [R 236] in
  let r443 = S (T T_RPAREN) :: r442 in
  let r444 = R 428 :: r443 in
  let r445 = [R 254] in
  let r446 = [R 359] in
  let r447 = S (T T_LIDENT) :: r446 in
  let r448 = [R 190] in
  let r449 = Sub (r416) :: r448 in
  let r450 = [R 695] in
  let r451 = Sub (r449) :: r450 in
  let r452 = S (T T_RPAREN) :: r451 in
  let r453 = Sub (r447) :: r452 in
  let r454 = [R 240] in
  let r455 = [R 127] in
  let r456 = Sub (r1) :: r455 in
  let r457 = S (T T_IN) :: r456 in
  let r458 = S (N N_module_expr) :: r457 in
  let r459 = R 270 :: r458 in
  let r460 = R 182 :: r459 in
  let r461 = [R 248] in
  let r462 = R 276 :: r461 in
  let r463 = Sub (r363) :: r462 in
  let r464 = R 484 :: r463 in
  let r465 = R 270 :: r464 in
  let r466 = R 182 :: r465 in
  let r467 = [R 128] in
  let r468 = Sub (r1) :: r467 in
  let r469 = S (T T_IN) :: r468 in
  let r470 = S (N N_module_expr) :: r469 in
  let r471 = R 270 :: r470 in
  let r472 = [R 331] in
  let r473 = S (N N_module_expr) :: r472 in
  let r474 = S (T T_MINUSGREATER) :: r473 in
  let r475 = S (N N_functor_args) :: r474 in
  let r476 = [R 200] in
  let r477 = [R 201] in
  let r478 = S (T T_RPAREN) :: r477 in
  let r479 = S (N N_module_type) :: r478 in
  let r480 = [R 342] in
  let r481 = S (T T_RPAREN) :: r480 in
  let r482 = [R 340] in
  let r483 = S (N N_module_type) :: r482 in
  let r484 = S (T T_MINUSGREATER) :: r483 in
  let r485 = S (N N_functor_args) :: r484 in
  let r486 = S (T T_UIDENT) :: r25 in
  let r487 = [R 770] in
  let r488 = Sub (r192) :: r487 in
  let r489 = S (T T_EQUAL) :: r488 in
  let r490 = Sub (r486) :: r489 in
  let r491 = S (T T_MODULE) :: r490 in
  let r492 = [R 515] in
  let r493 = Sub (r491) :: r492 in
  let r494 = [R 346] in
  let r495 = [R 769] in
  let r496 = Sub (r67) :: r495 in
  let r497 = S (T T_COLONEQUAL) :: r496 in
  let r498 = Sub (r375) :: r497 in
  let r499 = [R 768] in
  let r500 = R 500 :: r499 in
  let r501 = [R 771] in
  let r502 = [R 516] in
  let r503 = [R 345] in
  let r504 = [R 350] in
  let r505 = Sub (r87) :: r504 in
  let r506 = [R 336] in
  let r507 = [R 434] in
  let r508 = S (T T_RPAREN) :: r507 in
  let r509 = [R 615] in
  let r510 = [R 533] in
  let r511 = S (N N_expr) :: r510 in
  let r512 = [R 618] in
  let r513 = S (T T_RBRACKET) :: r512 in
  let r514 = [R 603] in
  let r515 = [R 536] in
  let r516 = R 422 :: r515 in
  let r517 = [R 423] in
  let r518 = [R 542] in
  let r519 = R 422 :: r518 in
  let r520 = R 430 :: r519 in
  let r521 = Sub (r375) :: r520 in
  let r522 = [R 486] in
  let r523 = Sub (r521) :: r522 in
  let r524 = [R 612] in
  let r525 = S (T T_RBRACE) :: r524 in
  let r526 = [R 579] in
  let r527 = [R 578] in
  let r528 = S (T T_GREATERDOT) :: r527 in
  let r529 = [R 139] in
  let r530 = Sub (r41) :: r529 in
  let r531 = R 270 :: r530 in
  let r532 = [R 592] in
  let r533 = S (T T_END) :: r532 in
  let r534 = R 270 :: r533 in
  let r535 = [R 135] in
  let r536 = S (N N_expr) :: r535 in
  let r537 = S (T T_THEN) :: r536 in
  let r538 = Sub (r1) :: r537 in
  let r539 = R 270 :: r538 in
  let r540 = [R 129] in
  let r541 = Sub (r34) :: r540 in
  let r542 = R 270 :: r541 in
  let r543 = [R 511] in
  let r544 = [R 304] in
  let r545 = Sub (r1) :: r544 in
  let r546 = S (T T_MINUSGREATER) :: r545 in
  let r547 = [R 241] in
  let r548 = Sub (r388) :: r547 in
  let r549 = [R 192] in
  let r550 = Sub (r1) :: r549 in
  let r551 = S (T T_MINUSGREATER) :: r550 in
  let r552 = [R 130] in
  let r553 = Sub (r551) :: r552 in
  let r554 = Sub (r548) :: r553 in
  let r555 = R 270 :: r554 in
  let r556 = [R 131] in
  let r557 = Sub (r551) :: r556 in
  let r558 = S (T T_RPAREN) :: r557 in
  let r559 = [R 123] in
  let r560 = S (T T_DONE) :: r559 in
  let r561 = Sub (r1) :: r560 in
  let r562 = S (T T_DO) :: r561 in
  let r563 = Sub (r1) :: r562 in
  let r564 = S (T T_IN) :: r563 in
  let r565 = S (N N_pattern) :: r564 in
  let r566 = R 270 :: r565 in
  let r567 = [R 114] in
  let r568 = S (T T_DOWNTO) :: r567 in
  let r569 = [R 137] in
  let r570 = S (T T_DONE) :: r569 in
  let r571 = Sub (r1) :: r570 in
  let r572 = S (T T_DO) :: r571 in
  let r573 = Sub (r1) :: r572 in
  let r574 = Sub (r568) :: r573 in
  let r575 = Sub (r1) :: r574 in
  let r576 = S (T T_EQUAL) :: r575 in
  let r577 = S (N N_pattern) :: r576 in
  let r578 = R 270 :: r577 in
  let r579 = [R 601] in
  let r580 = [R 611] in
  let r581 = S (T T_RPAREN) :: r580 in
  let r582 = S (T T_LPAREN) :: r581 in
  let r583 = S (T T_DOT) :: r582 in
  let r584 = [R 621] in
  let r585 = S (T T_RPAREN) :: r584 in
  let r586 = S (N N_module_type) :: r585 in
  let r587 = S (T T_COLON) :: r586 in
  let r588 = S (N N_module_expr) :: r587 in
  let r589 = R 270 :: r588 in
  let r590 = [R 256] in
  let r591 = Sub (r1) :: r590 in
  let r592 = S (T T_EQUAL) :: r591 in
  let r593 = [R 138] in
  let r594 = Sub (r41) :: r593 in
  let r595 = R 270 :: r594 in
  let r596 = [R 608] in
  let r597 = [R 584] in
  let r598 = S (T T_RBRACKET) :: r597 in
  let r599 = S (N N_expr) :: r598 in
  let r600 = S (T T_LBRACKET) :: r599 in
  let r601 = [R 585] in
  let r602 = S (T T_RPAREN) :: r601 in
  let r603 = S (N N_expr) :: r602 in
  let r604 = [R 165] in
  let r605 = [R 231] in
  let r606 = [R 232] in
  let r607 = [R 233] in
  let r608 = [R 607] in
  let r609 = [R 590] in
  let r610 = S (T T_RBRACE) :: r609 in
  let r611 = S (N N_expr) :: r610 in
  let r612 = S (T T_LBRACE) :: r611 in
  let r613 = [R 582] in
  let r614 = S (T T_RPAREN) :: r613 in
  let r615 = Sub (r1) :: r614 in
  let r616 = [R 527] in
  let r617 = [R 115] in
  let r618 = Sub (r1) :: r617 in
  let r619 = [R 167] in
  let r620 = Sub (r1) :: r619 in
  let r621 = [R 155] in
  let r622 = [R 149] in
  let r623 = [R 166] in
  let r624 = [R 548] in
  let r625 = Sub (r1) :: r624 in
  let r626 = [R 152] in
  let r627 = [R 156] in
  let r628 = [R 148] in
  let r629 = [R 151] in
  let r630 = [R 150] in
  let r631 = [R 160] in
  let r632 = [R 154] in
  let r633 = [R 153] in
  let r634 = [R 158] in
  let r635 = [R 147] in
  let r636 = [R 146] in
  let r637 = [R 169] in
  let r638 = [R 145] in
  let r639 = [R 159] in
  let r640 = [R 157] in
  let r641 = [R 161] in
  let r642 = [R 162] in
  let r643 = [R 163] in
  let r644 = [R 528] in
  let r645 = [R 164] in
  let r646 = [R 11] in
  let r647 = R 276 :: r646 in
  let r648 = Sub (r363) :: r647 in
  let r649 = [R 247] in
  let r650 = Sub (r1) :: r649 in
  let r651 = S (T T_EQUAL) :: r650 in
  let r652 = [R 453] in
  let r653 = [R 458] in
  let r654 = [R 463] in
  let r655 = [R 461] in
  let r656 = [R 452] in
  let r657 = [R 583] in
  let r658 = S (T T_RBRACKET) :: r657 in
  let r659 = Sub (r1) :: r658 in
  let r660 = [R 587] in
  let r661 = S (T T_RBRACKET) :: r660 in
  let r662 = S (N N_expr) :: r661 in
  let r663 = S (T T_LBRACKET) :: r662 in
  let r664 = [R 588] in
  let r665 = S (T T_RPAREN) :: r664 in
  let r666 = S (N N_expr) :: r665 in
  let r667 = [R 589] in
  let r668 = S (T T_RBRACE) :: r667 in
  let r669 = S (N N_expr) :: r668 in
  let r670 = [R 230] in
  let r671 = [R 176] in
  let r672 = [R 175] in
  let r673 = [R 586] in
  let r674 = S (T T_RBRACE) :: r673 in
  let r675 = S (N N_expr) :: r674 in
  let r676 = [R 177] in
  let r677 = [R 172] in
  let r678 = [R 173] in
  let r679 = [R 174] in
  let r680 = [R 179] in
  let r681 = [R 178] in
  let r682 = [R 180] in
  let r683 = [R 171] in
  let r684 = [R 259] in
  let r685 = [R 605] in
  let r686 = [R 617] in
  let r687 = [R 616] in
  let r688 = [R 620] in
  let r689 = [R 619] in
  let r690 = S (T T_LIDENT) :: r516 in
  let r691 = [R 606] in
  let r692 = S (T T_GREATERRBRACE) :: r691 in
  let r693 = [R 613] in
  let r694 = S (T T_RBRACE) :: r693 in
  let r695 = [R 487] in
  let r696 = Sub (r521) :: r695 in
  let r697 = [R 734] in
  let r698 = [R 732] in
  let r699 = Sub (r69) :: r698 in
  let r700 = [R 733] in
  let r701 = [R 122] in
  let r702 = S (T T_DONE) :: r701 in
  let r703 = Sub (r1) :: r702 in
  let r704 = S (T T_DO) :: r703 in
  let r705 = Sub (r1) :: r704 in
  let r706 = Sub (r568) :: r705 in
  let r707 = [R 195] in
  let r708 = Sub (r551) :: r707 in
  let r709 = S (T T_RPAREN) :: r708 in
  let r710 = [R 193] in
  let r711 = Sub (r1) :: r710 in
  let r712 = S (T T_MINUSGREATER) :: r711 in
  let r713 = [R 194] in
  let r714 = [R 638] in
  let r715 = S (T T_RPAREN) :: r714 in
  let r716 = [R 512] in
  let r717 = [R 134] in
  let r718 = [R 591] in
  let r719 = [R 602] in
  let r720 = [R 614] in
  let r721 = [R 325] in
  let r722 = S (N N_module_expr) :: r721 in
  let r723 = S (T T_EQUAL) :: r722 in
  let r724 = [R 125] in
  let r725 = Sub (r1) :: r724 in
  let r726 = S (T T_IN) :: r725 in
  let r727 = Sub (r723) :: r726 in
  let r728 = S (T T_UIDENT) :: r727 in
  let r729 = R 270 :: r728 in
  let r730 = [R 326] in
  let r731 = S (N N_module_expr) :: r730 in
  let r732 = S (T T_EQUAL) :: r731 in
  let r733 = [R 327] in
  let r734 = [R 126] in
  let r735 = Sub (r1) :: r734 in
  let r736 = S (T T_IN) :: r735 in
  let r737 = R 270 :: r736 in
  let r738 = R 205 :: r737 in
  let r739 = Sub (r113) :: r738 in
  let r740 = R 270 :: r739 in
  let r741 = [R 191] in
  let r742 = Sub (r1) :: r741 in
  let r743 = [R 694] in
  let r744 = [R 245] in
  let r745 = Sub (r1) :: r744 in
  let r746 = S (T T_EQUAL) :: r745 in
  let r747 = Sub (r69) :: r746 in
  let r748 = S (T T_DOT) :: r747 in
  let r749 = [R 244] in
  let r750 = Sub (r1) :: r749 in
  let r751 = S (T T_EQUAL) :: r750 in
  let r752 = Sub (r69) :: r751 in
  let r753 = [R 243] in
  let r754 = Sub (r1) :: r753 in
  let r755 = [R 439] in
  let r756 = S (T T_RPAREN) :: r755 in
  let r757 = [R 437] in
  let r758 = S (T T_RPAREN) :: r757 in
  let r759 = [R 438] in
  let r760 = S (T T_RPAREN) :: r759 in
  let r761 = [R 59] in
  let r762 = S (T T_RPAREN) :: r761 in
  let r763 = [R 755] in
  let r764 = Sub (r1) :: r763 in
  let r765 = S (T T_EQUAL) :: r764 in
  let r766 = S (T T_LIDENT) :: r765 in
  let r767 = R 351 :: r766 in
  let r768 = R 270 :: r767 in
  let r769 = [R 45] in
  let r770 = R 276 :: r769 in
  let r771 = [R 756] in
  let r772 = Sub (r1) :: r771 in
  let r773 = S (T T_EQUAL) :: r772 in
  let r774 = S (T T_LIDENT) :: r773 in
  let r775 = R 351 :: r774 in
  let r776 = [R 758] in
  let r777 = Sub (r1) :: r776 in
  let r778 = [R 754] in
  let r779 = Sub (r69) :: r778 in
  let r780 = S (T T_COLON) :: r779 in
  let r781 = [R 757] in
  let r782 = Sub (r1) :: r781 in
  let r783 = [R 314] in
  let r784 = Sub (r416) :: r783 in
  let r785 = S (T T_LIDENT) :: r784 in
  let r786 = R 477 :: r785 in
  let r787 = R 270 :: r786 in
  let r788 = [R 46] in
  let r789 = R 276 :: r788 in
  let r790 = [R 315] in
  let r791 = Sub (r416) :: r790 in
  let r792 = S (T T_LIDENT) :: r791 in
  let r793 = R 477 :: r792 in
  let r794 = [R 471] in
  let r795 = Sub (r69) :: r794 in
  let r796 = [R 317] in
  let r797 = Sub (r1) :: r796 in
  let r798 = S (T T_EQUAL) :: r797 in
  let r799 = [R 319] in
  let r800 = Sub (r1) :: r799 in
  let r801 = S (T T_EQUAL) :: r800 in
  let r802 = Sub (r69) :: r801 in
  let r803 = S (T T_DOT) :: r802 in
  let r804 = [R 472] in
  let r805 = Sub (r69) :: r804 in
  let r806 = [R 313] in
  let r807 = Sub (r795) :: r806 in
  let r808 = S (T T_COLON) :: r807 in
  let r809 = [R 316] in
  let r810 = Sub (r1) :: r809 in
  let r811 = S (T T_EQUAL) :: r810 in
  let r812 = [R 318] in
  let r813 = Sub (r1) :: r812 in
  let r814 = S (T T_EQUAL) :: r813 in
  let r815 = Sub (r69) :: r814 in
  let r816 = S (T T_DOT) :: r815 in
  let r817 = [R 219] in
  let r818 = S (T T_RBRACKET) :: r817 in
  let r819 = Sub (r15) :: r818 in
  let r820 = [R 469] in
  let r821 = [R 470] in
  let r822 = [R 708] in
  let r823 = R 276 :: r822 in
  let r824 = Sub (r723) :: r823 in
  let r825 = S (T T_UIDENT) :: r824 in
  let r826 = R 270 :: r825 in
  let r827 = [R 348] in
  let r828 = R 276 :: r827 in
  let r829 = R 424 :: r828 in
  let r830 = Sub (r87) :: r829 in
  let r831 = R 270 :: r830 in
  let r832 = [R 425] in
  let r833 = [R 709] in
  let r834 = R 266 :: r833 in
  let r835 = R 276 :: r834 in
  let r836 = Sub (r723) :: r835 in
  let r837 = [R 267] in
  let r838 = R 266 :: r837 in
  let r839 = R 276 :: r838 in
  let r840 = Sub (r723) :: r839 in
  let r841 = S (T T_UIDENT) :: r840 in
  let r842 = [R 187] in
  let r843 = S (T T_RBRACKET) :: r842 in
  let r844 = Sub (r15) :: r843 in
  let r845 = [R 714] in
  let r846 = R 276 :: r845 in
  let r847 = S (N N_module_expr) :: r846 in
  let r848 = R 270 :: r847 in
  let r849 = [R 361] in
  let r850 = S (T T_STRING) :: r849 in
  let r851 = [R 476] in
  let r852 = R 276 :: r851 in
  let r853 = Sub (r850) :: r852 in
  let r854 = S (T T_EQUAL) :: r853 in
  let r855 = Sub (r69) :: r854 in
  let r856 = S (T T_COLON) :: r855 in
  let r857 = Sub (r59) :: r856 in
  let r858 = R 270 :: r857 in
  let r859 = [R 692] in
  let r860 = R 276 :: r859 in
  let r861 = R 270 :: r860 in
  let r862 = Sub (r295) :: r861 in
  let r863 = S (T T_EQUAL) :: r862 in
  let r864 = Sub (r113) :: r863 in
  let r865 = R 270 :: r864 in
  let r866 = [R 549] in
  let r867 = R 276 :: r866 in
  let r868 = R 270 :: r867 in
  let r869 = R 205 :: r868 in
  let r870 = Sub (r113) :: r869 in
  let r871 = R 270 :: r870 in
  let r872 = R 182 :: r871 in
  let r873 = [R 467] in
  let r874 = [R 279] in
  let r875 = [R 379] in
  let r876 = R 276 :: r875 in
  let r877 = Sub (r192) :: r876 in
  let r878 = R 270 :: r877 in
  let r879 = [R 380] in
  let r880 = R 276 :: r879 in
  let r881 = Sub (r192) :: r880 in
  let r882 = R 270 :: r881 in
  let r883 = [R 328] in
  let r884 = S (N N_module_type) :: r883 in
  let r885 = S (T T_COLON) :: r884 in
  let r886 = [R 560] in
  let r887 = R 276 :: r886 in
  let r888 = Sub (r885) :: r887 in
  let r889 = S (T T_UIDENT) :: r888 in
  let r890 = R 270 :: r889 in
  let r891 = [R 561] in
  let r892 = R 276 :: r891 in
  let r893 = [R 338] in
  let r894 = R 276 :: r893 in
  let r895 = [R 329] in
  let r896 = [R 563] in
  let r897 = R 268 :: r896 in
  let r898 = R 276 :: r897 in
  let r899 = S (N N_module_type) :: r898 in
  let r900 = S (T T_COLON) :: r899 in
  let r901 = [R 269] in
  let r902 = R 268 :: r901 in
  let r903 = R 276 :: r902 in
  let r904 = S (N N_module_type) :: r903 in
  let r905 = S (T T_COLON) :: r904 in
  let r906 = S (T T_UIDENT) :: r905 in
  let r907 = [R 566] in
  let r908 = R 276 :: r907 in
  let r909 = S (N N_module_type) :: r908 in
  let r910 = R 270 :: r909 in
  let r911 = [R 86] in
  let r912 = S (T T_LIDENT) :: r911 in
  let r913 = [R 69] in
  let r914 = Sub (r912) :: r913 in
  let r915 = [R 81] in
  let r916 = Sub (r914) :: r915 in
  let r917 = [R 567] in
  let r918 = R 262 :: r917 in
  let r919 = R 276 :: r918 in
  let r920 = Sub (r916) :: r919 in
  let r921 = S (T T_COLON) :: r920 in
  let r922 = S (T T_LIDENT) :: r921 in
  let r923 = R 188 :: r922 in
  let r924 = R 760 :: r923 in
  let r925 = R 270 :: r924 in
  let r926 = [R 85] in
  let r927 = R 264 :: r926 in
  let r928 = R 276 :: r927 in
  let r929 = Sub (r914) :: r928 in
  let r930 = S (T T_EQUAL) :: r929 in
  let r931 = S (T T_LIDENT) :: r930 in
  let r932 = R 188 :: r931 in
  let r933 = R 760 :: r932 in
  let r934 = R 270 :: r933 in
  let r935 = R 182 :: r934 in
  let r936 = [R 189] in
  let r937 = S (T T_RBRACKET) :: r936 in
  let r938 = [R 72] in
  let r939 = S (T T_END) :: r938 in
  let r940 = R 285 :: r939 in
  let r941 = R 62 :: r940 in
  let r942 = [R 61] in
  let r943 = S (T T_RPAREN) :: r942 in
  let r944 = [R 64] in
  let r945 = R 276 :: r944 in
  let r946 = Sub (r69) :: r945 in
  let r947 = S (T T_COLON) :: r946 in
  let r948 = S (T T_LIDENT) :: r947 in
  let r949 = R 353 :: r948 in
  let r950 = [R 65] in
  let r951 = R 276 :: r950 in
  let r952 = Sub (r795) :: r951 in
  let r953 = S (T T_COLON) :: r952 in
  let r954 = S (T T_LIDENT) :: r953 in
  let r955 = R 479 :: r954 in
  let r956 = [R 63] in
  let r957 = R 276 :: r956 in
  let r958 = Sub (r914) :: r957 in
  let r959 = [R 74] in
  let r960 = Sub (r914) :: r959 in
  let r961 = S (T T_IN) :: r960 in
  let r962 = Sub (r486) :: r961 in
  let r963 = R 270 :: r962 in
  let r964 = [R 75] in
  let r965 = Sub (r914) :: r964 in
  let r966 = S (T T_IN) :: r965 in
  let r967 = Sub (r486) :: r966 in
  let r968 = [R 519] in
  let r969 = Sub (r69) :: r968 in
  let r970 = [R 70] in
  let r971 = Sub (r912) :: r970 in
  let r972 = S (T T_RBRACKET) :: r971 in
  let r973 = Sub (r969) :: r972 in
  let r974 = [R 87] in
  let r975 = S (T T_LIDENT) :: r974 in
  let r976 = S (T T_DOT) :: r975 in
  let r977 = [R 520] in
  let r978 = [R 105] in
  let r979 = Sub (r69) :: r978 in
  let r980 = S (T T_EQUAL) :: r979 in
  let r981 = Sub (r69) :: r980 in
  let r982 = [R 66] in
  let r983 = R 276 :: r982 in
  let r984 = Sub (r981) :: r983 in
  let r985 = [R 67] in
  let r986 = [R 286] in
  let r987 = [R 265] in
  let r988 = R 264 :: r987 in
  let r989 = R 276 :: r988 in
  let r990 = Sub (r914) :: r989 in
  let r991 = S (T T_EQUAL) :: r990 in
  let r992 = S (T T_LIDENT) :: r991 in
  let r993 = R 188 :: r992 in
  let r994 = R 760 :: r993 in
  let r995 = [R 83] in
  let r996 = Sub (r916) :: r995 in
  let r997 = S (T T_MINUSGREATER) :: r996 in
  let r998 = Sub (r63) :: r997 in
  let r999 = [R 84] in
  let r1000 = Sub (r916) :: r999 in
  let r1001 = [R 82] in
  let r1002 = Sub (r916) :: r1001 in
  let r1003 = S (T T_MINUSGREATER) :: r1002 in
  let r1004 = [R 263] in
  let r1005 = R 262 :: r1004 in
  let r1006 = R 276 :: r1005 in
  let r1007 = Sub (r916) :: r1006 in
  let r1008 = S (T T_COLON) :: r1007 in
  let r1009 = S (T T_LIDENT) :: r1008 in
  let r1010 = R 188 :: r1009 in
  let r1011 = R 760 :: r1010 in
  let r1012 = [R 280] in
  let r1013 = [R 551] in
  let r1014 = [R 555] in
  let r1015 = [R 273] in
  let r1016 = R 272 :: r1015 in
  let r1017 = R 276 :: r1016 in
  let r1018 = R 500 :: r1017 in
  let r1019 = R 735 :: r1018 in
  let r1020 = S (T T_LIDENT) :: r1019 in
  let r1021 = R 740 :: r1020 in
  let r1022 = [R 556] in
  let r1023 = [R 275] in
  let r1024 = R 274 :: r1023 in
  let r1025 = R 276 :: r1024 in
  let r1026 = R 500 :: r1025 in
  let r1027 = Sub (r152) :: r1026 in
  let r1028 = S (T T_COLONEQUAL) :: r1027 in
  let r1029 = S (T T_LIDENT) :: r1028 in
  let r1030 = R 740 :: r1029 in
  let r1031 = [R 77] in
  let r1032 = Sub (r44) :: r1031 in
  let r1033 = [R 35] in
  let r1034 = Sub (r1032) :: r1033 in
  let r1035 = [R 51] in
  let r1036 = Sub (r1034) :: r1035 in
  let r1037 = S (T T_EQUAL) :: r1036 in
  let r1038 = [R 712] in
  let r1039 = R 260 :: r1038 in
  let r1040 = R 276 :: r1039 in
  let r1041 = Sub (r1037) :: r1040 in
  let r1042 = S (T T_LIDENT) :: r1041 in
  let r1043 = R 188 :: r1042 in
  let r1044 = R 760 :: r1043 in
  let r1045 = R 270 :: r1044 in
  let r1046 = [R 80] in
  let r1047 = S (T T_END) :: r1046 in
  let r1048 = R 287 :: r1047 in
  let r1049 = R 60 :: r1048 in
  let r1050 = [R 48] in
  let r1051 = R 276 :: r1050 in
  let r1052 = Sub (r1) :: r1051 in
  let r1053 = [R 43] in
  let r1054 = R 276 :: r1053 in
  let r1055 = R 418 :: r1054 in
  let r1056 = Sub (r1034) :: r1055 in
  let r1057 = [R 44] in
  let r1058 = R 276 :: r1057 in
  let r1059 = R 418 :: r1058 in
  let r1060 = Sub (r1034) :: r1059 in
  let r1061 = [R 76] in
  let r1062 = S (T T_RPAREN) :: r1061 in
  let r1063 = [R 38] in
  let r1064 = Sub (r1034) :: r1063 in
  let r1065 = S (T T_IN) :: r1064 in
  let r1066 = Sub (r486) :: r1065 in
  let r1067 = R 270 :: r1066 in
  let r1068 = [R 251] in
  let r1069 = R 276 :: r1068 in
  let r1070 = Sub (r363) :: r1069 in
  let r1071 = R 484 :: r1070 in
  let r1072 = R 270 :: r1071 in
  let r1073 = [R 39] in
  let r1074 = Sub (r1034) :: r1073 in
  let r1075 = S (T T_IN) :: r1074 in
  let r1076 = Sub (r486) :: r1075 in
  let r1077 = [R 78] in
  let r1078 = Sub (r44) :: r1077 in
  let r1079 = S (T T_RBRACKET) :: r1078 in
  let r1080 = [R 54] in
  let r1081 = Sub (r1034) :: r1080 in
  let r1082 = S (T T_MINUSGREATER) :: r1081 in
  let r1083 = Sub (r548) :: r1082 in
  let r1084 = [R 36] in
  let r1085 = Sub (r1083) :: r1084 in
  let r1086 = [R 37] in
  let r1087 = Sub (r1034) :: r1086 in
  let r1088 = [R 250] in
  let r1089 = R 276 :: r1088 in
  let r1090 = Sub (r363) :: r1089 in
  let r1091 = [R 79] in
  let r1092 = S (T T_RPAREN) :: r1091 in
  let r1093 = [R 419] in
  let r1094 = [R 47] in
  let r1095 = R 276 :: r1094 in
  let r1096 = Sub (r981) :: r1095 in
  let r1097 = [R 49] in
  let r1098 = [R 288] in
  let r1099 = [R 52] in
  let r1100 = Sub (r1034) :: r1099 in
  let r1101 = S (T T_EQUAL) :: r1100 in
  let r1102 = [R 53] in
  let r1103 = [R 261] in
  let r1104 = R 260 :: r1103 in
  let r1105 = R 276 :: r1104 in
  let r1106 = Sub (r1037) :: r1105 in
  let r1107 = S (T T_LIDENT) :: r1106 in
  let r1108 = R 188 :: r1107 in
  let r1109 = R 760 :: r1108 in
  let r1110 = [R 284] in
  let r1111 = [R 700] in
  let r1112 = [R 704] in
  let r1113 = [R 697] in
  let r1114 = R 281 :: r1113 in
  let r1115 = [R 283] in
  let r1116 = R 281 :: r1115 in
  let r1117 = [R 211] in
  let r1118 = R 276 :: r1117 in
  let r1119 = R 500 :: r1118 in
  let r1120 = [R 594] in
  let r1121 = S (T T_RPAREN) :: r1120 in
  let r1122 = S (N N_module_expr) :: r1121 in
  let r1123 = R 270 :: r1122 in
  let r1124 = [R 595] in
  let r1125 = S (T T_RPAREN) :: r1124 in
  let r1126 = [R 581] in
  let r1127 = [R 118] in
  let r1128 = [R 120] in
  let r1129 = [R 119] in
  let r1130 = [R 217] in
  let r1131 = [R 218] in
  let r1132 = [R 440] in
  let r1133 = [R 441] in
  let r1134 = [R 442] in
  let r1135 = [R 719] in
  let r1136 = [R 728] in
  let r1137 = [R 290] in
  let r1138 = [R 726] in
  let r1139 = S (T T_SEMISEMI) :: r1138 in
  let r1140 = [R 727] in
  let r1141 = [R 292] in
  let r1142 = [R 295] in
  let r1143 = [R 294] in
  let r1144 = [R 293] in
  let r1145 = R 291 :: r1144 in
  let r1146 = [R 749] in
  let r1147 = S (T T_EOF) :: r1146 in
  let r1148 = R 291 :: r1147 in
  let r1149 = [R 748] in
  function
  | 0 | 1718 | 1722 | 1726 | 1730 | 1734 | 1755 -> Nothing
  | 1717 -> One ([R 0])
  | 1721 -> One ([R 1])
  | 1723 -> One ([R 2])
  | 1729 -> One ([R 3])
  | 1733 -> One ([R 4])
  | 1745 -> One ([R 5])
  | 1765 -> One ([R 6])
  | 428 -> One ([R 7])
  | 427 -> One ([R 8])
  | 197 -> One ([R 16])
  | 214 -> One ([R 17])
  | 210 -> One ([R 31])
  | 1552 -> One ([R 40])
  | 1549 -> One ([R 41])
  | 1547 -> One ([R 42])
  | 1588 -> One ([R 50])
  | 1555 -> One ([R 55])
  | 1418 -> One ([R 68])
  | 1397 | 1453 -> One ([R 71])
  | 1400 -> One ([R 73])
  | 497 -> One ([R 89])
  | 72 -> One ([R 90])
  | 496 -> One ([R 91])
  | 172 | 314 -> One ([R 92])
  | 173 -> One ([R 97])
  | 396 -> One ([R 98])
  | 71 -> One ([R 104])
  | 313 -> One ([R 109])
  | 334 -> One ([R 110])
  | 244 -> One ([R 112])
  | 987 -> One ([R 113])
  | 741 -> One ([R 124])
  | 924 -> One ([R 141])
  | 754 -> One ([R 142])
  | 775 -> One ([R 143])
  | 757 -> One ([R 144])
  | 773 -> One ([R 181])
  | 1 -> One (R 182 :: r7)
  | 61 -> One (R 182 :: r24)
  | 65 -> One (R 182 :: r28)
  | 68 -> One (R 182 :: r39)
  | 75 -> One (R 182 :: r47)
  | 93 -> One (R 182 :: r75)
  | 429 -> One (R 182 :: r309)
  | 430 -> One (R 182 :: r313)
  | 435 -> One (R 182 :: r321)
  | 448 -> One (R 182 :: r332)
  | 463 -> One (R 182 :: r348)
  | 466 -> One (R 182 :: r353)
  | 471 -> One (R 182 :: r368)
  | 490 -> One (R 182 :: r391)
  | 511 -> One (R 182 :: r404)
  | 592 -> One (R 182 :: r471)
  | 674 -> One (R 182 :: r531)
  | 677 -> One (R 182 :: r534)
  | 680 -> One (R 182 :: r539)
  | 683 -> One (R 182 :: r542)
  | 689 -> One (R 182 :: r555)
  | 697 -> One (R 182 :: r566)
  | 702 -> One (R 182 :: r578)
  | 718 -> One (R 182 :: r589)
  | 732 -> One (R 182 :: r595)
  | 1070 -> One (R 182 :: r729)
  | 1085 -> One (R 182 :: r740)
  | 1234 -> One (R 182 :: r826)
  | 1235 -> One (R 182 :: r831)
  | 1261 -> One (R 182 :: r848)
  | 1266 -> One (R 182 :: r858)
  | 1290 -> One (R 182 :: r878)
  | 1291 -> One (R 182 :: r882)
  | 1300 -> One (R 182 :: r890)
  | 1329 -> One (R 182 :: r910)
  | 1338 -> One (R 182 :: r925)
  | 1682 -> One (R 182 :: r1123)
  | 599 -> One ([R 202])
  | 598 -> One ([R 203])
  | 606 -> One ([R 204])
  | 138 | 639 -> One ([R 215])
  | 308 -> One ([R 223])
  | 309 -> One ([R 224])
  | 923 -> One ([R 229])
  | 848 -> One ([R 249])
  | 1553 -> One ([R 252])
  | 573 -> One ([R 253])
  | 84 -> One (R 270 :: r51)
  | 150 -> One (R 270 :: r101)
  | 268 -> One (R 270 :: r220)
  | 433 -> One (R 270 :: r316)
  | 459 -> One (R 270 :: r343)
  | 595 -> One (R 270 :: r475)
  | 604 -> One (R 270 :: r485)
  | 823 -> One (R 270 :: r648)
  | 1157 -> One (R 270 :: r775)
  | 1185 -> One (R 270 :: r793)
  | 1252 -> One (R 270 :: r841)
  | 1321 -> One (R 270 :: r906)
  | 1350 -> One (R 270 :: r941)
  | 1356 -> One (R 270 :: r949)
  | 1367 -> One (R 270 :: r955)
  | 1378 -> One (R 270 :: r958)
  | 1383 -> One (R 270 :: r967)
  | 1407 -> One (R 270 :: r984)
  | 1423 -> One (R 270 :: r994)
  | 1460 -> One (R 270 :: r1011)
  | 1481 -> One (R 270 :: r1021)
  | 1491 -> One (R 270 :: r1030)
  | 1514 -> One (R 270 :: r1049)
  | 1517 -> One (R 270 :: r1052)
  | 1521 -> One (R 270 :: r1056)
  | 1522 -> One (R 270 :: r1060)
  | 1533 -> One (R 270 :: r1076)
  | 1541 -> One (R 270 :: r1085)
  | 1580 -> One (R 270 :: r1096)
  | 1600 -> One (R 270 :: r1109)
  | 1480 -> One (R 272 :: r1014)
  | 1622 -> One (R 272 :: r1112)
  | 1490 -> One (R 274 :: r1022)
  | 380 -> One (R 276 :: r293)
  | 1416 -> One (R 276 :: r985)
  | 1478 -> One (R 276 :: r1013)
  | 1586 -> One (R 276 :: r1097)
  | 1620 -> One (R 276 :: r1111)
  | 1627 -> One (R 276 :: r1114)
  | 1647 -> One (R 276 :: r1116)
  | 1750 -> One (R 276 :: r1139)
  | 1761 -> One (R 276 :: r1145)
  | 1766 -> One (R 276 :: r1148)
  | 1289 -> One (R 278 :: r874)
  | 1471 -> One (R 278 :: r1012)
  | 426 -> One (R 281 :: r305)
  | 1610 -> One (R 281 :: r1110)
  | 1419 -> One (R 285 :: r986)
  | 1589 -> One (R 287 :: r1098)
  | 1748 -> One (R 289 :: r1137)
  | 1756 -> One (R 291 :: r1141)
  | 1757 -> One (R 291 :: r1142)
  | 1758 -> One (R 291 :: r1143)
  | 547 -> One ([R 297])
  | 551 -> One ([R 299])
  | 764 -> One ([R 301])
  | 849 -> One ([R 302])
  | 1031 -> One ([R 305])
  | 271 -> One ([R 306])
  | 274 -> One ([R 307])
  | 273 -> One ([R 309])
  | 272 -> One ([R 311])
  | 270 -> One ([R 312])
  | 650 -> One ([R 332])
  | 660 -> One ([R 333])
  | 661 -> One ([R 334])
  | 659 -> One ([R 335])
  | 662 -> One ([R 337])
  | 636 -> One ([R 343])
  | 610 -> One ([R 344])
  | 642 -> One ([R 347])
  | 641 -> One ([R 349])
  | 298 | 1171 -> One ([R 352])
  | 1360 -> One ([R 354])
  | 1358 -> One ([R 355])
  | 1361 -> One ([R 356])
  | 1359 -> One ([R 357])
  | 584 -> One ([R 360])
  | 1274 -> One ([R 362])
  | 349 -> One ([R 363])
  | 339 -> One ([R 364])
  | 362 -> One ([R 365])
  | 340 -> One ([R 366])
  | 361 -> One ([R 367])
  | 356 -> One ([R 368])
  | 89 | 97 -> One ([R 381])
  | 105 | 727 -> One ([R 382])
  | 128 -> One ([R 383])
  | 118 -> One ([R 385])
  | 121 -> One ([R 387])
  | 124 -> One ([R 389])
  | 112 -> One ([R 390])
  | 127 | 946 -> One ([R 391])
  | 111 -> One ([R 392])
  | 110 -> One ([R 393])
  | 109 -> One ([R 394])
  | 108 -> One ([R 395])
  | 107 -> One ([R 396])
  | 100 | 447 | 717 -> One ([R 397])
  | 99 | 716 -> One ([R 398])
  | 98 -> One ([R 399])
  | 104 | 726 | 1018 -> One ([R 400])
  | 103 | 725 -> One ([R 401])
  | 87 -> One ([R 402])
  | 101 -> One ([R 403])
  | 114 -> One ([R 404])
  | 106 -> One ([R 405])
  | 113 -> One ([R 406])
  | 102 -> One ([R 407])
  | 126 -> One ([R 408])
  | 129 -> One ([R 409])
  | 125 -> One ([R 411])
  | 231 -> One ([R 412])
  | 230 -> One (R 413 :: r206)
  | 185 -> One (R 414 :: r173)
  | 186 -> One ([R 415])
  | 548 -> One (R 416 :: r412)
  | 549 -> One ([R 417])
  | 974 -> One ([R 431])
  | 144 -> One ([R 432])
  | 521 -> One ([R 444])
  | 515 -> One ([R 445])
  | 516 -> One ([R 447])
  | 514 | 728 -> One ([R 454])
  | 841 -> One ([R 459])
  | 843 -> One ([R 462])
  | 579 -> One ([R 464])
  | 1506 -> One ([R 468])
  | 385 | 1209 -> One ([R 478])
  | 1371 -> One ([R 480])
  | 1369 -> One ([R 481])
  | 1372 -> One ([R 482])
  | 1370 -> One ([R 483])
  | 1562 -> One (R 484 :: r1090)
  | 474 -> One ([R 485])
  | 337 -> One ([R 488])
  | 338 -> One ([R 489])
  | 336 -> One ([R 490])
  | 409 -> One ([R 492])
  | 408 -> One ([R 493])
  | 410 -> One ([R 494])
  | 405 -> One ([R 495])
  | 406 -> One ([R 496])
  | 1661 -> One ([R 498])
  | 1659 -> One ([R 499])
  | 643 -> One ([R 502])
  | 607 -> One ([R 503])
  | 926 -> One ([R 504])
  | 925 -> One ([R 505])
  | 259 -> One ([R 507])
  | 223 -> One ([R 531])
  | 954 -> One ([R 534])
  | 955 -> One ([R 535])
  | 1054 -> One ([R 537])
  | 1055 -> One ([R 538])
  | 541 -> One ([R 540])
  | 542 -> One ([R 541])
  | 977 -> One ([R 543])
  | 978 -> One ([R 544])
  | 778 -> One ([R 546])
  | 782 -> One ([R 547])
  | 1501 -> One ([R 552])
  | 1470 -> One ([R 553])
  | 1473 -> One ([R 554])
  | 1472 -> One ([R 559])
  | 1476 -> One ([R 562])
  | 1475 -> One ([R 564])
  | 1474 -> One ([R 565])
  | 1502 -> One ([R 568])
  | 445 -> One ([R 571])
  | 442 -> One ([R 573])
  | 708 -> One ([R 596])
  | 760 -> One ([R 597])
  | 759 | 774 -> One ([R 598])
  | 711 | 756 -> One ([R 599])
  | 868 | 920 -> One ([R 604])
  | 758 -> One ([R 609])
  | 498 -> One ([R 622])
  | 501 -> One ([R 625])
  | 502 -> One ([R 629])
  | 544 -> One ([R 631])
  | 506 -> One ([R 632])
  | 543 -> One ([R 634])
  | 524 -> One ([R 639])
  | 28 -> One ([R 640])
  | 8 -> One ([R 641])
  | 52 -> One ([R 643])
  | 51 -> One ([R 644])
  | 50 -> One ([R 645])
  | 49 -> One ([R 646])
  | 48 -> One ([R 647])
  | 47 -> One ([R 648])
  | 46 -> One ([R 649])
  | 45 -> One ([R 650])
  | 44 -> One ([R 651])
  | 43 -> One ([R 652])
  | 42 -> One ([R 653])
  | 41 -> One ([R 654])
  | 40 -> One ([R 655])
  | 39 -> One ([R 656])
  | 38 -> One ([R 657])
  | 37 -> One ([R 658])
  | 36 -> One ([R 659])
  | 35 -> One ([R 660])
  | 34 -> One ([R 661])
  | 33 -> One ([R 662])
  | 32 -> One ([R 663])
  | 31 -> One ([R 664])
  | 30 -> One ([R 665])
  | 29 -> One ([R 666])
  | 27 -> One ([R 667])
  | 26 -> One ([R 668])
  | 25 -> One ([R 669])
  | 24 -> One ([R 670])
  | 23 -> One ([R 671])
  | 22 -> One ([R 672])
  | 21 -> One ([R 673])
  | 20 -> One ([R 674])
  | 19 -> One ([R 675])
  | 18 -> One ([R 676])
  | 17 -> One ([R 677])
  | 16 -> One ([R 678])
  | 15 -> One ([R 679])
  | 14 -> One ([R 680])
  | 13 -> One ([R 681])
  | 12 -> One ([R 682])
  | 11 -> One ([R 683])
  | 10 -> One ([R 684])
  | 9 -> One ([R 685])
  | 7 -> One ([R 686])
  | 6 -> One ([R 687])
  | 5 -> One ([R 688])
  | 4 -> One ([R 689])
  | 3 -> One ([R 690])
  | 1613 -> One ([R 691])
  | 1633 -> One ([R 696])
  | 1617 | 1632 -> One ([R 698])
  | 1619 | 1634 -> One ([R 699])
  | 1624 -> One ([R 701])
  | 1614 -> One ([R 702])
  | 1609 -> One ([R 703])
  | 1612 -> One ([R 707])
  | 1616 -> One ([R 710])
  | 1615 -> One ([R 711])
  | 1625 -> One ([R 713])
  | 462 -> One ([R 715])
  | 461 -> One ([R 716])
  | 1738 -> One ([R 720])
  | 1739 -> One ([R 721])
  | 1741 -> One ([R 722])
  | 1742 -> One ([R 723])
  | 1740 -> One ([R 724])
  | 1737 -> One ([R 725])
  | 1744 -> One ([R 729])
  | 200 -> One ([R 731])
  | 613 -> One (R 740 :: r498)
  | 415 -> One ([R 741])
  | 155 -> One ([R 746])
  | 157 -> One ([R 747])
  | 709 -> One ([R 752])
  | 985 -> One ([R 753])
  | 1342 -> One ([R 761])
  | 1169 -> One ([R 762])
  | 1172 -> One ([R 763])
  | 1170 -> One ([R 764])
  | 1207 -> One ([R 765])
  | 1210 -> One ([R 766])
  | 1208 -> One ([R 767])
  | 616 -> One ([R 772])
  | 617 -> One ([R 773])
  | 964 -> One (S (T T_WITH) :: r696)
  | 627 | 1743 -> One (S (T T_UIDENT) :: r50)
  | 206 -> One (S (T T_UIDENT) :: r195)
  | 1248 -> One (S (T T_UIDENT) :: r836)
  | 1316 -> One (S (T T_UIDENT) :: r900)
  | 454 -> One (S (T T_TYPE) :: r338)
  | 581 -> One (S (T T_TYPE) :: r453)
  | 322 -> One (S (T T_STAR) :: r255)
  | 1746 -> One (S (T T_SEMISEMI) :: r1136)
  | 1753 -> One (S (T T_SEMISEMI) :: r1140)
  | 390 -> One (S (T T_RPAREN) :: r54)
  | 175 | 315 -> One (S (T T_RPAREN) :: r155)
  | 282 -> One (S (T T_RPAREN) :: r222)
  | 284 -> One (S (T T_RPAREN) :: r224)
  | 291 -> One (S (T T_RPAREN) :: r227)
  | 391 -> One (S (T T_RPAREN) :: r298)
  | 509 -> One (S (T T_RPAREN) :: r401)
  | 528 -> One (S (T T_RPAREN) :: r408)
  | 597 -> One (S (T T_RPAREN) :: r476)
  | 652 -> One (S (T T_RPAREN) :: r506)
  | 947 -> One (S (T T_RPAREN) :: r685)
  | 1692 -> One (S (T T_RPAREN) :: r1126)
  | 119 -> One (S (T T_RBRACKET) :: r83)
  | 188 -> One (S (T T_RBRACKET) :: r174)
  | 295 | 316 -> One (S (T T_RBRACKET) :: r229)
  | 393 -> One (S (T T_RBRACKET) :: r299)
  | 956 -> One (S (T T_RBRACKET) :: r688)
  | 958 -> One (S (T T_RBRACKET) :: r689)
  | 122 -> One (S (T T_RBRACE) :: r84)
  | 237 -> One (S (T T_QUOTE) :: r209)
  | 1381 -> One (S (T T_OPEN) :: r963)
  | 1525 -> One (S (T T_OPEN) :: r1067)
  | 145 -> One (S (T T_MODULE) :: r96)
  | 328 -> One (S (T T_MINUSGREATER) :: r258)
  | 1445 -> One (S (T T_MINUSGREATER) :: r1000)
  | 115 -> One (S (T T_LPAREN) :: r82)
  | 397 -> One (S (T T_LPAREN) :: r302)
  | 141 -> One (S (T T_LIDENT) :: r91)
  | 299 -> One (S (T T_LIDENT) :: r245)
  | 556 -> One (S (T T_LIDENT) :: r418)
  | 564 -> One (S (T T_LIDENT) :: r424)
  | 742 -> One (S (T T_LIDENT) :: r605)
  | 744 -> One (S (T T_LIDENT) :: r606)
  | 748 -> One (S (T T_LIDENT) :: r608)
  | 1173 -> One (S (T T_LIDENT) :: r780)
  | 1211 -> One (S (T T_LIDENT) :: r808)
  | 1572 -> One (S (T T_LIDENT) :: r1093)
  | 440 -> One (S (T T_INT) :: r325)
  | 443 -> One (S (T T_INT) :: r326)
  | 761 -> One (S (T T_IN) :: r618)
  | 765 -> One (S (T T_IN) :: r620)
  | 1545 -> One (S (T T_IN) :: r1087)
  | 667 -> One (S (T T_GREATERRBRACE) :: r514)
  | 1057 -> One (S (T T_GREATERRBRACE) :: r719)
  | 180 -> One (S (T T_GREATER) :: r160)
  | 277 -> One (S (T T_GREATER) :: r221)
  | 1099 -> One (S (T T_EQUAL) :: r742)
  | 1123 -> One (S (T T_EQUAL) :: r754)
  | 1163 -> One (S (T T_EQUAL) :: r777)
  | 1181 -> One (S (T T_EQUAL) :: r782)
  | 1715 -> One (S (T T_EOF) :: r1130)
  | 1719 -> One (S (T T_EOF) :: r1131)
  | 1724 -> One (S (T T_EOF) :: r1132)
  | 1727 -> One (S (T T_EOF) :: r1133)
  | 1731 -> One (S (T T_EOF) :: r1134)
  | 1770 -> One (S (T T_EOF) :: r1149)
  | 1044 -> One (S (T T_END) :: r718)
  | 174 -> One (S (T T_DOTDOT) :: r154)
  | 350 -> One (S (T T_DOTDOT) :: r262)
  | 351 -> One (S (T T_DOTDOT) :: r263)
  | 79 -> One (S (T T_DOT) :: r49)
  | 202 -> One (S (T T_DOT) :: r191)
  | 261 -> One (S (T T_DOT) :: r218)
  | 480 | 857 | 903 -> One (S (T T_DOT) :: r383)
  | 637 -> One (S (T T_DOT) :: r505)
  | 1118 -> One (S (T T_DOT) :: r752)
  | 1196 -> One (S (T T_DOT) :: r805)
  | 181 -> One (S (T T_COLON) :: r165)
  | 601 -> One (S (T T_COLON) :: r479)
  | 1439 -> One (S (T T_COLON) :: r998)
  | 476 -> One (S (T T_BARRBRACKET) :: r369)
  | 553 -> One (S (T T_BARRBRACKET) :: r413)
  | 665 -> One (S (T T_BARRBRACKET) :: r509)
  | 949 -> One (S (T T_BARRBRACKET) :: r686)
  | 951 -> One (S (T T_BARRBRACKET) :: r687)
  | 1062 -> One (S (T T_BARRBRACKET) :: r720)
  | 248 -> One (S (T T_BAR) :: r212)
  | 438 -> One (S (N N_pattern) :: r323)
  | 692 | 1006 -> One (S (N N_pattern) :: r328)
  | 489 -> One (S (N N_pattern) :: r385)
  | 517 -> One (S (N N_pattern) :: r405)
  | 519 -> One (S (N N_pattern) :: r406)
  | 530 -> One (S (N N_pattern) :: r409)
  | 532 -> One (S (N N_pattern) :: r410)
  | 833 -> One (S (N N_pattern) :: r652)
  | 835 -> One (S (N N_pattern) :: r653)
  | 837 -> One (S (N N_pattern) :: r654)
  | 844 -> One (S (N N_pattern) :: r656)
  | 1230 -> One (S (N N_pattern) :: r820)
  | 453 -> One (S (N N_module_type) :: r334)
  | 603 -> One (S (N N_module_type) :: r481)
  | 634 -> One (S (N N_module_type) :: r503)
  | 656 -> One (S (N N_module_type) :: r508)
  | 1076 -> One (S (N N_module_type) :: r732)
  | 1138 -> One (S (N N_module_type) :: r756)
  | 1141 -> One (S (N N_module_type) :: r758)
  | 1144 -> One (S (N N_module_type) :: r760)
  | 1239 -> One (S (N N_module_type) :: r832)
  | 1687 -> One (S (N N_module_type) :: r1125)
  | 458 -> One (S (N N_module_expr) :: r340)
  | 572 -> One (S (N N_let_pattern) :: r444)
  | 470 -> One (S (N N_expr) :: r354)
  | 669 -> One (S (N N_expr) :: r517)
  | 673 -> One (S (N N_expr) :: r528)
  | 740 -> One (S (N N_expr) :: r604)
  | 755 -> One (S (N N_expr) :: r616)
  | 769 -> One (S (N N_expr) :: r621)
  | 771 -> One (S (N N_expr) :: r622)
  | 776 -> One (S (N N_expr) :: r623)
  | 783 -> One (S (N N_expr) :: r626)
  | 785 -> One (S (N N_expr) :: r627)
  | 787 -> One (S (N N_expr) :: r628)
  | 789 -> One (S (N N_expr) :: r629)
  | 791 -> One (S (N N_expr) :: r630)
  | 793 -> One (S (N N_expr) :: r631)
  | 795 -> One (S (N N_expr) :: r632)
  | 797 -> One (S (N N_expr) :: r633)
  | 799 -> One (S (N N_expr) :: r634)
  | 801 -> One (S (N N_expr) :: r635)
  | 803 -> One (S (N N_expr) :: r636)
  | 805 -> One (S (N N_expr) :: r637)
  | 807 -> One (S (N N_expr) :: r638)
  | 809 -> One (S (N N_expr) :: r639)
  | 811 -> One (S (N N_expr) :: r640)
  | 813 -> One (S (N N_expr) :: r641)
  | 815 -> One (S (N N_expr) :: r642)
  | 817 -> One (S (N N_expr) :: r643)
  | 819 -> One (S (N N_expr) :: r644)
  | 821 -> One (S (N N_expr) :: r645)
  | 875 -> One (S (N N_expr) :: r671)
  | 880 -> One (S (N N_expr) :: r672)
  | 885 -> One (S (N N_expr) :: r676)
  | 891 -> One (S (N N_expr) :: r677)
  | 896 -> One (S (N N_expr) :: r678)
  | 901 -> One (S (N N_expr) :: r679)
  | 908 -> One (S (N N_expr) :: r680)
  | 913 -> One (S (N N_expr) :: r681)
  | 918 -> One (S (N N_expr) :: r682)
  | 921 -> One (S (N N_expr) :: r683)
  | 1041 -> One (S (N N_expr) :: r717)
  | 567 -> One (Sub (r1) :: r428)
  | 688 -> One (Sub (r1) :: r546)
  | 998 -> One (Sub (r1) :: r706)
  | 1232 -> One (Sub (r1) :: r821)
  | 1700 -> One (Sub (r1) :: r1128)
  | 1702 -> One (Sub (r1) :: r1129)
  | 2 -> One (Sub (r11) :: r12)
  | 55 -> One (Sub (r11) :: r13)
  | 59 -> One (Sub (r11) :: r18)
  | 91 -> One (Sub (r11) :: r58)
  | 366 -> One (Sub (r11) :: r273)
  | 779 -> One (Sub (r11) :: r625)
  | 1228 -> One (Sub (r11) :: r819)
  | 1259 -> One (Sub (r11) :: r844)
  | 1526 -> One (Sub (r11) :: r1072)
  | 686 -> One (Sub (r32) :: r543)
  | 1035 -> One (Sub (r32) :: r716)
  | 1698 -> One (Sub (r34) :: r1127)
  | 74 -> One (Sub (r41) :: r42)
  | 672 -> One (Sub (r41) :: r526)
  | 707 -> One (Sub (r41) :: r579)
  | 736 -> One (Sub (r41) :: r596)
  | 746 -> One (Sub (r41) :: r607)
  | 869 -> One (Sub (r41) :: r670)
  | 192 -> One (Sub (r44) :: r185)
  | 212 -> One (Sub (r44) :: r196)
  | 286 -> One (Sub (r44) :: r225)
  | 534 -> One (Sub (r59) :: r411)
  | 839 -> One (Sub (r59) :: r655)
  | 201 -> One (Sub (r61) :: r189)
  | 220 -> One (Sub (r61) :: r200)
  | 327 -> One (Sub (r61) :: r256)
  | 1010 -> One (Sub (r61) :: r712)
  | 215 -> One (Sub (r63) :: r199)
  | 1447 -> One (Sub (r63) :: r1003)
  | 199 -> One (Sub (r65) :: r188)
  | 234 -> One (Sub (r67) :: r207)
  | 620 -> One (Sub (r67) :: r500)
  | 289 -> One (Sub (r69) :: r226)
  | 293 -> One (Sub (r69) :: r228)
  | 376 -> One (Sub (r69) :: r292)
  | 486 -> One (Sub (r69) :: r384)
  | 559 -> One (Sub (r69) :: r423)
  | 574 -> One (Sub (r69) :: r445)
  | 729 -> One (Sub (r69) :: r592)
  | 826 -> One (Sub (r69) :: r651)
  | 968 -> One (Sub (r69) :: r697)
  | 972 -> One (Sub (r69) :: r700)
  | 1021 -> One (Sub (r69) :: r715)
  | 1152 -> One (Sub (r69) :: r762)
  | 1352 -> One (Sub (r69) :: r943)
  | 1394 -> One (Sub (r69) :: r977)
  | 161 -> One (Sub (r87) :: r149)
  | 262 -> One (Sub (r87) :: r219)
  | 1735 -> One (Sub (r87) :: r1135)
  | 1288 -> One (Sub (r98) :: r873)
  | 494 -> One (Sub (r111) :: r393)
  | 167 -> One (Sub (r144) :: r150)
  | 158 -> One (Sub (r146) :: r148)
  | 1344 -> One (Sub (r146) :: r937)
  | 171 -> One (Sub (r152) :: r153)
  | 363 -> One (Sub (r152) :: r270)
  | 1664 -> One (Sub (r152) :: r1119)
  | 227 -> One (Sub (r168) :: r201)
  | 190 -> One (Sub (r170) :: r176)
  | 194 -> One (Sub (r170) :: r187)
  | 191 -> One (Sub (r182) :: r184)
  | 203 -> One (Sub (r192) :: r194)
  | 628 -> One (Sub (r192) :: r501)
  | 1307 -> One (Sub (r192) :: r894)
  | 256 -> One (Sub (r214) :: r216)
  | 297 -> One (Sub (r237) :: r239)
  | 319 -> One (Sub (r237) :: r253)
  | 344 -> One (Sub (r237) :: r261)
  | 352 -> One (Sub (r237) :: r265)
  | 357 -> One (Sub (r237) :: r267)
  | 318 -> One (Sub (r250) :: r251)
  | 389 -> One (Sub (r295) :: r297)
  | 412 -> One (Sub (r295) :: r304)
  | 943 -> One (Sub (r363) :: r684)
  | 478 -> One (Sub (r379) :: r381)
  | 587 -> One (Sub (r388) :: r454)
  | 503 -> One (Sub (r396) :: r397)
  | 555 -> One (Sub (r416) :: r417)
  | 569 -> One (Sub (r416) :: r438)
  | 557 -> One (Sub (r420) :: r422)
  | 565 -> One (Sub (r420) :: r427)
  | 568 -> One (Sub (r434) :: r437)
  | 570 -> One (Sub (r440) :: r441)
  | 693 -> One (Sub (r447) :: r558)
  | 1007 -> One (Sub (r447) :: r709)
  | 1112 -> One (Sub (r447) :: r748)
  | 1190 -> One (Sub (r447) :: r803)
  | 1218 -> One (Sub (r447) :: r816)
  | 1103 -> One (Sub (r449) :: r743)
  | 1304 -> One (Sub (r486) :: r892)
  | 632 -> One (Sub (r491) :: r502)
  | 612 -> One (Sub (r493) :: r494)
  | 670 -> One (Sub (r523) :: r525)
  | 963 -> One (Sub (r523) :: r694)
  | 1015 -> One (Sub (r551) :: r713)
  | 960 -> One (Sub (r690) :: r692)
  | 1083 -> One (Sub (r723) :: r733)
  | 1156 -> One (Sub (r768) :: r770)
  | 1184 -> One (Sub (r787) :: r789)
  | 1189 -> One (Sub (r795) :: r798)
  | 1217 -> One (Sub (r795) :: r811)
  | 1314 -> One (Sub (r885) :: r895)
  | 1568 -> One (Sub (r916) :: r1092)
  | 1592 -> One (Sub (r916) :: r1101)
  | 1537 -> One (Sub (r969) :: r1079)
  | 1524 -> One (Sub (r1034) :: r1062)
  | 1596 -> One (Sub (r1037) :: r1102)
  | 768 -> One (r0)
  | 1714 -> One (r2)
  | 1713 -> One (r3)
  | 1712 -> One (r4)
  | 1711 -> One (r5)
  | 1710 -> One (r6)
  | 58 -> One (r7)
  | 53 -> One (r8)
  | 54 -> One (r10)
  | 57 -> One (r12)
  | 56 -> One (r13)
  | 1626 -> One (r14)
  | 1709 -> One (r16)
  | 1708 -> One (r17)
  | 60 -> One (r18)
  | 1707 -> One (r19)
  | 1706 -> One (r20)
  | 1705 -> One (r21)
  | 1704 -> One (r22)
  | 63 -> One (r23)
  | 62 -> One (r24)
  | 64 -> One (r25)
  | 1697 -> One (r26)
  | 67 -> One (r27)
  | 66 -> One (r28)
  | 1032 -> One (r29)
  | 1030 -> One (r30)
  | 687 -> One (r31)
  | 1037 -> One (r33)
  | 1696 -> One (r35)
  | 1695 -> One (r36)
  | 1694 -> One (r37)
  | 70 -> One (r38)
  | 69 -> One (r39)
  | 73 -> One (r40)
  | 1681 -> One (r42)
  | 78 -> One (r43)
  | 83 -> One (r45)
  | 77 -> One (r46)
  | 76 -> One (r47)
  | 82 -> One (r48)
  | 80 -> One (r49)
  | 81 -> One (r50)
  | 85 -> One (r51)
  | 1691 -> One (r52)
  | 1690 -> One (r53)
  | 88 -> One (r54)
  | 90 | 469 | 671 | 984 -> One (r55)
  | 1680 -> One (r56)
  | 1679 -> One (r57)
  | 92 -> One (r58)
  | 135 -> One (r60)
  | 219 -> One (r62)
  | 198 -> One (r64)
  | 235 -> One (r66)
  | 245 -> One (r68)
  | 1678 -> One (r70)
  | 1677 -> One (r71)
  | 134 -> One (r72)
  | 133 -> One (r73)
  | 95 -> One (r74)
  | 94 -> One (r75)
  | 130 -> One (r76)
  | 132 -> One (r78)
  | 131 -> One (r79)
  | 96 -> One (r80)
  | 117 -> One (r81)
  | 116 -> One (r82)
  | 120 -> One (r83)
  | 123 -> One (r84)
  | 136 | 149 -> One (r85)
  | 139 -> One (r86)
  | 140 -> One (r88)
  | 137 -> One (r89)
  | 143 -> One (r90)
  | 142 -> One (r91)
  | 1676 -> One (r92)
  | 1675 -> One (r93)
  | 148 -> One (r94)
  | 147 -> One (r95)
  | 146 -> One (r96)
  | 1505 -> One (r97)
  | 1674 -> One (r99)
  | 1673 -> One (r100)
  | 151 -> One (r101)
  | 420 -> One (r102)
  | 419 -> One (r103)
  | 418 -> One (r104)
  | 179 -> One (r110)
  | 311 -> One (r112)
  | 343 -> One (r114)
  | 342 -> One (r115)
  | 341 | 411 -> One (r116)
  | 1660 -> One (r118)
  | 1672 -> One (r120)
  | 1671 -> One (r121)
  | 1670 -> One (r122)
  | 1669 -> One (r123)
  | 1668 -> One (r124)
  | 382 -> One (r128)
  | 375 -> One (r129)
  | 374 -> One (r130)
  | 1658 -> One (r134)
  | 1657 -> One (r135)
  | 1656 -> One (r136)
  | 1655 -> One (r137)
  | 1654 -> One (r138)
  | 160 -> One (r140)
  | 163 -> One (r142)
  | 159 -> One (r143)
  | 164 -> One (r145)
  | 166 -> One (r147)
  | 165 -> One (r148)
  | 162 -> One (r149)
  | 168 -> One (r150)
  | 347 -> One (r151)
  | 348 -> One (r153)
  | 312 -> One (r154)
  | 176 -> One (r155)
  | 281 -> One (r156)
  | 280 -> One (r157)
  | 279 -> One (r158)
  | 178 -> One (r159)
  | 276 -> One (r160)
  | 275 -> One (r161)
  | 267 -> One (r163)
  | 266 -> One (r164)
  | 182 -> One (r165)
  | 243 -> One (r167)
  | 224 -> One (r169)
  | 255 -> One (r171)
  | 254 -> One (r172)
  | 187 -> One (r173)
  | 189 -> One (r174)
  | 253 -> One (r175)
  | 252 -> One (r176)
  | 196 -> One (r177)
  | 195 -> One (r178)
  | 242 -> One (r180)
  | 229 -> One (r181)
  | 247 -> One (r183)
  | 246 -> One (r184)
  | 193 -> One (r185)
  | 226 -> One (r186)
  | 225 -> One (r187)
  | 222 -> One (r188)
  | 211 -> One (r189)
  | 209 -> One (r190)
  | 208 -> One (r191)
  | 205 -> One (r193)
  | 204 -> One (r194)
  | 207 -> One (r195)
  | 213 -> One (r196)
  | 218 -> One (r197)
  | 217 -> One (r198)
  | 216 -> One (r199)
  | 221 -> One (r200)
  | 228 -> One (r201)
  | 241 -> One (r202)
  | 240 -> One (r204)
  | 233 -> One (r205)
  | 232 -> One (r206)
  | 236 -> One (r207)
  | 239 -> One (r208)
  | 238 -> One (r209)
  | 251 -> One (r210)
  | 250 -> One (r211)
  | 249 -> One (r212)
  | 260 -> One (r213)
  | 258 -> One (r215)
  | 257 -> One (r216)
  | 265 -> One (r217)
  | 264 -> One (r218)
  | 263 -> One (r219)
  | 269 -> One (r220)
  | 278 -> One (r221)
  | 283 -> One (r222)
  | 288 -> One (r223)
  | 285 -> One (r224)
  | 287 -> One (r225)
  | 290 -> One (r226)
  | 292 -> One (r227)
  | 294 -> One (r228)
  | 296 -> One (r229)
  | 310 -> One (r236)
  | 307 -> One (r238)
  | 306 -> One (r239)
  | 305 -> One (r240)
  | 304 -> One (r241)
  | 303 -> One (r242)
  | 302 -> One (r243)
  | 301 -> One (r244)
  | 300 -> One (r245)
  | 333 -> One (r246)
  | 332 -> One (r247)
  | 317 | 388 -> One (r248)
  | 326 -> One (r249)
  | 325 -> One (r251)
  | 321 -> One (r252)
  | 320 -> One (r253)
  | 324 -> One (r254)
  | 323 -> One (r255)
  | 331 -> One (r256)
  | 330 -> One (r257)
  | 329 -> One (r258)
  | 335 | 387 -> One (r259)
  | 346 -> One (r260)
  | 345 -> One (r261)
  | 360 -> One (r262)
  | 355 -> One (r263)
  | 354 -> One (r264)
  | 353 -> One (r265)
  | 359 -> One (r266)
  | 358 -> One (r267)
  | 1653 -> One (r268)
  | 365 -> One (r269)
  | 364 -> One (r270)
  | 1652 -> One (r271)
  | 1651 -> One (r272)
  | 367 -> One (r273)
  | 407 -> One (r274)
  | 425 -> One (r276)
  | 424 -> One (r277)
  | 423 -> One (r278)
  | 422 -> One (r279)
  | 421 -> One (r280)
  | 404 -> One (r284)
  | 403 -> One (r285)
  | 386 -> One (r286)
  | 384 -> One (r287)
  | 383 -> One (r288)
  | 379 -> One (r290)
  | 378 -> One (r291)
  | 377 -> One (r292)
  | 381 -> One (r293)
  | 395 -> One (r294)
  | 402 -> One (r296)
  | 401 -> One (r297)
  | 392 -> One (r298)
  | 394 -> One (r299)
  | 400 -> One (r300)
  | 399 -> One (r301)
  | 398 -> One (r302)
  | 414 -> One (r303)
  | 413 -> One (r304)
  | 1650 -> One (r305)
  | 1646 -> One (r306)
  | 1645 -> One (r307)
  | 1644 -> One (r308)
  | 1643 -> One (r309)
  | 1642 -> One (r310)
  | 1641 -> One (r311)
  | 432 -> One (r312)
  | 431 -> One (r313)
  | 1640 -> One (r314)
  | 1639 -> One (r315)
  | 434 -> One (r316)
  | 1638 -> One (r317)
  | 1637 -> One (r318)
  | 1155 -> One (r319)
  | 437 -> One (r320)
  | 436 -> One (r321)
  | 1151 -> One (r322)
  | 1150 -> One (r323)
  | 439 -> One (r324)
  | 441 -> One (r325)
  | 444 -> One (r326)
  | 1020 -> One (r327)
  | 1019 -> One (r328)
  | 452 -> One (r329)
  | 451 -> One (r330)
  | 450 -> One (r331)
  | 449 -> One (r332)
  | 1149 -> One (r333)
  | 1148 -> One (r334)
  | 1147 -> One (r335)
  | 457 -> One (r336)
  | 456 -> One (r337)
  | 455 -> One (r338)
  | 655 -> One (r339)
  | 654 -> One (r340)
  | 1137 -> One (r341)
  | 1136 -> One (r342)
  | 460 -> One (r343)
  | 1135 -> One (r344)
  | 1134 -> One (r345)
  | 1133 -> One (r346)
  | 465 -> One (r347)
  | 464 -> One (r348)
  | 1132 -> One (r349)
  | 1131 -> One (r350)
  | 1130 -> One (r351)
  | 468 -> One (r352)
  | 467 -> One (r353)
  | 1129 -> One (r354)
  | 526 -> One (r355)
  | 842 -> One (r358)
  | 832 -> One (r360)
  | 831 -> One (r361)
  | 830 -> One (r362)
  | 1128 -> One (r364)
  | 1127 -> One (r365)
  | 475 -> One (r366)
  | 473 -> One (r367)
  | 472 -> One (r368)
  | 552 -> One (r369)
  | 540 -> One (r370)
  | 539 -> One (r372)
  | 538 -> One (r373)
  | 479 -> One (r374)
  | 546 -> One (r376)
  | 488 -> One (r377)
  | 485 -> One (r378)
  | 484 -> One (r380)
  | 483 -> One (r381)
  | 482 -> One (r382)
  | 481 -> One (r383)
  | 487 -> One (r384)
  | 545 -> One (r385)
  | 499 | 825 -> One (r387)
  | 500 -> One (r389)
  | 492 -> One (r390)
  | 491 -> One (r391)
  | 493 -> One (r392)
  | 495 -> One (r393)
  | 505 -> One (r395)
  | 504 -> One (r397)
  | 537 -> One (r398)
  | 536 -> One (r399)
  | 508 -> One (r400)
  | 510 -> One (r401)
  | 527 -> One (r402)
  | 513 -> One (r403)
  | 512 -> One (r404)
  | 518 -> One (r405)
  | 520 -> One (r406)
  | 523 -> One (r407)
  | 529 -> One (r408)
  | 531 -> One (r409)
  | 533 -> One (r410)
  | 535 -> One (r411)
  | 550 -> One (r412)
  | 554 -> One (r413)
  | 1098 -> One (r414)
  | 589 -> One (r415)
  | 1126 -> One (r417)
  | 563 -> One (r418)
  | 558 -> One (r419)
  | 562 -> One (r421)
  | 561 -> One (r422)
  | 560 -> One (r423)
  | 1110 -> One (r424)
  | 1109 -> One (r425)
  | 1108 -> One (r426)
  | 566 -> One (r427)
  | 1107 -> One (r428)
  | 939 -> One (r429)
  | 938 -> One (r430)
  | 937 -> One (r431)
  | 945 -> One (r433)
  | 942 -> One (r435)
  | 941 -> One (r436)
  | 940 -> One (r437)
  | 1106 -> One (r438)
  | 571 -> One (r439)
  | 580 -> One (r441)
  | 578 -> One (r442)
  | 577 -> One (r443)
  | 576 -> One (r444)
  | 575 -> One (r445)
  | 583 -> One (r446)
  | 1102 -> One (r448)
  | 1105 -> One (r450)
  | 586 -> One (r451)
  | 585 -> One (r452)
  | 582 -> One (r453)
  | 588 -> One (r454)
  | 1069 -> One (r455)
  | 1068 -> One (r456)
  | 1067 -> One (r457)
  | 1066 -> One (r458)
  | 1065 -> One (r459)
  | 591 -> One (r460)
  | 1097 -> One (r461)
  | 1096 -> One (r462)
  | 1095 -> One (r463)
  | 1094 -> One (r464)
  | 1093 -> One (r465)
  | 1611 -> One (r466)
  | 1064 -> One (r467)
  | 664 -> One (r468)
  | 663 -> One (r469)
  | 594 -> One (r470)
  | 593 -> One (r471)
  | 651 -> One (r472)
  | 649 -> One (r473)
  | 648 -> One (r474)
  | 596 -> One (r475)
  | 600 -> One (r476)
  | 647 -> One (r477)
  | 646 -> One (r478)
  | 602 -> One (r479)
  | 645 -> One (r480)
  | 644 -> One (r481)
  | 611 -> One (r482)
  | 609 -> One (r483)
  | 608 -> One (r484)
  | 605 -> One (r485)
  | 626 -> One (r487)
  | 625 -> One (r488)
  | 624 -> One (r489)
  | 623 -> One (r490)
  | 630 -> One (r492)
  | 631 -> One (r494)
  | 619 -> One (r495)
  | 618 -> One (r496)
  | 615 -> One (r497)
  | 614 -> One (r498)
  | 622 -> One (r499)
  | 621 -> One (r500)
  | 629 -> One (r501)
  | 633 -> One (r502)
  | 635 -> One (r503)
  | 640 -> One (r504)
  | 638 -> One (r505)
  | 653 -> One (r506)
  | 658 -> One (r507)
  | 657 -> One (r508)
  | 1061 -> One (r509)
  | 953 -> One (r510)
  | 1060 -> One (r512)
  | 1059 -> One (r513)
  | 1056 -> One (r514)
  | 1053 -> One (r515)
  | 668 -> One (r516)
  | 1052 -> One (r517)
  | 976 -> One (r518)
  | 975 -> One (r519)
  | 967 -> One (r520)
  | 979 -> One (r522)
  | 1051 -> One (r524)
  | 1050 -> One (r525)
  | 1049 -> One (r526)
  | 1048 -> One (r527)
  | 1047 -> One (r528)
  | 1046 -> One (r529)
  | 676 -> One (r530)
  | 675 -> One (r531)
  | 1043 -> One (r532)
  | 679 -> One (r533)
  | 678 -> One (r534)
  | 1040 -> One (r535)
  | 1039 -> One (r536)
  | 1038 -> One (r537)
  | 682 -> One (r538)
  | 681 -> One (r539)
  | 1034 -> One (r540)
  | 685 -> One (r541)
  | 684 -> One (r542)
  | 1033 -> One (r543)
  | 1029 -> One (r544)
  | 1028 -> One (r545)
  | 1027 -> One (r546)
  | 1014 -> One (r547)
  | 1005 -> One (r549)
  | 696 -> One (r550)
  | 1026 -> One (r552)
  | 1025 -> One (r553)
  | 691 -> One (r554)
  | 690 -> One (r555)
  | 1024 -> One (r556)
  | 695 -> One (r557)
  | 694 -> One (r558)
  | 997 -> One (r559)
  | 996 -> One (r560)
  | 995 -> One (r561)
  | 994 -> One (r562)
  | 701 -> One (r563)
  | 700 -> One (r564)
  | 699 -> One (r565)
  | 698 -> One (r566)
  | 988 -> One (r567)
  | 993 -> One (r569)
  | 992 -> One (r570)
  | 991 -> One (r571)
  | 990 -> One (r572)
  | 989 -> One (r573)
  | 986 -> One (r574)
  | 706 -> One (r575)
  | 705 -> One (r576)
  | 704 -> One (r577)
  | 703 -> One (r578)
  | 710 -> One (r579)
  | 715 -> One (r580)
  | 714 -> One (r581)
  | 713 | 983 -> One (r582)
  | 982 -> One (r583)
  | 724 -> One (r584)
  | 723 -> One (r585)
  | 722 -> One (r586)
  | 721 -> One (r587)
  | 720 -> One (r588)
  | 719 -> One (r589)
  | 936 -> One (r590)
  | 731 -> One (r591)
  | 730 -> One (r592)
  | 735 -> One (r593)
  | 734 -> One (r594)
  | 733 -> One (r595)
  | 737 -> One (r596)
  | 879 | 932 -> One (r597)
  | 878 | 931 -> One (r598)
  | 877 | 930 -> One (r599)
  | 738 | 871 -> One (r600)
  | 874 | 929 -> One (r601)
  | 873 | 928 -> One (r602)
  | 739 | 872 -> One (r603)
  | 927 -> One (r604)
  | 743 -> One (r605)
  | 745 -> One (r606)
  | 747 -> One (r607)
  | 749 -> One (r608)
  | 856 | 900 -> One (r609)
  | 855 | 899 -> One (r610)
  | 854 | 898 -> One (r611)
  | 750 | 887 -> One (r612)
  | 753 | 890 -> One (r613)
  | 752 | 889 -> One (r614)
  | 751 | 888 -> One (r615)
  | 850 -> One (r616)
  | 763 -> One (r617)
  | 762 -> One (r618)
  | 767 -> One (r619)
  | 766 -> One (r620)
  | 770 -> One (r621)
  | 772 -> One (r622)
  | 777 -> One (r623)
  | 781 -> One (r624)
  | 780 -> One (r625)
  | 784 -> One (r626)
  | 786 -> One (r627)
  | 788 -> One (r628)
  | 790 -> One (r629)
  | 792 -> One (r630)
  | 794 -> One (r631)
  | 796 -> One (r632)
  | 798 -> One (r633)
  | 800 -> One (r634)
  | 802 -> One (r635)
  | 804 -> One (r636)
  | 806 -> One (r637)
  | 808 -> One (r638)
  | 810 -> One (r639)
  | 812 -> One (r640)
  | 814 -> One (r641)
  | 816 -> One (r642)
  | 818 -> One (r643)
  | 820 -> One (r644)
  | 822 -> One (r645)
  | 847 -> One (r646)
  | 846 -> One (r647)
  | 824 -> One (r648)
  | 829 -> One (r649)
  | 828 -> One (r650)
  | 827 -> One (r651)
  | 834 -> One (r652)
  | 836 -> One (r653)
  | 838 -> One (r654)
  | 840 -> One (r655)
  | 845 -> One (r656)
  | 853 | 895 -> One (r657)
  | 852 | 894 -> One (r658)
  | 851 | 893 -> One (r659)
  | 864 | 912 -> One (r660)
  | 863 | 911 -> One (r661)
  | 862 | 910 -> One (r662)
  | 858 | 904 -> One (r663)
  | 861 | 907 -> One (r664)
  | 860 | 906 -> One (r665)
  | 859 | 905 -> One (r666)
  | 867 | 917 -> One (r667)
  | 866 | 916 -> One (r668)
  | 865 | 915 -> One (r669)
  | 870 -> One (r670)
  | 876 -> One (r671)
  | 881 -> One (r672)
  | 884 | 935 -> One (r673)
  | 883 | 934 -> One (r674)
  | 882 | 933 -> One (r675)
  | 886 -> One (r676)
  | 892 -> One (r677)
  | 897 -> One (r678)
  | 902 -> One (r679)
  | 909 -> One (r680)
  | 914 -> One (r681)
  | 919 -> One (r682)
  | 922 -> One (r683)
  | 944 -> One (r684)
  | 948 -> One (r685)
  | 950 -> One (r686)
  | 952 -> One (r687)
  | 957 -> One (r688)
  | 959 -> One (r689)
  | 962 -> One (r691)
  | 961 -> One (r692)
  | 981 -> One (r693)
  | 980 -> One (r694)
  | 966 -> One (r695)
  | 965 -> One (r696)
  | 969 -> One (r697)
  | 971 -> One (r698)
  | 970 | 1111 -> One (r699)
  | 973 -> One (r700)
  | 1004 -> One (r701)
  | 1003 -> One (r702)
  | 1002 -> One (r703)
  | 1001 -> One (r704)
  | 1000 -> One (r705)
  | 999 -> One (r706)
  | 1017 -> One (r707)
  | 1009 -> One (r708)
  | 1008 -> One (r709)
  | 1013 -> One (r710)
  | 1012 -> One (r711)
  | 1011 -> One (r712)
  | 1016 -> One (r713)
  | 1023 -> One (r714)
  | 1022 -> One (r715)
  | 1036 -> One (r716)
  | 1042 -> One (r717)
  | 1045 -> One (r718)
  | 1058 -> One (r719)
  | 1063 -> One (r720)
  | 1075 -> One (r721)
  | 1074 -> One (r722)
  | 1082 -> One (r724)
  | 1081 -> One (r725)
  | 1080 -> One (r726)
  | 1073 -> One (r727)
  | 1072 -> One (r728)
  | 1071 -> One (r729)
  | 1079 -> One (r730)
  | 1078 -> One (r731)
  | 1077 -> One (r732)
  | 1084 -> One (r733)
  | 1092 -> One (r734)
  | 1091 -> One (r735)
  | 1090 -> One (r736)
  | 1089 -> One (r737)
  | 1088 -> One (r738)
  | 1087 -> One (r739)
  | 1086 -> One (r740)
  | 1101 -> One (r741)
  | 1100 -> One (r742)
  | 1104 -> One (r743)
  | 1117 -> One (r744)
  | 1116 -> One (r745)
  | 1115 -> One (r746)
  | 1114 -> One (r747)
  | 1113 -> One (r748)
  | 1122 -> One (r749)
  | 1121 -> One (r750)
  | 1120 -> One (r751)
  | 1119 -> One (r752)
  | 1125 -> One (r753)
  | 1124 -> One (r754)
  | 1140 -> One (r755)
  | 1139 -> One (r756)
  | 1143 -> One (r757)
  | 1142 -> One (r758)
  | 1146 -> One (r759)
  | 1145 -> One (r760)
  | 1154 -> One (r761)
  | 1153 -> One (r762)
  | 1180 -> One (r763)
  | 1179 -> One (r764)
  | 1178 -> One (r765)
  | 1177 -> One (r766)
  | 1168 -> One (r767)
  | 1167 -> One (r769)
  | 1166 -> One (r770)
  | 1162 -> One (r771)
  | 1161 -> One (r772)
  | 1160 -> One (r773)
  | 1159 -> One (r774)
  | 1158 -> One (r775)
  | 1165 -> One (r776)
  | 1164 -> One (r777)
  | 1176 -> One (r778)
  | 1175 -> One (r779)
  | 1174 -> One (r780)
  | 1183 -> One (r781)
  | 1182 -> One (r782)
  | 1227 -> One (r783)
  | 1216 -> One (r784)
  | 1215 -> One (r785)
  | 1206 -> One (r786)
  | 1205 -> One (r788)
  | 1204 -> One (r789)
  | 1203 -> One (r790)
  | 1188 -> One (r791)
  | 1187 -> One (r792)
  | 1186 -> One (r793)
  | 1202 -> One (r794)
  | 1201 -> One (r796)
  | 1200 -> One (r797)
  | 1199 -> One (r798)
  | 1195 -> One (r799)
  | 1194 -> One (r800)
  | 1193 -> One (r801)
  | 1192 -> One (r802)
  | 1191 -> One (r803)
  | 1198 -> One (r804)
  | 1197 -> One (r805)
  | 1214 -> One (r806)
  | 1213 -> One (r807)
  | 1212 -> One (r808)
  | 1226 -> One (r809)
  | 1225 -> One (r810)
  | 1224 -> One (r811)
  | 1223 -> One (r812)
  | 1222 -> One (r813)
  | 1221 -> One (r814)
  | 1220 -> One (r815)
  | 1219 -> One (r816)
  | 1636 -> One (r817)
  | 1635 -> One (r818)
  | 1229 -> One (r819)
  | 1231 -> One (r820)
  | 1233 -> One (r821)
  | 1247 -> One (r822)
  | 1246 -> One (r823)
  | 1245 -> One (r824)
  | 1244 -> One (r825)
  | 1243 -> One (r826)
  | 1242 -> One (r827)
  | 1241 -> One (r828)
  | 1238 -> One (r829)
  | 1237 -> One (r830)
  | 1236 -> One (r831)
  | 1240 -> One (r832)
  | 1258 -> One (r833)
  | 1251 -> One (r834)
  | 1250 -> One (r835)
  | 1249 -> One (r836)
  | 1257 -> One (r837)
  | 1256 -> One (r838)
  | 1255 -> One (r839)
  | 1254 -> One (r840)
  | 1253 -> One (r841)
  | 1631 -> One (r842)
  | 1630 -> One (r843)
  | 1260 -> One (r844)
  | 1265 -> One (r845)
  | 1264 -> One (r846)
  | 1263 -> One (r847)
  | 1262 -> One (r848)
  | 1273 -> One (r849)
  | 1276 -> One (r851)
  | 1275 -> One (r852)
  | 1272 -> One (r853)
  | 1271 -> One (r854)
  | 1270 -> One (r855)
  | 1269 -> One (r856)
  | 1268 -> One (r857)
  | 1267 -> One (r858)
  | 1284 -> One (r859)
  | 1283 -> One (r860)
  | 1282 -> One (r861)
  | 1281 -> One (r862)
  | 1287 -> One (r866)
  | 1286 -> One (r867)
  | 1285 -> One (r868)
  | 1337 -> One (r869)
  | 1336 -> One (r870)
  | 1335 -> One (r871)
  | 1334 -> One (r872)
  | 1504 -> One (r873)
  | 1503 -> One (r874)
  | 1299 -> One (r875)
  | 1298 -> One (r876)
  | 1297 -> One (r877)
  | 1296 -> One (r878)
  | 1295 -> One (r879)
  | 1294 -> One (r880)
  | 1293 -> One (r881)
  | 1292 -> One (r882)
  | 1311 -> One (r883)
  | 1310 -> One (r884)
  | 1313 -> One (r886)
  | 1312 -> One (r887)
  | 1303 -> One (r888)
  | 1302 -> One (r889)
  | 1301 -> One (r890)
  | 1306 -> One (r891)
  | 1305 -> One (r892)
  | 1309 -> One (r893)
  | 1308 -> One (r894)
  | 1315 -> One (r895)
  | 1328 -> One (r896)
  | 1320 -> One (r897)
  | 1319 -> One (r898)
  | 1318 -> One (r899)
  | 1317 -> One (r900)
  | 1327 -> One (r901)
  | 1326 -> One (r902)
  | 1325 -> One (r903)
  | 1324 -> One (r904)
  | 1323 -> One (r905)
  | 1322 -> One (r906)
  | 1333 -> One (r907)
  | 1332 -> One (r908)
  | 1331 -> One (r909)
  | 1330 -> One (r910)
  | 1380 -> One (r911)
  | 1398 -> One (r913)
  | 1455 -> One (r915)
  | 1469 -> One (r917)
  | 1459 -> One (r918)
  | 1458 -> One (r919)
  | 1438 -> One (r920)
  | 1437 -> One (r921)
  | 1436 -> One (r922)
  | 1435 -> One (r923)
  | 1434 -> One (r924)
  | 1433 -> One (r925)
  | 1432 -> One (r926)
  | 1422 -> One (r927)
  | 1421 -> One (r928)
  | 1349 -> One (r929)
  | 1348 -> One (r930)
  | 1347 -> One (r931)
  | 1343 -> One (r932)
  | 1341 -> One (r933)
  | 1340 -> One (r934)
  | 1339 -> One (r935)
  | 1346 -> One (r936)
  | 1345 -> One (r937)
  | 1415 -> One (r938)
  | 1414 -> One (r939)
  | 1355 -> One (r940)
  | 1351 -> One (r941)
  | 1354 -> One (r942)
  | 1353 -> One (r943)
  | 1366 -> One (r944)
  | 1365 -> One (r945)
  | 1364 -> One (r946)
  | 1363 -> One (r947)
  | 1362 -> One (r948)
  | 1357 -> One (r949)
  | 1377 -> One (r950)
  | 1376 -> One (r951)
  | 1375 -> One (r952)
  | 1374 -> One (r953)
  | 1373 -> One (r954)
  | 1368 -> One (r955)
  | 1406 -> One (r956)
  | 1405 -> One (r957)
  | 1379 -> One (r958)
  | 1404 -> One (r959)
  | 1403 -> One (r960)
  | 1402 -> One (r961)
  | 1401 -> One (r962)
  | 1382 -> One (r963)
  | 1399 -> One (r964)
  | 1386 -> One (r965)
  | 1385 -> One (r966)
  | 1384 -> One (r967)
  | 1396 | 1444 -> One (r968)
  | 1393 -> One (r970)
  | 1389 -> One (r971)
  | 1388 -> One (r972)
  | 1387 | 1443 -> One (r973)
  | 1392 | 1452 -> One (r974)
  | 1391 | 1451 -> One (r975)
  | 1390 | 1450 -> One (r976)
  | 1395 -> One (r977)
  | 1411 -> One (r978)
  | 1410 -> One (r979)
  | 1409 -> One (r980)
  | 1413 -> One (r982)
  | 1412 -> One (r983)
  | 1408 -> One (r984)
  | 1417 -> One (r985)
  | 1420 -> One (r986)
  | 1431 -> One (r987)
  | 1430 -> One (r988)
  | 1429 -> One (r989)
  | 1428 -> One (r990)
  | 1427 -> One (r991)
  | 1426 -> One (r992)
  | 1425 -> One (r993)
  | 1424 -> One (r994)
  | 1457 -> One (r995)
  | 1442 -> One (r996)
  | 1441 -> One (r997)
  | 1440 -> One (r998)
  | 1456 -> One (r999)
  | 1446 -> One (r1000)
  | 1454 -> One (r1001)
  | 1449 -> One (r1002)
  | 1448 -> One (r1003)
  | 1468 -> One (r1004)
  | 1467 -> One (r1005)
  | 1466 -> One (r1006)
  | 1465 -> One (r1007)
  | 1464 -> One (r1008)
  | 1463 -> One (r1009)
  | 1462 -> One (r1010)
  | 1461 -> One (r1011)
  | 1477 -> One (r1012)
  | 1479 -> One (r1013)
  | 1489 -> One (r1014)
  | 1488 -> One (r1015)
  | 1487 -> One (r1016)
  | 1486 -> One (r1017)
  | 1485 -> One (r1018)
  | 1484 -> One (r1019)
  | 1483 -> One (r1020)
  | 1482 -> One (r1021)
  | 1500 -> One (r1022)
  | 1499 -> One (r1023)
  | 1498 -> One (r1024)
  | 1497 -> One (r1025)
  | 1496 -> One (r1026)
  | 1495 -> One (r1027)
  | 1494 -> One (r1028)
  | 1493 -> One (r1029)
  | 1492 -> One (r1030)
  | 1550 -> One (r1031)
  | 1548 -> One (r1033)
  | 1591 -> One (r1035)
  | 1513 -> One (r1036)
  | 1608 -> One (r1038)
  | 1599 -> One (r1039)
  | 1598 -> One (r1040)
  | 1512 -> One (r1041)
  | 1511 -> One (r1042)
  | 1510 -> One (r1043)
  | 1509 -> One (r1044)
  | 1508 -> One (r1045)
  | 1585 -> One (r1046)
  | 1584 -> One (r1047)
  | 1516 -> One (r1048)
  | 1515 -> One (r1049)
  | 1520 -> One (r1050)
  | 1519 -> One (r1051)
  | 1518 -> One (r1052)
  | 1579 -> One (r1053)
  | 1578 -> One (r1054)
  | 1577 -> One (r1055)
  | 1576 -> One (r1056)
  | 1575 -> One (r1057)
  | 1574 -> One (r1058)
  | 1571 -> One (r1059)
  | 1523 -> One (r1060)
  | 1567 -> One (r1061)
  | 1566 -> One (r1062)
  | 1561 -> One (r1063)
  | 1560 -> One (r1064)
  | 1559 -> One (r1065)
  | 1558 -> One (r1066)
  | 1532 -> One (r1067)
  | 1531 -> One (r1068)
  | 1530 -> One (r1069)
  | 1529 -> One (r1070)
  | 1528 -> One (r1071)
  | 1527 -> One (r1072)
  | 1557 -> One (r1073)
  | 1536 -> One (r1074)
  | 1535 -> One (r1075)
  | 1534 -> One (r1076)
  | 1540 -> One (r1077)
  | 1539 -> One (r1078)
  | 1538 -> One (r1079)
  | 1554 -> One (r1080)
  | 1544 -> One (r1081)
  | 1543 -> One (r1082)
  | 1556 -> One (r1084)
  | 1542 -> One (r1085)
  | 1551 -> One (r1086)
  | 1546 -> One (r1087)
  | 1565 -> One (r1088)
  | 1564 -> One (r1089)
  | 1563 -> One (r1090)
  | 1570 -> One (r1091)
  | 1569 -> One (r1092)
  | 1573 -> One (r1093)
  | 1583 -> One (r1094)
  | 1582 -> One (r1095)
  | 1581 -> One (r1096)
  | 1587 -> One (r1097)
  | 1590 -> One (r1098)
  | 1595 -> One (r1099)
  | 1594 -> One (r1100)
  | 1593 -> One (r1101)
  | 1597 -> One (r1102)
  | 1607 -> One (r1103)
  | 1606 -> One (r1104)
  | 1605 -> One (r1105)
  | 1604 -> One (r1106)
  | 1603 -> One (r1107)
  | 1602 -> One (r1108)
  | 1601 -> One (r1109)
  | 1618 -> One (r1110)
  | 1621 -> One (r1111)
  | 1623 -> One (r1112)
  | 1629 -> One (r1113)
  | 1628 -> One (r1114)
  | 1649 -> One (r1115)
  | 1648 -> One (r1116)
  | 1667 -> One (r1117)
  | 1666 -> One (r1118)
  | 1665 -> One (r1119)
  | 1686 -> One (r1120)
  | 1685 -> One (r1121)
  | 1684 -> One (r1122)
  | 1683 -> One (r1123)
  | 1689 -> One (r1124)
  | 1688 -> One (r1125)
  | 1693 -> One (r1126)
  | 1699 -> One (r1127)
  | 1701 -> One (r1128)
  | 1703 -> One (r1129)
  | 1716 -> One (r1130)
  | 1720 -> One (r1131)
  | 1725 -> One (r1132)
  | 1728 -> One (r1133)
  | 1732 -> One (r1134)
  | 1736 -> One (r1135)
  | 1747 -> One (r1136)
  | 1749 -> One (r1137)
  | 1752 -> One (r1138)
  | 1751 -> One (r1139)
  | 1754 -> One (r1140)
  | 1764 -> One (r1141)
  | 1760 -> One (r1142)
  | 1759 -> One (r1143)
  | 1763 -> One (r1144)
  | 1762 -> One (r1145)
  | 1769 -> One (r1146)
  | 1768 -> One (r1147)
  | 1767 -> One (r1148)
  | 1771 -> One (r1149)
  | 507 -> Select (function
    | -1 -> [R 98]
    | _ -> S (T T_DOT) :: r400)
  | 712 -> Select (function
    | -1 -> [R 98]
    | _ -> r583)
  | 152 -> Select (function
    | -1 -> r109
    | _ -> R 182 :: r127)
  | 368 -> Select (function
    | -1 -> r109
    | _ -> R 182 :: r283)
  | 1277 -> Select (function
    | -1 -> r872
    | _ -> R 182 :: r865)
  | 1507 -> Select (function
    | -1 -> S (T T_TYPE) :: r935
    | _ -> R 182 :: r1045)
  | 525 -> Select (function
    | -1 -> [R 631]
    | _ -> r356)
  | 522 -> Select (function
    | -1 -> [R 632]
    | _ -> S (N N_pattern) :: r407)
  | 156 -> Select (function
    | -1 -> r133
    | _ -> R 740 :: r139)
  | 371 -> Select (function
    | -1 -> r133
    | _ -> R 740 :: r289)
  | 446 -> Select (function
    | 475 | 568 | 727 | 824 | 943 | 1095 | 1529 | 1563 -> r80
    | -1 -> S (T T_RPAREN) :: r54
    | _ -> S (N N_pattern) :: r328)
  | 86 -> Select (function
    | -1 -> S (T T_RPAREN) :: r54
    | _ -> Sub (r1) :: r53)
  | 477 -> Select (function
    | -1 -> S (T T_RBRACKET) :: r299
    | _ -> Sub (r371) :: r373)
  | 666 -> Select (function
    | -1 -> S (T T_RBRACKET) :: r299
    | _ -> Sub (r511) :: r513)
  | 590 -> Select (function
    | 60 | 92 | 367 | 434 | 1229 | 1260 -> r466
    | _ -> S (T T_OPEN) :: r460)
  | 177 -> Select (function
    | -1 -> r110
    | _ -> S (T T_COLON) :: r159)
  | 183 -> Select (function
    | 1111 -> r89
    | _ -> Sub (r87) :: r166)
  | 184 -> Select (function
    | 1111 -> r88
    | _ -> r166)
  | 417 -> Select (function
    | -1 -> r105
    | _ -> r110)
  | 1663 -> Select (function
    | -1 -> r105
    | _ -> r110)
  | 1662 -> Select (function
    | -1 -> r106
    | _ -> r125)
  | 416 -> Select (function
    | -1 -> r106
    | _ -> r281)
  | 154 -> Select (function
    | -1 -> r107
    | _ -> r126)
  | 370 -> Select (function
    | -1 -> r107
    | _ -> r282)
  | 153 -> Select (function
    | -1 -> r108
    | _ -> r127)
  | 369 -> Select (function
    | -1 -> r108
    | _ -> r283)
  | 373 -> Select (function
    | -1 -> r131
    | _ -> r110)
  | 170 -> Select (function
    | -1 -> r131
    | _ -> r110)
  | 169 -> Select (function
    | -1 -> r132
    | _ -> r139)
  | 372 -> Select (function
    | -1 -> r132
    | _ -> r289)
  | 1280 -> Select (function
    | -1 -> r869
    | _ -> r863)
  | 1279 -> Select (function
    | -1 -> r870
    | _ -> r864)
  | 1278 -> Select (function
    | -1 -> r871
    | _ -> r865)
  | _ -> raise Not_found
