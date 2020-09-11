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
    | MenhirInterpreter.T MenhirInterpreter.T_SHARP -> ()
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
    | MenhirInterpreter.T MenhirInterpreter.T_AND -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_AMPERSAND -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_AMPERAMPER -> ()
    | MenhirInterpreter.N MenhirInterpreter.N_with_type_binder -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_with_constraints -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_with_constraint -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_virtual_flag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_value_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_value_description -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_value -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_val_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_val_ident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_typevar_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_variance -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_variable -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_parameters -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_parameter_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_parameter -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_kind -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_declarations -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_constraint -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_toplevel_directive -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_tag_field -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_subtractive -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_structure_tail -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_structure_item -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_structure -> []
    | MenhirInterpreter.N MenhirInterpreter.N_strict_binding -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_str_type_extension -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_str_include_statement -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_str_extension_constructors -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_str_exception_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_single_attr_id -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_simple_pattern_not_ident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_simple_pattern -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_simple_labeled_expr_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_simple_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_simple_delimited_pattern -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_simple_core_type_or_tuple -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_simple_core_type2 -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_simple_core_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_signed_constant -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_signature_item -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_signature -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_sig_type_extension -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_sig_include_statement -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_sig_extension_constructors -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_sig_exception_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_seq_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_row_field_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_row_field -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_record_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_rec_module_declarations -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_rec_module_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_rec_module_bindings -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_rec_module_binding -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_rec_flag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_private_virtual_flags -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_private_flag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_primitive_declaration_body -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_primitive_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_post_item_attributes -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_post_item_attribute -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_poly_type_no_attr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_poly_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_payload -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_pattern_var -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_pattern_semi_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_pattern_no_exn_comma_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_pattern_no_exn -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_pattern_gen -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_pattern_comma_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_pattern -> default_pattern ()
    | MenhirInterpreter.N MenhirInterpreter.N_parse_expression -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_parent_binder -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_paren_module_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_package_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_override_flag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_optional_type_variable -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_optional_type_parameters -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_optional_type_parameter_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_optional_type_parameter -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_opt_type_constraint -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_opt_semi -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_opt_pattern_type_constraint -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_opt_default -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_opt_bar -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_opt_ampersand -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_operator -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_open_statement -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_nonrec_flag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_name_tag_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_name_tag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mutable_flag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mty_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_module_type_declaration_body -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_module_type_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_module_type -> default_module_type ()
    | MenhirInterpreter.N MenhirInterpreter.N_module_expr -> default_module_expr ()
    | MenhirInterpreter.N MenhirInterpreter.N_module_declaration_body -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_module_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_module_binding_body -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_module_binding -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_module_alias -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mod_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mod_ext_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_method_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_meth_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_match_cases -> []
    | MenhirInterpreter.N MenhirInterpreter.N_match_case -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_lwt_bindings -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_lwt_binding -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_lident_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_let_pattern -> default_pattern ()
    | MenhirInterpreter.N MenhirInterpreter.N_let_exception_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_let_bindings -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_let_binding_body -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_let_binding -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_lbl_pattern_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_lbl_pattern -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_lbl_expr_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_lbl_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_labeled_simple_pattern -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_labeled_simple_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_label_var -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_label_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_label_let_pattern -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_label_ident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_label_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_label_declarations -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_label_declaration_semi -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_label_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_label -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_item_extension -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_interface -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_implementation -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_ident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_generalized_constructor_arguments -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_functor_args -> []
    | MenhirInterpreter.N MenhirInterpreter.N_functor_arg_name -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_functor_arg -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_fun_def -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_fun_binding -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_floating_attribute -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_field_semi -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_field_expr_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_field_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_field -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_extension_constructor_rebind -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_extension_constructor_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_extension -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_ext_attributes -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_expr_semi_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_expr_comma_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_expr -> default_expr ()
    | MenhirInterpreter.N MenhirInterpreter.N_direction_flag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_core_type_no_attr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_core_type_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_core_type_comma_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_core_type2 -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_core_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constructor_declarations -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constructor_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constructor_arguments -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constraints -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constrain_field -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constrain -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constr_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constr_ident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constant -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_clty_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_type_parameters -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_type_declarations -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_type_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_structure -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_simple_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_signature -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_sig_fields -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_sig_field -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_sig_body -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_self_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_self_pattern -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_fun_def -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_fun_binding -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_fields -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_field -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_descriptions -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_description -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_declarations -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_bar_extension_constructor_rebind -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_bar_extension_constructor_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_bar_constructor_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_attributes -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_attribute -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_attr_id -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_and_type_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_and_module_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_and_module_binding -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_and_let_binding -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_and_class_type_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_and_class_description -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_and_class_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_amper_type_list -> raise Not_found
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
  [|0;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;2;3;2;1;2;1;2;1;1;2;1;2;1;1;1;1;1;2;1;1;2;3;3;3;1;2;1;2;1;1;2;1;1;1;2;1;2;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;2;3;3;4;1;1;1;2;1;1;1;2;1;2;3;1;1;2;3;1;1;2;1;2;1;3;1;1;1;1;1;1;2;2;2;3;2;3;1;4;5;1;1;1;2;1;2;1;2;1;1;2;1;2;3;1;2;1;2;1;1;2;1;1;2;2;1;2;1;2;1;1;1;2;3;2;1;2;3;4;2;3;2;3;1;3;4;2;3;1;2;1;3;1;1;2;1;1;3;2;3;1;1;2;3;2;3;4;4;2;3;5;2;1;2;3;2;3;4;5;3;4;3;4;4;5;6;2;1;1;2;3;2;3;3;4;5;6;1;1;2;1;3;4;5;2;3;1;2;3;4;5;4;2;3;2;1;1;2;1;1;1;1;1;2;1;1;1;1;2;3;4;5;6;7;2;3;1;2;1;1;2;1;2;2;3;4;3;4;3;3;2;1;1;2;3;1;2;2;3;4;5;2;3;1;4;4;5;6;7;5;2;6;7;1;2;1;2;3;4;5;6;7;1;2;3;1;1;2;1;1;2;4;5;3;4;8;9;1;2;2;2;1;1;1;2;3;4;2;3;1;1;1;1;2;3;3;3;3;3;1;3;2;3;1;1;1;1;1;2;3;4;5;1;2;1;1;1;2;1;2;2;1;2;2;1;1;2;3;4;5;5;1;2;3;4;1;2;1;2;3;4;1;1;1;2;1;1;1;2;2;3;1;4;2;1;1;1;1;2;2;2;3;2;3;1;2;1;1;2;3;3;1;2;4;5;6;2;1;2;3;3;3;4;5;1;1;2;3;4;2;3;4;3;2;3;1;5;2;3;2;1;2;3;3;1;1;3;4;5;2;1;2;3;2;5;6;2;3;1;1;2;3;1;1;1;2;1;2;1;1;1;2;3;1;2;3;4;1;5;2;3;2;3;3;4;5;2;2;1;1;4;5;5;6;7;1;1;1;1;1;2;1;3;1;1;1;1;2;3;1;2;3;1;4;3;1;1;2;2;3;1;2;1;1;1;1;1;2;1;1;1;1;1;1;2;3;1;1;1;2;3;2;3;2;1;2;1;2;3;4;4;5;2;3;1;1;2;2;3;2;3;3;4;2;2;3;3;4;1;3;3;2;3;3;4;5;3;1;1;4;2;2;3;4;1;1;2;1;1;2;3;3;4;2;1;2;3;1;1;2;3;4;5;6;1;1;1;2;1;2;1;1;1;1;1;2;1;2;1;2;1;2;3;1;2;1;2;1;1;2;1;2;3;3;4;5;1;2;3;4;1;2;5;6;1;1;2;3;4;1;2;3;4;1;1;2;1;1;2;3;4;4;5;6;7;8;9;1;1;1;1;1;2;3;2;3;2;3;2;3;1;1;1;1;2;1;2;1;2;1;2;2;3;4;5;6;1;2;1;2;3;1;1;2;3;1;1;2;1;2;3;2;3;2;3;2;1;2;1;2;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;3;2;3;2;3;1;2;3;4;5;3;1;2;1;2;3;4;5;1;2;3;1;2;3;2;3;2;3;2;3;2;3;2;1;3;4;2;2;7;3;4;5;6;7;3;4;5;6;7;3;4;5;2;1;2;1;1;2;4;5;3;4;5;3;4;5;3;4;5;3;4;4;2;3;5;6;1;3;4;4;5;6;3;4;5;1;2;3;2;3;1;2;3;1;2;3;1;1;2;1;2;3;4;1;2;3;4;4;5;1;2;3;1;5;1;1;6;7;8;9;10;5;6;7;8;4;5;6;7;8;9;10;2;1;2;3;4;1;2;1;2;3;4;1;1;2;5;7;3;4;3;4;5;2;3;3;4;2;3;1;3;4;5;6;7;3;3;4;3;2;3;2;2;3;3;2;3;2;3;4;2;2;3;4;7;2;3;4;1;2;3;4;5;6;7;1;2;2;3;4;5;6;1;2;3;2;3;4;5;2;4;5;2;1;2;3;4;1;2;1;2;3;1;1;2;5;2;3;4;5;6;7;8;3;4;5;6;7;2;4;5;2;3;4;5;6;7;8;2;3;3;4;3;4;5;6;3;4;5;6;4;5;5;6;7;5;6;7;7;8;9;2;4;5;3;4;5;6;1;2;1;2;3;4;1;2;1;2;3;4;1;2;3;4;5;6;1;2;7;8;1;2;3;4;5;6;7;4;5;6;1;1;1;2;3;1;2;3;4;5;1;2;6;2;3;4;5;6;4;5;3;4;5;6;7;1;2;3;4;1;2;3;1;2;3;1;4;1;2;3;5;6;7;1;2;1;2;3;3;4;1;2;1;2;1;2;3;4;5;1;2;3;4;5;3;4;1;2;3;1;2;1;2;1;2;3;4;5;1;2;3;6;7;1;1;2;1;1;2;3;1;2;3;4;1;1;3;4;2;1;2;1;2;3;3;4;1;2;1;2;8;9;2;3;4;5;6;1;2;3;4;5;1;2;3;1;2;3;4;1;1;1;2;1;2;3;1;3;1;5;4;6;7;8;1;1;1;2;3;4;5;6;7;2;1;1;2;1;1;1;1;1;2;3;4;5;6;2;1;1;1;1;1;1;1;2;1;1;1;2;3;4;5;6;7;8;2;1;1;1;2;3;4;5;6;7;8;2;1;2;2;2;1;2;3;4;5;1;1;2;3;4;1;2;1;2;3;1;2;3;4;5;6;7;3;4;5;6;7;3;4;5;6;5;6;7;1;1;2;3;4;5;6;7;3;4;5;6;7;3;4;5;6;7;8;9;10;11;6;7;8;5;2;3;1;2;3;4;1;2;3;1;1;1;2;3;4;1;2;1;2;1;2;1;1;2;1;3;2;3;2;3;2;3;3;4;5;4;1;2;5;6;1;2;3;4;1;2;1;2;2;1;2;3;4;1;2;6;7;1;1;1;1;1;2;1;1;1;1;1;1;2;3;4;5;2;1;1;1;1;1;1;1;1;2;1;1;1;1;2;3;4;5;6;7;2;1;2;1;2;3;1;1;1;3;4;3;4;3;4;5;6;7;2;3;4;5;6;7;8;2;3;3;4;5;3;4;2;3;4;8;5;6;7;1;2;8;9;2;1;1;1;3;4;4;5;2;3;4;4;5;6;5;6;3;4;2;3;4;5;6;3;4;5;6;7;8;4;5;3;4;5;6;3;4;3;2;3;4;5;6;1;2;1;0;1;2;1;0;1;2;1;|]

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
  | T_SHARP -> true
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
  let r0 = [R 453] in
  let r1 = S (N N_expr) :: r0 in
  let r2 = [R 133] in
  let r3 = S (T T_DONE) :: r2 in
  let r4 = Sub (r1) :: r3 in
  let r5 = S (T T_DO) :: r4 in
  let r6 = Sub (r1) :: r5 in
  let r7 = [R 573] in
  let r8 = S (T T_AND) :: r7 in
  let r9 = [R 14] in
  let r10 = Sub (r8) :: r9 in
  let r11 = [R 193] in
  let r12 = R 17 :: r11 in
  let r13 = [R 15] in
  let r14 = [R 417] in
  let r15 = S (N N_structure) :: r14 in
  let r16 = [R 16] in
  let r17 = S (T T_RBRACKET) :: r16 in
  let r18 = Sub (r15) :: r17 in
  let r19 = [R 152] in
  let r20 = S (T T_DONE) :: r19 in
  let r21 = Sub (r1) :: r20 in
  let r22 = S (T T_DO) :: r21 in
  let r23 = Sub (r1) :: r22 in
  let r24 = [R 295] in
  let r25 = [R 129] in
  let r26 = Sub (r1) :: r25 in
  let r27 = [R 146] in
  let r28 = S (N N_match_cases) :: r27 in
  let r29 = R 361 :: r28 in
  let r30 = S (T T_WITH) :: r29 in
  let r31 = Sub (r1) :: r30 in
  let r32 = [R 550] in
  let r33 = S (T T_QUESTIONQUESTION) :: r32 in
  let r34 = [R 538] in
  let r35 = [R 48] in
  let r36 = S (T T_LIDENT) :: r35 in
  let r37 = [R 540] in
  let r38 = Sub (r36) :: r37 in
  let r39 = [R 49] in
  let r40 = S (T T_LIDENT) :: r39 in
  let r41 = [R 296] in
  let r42 = [R 192] in
  let r43 = [R 18] in
  let r44 = [R 519] in
  let r45 = S (T T_RPAREN) :: r44 in
  let r46 = Sub (r1) :: r45 in
  let r47 = [R 99] in
  let r48 = [R 698] in
  let r49 = [R 194] in
  let r50 = S (T T_RBRACKET) :: r49 in
  let r51 = Sub (r15) :: r50 in
  let r52 = S (T T_LIDENT) :: r48 in
  let r53 = [R 489] in
  let r54 = S (T T_UNDERSCORE) :: r53 in
  let r55 = [R 486] in
  let r56 = Sub (r54) :: r55 in
  let r57 = [R 507] in
  let r58 = Sub (r56) :: r57 in
  let r59 = [R 114] in
  let r60 = Sub (r58) :: r59 in
  let r61 = [R 123] in
  let r62 = Sub (r60) :: r61 in
  let r63 = [R 112] in
  let r64 = Sub (r62) :: r63 in
  let r65 = [R 706] in
  let r66 = R 427 :: r65 in
  let r67 = Sub (r64) :: r66 in
  let r68 = S (T T_COLON) :: r67 in
  let r69 = Sub (r52) :: r68 in
  let r70 = [R 355] in
  let r71 = S (T T_AMPERAMPER) :: r70 in
  let r72 = [R 699] in
  let r73 = S (T T_RPAREN) :: r72 in
  let r74 = [R 292] in
  let r75 = [R 495] in
  let r76 = [R 221] in
  let r77 = S (T T_LIDENT) :: r76 in
  let r78 = [R 488] in
  let r79 = Sub (r77) :: r78 in
  let r80 = [R 115] in
  let r81 = Sub (r60) :: r80 in
  let r82 = S (T T_MINUSGREATER) :: r81 in
  let r83 = Sub (r60) :: r82 in
  let r84 = S (T T_COLON) :: r83 in
  let r85 = [R 116] in
  let r86 = Sub (r60) :: r85 in
  let r87 = S (T T_MINUSGREATER) :: r86 in
  let r88 = [R 381] in
  let r89 = S (N N_module_type) :: r88 in
  let r90 = [R 505] in
  let r91 = S (T T_RPAREN) :: r90 in
  let r92 = Sub (r89) :: r91 in
  let r93 = R 191 :: r92 in
  let r94 = [R 315] in
  let r95 = S (T T_END) :: r94 in
  let r96 = R 463 :: r95 in
  let r97 = [R 671] in
  let r98 = R 427 :: r97 in
  let r99 = R 105 :: r98 in
  let r100 = R 674 :: r99 in
  let r101 = S (T T_LIDENT) :: r100 in
  let r102 = R 374 :: r101 in
  let r103 = R 333 :: r102 in
  let r104 = R 191 :: r103 in
  let r105 = [R 378] in
  let r106 = S (T T_UNDERSCORE) :: r105 in
  let r107 = [R 371] in
  let r108 = Sub (r106) :: r107 in
  let r109 = R 693 :: r108 in
  let r110 = [R 372] in
  let r111 = Sub (r109) :: r110 in
  let r112 = [R 376] in
  let r113 = S (T T_RPAREN) :: r112 in
  let r114 = [R 377] in
  let r115 = [R 373] in
  let r116 = [R 679] in
  let r117 = [R 95] in
  let r118 = S (T T_FALSE) :: r117 in
  let r119 = [R 108] in
  let r120 = R 17 :: r119 in
  let r121 = R 216 :: r120 in
  let r122 = Sub (r118) :: r121 in
  let r123 = [R 109] in
  let r124 = Sub (r122) :: r123 in
  let r125 = [R 678] in
  let r126 = [R 93] in
  let r127 = [R 684] in
  let r128 = [R 117] in
  let r129 = Sub (r60) :: r128 in
  let r130 = S (T T_MINUSGREATER) :: r129 in
  let r131 = [R 494] in
  let r132 = [R 225] in
  let r133 = [R 493] in
  let r134 = [R 424] in
  let r135 = Sub (r62) :: r134 in
  let r136 = [R 202] in
  let r137 = R 17 :: r136 in
  let r138 = S (T T_SEMI) :: r137 in
  let r139 = R 17 :: r138 in
  let r140 = Sub (r135) :: r139 in
  let r141 = [R 696] in
  let r142 = [R 450] in
  let r143 = Sub (r56) :: r142 in
  let r144 = [R 451] in
  let r145 = Sub (r143) :: r144 in
  let r146 = [R 503] in
  let r147 = S (T T_RBRACKET) :: r146 in
  let r148 = Sub (r145) :: r147 in
  let r149 = [R 502] in
  let r150 = [R 501] in
  let r151 = S (T T_RBRACKET) :: r150 in
  let r152 = [R 499] in
  let r153 = S (T T_RBRACKET) :: r152 in
  let r154 = Sub (r145) :: r153 in
  let r155 = [R 330] in
  let r156 = Sub (r77) :: r155 in
  let r157 = [R 496] in
  let r158 = [R 685] in
  let r159 = S (T T_LIDENT) :: r158 in
  let r160 = S (T T_DOT) :: r159 in
  let r161 = S (T T_UIDENT) :: r74 in
  let r162 = [R 294] in
  let r163 = S (T T_RPAREN) :: r162 in
  let r164 = [R 293] in
  let r165 = [R 452] in
  let r166 = [R 660] in
  let r167 = [R 5] in
  let r168 = Sub (r62) :: r167 in
  let r169 = [R 659] in
  let r170 = R 17 :: r169 in
  let r171 = Sub (r168) :: r170 in
  let r172 = [R 121] in
  let r173 = Sub (r56) :: r172 in
  let r174 = [R 508] in
  let r175 = [R 122] in
  let r176 = [R 118] in
  let r177 = [R 124] in
  let r178 = Sub (r77) :: r177 in
  let r179 = [R 6] in
  let r180 = [R 498] in
  let r181 = [R 500] in
  let r182 = S (T T_RBRACKET) :: r181 in
  let r183 = Sub (r145) :: r182 in
  let r184 = S (T T_BACKQUOTE) :: r156 in
  let r185 = [R 331] in
  let r186 = Sub (r184) :: r185 in
  let r187 = [R 504] in
  let r188 = S (T T_RBRACKET) :: r187 in
  let r189 = [R 425] in
  let r190 = Sub (r62) :: r189 in
  let r191 = [R 697] in
  let r192 = [R 94] in
  let r193 = [R 487] in
  let r194 = [R 497] in
  let r195 = [R 120] in
  let r196 = [R 119] in
  let r197 = [R 92] in
  let r198 = [R 19] in
  let r199 = R 17 :: r198 in
  let r200 = R 216 :: r199 in
  let r201 = [R 106] in
  let r202 = Sub (r173) :: r201 in
  let r203 = [R 217] in
  let r204 = S (T T_LIDENT) :: r132 in
  let r205 = [R 226] in
  let r206 = R 17 :: r205 in
  let r207 = Sub (r135) :: r206 in
  let r208 = S (T T_COLON) :: r207 in
  let r209 = Sub (r204) :: r208 in
  let r210 = R 328 :: r209 in
  let r211 = [R 228] in
  let r212 = Sub (r210) :: r211 in
  let r213 = [R 107] in
  let r214 = S (T T_RBRACE) :: r213 in
  let r215 = [R 227] in
  let r216 = R 17 :: r215 in
  let r217 = S (T T_SEMI) :: r216 in
  let r218 = R 17 :: r217 in
  let r219 = Sub (r135) :: r218 in
  let r220 = S (T T_COLON) :: r219 in
  let r221 = [R 219] in
  let r222 = [R 218] in
  let r223 = Sub (r56) :: r222 in
  let r224 = [R 680] in
  let r225 = S (T T_RBRACE) :: r224 in
  let r226 = Sub (r212) :: r225 in
  let r227 = [R 682] in
  let r228 = [R 681] in
  let r229 = [R 683] in
  let r230 = S (T T_RBRACE) :: r229 in
  let r231 = [R 426] in
  let r232 = S (T T_RBRACKET) :: r231 in
  let r233 = Sub (r15) :: r232 in
  let r234 = [R 195] in
  let r235 = R 17 :: r234 in
  let r236 = R 216 :: r235 in
  let r237 = Sub (r118) :: r236 in
  let r238 = [R 624] in
  let r239 = Sub (r237) :: r238 in
  let r240 = [R 631] in
  let r241 = R 427 :: r240 in
  let r242 = Sub (r239) :: r241 in
  let r243 = R 432 :: r242 in
  let r244 = [R 20] in
  let r245 = R 17 :: r244 in
  let r246 = R 216 :: r245 in
  let r247 = Sub (r118) :: r246 in
  let r248 = [R 100] in
  let r249 = S (T T_FALSE) :: r248 in
  let r250 = [R 21] in
  let r251 = R 17 :: r250 in
  let r252 = Sub (r249) :: r251 in
  let r253 = S (T T_EQUAL) :: r252 in
  let r254 = [R 98] in
  let r255 = [R 428] in
  let r256 = [R 196] in
  let r257 = R 17 :: r256 in
  let r258 = Sub (r249) :: r257 in
  let r259 = [R 661] in
  let r260 = [R 655] in
  let r261 = S (T T_UIDENT) :: r24 in
  let r262 = [R 335] in
  let r263 = R 427 :: r262 in
  let r264 = Sub (r261) :: r263 in
  let r265 = R 191 :: r264 in
  let r266 = [R 74] in
  let r267 = R 41 :: r266 in
  let r268 = R 52 :: r267 in
  let r269 = [R 185] in
  let r270 = S (T T_END) :: r269 in
  let r271 = Sub (r268) :: r270 in
  let r272 = [R 50] in
  let r273 = S (T T_RPAREN) :: r272 in
  let r274 = [R 555] in
  let r275 = S (T T_LIDENT) :: r127 in
  let r276 = [R 560] in
  let r277 = [R 484] in
  let r278 = [R 482] in
  let r279 = [R 566] in
  let r280 = S (T T_RPAREN) :: r279 in
  let r281 = [R 568] in
  let r282 = S (T T_RPAREN) :: r281 in
  let r283 = S (T T_UIDENT) :: r282 in
  let r284 = [R 569] in
  let r285 = S (T T_RPAREN) :: r284 in
  let r286 = [R 319] in
  let r287 = S (N N_module_expr) :: r286 in
  let r288 = R 17 :: r287 in
  let r289 = S (T T_OF) :: r288 in
  let r290 = [R 307] in
  let r291 = S (T T_END) :: r290 in
  let r292 = S (N N_structure) :: r291 in
  let r293 = [R 299] in
  let r294 = S (N N_module_expr) :: r293 in
  let r295 = S (T T_EQUAL) :: r294 in
  let r296 = [R 441] in
  let r297 = R 427 :: r296 in
  let r298 = Sub (r295) :: r297 in
  let r299 = S (T T_UIDENT) :: r298 in
  let r300 = S (T T_REC) :: r299 in
  let r301 = [R 323] in
  let r302 = R 427 :: r301 in
  let r303 = R 324 :: r302 in
  let r304 = Sub (r77) :: r303 in
  let r305 = R 191 :: r304 in
  let r306 = [R 325] in
  let r307 = [R 320] in
  let r308 = S (T T_RPAREN) :: r307 in
  let r309 = [R 316] in
  let r310 = S (N N_module_type) :: r309 in
  let r311 = S (T T_MINUSGREATER) :: r310 in
  let r312 = S (N N_functor_args) :: r311 in
  let r313 = [R 210] in
  let r314 = [R 211] in
  let r315 = S (T T_RPAREN) :: r314 in
  let r316 = S (N N_module_type) :: r315 in
  let r317 = [R 715] in
  let r318 = Sub (r161) :: r317 in
  let r319 = S (T T_COLONEQUAL) :: r318 in
  let r320 = S (T T_UIDENT) :: r319 in
  let r321 = S (T T_MODULE) :: r320 in
  let r322 = [R 716] in
  let r323 = Sub (r321) :: r322 in
  let r324 = [R 318] in
  let r325 = [R 713] in
  let r326 = Sub (r62) :: r325 in
  let r327 = S (T T_COLONEQUAL) :: r326 in
  let r328 = Sub (r204) :: r327 in
  let r329 = [R 692] in
  let r330 = Sub (r77) :: r329 in
  let r331 = S (T T_QUOTE) :: r330 in
  let r332 = [R 686] in
  let r333 = Sub (r331) :: r332 in
  let r334 = R 693 :: r333 in
  let r335 = [R 687] in
  let r336 = Sub (r334) :: r335 in
  let r337 = [R 691] in
  let r338 = S (T T_RPAREN) :: r337 in
  let r339 = [R 688] in
  let r340 = [R 239] in
  let r341 = S (T T_LIDENT) :: r340 in
  let r342 = [R 718] in
  let r343 = S (T T_EQUAL) :: r342 in
  let r344 = [R 712] in
  let r345 = R 105 :: r344 in
  let r346 = Sub (r62) :: r345 in
  let r347 = [R 102] in
  let r348 = Sub (r64) :: r347 in
  let r349 = S (T T_EQUAL) :: r348 in
  let r350 = Sub (r64) :: r349 in
  let r351 = [R 104] in
  let r352 = [R 714] in
  let r353 = Sub (r161) :: r352 in
  let r354 = [R 717] in
  let r355 = [R 317] in
  let r356 = [R 327] in
  let r357 = Sub (r77) :: r356 in
  let r358 = [R 298] in
  let r359 = R 427 :: r358 in
  let r360 = Sub (r295) :: r359 in
  let r361 = [R 383] in
  let r362 = S (T T_RPAREN) :: r361 in
  let r363 = [R 384] in
  let r364 = S (T T_RPAREN) :: r363 in
  let r365 = S (N N_expr) :: r364 in
  let r366 = [R 128] in
  let r367 = S (N N_match_cases) :: r366 in
  let r368 = R 361 :: r367 in
  let r369 = S (T T_WITH) :: r368 in
  let r370 = Sub (r1) :: r369 in
  let r371 = [R 145] in
  let r372 = S (N N_match_cases) :: r371 in
  let r373 = R 361 :: r372 in
  let r374 = S (T T_WITH) :: r373 in
  let r375 = Sub (r1) :: r374 in
  let r376 = [R 546] in
  let r377 = S (T T_RPAREN) :: r376 in
  let r378 = S (N N_module_expr) :: r377 in
  let r379 = [R 308] in
  let r380 = S (N N_module_expr) :: r379 in
  let r381 = S (T T_MINUSGREATER) :: r380 in
  let r382 = S (N N_functor_args) :: r381 in
  let r383 = [R 310] in
  let r384 = [R 382] in
  let r385 = S (T T_RPAREN) :: r384 in
  let r386 = [R 547] in
  let r387 = S (T T_RPAREN) :: r386 in
  let r388 = [R 401] in
  let r389 = S (N N_pattern) :: r388 in
  let r390 = Sub (r249) :: r389 in
  let r391 = [R 410] in
  let r392 = Sub (r390) :: r391 in
  let r393 = [R 266] in
  let r394 = Sub (r1) :: r393 in
  let r395 = S (T T_EQUAL) :: r394 in
  let r396 = Sub (r392) :: r395 in
  let r397 = [R 275] in
  let r398 = R 427 :: r397 in
  let r399 = Sub (r396) :: r398 in
  let r400 = R 439 :: r399 in
  let r401 = [R 512] in
  let r402 = [R 413] in
  let r403 = S (N N_pattern) :: r402 in
  let r404 = [R 510] in
  let r405 = S (T T_RBRACKET) :: r404 in
  let r406 = R 367 :: r405 in
  let r407 = [R 238] in
  let r408 = S (T T_LIDENT) :: r407 in
  let r409 = [R 257] in
  let r410 = R 366 :: r409 in
  let r411 = Sub (r408) :: r410 in
  let r412 = [R 258] in
  let r413 = Sub (r411) :: r412 in
  let r414 = [R 509] in
  let r415 = S (T T_RBRACE) :: r414 in
  let r416 = [R 260] in
  let r417 = [R 365] in
  let r418 = [R 256] in
  let r419 = S (T T_UNDERSCORE) :: r274 in
  let r420 = [R 554] in
  let r421 = Sub (r419) :: r420 in
  let r422 = [R 404] in
  let r423 = Sub (r421) :: r422 in
  let r424 = [R 87] in
  let r425 = [R 395] in
  let r426 = S (N N_pattern) :: r425 in
  let r427 = S (T T_INT) :: r424 in
  let r428 = [R 481] in
  let r429 = Sub (r427) :: r428 in
  let r430 = [R 557] in
  let r431 = [R 398] in
  let r432 = [R 393] in
  let r433 = [R 402] in
  let r434 = [R 563] in
  let r435 = S (T T_RBRACKET) :: r434 in
  let r436 = S (T T_LBRACKET) :: r435 in
  let r437 = [R 564] in
  let r438 = [R 565] in
  let r439 = [R 399] in
  let r440 = [R 394] in
  let r441 = [R 391] in
  let r442 = [R 567] in
  let r443 = S (T T_RPAREN) :: r442 in
  let r444 = [R 511] in
  let r445 = S (T T_BARRBRACKET) :: r444 in
  let r446 = [R 632] in
  let r447 = Sub (r1) :: r446 in
  let r448 = S (T T_EQUAL) :: r447 in
  let r449 = [R 204] in
  let r450 = Sub (r448) :: r449 in
  let r451 = [R 263] in
  let r452 = [R 240] in
  let r453 = S (T T_LIDENT) :: r452 in
  let r454 = [R 248] in
  let r455 = [R 236] in
  let r456 = Sub (r453) :: r455 in
  let r457 = [R 247] in
  let r458 = S (T T_RPAREN) :: r457 in
  let r459 = [R 237] in
  let r460 = [R 244] in
  let r461 = [R 243] in
  let r462 = S (T T_RPAREN) :: r461 in
  let r463 = R 363 :: r462 in
  let r464 = [R 364] in
  let r465 = [R 270] in
  let r466 = R 17 :: r465 in
  let r467 = R 216 :: r466 in
  let r468 = Sub (r118) :: r467 in
  let r469 = [R 140] in
  let r470 = Sub (r1) :: r469 in
  let r471 = S (T T_IN) :: r470 in
  let r472 = Sub (r468) :: r471 in
  let r473 = R 191 :: r472 in
  let r474 = [R 262] in
  let r475 = R 427 :: r474 in
  let r476 = Sub (r396) :: r475 in
  let r477 = R 439 :: r476 in
  let r478 = R 191 :: r477 in
  let r479 = [R 141] in
  let r480 = Sub (r1) :: r479 in
  let r481 = S (T T_IN) :: r480 in
  let r482 = Sub (r261) :: r481 in
  let r483 = R 191 :: r482 in
  let r484 = [R 532] in
  let r485 = [R 189] in
  let r486 = S (N N_expr) :: r485 in
  let r487 = [R 535] in
  let r488 = S (T T_RBRACKET) :: r487 in
  let r489 = R 367 :: r488 in
  let r490 = [R 542] in
  let r491 = [R 199] in
  let r492 = [R 198] in
  let r493 = [R 252] in
  let r494 = R 370 :: r493 in
  let r495 = Sub (r408) :: r494 in
  let r496 = [R 253] in
  let r497 = Sub (r495) :: r496 in
  let r498 = [R 448] in
  let r499 = Sub (r497) :: r498 in
  let r500 = [R 529] in
  let r501 = S (T T_RBRACE) :: r500 in
  let r502 = [R 514] in
  let r503 = [R 513] in
  let r504 = S (T T_GREATERDOT) :: r503 in
  let r505 = [R 184] in
  let r506 = Sub (r33) :: r505 in
  let r507 = [R 521] in
  let r508 = S (T T_END) :: r507 in
  let r509 = [R 151] in
  let r510 = S (N N_expr) :: r509 in
  let r511 = S (T T_THEN) :: r510 in
  let r512 = Sub (r1) :: r511 in
  let r513 = [R 142] in
  let r514 = S (N N_match_cases) :: r513 in
  let r515 = R 361 :: r514 in
  let r516 = [R 278] in
  let r517 = Sub (r1) :: r516 in
  let r518 = S (T T_MINUSGREATER) :: r517 in
  let r519 = [R 279] in
  let r520 = Sub (r1) :: r519 in
  let r521 = S (T T_MINUSGREATER) :: r520 in
  let r522 = [R 250] in
  let r523 = Sub (r421) :: r522 in
  let r524 = [R 206] in
  let r525 = Sub (r1) :: r524 in
  let r526 = S (T T_MINUSGREATER) :: r525 in
  let r527 = [R 143] in
  let r528 = Sub (r526) :: r527 in
  let r529 = Sub (r523) :: r528 in
  let r530 = [R 416] in
  let r531 = S (T T_UNDERSCORE) :: r530 in
  let r532 = [R 246] in
  let r533 = [R 245] in
  let r534 = S (T T_RPAREN) :: r533 in
  let r535 = R 363 :: r534 in
  let r536 = [R 272] in
  let r537 = [R 273] in
  let r538 = S (T T_LIDENT) :: r537 in
  let r539 = [R 144] in
  let r540 = Sub (r526) :: r539 in
  let r541 = S (T T_RPAREN) :: r540 in
  let r542 = [R 135] in
  let r543 = S (T T_DONE) :: r542 in
  let r544 = Sub (r1) :: r543 in
  let r545 = S (T T_DO) :: r544 in
  let r546 = Sub (r1) :: r545 in
  let r547 = S (T T_IN) :: r546 in
  let r548 = S (N N_pattern) :: r547 in
  let r549 = [R 126] in
  let r550 = S (T T_DOWNTO) :: r549 in
  let r551 = [R 153] in
  let r552 = S (T T_DONE) :: r551 in
  let r553 = Sub (r1) :: r552 in
  let r554 = S (T T_DO) :: r553 in
  let r555 = Sub (r1) :: r554 in
  let r556 = Sub (r550) :: r555 in
  let r557 = Sub (r1) :: r556 in
  let r558 = S (T T_EQUAL) :: r557 in
  let r559 = S (N N_pattern) :: r558 in
  let r560 = [R 539] in
  let r561 = [R 525] in
  let r562 = S (T T_RPAREN) :: r561 in
  let r563 = S (T T_LPAREN) :: r562 in
  let r564 = S (T T_DOT) :: r563 in
  let r565 = [R 548] in
  let r566 = S (T T_RPAREN) :: r565 in
  let r567 = Sub (r89) :: r566 in
  let r568 = S (T T_COLON) :: r567 in
  let r569 = S (N N_module_expr) :: r568 in
  let r570 = [R 183] in
  let r571 = Sub (r33) :: r570 in
  let r572 = [R 544] in
  let r573 = [R 545] in
  let r574 = [R 528] in
  let r575 = S (T T_RBRACE) :: r574 in
  let r576 = S (N N_expr) :: r575 in
  let r577 = S (T T_LBRACE) :: r576 in
  let r578 = [R 526] in
  let r579 = S (T T_RPAREN) :: r578 in
  let r580 = Sub (r1) :: r579 in
  let r581 = [R 176] in
  let r582 = [R 235] in
  let r583 = S (T T_LIDENT) :: r582 in
  let r584 = [R 232] in
  let r585 = [R 233] in
  let r586 = [R 234] in
  let r587 = [R 231] in
  let r588 = [R 179] in
  let r589 = [R 127] in
  let r590 = Sub (r1) :: r589 in
  let r591 = [R 138] in
  let r592 = Sub (r1) :: r591 in
  let r593 = [R 182] in
  let r594 = S (N N_expr) :: r593 in
  let r595 = [R 187] in
  let r596 = [R 166] in
  let r597 = [R 160] in
  let r598 = [R 177] in
  let r599 = [R 163] in
  let r600 = [R 167] in
  let r601 = [R 159] in
  let r602 = [R 162] in
  let r603 = [R 161] in
  let r604 = [R 171] in
  let r605 = [R 165] in
  let r606 = [R 164] in
  let r607 = [R 169] in
  let r608 = [R 158] in
  let r609 = [R 157] in
  let r610 = [R 154] in
  let r611 = [R 156] in
  let r612 = [R 170] in
  let r613 = [R 168] in
  let r614 = [R 172] in
  let r615 = [R 173] in
  let r616 = [R 174] in
  let r617 = [R 188] in
  let r618 = [R 175] in
  let r619 = [R 456] in
  let r620 = Sub (r1) :: r619 in
  let r621 = [R 10] in
  let r622 = R 427 :: r621 in
  let r623 = Sub (r396) :: r622 in
  let r624 = [R 267] in
  let r625 = Sub (r1) :: r624 in
  let r626 = S (T T_EQUAL) :: r625 in
  let r627 = [R 411] in
  let r628 = [R 412] in
  let r629 = [R 407] in
  let r630 = [R 408] in
  let r631 = [R 405] in
  let r632 = [R 527] in
  let r633 = S (T T_RBRACKET) :: r632 in
  let r634 = Sub (r1) :: r633 in
  let r635 = [R 180] in
  let r636 = [R 181] in
  let r637 = [R 178] in
  let r638 = [R 524] in
  let r639 = [R 534] in
  let r640 = [R 533] in
  let r641 = S (T T_BARRBRACKET) :: r640 in
  let r642 = [R 537] in
  let r643 = [R 536] in
  let r644 = S (T T_RBRACKET) :: r643 in
  let r645 = Sub (r204) :: r491 in
  let r646 = [R 200] in
  let r647 = R 367 :: r646 in
  let r648 = Sub (r645) :: r647 in
  let r649 = [R 543] in
  let r650 = S (T T_GREATERRBRACE) :: r649 in
  let r651 = [R 530] in
  let r652 = S (T T_RBRACE) :: r651 in
  let r653 = [R 447] in
  let r654 = Sub (r497) :: r653 in
  let r655 = [R 670] in
  let r656 = [R 668] in
  let r657 = Sub (r64) :: r656 in
  let r658 = [R 669] in
  let r659 = [R 251] in
  let r660 = [R 134] in
  let r661 = S (T T_DONE) :: r660 in
  let r662 = Sub (r1) :: r661 in
  let r663 = S (T T_DO) :: r662 in
  let r664 = Sub (r1) :: r663 in
  let r665 = Sub (r550) :: r664 in
  let r666 = [R 209] in
  let r667 = Sub (r526) :: r666 in
  let r668 = S (T T_RPAREN) :: r667 in
  let r669 = [R 249] in
  let r670 = [R 207] in
  let r671 = Sub (r1) :: r670 in
  let r672 = S (T T_MINUSGREATER) :: r671 in
  let r673 = [R 208] in
  let r674 = S (N N_pattern) :: r518 in
  let r675 = [R 282] in
  let r676 = [R 150] in
  let r677 = [R 520] in
  let r678 = [R 541] in
  let r679 = [R 531] in
  let r680 = S (T T_BARRBRACKET) :: r679 in
  let r681 = [R 139] in
  let r682 = Sub (r1) :: r681 in
  let r683 = S (T T_IN) :: r682 in
  let r684 = Sub (r295) :: r683 in
  let r685 = S (T T_UIDENT) :: r684 in
  let r686 = [R 300] in
  let r687 = S (N N_module_expr) :: r686 in
  let r688 = S (T T_EQUAL) :: r687 in
  let r689 = [R 301] in
  let r690 = [R 634] in
  let r691 = Sub (r450) :: r690 in
  let r692 = S (T T_RPAREN) :: r691 in
  let r693 = Sub (r538) :: r692 in
  let r694 = [R 205] in
  let r695 = Sub (r1) :: r694 in
  let r696 = [R 633] in
  let r697 = [R 265] in
  let r698 = Sub (r1) :: r697 in
  let r699 = S (T T_EQUAL) :: r698 in
  let r700 = Sub (r64) :: r699 in
  let r701 = S (T T_DOT) :: r700 in
  let r702 = [R 264] in
  let r703 = Sub (r1) :: r702 in
  let r704 = S (T T_EQUAL) :: r703 in
  let r705 = Sub (r64) :: r704 in
  let r706 = [R 155] in
  let r707 = S (T T_RPAREN) :: r706 in
  let r708 = S (N N_expr) :: r707 in
  let r709 = S (T T_COMMA) :: r708 in
  let r710 = S (N N_expr) :: r709 in
  let r711 = S (T T_LPAREN) :: r710 in
  let r712 = [R 522] in
  let r713 = [R 387] in
  let r714 = S (T T_RPAREN) :: r713 in
  let r715 = [R 385] in
  let r716 = S (T T_RPAREN) :: r715 in
  let r717 = [R 386] in
  let r718 = S (T T_RPAREN) :: r717 in
  let r719 = [R 224] in
  let r720 = S (T T_RBRACKET) :: r719 in
  let r721 = Sub (r15) :: r720 in
  let r722 = [R 420] in
  let r723 = [R 421] in
  let r724 = [R 203] in
  let r725 = S (T T_RBRACKET) :: r724 in
  let r726 = Sub (r15) :: r725 in
  let r727 = [R 630] in
  let r728 = R 427 :: r727 in
  let r729 = S (N N_module_expr) :: r728 in
  let r730 = [R 430] in
  let r731 = S (T T_STRING) :: r730 in
  let r732 = [R 429] in
  let r733 = R 427 :: r732 in
  let r734 = Sub (r731) :: r733 in
  let r735 = S (T T_EQUAL) :: r734 in
  let r736 = Sub (r64) :: r735 in
  let r737 = S (T T_COLON) :: r736 in
  let r738 = Sub (r52) :: r737 in
  let r739 = [R 623] in
  let r740 = R 427 :: r739 in
  let r741 = R 17 :: r740 in
  let r742 = Sub (r249) :: r741 in
  let r743 = S (T T_EQUAL) :: r742 in
  let r744 = Sub (r118) :: r743 in
  let r745 = [R 457] in
  let r746 = R 427 :: r745 in
  let r747 = R 17 :: r746 in
  let r748 = R 216 :: r747 in
  let r749 = Sub (r118) :: r748 in
  let r750 = R 191 :: r749 in
  let r751 = [R 418] in
  let r752 = [R 464] in
  let r753 = [R 444] in
  let r754 = R 427 :: r753 in
  let r755 = S (N N_module_type) :: r754 in
  let r756 = S (T T_COLON) :: r755 in
  let r757 = S (T T_UIDENT) :: r756 in
  let r758 = S (T T_REC) :: r757 in
  let r759 = [R 303] in
  let r760 = S (N N_module_type) :: r759 in
  let r761 = S (T T_COLON) :: r760 in
  let r762 = [R 302] in
  let r763 = R 427 :: r762 in
  let r764 = [R 305] in
  let r765 = Sub (r761) :: r764 in
  let r766 = [R 304] in
  let r767 = Sub (r761) :: r766 in
  let r768 = S (T T_RPAREN) :: r767 in
  let r769 = S (N N_module_type) :: r768 in
  let r770 = [R 297] in
  let r771 = R 427 :: r770 in
  let r772 = [R 461] in
  let r773 = R 427 :: r772 in
  let r774 = S (N N_module_type) :: r773 in
  let r775 = [R 85] in
  let r776 = S (T T_LIDENT) :: r775 in
  let r777 = [R 65] in
  let r778 = Sub (r776) :: r777 in
  let r779 = [R 80] in
  let r780 = R 427 :: r779 in
  let r781 = Sub (r778) :: r780 in
  let r782 = S (T T_EQUAL) :: r781 in
  let r783 = S (T T_LIDENT) :: r782 in
  let r784 = R 83 :: r783 in
  let r785 = R 710 :: r784 in
  let r786 = R 191 :: r785 in
  let r787 = [R 84] in
  let r788 = S (T T_RBRACKET) :: r787 in
  let r789 = [R 55] in
  let r790 = R 62 :: r789 in
  let r791 = R 54 :: r790 in
  let r792 = [R 66] in
  let r793 = S (T T_END) :: r792 in
  let r794 = Sub (r791) :: r793 in
  let r795 = [R 53] in
  let r796 = S (T T_RPAREN) :: r795 in
  let r797 = [R 709] in
  let r798 = Sub (r64) :: r797 in
  let r799 = S (T T_COLON) :: r798 in
  let r800 = Sub (r204) :: r799 in
  let r801 = [R 57] in
  let r802 = R 427 :: r801 in
  let r803 = Sub (r800) :: r802 in
  let r804 = [R 707] in
  let r805 = Sub (r64) :: r804 in
  let r806 = S (T T_COLON) :: r805 in
  let r807 = Sub (r204) :: r806 in
  let r808 = [R 708] in
  let r809 = Sub (r64) :: r808 in
  let r810 = S (T T_COLON) :: r809 in
  let r811 = Sub (r204) :: r810 in
  let r812 = [R 422] in
  let r813 = Sub (r64) :: r812 in
  let r814 = [R 58] in
  let r815 = R 427 :: r814 in
  let r816 = Sub (r813) :: r815 in
  let r817 = S (T T_COLON) :: r816 in
  let r818 = Sub (r204) :: r817 in
  let r819 = R 434 :: r818 in
  let r820 = [R 423] in
  let r821 = Sub (r64) :: r820 in
  let r822 = [R 56] in
  let r823 = R 427 :: r822 in
  let r824 = Sub (r778) :: r823 in
  let r825 = Sub (r64) :: r196 in
  let r826 = [R 64] in
  let r827 = Sub (r776) :: r826 in
  let r828 = S (T T_RBRACKET) :: r827 in
  let r829 = [R 86] in
  let r830 = S (T T_LIDENT) :: r829 in
  let r831 = [R 103] in
  let r832 = Sub (r64) :: r831 in
  let r833 = S (T T_EQUAL) :: r832 in
  let r834 = Sub (r64) :: r833 in
  let r835 = [R 59] in
  let r836 = R 427 :: r835 in
  let r837 = Sub (r834) :: r836 in
  let r838 = [R 60] in
  let r839 = [R 75] in
  let r840 = Sub (r778) :: r839 in
  let r841 = [R 25] in
  let r842 = R 427 :: r841 in
  let r843 = Sub (r840) :: r842 in
  let r844 = S (T T_COLON) :: r843 in
  let r845 = S (T T_LIDENT) :: r844 in
  let r846 = R 83 :: r845 in
  let r847 = [R 76] in
  let r848 = Sub (r840) :: r847 in
  let r849 = S (T T_MINUSGREATER) :: r848 in
  let r850 = Sub (r58) :: r849 in
  let r851 = S (T T_COLON) :: r850 in
  let r852 = [R 77] in
  let r853 = Sub (r840) :: r852 in
  let r854 = S (T T_MINUSGREATER) :: r853 in
  let r855 = [R 78] in
  let r856 = Sub (r840) :: r855 in
  let r857 = S (T T_MINUSGREATER) :: r856 in
  let r858 = [R 79] in
  let r859 = Sub (r840) :: r858 in
  let r860 = [R 13] in
  let r861 = R 427 :: r860 in
  let r862 = R 105 :: r861 in
  let r863 = R 674 :: r862 in
  let r864 = S (T T_LIDENT) :: r863 in
  let r865 = R 374 :: r864 in
  let r866 = [R 465] in
  let r867 = [R 12] in
  let r868 = R 427 :: r867 in
  let r869 = S (N N_module_type) :: r868 in
  let r870 = S (T T_COLON) :: r869 in
  let r871 = S (T T_UIDENT) :: r870 in
  let r872 = [R 479] in
  let r873 = [R 9] in
  let r874 = R 427 :: r873 in
  let r875 = Sub (r778) :: r874 in
  let r876 = S (T T_EQUAL) :: r875 in
  let r877 = S (T T_LIDENT) :: r876 in
  let r878 = R 83 :: r877 in
  let r879 = R 710 :: r878 in
  let r880 = [R 8] in
  let r881 = R 427 :: r880 in
  let r882 = Sub (r840) :: r881 in
  let r883 = S (T T_COLON) :: r882 in
  let r884 = S (T T_LIDENT) :: r883 in
  let r885 = R 83 :: r884 in
  let r886 = R 710 :: r885 in
  let r887 = [R 70] in
  let r888 = Sub (r36) :: r887 in
  let r889 = [R 28] in
  let r890 = Sub (r888) :: r889 in
  let r891 = [R 43] in
  let r892 = Sub (r890) :: r891 in
  let r893 = S (T T_EQUAL) :: r892 in
  let r894 = [R 22] in
  let r895 = R 427 :: r894 in
  let r896 = Sub (r893) :: r895 in
  let r897 = S (T T_LIDENT) :: r896 in
  let r898 = R 83 :: r897 in
  let r899 = [R 71] in
  let r900 = S (T T_END) :: r899 in
  let r901 = Sub (r268) :: r900 in
  let r902 = [R 704] in
  let r903 = Sub (r1) :: r902 in
  let r904 = S (T T_EQUAL) :: r903 in
  let r905 = Sub (r204) :: r904 in
  let r906 = R 328 :: r905 in
  let r907 = R 17 :: r906 in
  let r908 = R 379 :: r907 in
  let r909 = [R 35] in
  let r910 = R 427 :: r909 in
  let r911 = [R 703] in
  let r912 = Sub (r64) :: r911 in
  let r913 = S (T T_COLON) :: r912 in
  let r914 = Sub (r204) :: r913 in
  let r915 = [R 702] in
  let r916 = Sub (r64) :: r915 in
  let r917 = S (T T_COLON) :: r916 in
  let r918 = [R 705] in
  let r919 = Sub (r1) :: r918 in
  let r920 = [R 289] in
  let r921 = Sub (r448) :: r920 in
  let r922 = Sub (r204) :: r921 in
  let r923 = R 432 :: r922 in
  let r924 = R 17 :: r923 in
  let r925 = R 379 :: r924 in
  let r926 = [R 36] in
  let r927 = R 427 :: r926 in
  let r928 = [R 288] in
  let r929 = Sub (r813) :: r928 in
  let r930 = S (T T_COLON) :: r929 in
  let r931 = Sub (r204) :: r930 in
  let r932 = [R 287] in
  let r933 = Sub (r813) :: r932 in
  let r934 = S (T T_COLON) :: r933 in
  let r935 = [R 290] in
  let r936 = Sub (r1) :: r935 in
  let r937 = S (T T_EQUAL) :: r936 in
  let r938 = [R 291] in
  let r939 = Sub (r1) :: r938 in
  let r940 = S (T T_EQUAL) :: r939 in
  let r941 = Sub (r64) :: r940 in
  let r942 = S (T T_DOT) :: r941 in
  let r943 = [R 38] in
  let r944 = R 427 :: r943 in
  let r945 = Sub (r1) :: r944 in
  let r946 = [R 34] in
  let r947 = R 427 :: r946 in
  let r948 = R 389 :: r947 in
  let r949 = Sub (r890) :: r948 in
  let r950 = R 17 :: r949 in
  let r951 = [R 73] in
  let r952 = S (T T_RPAREN) :: r951 in
  let r953 = [R 69] in
  let r954 = Sub (r36) :: r953 in
  let r955 = S (T T_RBRACKET) :: r954 in
  let r956 = [R 46] in
  let r957 = Sub (r890) :: r956 in
  let r958 = S (T T_MINUSGREATER) :: r957 in
  let r959 = Sub (r523) :: r958 in
  let r960 = [R 29] in
  let r961 = Sub (r959) :: r960 in
  let r962 = [R 31] in
  let r963 = Sub (r890) :: r962 in
  let r964 = [R 72] in
  let r965 = S (T T_RPAREN) :: r964 in
  let r966 = [R 388] in
  let r967 = [R 37] in
  let r968 = R 427 :: r967 in
  let r969 = Sub (r834) :: r968 in
  let r970 = [R 39] in
  let r971 = [R 44] in
  let r972 = Sub (r890) :: r971 in
  let r973 = S (T T_EQUAL) :: r972 in
  let r974 = [R 45] in
  let r975 = [R 636] in
  let r976 = [R 656] in
  let r977 = [R 11] in
  let r978 = R 427 :: r977 in
  let r979 = Sub (r295) :: r978 in
  let r980 = S (T T_UIDENT) :: r979 in
  let r981 = [R 652] in
  let r982 = [R 7] in
  let r983 = R 427 :: r982 in
  let r984 = Sub (r893) :: r983 in
  let r985 = S (T T_LIDENT) :: r984 in
  let r986 = R 83 :: r985 in
  let r987 = R 710 :: r986 in
  let r988 = [R 635] in
  let r989 = R 654 :: r988 in
  let r990 = [R 403] in
  let r991 = S (T T_RPAREN) :: r990 in
  let r992 = S (N N_pattern) :: r991 in
  let r993 = S (T T_COMMA) :: r992 in
  let r994 = S (N N_pattern) :: r993 in
  let r995 = S (T T_LPAREN) :: r994 in
  let r996 = [R 51] in
  let r997 = S (T T_RPAREN) :: r996 in
  let r998 = [R 458] in
  let r999 = Sub (r237) :: r998 in
  let r1000 = [R 462] in
  let r1001 = R 427 :: r1000 in
  let r1002 = Sub (r999) :: r1001 in
  let r1003 = R 432 :: r1002 in
  let r1004 = [R 130] in
  let r1005 = S (N N_match_cases) :: r1004 in
  let r1006 = [R 132] in
  let r1007 = [R 131] in
  let r1008 = [R 222] in
  let r1009 = [R 223] in
  let r1010 = [R 390] in
  function
  | 0 | 1546 | 1550 -> Nothing
  | 1545 -> One ([R 0])
  | 1549 -> One ([R 1])
  | 1553 -> One ([R 2])
  | 381 -> One ([R 3])
  | 380 -> One ([R 4])
  | 79 -> One (R 17 :: r42)
  | 81 -> One (R 17 :: r43)
  | 135 -> One (R 17 :: r96)
  | 204 -> One (R 17 :: r166)
  | 411 -> One (R 17 :: r292)
  | 419 -> One (R 17 :: r312)
  | 496 -> One (R 17 :: r365)
  | 509 -> One (R 17 :: r382)
  | 803 -> One (R 17 :: r623)
  | 1145 -> One (R 17 :: r794)
  | 1154 -> One (R 17 :: r803)
  | 1171 -> One (R 17 :: r819)
  | 1186 -> One (R 17 :: r824)
  | 1201 -> One (R 17 :: r837)
  | 1248 -> One (R 17 :: r865)
  | 1263 -> One (R 17 :: r871)
  | 1280 -> One (R 17 :: r879)
  | 1291 -> One (R 17 :: r886)
  | 1310 -> One (R 17 :: r901)
  | 1366 -> One (R 17 :: r945)
  | 1379 -> One (R 17 :: r961)
  | 1404 -> One (R 17 :: r969)
  | 1432 -> One (R 17 :: r980)
  | 1450 -> One (R 17 :: r987)
  | 1458 -> One ([R 23])
  | 1457 -> One ([R 24])
  | 1300 -> One ([R 26])
  | 1299 -> One ([R 27])
  | 1387 -> One ([R 30])
  | 1390 -> One ([R 32])
  | 1385 -> One ([R 33])
  | 1410 -> One ([R 40])
  | 1411 -> One ([R 42])
  | 1392 -> One ([R 47])
  | 1210 -> One ([R 61])
  | 1211 -> One ([R 63])
  | 1200 -> One ([R 67])
  | 1196 -> One ([R 68])
  | 1289 -> One ([R 81])
  | 1288 -> One ([R 82])
  | 564 -> One ([R 88])
  | 68 -> One ([R 89])
  | 561 -> One ([R 90])
  | 157 | 276 -> One ([R 91])
  | 158 -> One ([R 96])
  | 345 -> One ([R 97])
  | 67 -> One ([R 101])
  | 311 -> One ([R 110])
  | 306 -> One ([R 111])
  | 270 -> One ([R 113])
  | 906 -> One ([R 125])
  | 717 -> One ([R 136])
  | 844 -> One ([R 137])
  | 744 -> One ([R 147])
  | 753 -> One ([R 148])
  | 733 -> One ([R 149])
  | 751 -> One ([R 186])
  | 865 -> One ([R 190])
  | 1 -> One (R 191 :: r6)
  | 60 -> One (R 191 :: r23)
  | 63 -> One (R 191 :: r26)
  | 65 -> One (R 191 :: r31)
  | 71 -> One (R 191 :: r38)
  | 90 -> One (R 191 :: r69)
  | 388 -> One (R 191 :: r271)
  | 402 -> One (R 191 :: r283)
  | 500 -> One (R 191 :: r370)
  | 502 -> One (R 191 :: r375)
  | 507 -> One (R 191 :: r378)
  | 535 -> One (R 191 :: r400)
  | 556 -> One (R 191 :: r423)
  | 562 -> One (R 191 :: r426)
  | 650 -> One (R 191 :: r506)
  | 652 -> One (R 191 :: r508)
  | 654 -> One (R 191 :: r512)
  | 656 -> One (R 191 :: r515)
  | 661 -> One (R 191 :: r529)
  | 681 -> One (R 191 :: r548)
  | 685 -> One (R 191 :: r559)
  | 697 -> One (R 191 :: r569)
  | 707 -> One (R 191 :: r571)
  | 977 -> One (R 191 :: r685)
  | 1078 -> One (R 191 :: r729)
  | 1082 -> One (R 191 :: r738)
  | 1104 -> One (R 191 :: r758)
  | 1127 -> One (R 191 :: r774)
  | 879 -> One ([R 201])
  | 423 -> One ([R 212])
  | 422 -> One ([R 213])
  | 485 -> One ([R 214])
  | 486 -> One ([R 215])
  | 124 | 478 -> One ([R 220])
  | 292 -> One ([R 229])
  | 293 -> One ([R 230])
  | 845 -> One ([R 241])
  | 847 -> One ([R 242])
  | 887 -> One ([R 254])
  | 886 -> One ([R 255])
  | 546 -> One ([R 259])
  | 550 -> One ([R 261])
  | 741 -> One ([R 268])
  | 828 -> One ([R 269])
  | 666 -> One ([R 271])
  | 677 -> One ([R 274])
  | 737 -> One ([R 276])
  | 829 -> One ([R 277])
  | 946 -> One ([R 280])
  | 951 -> One ([R 281])
  | 255 -> One ([R 283])
  | 254 -> One ([R 284])
  | 256 -> One ([R 285])
  | 167 -> One ([R 286])
  | 524 -> One ([R 306])
  | 522 -> One ([R 309])
  | 513 -> One ([R 311])
  | 523 -> One ([R 312])
  | 525 -> One ([R 313])
  | 427 -> One ([R 314])
  | 481 -> One ([R 321])
  | 475 -> One ([R 322])
  | 480 -> One ([R 326])
  | 1156 -> One (R 328 :: r807)
  | 1321 -> One (R 328 :: r914)
  | 282 | 1326 -> One ([R 329])
  | 242 -> One ([R 332])
  | 139 -> One ([R 334])
  | 86 | 93 -> One ([R 336])
  | 106 -> One ([R 337])
  | 105 -> One ([R 338])
  | 104 -> One ([R 339])
  | 103 -> One ([R 340])
  | 102 -> One ([R 341])
  | 107 -> One ([R 342])
  | 112 | 703 -> One ([R 343])
  | 96 | 401 | 506 -> One ([R 344])
  | 95 | 505 -> One ([R 345])
  | 100 | 533 | 559 -> One ([R 346])
  | 99 | 532 -> One ([R 347])
  | 84 -> One ([R 348])
  | 109 -> One ([R 349])
  | 101 -> One ([R 350])
  | 108 -> One ([R 351])
  | 98 -> One ([R 352])
  | 111 -> One ([R 353])
  | 113 -> One ([R 354])
  | 110 -> One ([R 356])
  | 94 -> One ([R 357])
  | 97 -> One ([R 358])
  | 206 -> One ([R 359])
  | 205 -> One (R 360 :: r171)
  | 174 -> One (R 361 :: r148)
  | 1524 -> One (R 361 :: r1005)
  | 175 -> One ([R 362])
  | 547 -> One (R 367 :: r416)
  | 617 -> One (R 367 :: r445)
  | 863 -> One (R 367 :: r641)
  | 871 -> One (R 367 :: r644)
  | 973 -> One (R 367 :: r680)
  | 548 | 600 | 864 | 878 -> One ([R 368])
  | 895 -> One ([R 369])
  | 367 -> One ([R 375])
  | 382 -> One (R 379 :: r265)
  | 635 -> One (R 379 :: r483)
  | 1370 -> One (R 379 :: r950)
  | 383 -> One ([R 380])
  | 574 -> One ([R 392])
  | 579 -> One ([R 396])
  | 573 -> One ([R 397])
  | 567 -> One ([R 400])
  | 810 -> One ([R 406])
  | 824 -> One ([R 409])
  | 601 -> One ([R 414])
  | 672 -> One ([R 415])
  | 1303 -> One ([R 419])
  | 353 -> One (R 427 :: r255)
  | 1208 -> One (R 427 :: r838)
  | 1276 -> One (R 427 :: r872)
  | 1408 -> One (R 427 :: r970)
  | 1445 -> One (R 427 :: r981)
  | 1460 -> One (R 427 :: r989)
  | 1089 -> One ([R 431])
  | 1341 -> One (R 432 :: r931)
  | 319 | 1346 -> One ([R 433])
  | 1175 -> One ([R 435])
  | 1173 -> One ([R 436])
  | 1176 -> One ([R 437])
  | 1174 -> One ([R 438])
  | 537 -> One ([R 440])
  | 1438 -> One ([R 442])
  | 1437 -> One ([R 443])
  | 1270 -> One ([R 445])
  | 1269 -> One ([R 446])
  | 186 -> One ([R 449])
  | 798 -> One ([R 454])
  | 802 -> One ([R 455])
  | 1503 -> One ([R 459])
  | 1500 -> One ([R 460])
  | 1102 -> One (R 463 :: r751)
  | 1103 -> One (R 463 :: r752)
  | 1257 -> One (R 463 :: r866)
  | 1246 -> One ([R 466])
  | 1271 -> One ([R 467])
  | 1247 -> One ([R 468])
  | 1259 -> One ([R 469])
  | 1261 -> One ([R 470])
  | 1274 -> One ([R 471])
  | 1275 -> One ([R 472])
  | 1262 -> One ([R 473])
  | 1273 -> One ([R 474])
  | 1272 -> One ([R 475])
  | 1260 -> One ([R 476])
  | 1290 -> One ([R 477])
  | 1279 -> One ([R 478])
  | 1278 -> One ([R 480])
  | 399 -> One ([R 483])
  | 396 -> One ([R 485])
  | 185 -> One ([R 490])
  | 190 -> One ([R 491])
  | 267 -> One ([R 492])
  | 212 | 1238 -> One ([R 506])
  | 690 -> One ([R 515])
  | 706 -> One ([R 516])
  | 705 | 752 -> One ([R 517])
  | 692 | 732 -> One ([R 518])
  | 841 | 858 -> One ([R 523])
  | 704 -> One ([R 549])
  | 848 -> One ([R 551])
  | 846 -> One ([R 552])
  | 565 -> One ([R 553])
  | 569 -> One ([R 556])
  | 614 -> One ([R 558])
  | 613 -> One ([R 559])
  | 568 -> One ([R 561])
  | 605 -> One ([R 562])
  | 590 -> One ([R 570])
  | 28 -> One ([R 571])
  | 8 -> One ([R 572])
  | 52 -> One ([R 574])
  | 51 -> One ([R 575])
  | 50 -> One ([R 576])
  | 49 -> One ([R 577])
  | 48 -> One ([R 578])
  | 47 -> One ([R 579])
  | 46 -> One ([R 580])
  | 45 -> One ([R 581])
  | 44 -> One ([R 582])
  | 43 -> One ([R 583])
  | 42 -> One ([R 584])
  | 41 -> One ([R 585])
  | 40 -> One ([R 586])
  | 39 -> One ([R 587])
  | 38 -> One ([R 588])
  | 37 -> One ([R 589])
  | 36 -> One ([R 590])
  | 35 -> One ([R 591])
  | 34 -> One ([R 592])
  | 33 -> One ([R 593])
  | 32 -> One ([R 594])
  | 31 -> One ([R 595])
  | 30 -> One ([R 596])
  | 29 -> One ([R 597])
  | 27 -> One ([R 598])
  | 26 -> One ([R 599])
  | 25 -> One ([R 600])
  | 24 -> One ([R 601])
  | 23 -> One ([R 602])
  | 22 -> One ([R 603])
  | 21 -> One ([R 604])
  | 20 -> One ([R 605])
  | 19 -> One ([R 606])
  | 18 -> One ([R 607])
  | 17 -> One ([R 608])
  | 16 -> One ([R 609])
  | 15 -> One ([R 610])
  | 14 -> One ([R 611])
  | 13 -> One ([R 612])
  | 12 -> One ([R 613])
  | 11 -> One ([R 614])
  | 10 -> One ([R 615])
  | 9 -> One ([R 616])
  | 7 -> One ([R 617])
  | 6 -> One ([R 618])
  | 5 -> One ([R 619])
  | 4 -> One ([R 620])
  | 3 -> One ([R 621])
  | 1430 -> One ([R 622])
  | 366 -> One ([R 625])
  | 357 -> One ([R 626])
  | 365 -> One ([R 627])
  | 356 -> One ([R 628])
  | 355 -> One ([R 629])
  | 1424 -> One ([R 637])
  | 1443 | 1463 -> One ([R 638])
  | 1444 | 1464 -> One ([R 639])
  | 1439 -> One ([R 640])
  | 1421 -> One ([R 641])
  | 1422 -> One ([R 642])
  | 1427 -> One ([R 643])
  | 1429 -> One ([R 644])
  | 1442 -> One ([R 645])
  | 1431 -> One ([R 646])
  | 1441 -> One ([R 647])
  | 1440 -> One ([R 648])
  | 1449 -> One ([R 649])
  | 1448 -> One ([R 650])
  | 1428 -> One ([R 651])
  | 1447 -> One ([R 653])
  | 1425 -> One (R 654 :: r976)
  | 499 -> One ([R 657])
  | 498 -> One ([R 658])
  | 371 -> One ([R 662])
  | 372 -> One ([R 663])
  | 374 -> One ([R 664])
  | 376 -> One ([R 665])
  | 373 -> One ([R 666])
  | 370 -> One ([R 667])
  | 1256 -> One ([R 672])
  | 1255 -> One ([R 673])
  | 317 -> One ([R 675])
  | 304 -> One ([R 676])
  | 326 -> One ([R 677])
  | 430 -> One (R 689 :: r328)
  | 460 -> One ([R 690])
  | 141 -> One ([R 694])
  | 142 -> One ([R 695])
  | 375 -> One ([R 700])
  | 378 -> One ([R 701])
  | 1161 -> One (R 710 :: r811)
  | 1214 -> One (R 710 :: r846)
  | 1305 -> One (R 710 :: r898)
  | 1137 -> One ([R 711])
  | 448 -> One ([R 719])
  | 882 -> One (S (T T_WITH) :: r654)
  | 346 | 377 -> One (S (T T_UIDENT) :: r41)
  | 195 -> One (S (T T_UIDENT) :: r164)
  | 407 -> One (S (T T_TYPE) :: r289)
  | 1005 -> One (S (T T_TYPE) :: r693)
  | 1134 | 1304 -> One (S (T T_TYPE) :: r786)
  | 341 -> One (S (T T_RPAREN) :: r47)
  | 160 | 277 -> One (S (T T_RPAREN) :: r126)
  | 260 -> One (S (T T_RPAREN) :: r192)
  | 263 -> One (S (T T_RPAREN) :: r193)
  | 421 -> One (S (T T_RPAREN) :: r313)
  | 515 -> One (S (T T_RPAREN) :: r383)
  | 584 -> One (S (T T_RPAREN) :: r437)
  | 586 -> One (S (T T_RPAREN) :: r438)
  | 859 -> One (S (T T_RPAREN) :: r638)
  | 1033 -> One (S (T T_RPAREN) :: r711)
  | 1042 -> One (S (T T_RPAREN) :: r712)
  | 1107 -> One (S (T T_RPAREN) :: r765)
  | 1475 -> One (S (T T_RPAREN) :: r995)
  | 178 -> One (S (T T_RBRACKET) :: r149)
  | 229 -> One (S (T T_RBRACKET) :: r180)
  | 272 | 278 -> One (S (T T_RBRACKET) :: r197)
  | 342 -> One (S (T T_RBRACKET) :: r254)
  | 869 -> One (S (T T_RBRACKET) :: r642)
  | 220 -> One (S (T T_QUOTE) :: r178)
  | 335 -> One (S (T T_PLUSEQ) :: r243)
  | 1493 -> One (S (T T_PLUSEQ) :: r1003)
  | 131 -> One (S (T T_MODULE) :: r93)
  | 299 -> One (S (T T_MINUSGREATER) :: r223)
  | 1233 -> One (S (T T_MINUSGREATER) :: r859)
  | 127 -> One (S (T T_LIDENT) :: r84)
  | 1219 -> One (S (T T_LIDENT) :: r851)
  | 1400 -> One (S (T T_LIDENT) :: r966)
  | 742 -> One (S (T T_LESSMINUS) :: r594)
  | 313 -> One (S (T T_LBRACE) :: r226)
  | 394 -> One (S (T T_INT) :: r277)
  | 397 -> One (S (T T_INT) :: r278)
  | 734 -> One (S (T T_IN) :: r590)
  | 738 -> One (S (T T_IN) :: r592)
  | 1383 -> One (S (T T_IN) :: r963)
  | 642 -> One (S (T T_GREATERRBRACE) :: r490)
  | 967 -> One (S (T T_GREATERRBRACE) :: r678)
  | 164 -> One (S (T T_GREATER) :: r131)
  | 168 -> One (S (T T_GREATER) :: r133)
  | 359 -> One (S (T T_EQUAL) :: r258)
  | 465 -> One (S (T T_EQUAL) :: r353)
  | 1011 -> One (S (T T_EQUAL) :: r695)
  | 1335 -> One (S (T T_EQUAL) :: r919)
  | 1543 -> One (S (T T_EOF) :: r1008)
  | 1547 -> One (S (T T_EOF) :: r1009)
  | 1551 -> One (S (T T_EOF) :: r1010)
  | 958 -> One (S (T T_END) :: r677)
  | 156 -> One (S (T T_DOTDOT) :: r116)
  | 318 -> One (S (T T_DOTDOT) :: r227)
  | 74 -> One (S (T T_DOT) :: r40)
  | 244 -> One (S (T T_DOT) :: r190)
  | 443 -> One (S (T T_DOT) :: r341)
  | 476 -> One (S (T T_DOT) :: r357)
  | 582 -> One (S (T T_DOT) :: r436)
  | 1025 -> One (S (T T_DOT) :: r705)
  | 1180 -> One (S (T T_DOT) :: r821)
  | 1192 -> One (S (T T_DOT) :: r830)
  | 170 -> One (S (T T_COLON) :: r140)
  | 425 -> One (S (T T_COLON) :: r316)
  | 1108 -> One (S (T T_COLON) :: r769)
  | 539 -> One (S (T T_BARRBRACKET) :: r401)
  | 640 -> One (S (T T_BARRBRACKET) :: r484)
  | 861 -> One (S (T T_BARRBRACKET) :: r639)
  | 181 | 1231 -> One (S (T T_BAR) :: r154)
  | 231 -> One (S (T T_BAR) :: r183)
  | 379 -> One (S (N N_structure) :: r260)
  | 1423 -> One (S (N N_structure) :: r975)
  | 390 -> One (S (N N_pattern) :: r273)
  | 400 | 558 | 674 | 925 -> One (S (N N_pattern) :: r280)
  | 555 -> One (S (N N_pattern) :: r418)
  | 575 -> One (S (N N_pattern) :: r431)
  | 577 -> One (S (N N_pattern) :: r432)
  | 580 -> One (S (N N_pattern) :: r433)
  | 588 -> One (S (N N_pattern) :: r439)
  | 593 -> One (S (N N_pattern) :: r440)
  | 811 -> One (S (N N_pattern) :: r627)
  | 816 -> One (S (N N_pattern) :: r628)
  | 818 -> One (S (N N_pattern) :: r629)
  | 820 -> One (S (N N_pattern) :: r630)
  | 1072 -> One (S (N N_pattern) :: r722)
  | 417 -> One (S (N N_module_type) :: r306)
  | 418 -> One (S (N N_module_type) :: r308)
  | 473 -> One (S (N N_module_type) :: r355)
  | 519 -> One (S (N N_module_type) :: r385)
  | 980 -> One (S (N N_module_type) :: r688)
  | 495 -> One (S (N N_module_expr) :: r362)
  | 665 -> One (S (N N_let_pattern) :: r535)
  | 645 -> One (S (N N_expr) :: r492)
  | 649 -> One (S (N N_expr) :: r504)
  | 716 -> One (S (N N_expr) :: r581)
  | 731 -> One (S (N N_expr) :: r588)
  | 745 -> One (S (N N_expr) :: r595)
  | 747 -> One (S (N N_expr) :: r596)
  | 749 -> One (S (N N_expr) :: r597)
  | 754 -> One (S (N N_expr) :: r598)
  | 756 -> One (S (N N_expr) :: r599)
  | 758 -> One (S (N N_expr) :: r600)
  | 760 -> One (S (N N_expr) :: r601)
  | 762 -> One (S (N N_expr) :: r602)
  | 764 -> One (S (N N_expr) :: r603)
  | 766 -> One (S (N N_expr) :: r604)
  | 768 -> One (S (N N_expr) :: r605)
  | 770 -> One (S (N N_expr) :: r606)
  | 772 -> One (S (N N_expr) :: r607)
  | 774 -> One (S (N N_expr) :: r608)
  | 776 -> One (S (N N_expr) :: r609)
  | 778 -> One (S (N N_expr) :: r610)
  | 780 -> One (S (N N_expr) :: r611)
  | 782 -> One (S (N N_expr) :: r612)
  | 784 -> One (S (N N_expr) :: r613)
  | 786 -> One (S (N N_expr) :: r614)
  | 788 -> One (S (N N_expr) :: r615)
  | 790 -> One (S (N N_expr) :: r616)
  | 793 -> One (S (N N_expr) :: r617)
  | 795 -> One (S (N N_expr) :: r618)
  | 834 -> One (S (N N_expr) :: r635)
  | 839 -> One (S (N N_expr) :: r636)
  | 842 -> One (S (N N_expr) :: r637)
  | 897 -> One (S (N N_expr) :: r659)
  | 955 -> One (S (N N_expr) :: r676)
  | 633 -> One (Sub (r1) :: r464)
  | 660 -> One (Sub (r1) :: r521)
  | 917 -> One (Sub (r1) :: r665)
  | 1074 -> One (Sub (r1) :: r723)
  | 1527 -> One (Sub (r1) :: r1006)
  | 1529 -> One (Sub (r1) :: r1007)
  | 2 -> One (Sub (r10) :: r12)
  | 55 -> One (Sub (r10) :: r13)
  | 58 -> One (Sub (r10) :: r18)
  | 88 -> One (Sub (r10) :: r51)
  | 329 -> One (Sub (r10) :: r233)
  | 799 -> One (Sub (r10) :: r620)
  | 1070 -> One (Sub (r10) :: r721)
  | 1076 -> One (Sub (r10) :: r726)
  | 70 -> One (Sub (r33) :: r34)
  | 648 -> One (Sub (r33) :: r502)
  | 689 -> One (Sub (r33) :: r560)
  | 712 -> One (Sub (r33) :: r573)
  | 723 -> One (Sub (r33) :: r586)
  | 725 -> One (Sub (r33) :: r587)
  | 121 -> One (Sub (r36) :: r75)
  | 188 -> One (Sub (r36) :: r157)
  | 265 -> One (Sub (r36) :: r194)
  | 595 -> One (Sub (r52) :: r441)
  | 822 -> One (Sub (r52) :: r631)
  | 214 -> One (Sub (r56) :: r175)
  | 297 -> One (Sub (r56) :: r221)
  | 931 -> One (Sub (r56) :: r672)
  | 1224 -> One (Sub (r58) :: r854)
  | 1228 -> One (Sub (r58) :: r857)
  | 130 -> One (Sub (r60) :: r87)
  | 163 -> One (Sub (r60) :: r130)
  | 218 -> One (Sub (r60) :: r176)
  | 224 -> One (Sub (r62) :: r179)
  | 268 -> One (Sub (r64) :: r195)
  | 552 -> One (Sub (r64) :: r417)
  | 609 -> One (Sub (r64) :: r443)
  | 625 -> One (Sub (r64) :: r459)
  | 667 -> One (Sub (r64) :: r536)
  | 806 -> One (Sub (r64) :: r626)
  | 889 -> One (Sub (r64) :: r655)
  | 893 -> One (Sub (r64) :: r658)
  | 1147 -> One (Sub (r64) :: r796)
  | 1484 -> One (Sub (r64) :: r997)
  | 92 -> One (Sub (r71) :: r73)
  | 146 -> One (Sub (r77) :: r114)
  | 245 -> One (Sub (r77) :: r191)
  | 368 -> One (Sub (r77) :: r259)
  | 406 -> One (Sub (r89) :: r285)
  | 528 -> One (Sub (r89) :: r387)
  | 1054 -> One (Sub (r89) :: r714)
  | 1057 -> One (Sub (r89) :: r716)
  | 1060 -> One (Sub (r89) :: r718)
  | 151 -> One (Sub (r109) :: r115)
  | 143 -> One (Sub (r111) :: r113)
  | 275 -> One (Sub (r118) :: r200)
  | 159 -> One (Sub (r124) :: r125)
  | 321 -> One (Sub (r124) :: r228)
  | 202 -> One (Sub (r143) :: r165)
  | 180 -> One (Sub (r145) :: r151)
  | 192 -> One (Sub (r161) :: r163)
  | 210 -> One (Sub (r173) :: r174)
  | 239 -> One (Sub (r186) :: r188)
  | 280 -> One (Sub (r202) :: r203)
  | 283 -> One (Sub (r204) :: r220)
  | 710 -> One (Sub (r204) :: r572)
  | 1327 -> One (Sub (r204) :: r917)
  | 1347 -> One (Sub (r204) :: r934)
  | 281 -> One (Sub (r212) :: r214)
  | 322 -> One (Sub (r212) :: r230)
  | 1117 -> One (Sub (r261) :: r771)
  | 392 -> One (Sub (r275) :: r276)
  | 987 -> One (Sub (r295) :: r689)
  | 469 -> One (Sub (r321) :: r354)
  | 429 -> One (Sub (r323) :: r324)
  | 438 -> One (Sub (r334) :: r339)
  | 431 -> One (Sub (r336) :: r338)
  | 1139 -> One (Sub (r336) :: r788)
  | 446 -> One (Sub (r343) :: r346)
  | 452 -> One (Sub (r350) :: r351)
  | 540 -> One (Sub (r403) :: r406)
  | 541 -> One (Sub (r413) :: r415)
  | 929 -> One (Sub (r421) :: r669)
  | 570 -> One (Sub (r429) :: r430)
  | 620 -> One (Sub (r450) :: r451)
  | 1015 -> One (Sub (r450) :: r696)
  | 621 -> One (Sub (r453) :: r454)
  | 630 -> One (Sub (r453) :: r460)
  | 622 -> One (Sub (r456) :: r458)
  | 631 -> One (Sub (r456) :: r463)
  | 646 -> One (Sub (r499) :: r501)
  | 881 -> One (Sub (r499) :: r652)
  | 936 -> One (Sub (r526) :: r673)
  | 663 -> One (Sub (r531) :: r532)
  | 675 -> One (Sub (r538) :: r541)
  | 926 -> One (Sub (r538) :: r668)
  | 1019 -> One (Sub (r538) :: r701)
  | 1354 -> One (Sub (r538) :: r942)
  | 718 -> One (Sub (r583) :: r584)
  | 721 -> One (Sub (r583) :: r585)
  | 874 -> One (Sub (r648) :: r650)
  | 949 -> One (Sub (r674) :: r675)
  | 1106 -> One (Sub (r761) :: r763)
  | 1353 -> One (Sub (r813) :: r937)
  | 1189 -> One (Sub (r825) :: r828)
  | 1375 -> One (Sub (r825) :: r955)
  | 1396 -> One (Sub (r840) :: r965)
  | 1413 -> One (Sub (r840) :: r973)
  | 1373 -> One (Sub (r890) :: r952)
  | 1417 -> One (Sub (r893) :: r974)
  | 1316 -> One (Sub (r908) :: r910)
  | 1338 -> One (Sub (r925) :: r927)
  | 797 -> One (r0)
  | 1542 -> One (r2)
  | 1541 -> One (r3)
  | 1540 -> One (r4)
  | 1539 -> One (r5)
  | 1538 -> One (r6)
  | 53 -> One (r7)
  | 54 -> One (r9)
  | 1537 -> One (r11)
  | 57 -> One (r12)
  | 56 -> One (r13)
  | 1465 -> One (r14)
  | 1536 -> One (r16)
  | 1535 -> One (r17)
  | 59 -> One (r18)
  | 1534 -> One (r19)
  | 1533 -> One (r20)
  | 1532 -> One (r21)
  | 1531 -> One (r22)
  | 61 -> One (r23)
  | 62 -> One (r24)
  | 1523 -> One (r25)
  | 64 -> One (r26)
  | 1522 -> One (r27)
  | 1521 -> One (r28)
  | 1520 -> One (r29)
  | 1519 -> One (r30)
  | 66 -> One (r31)
  | 69 -> One (r32)
  | 1518 -> One (r34)
  | 73 -> One (r35)
  | 78 -> One (r37)
  | 72 -> One (r38)
  | 77 -> One (r39)
  | 75 -> One (r40)
  | 76 -> One (r41)
  | 80 -> One (r42)
  | 82 -> One (r43)
  | 1041 -> One (r44)
  | 1040 -> One (r45)
  | 83 -> One (r46)
  | 85 -> One (r47)
  | 87 | 534 | 647 | 903 -> One (r48)
  | 1517 -> One (r49)
  | 1516 -> One (r50)
  | 89 -> One (r51)
  | 119 -> One (r53)
  | 187 -> One (r55)
  | 209 -> One (r57)
  | 208 -> One (r59)
  | 217 -> One (r61)
  | 262 -> One (r63)
  | 1515 -> One (r65)
  | 1514 -> One (r66)
  | 118 -> One (r67)
  | 117 -> One (r68)
  | 91 -> One (r69)
  | 114 -> One (r70)
  | 116 -> One (r72)
  | 115 -> One (r73)
  | 120 | 134 -> One (r74)
  | 122 -> One (r75)
  | 125 -> One (r76)
  | 126 -> One (r78)
  | 123 -> One (r79)
  | 1513 -> One (r80)
  | 1512 -> One (r81)
  | 1511 -> One (r82)
  | 129 -> One (r83)
  | 128 -> One (r84)
  | 1510 -> One (r85)
  | 1509 -> One (r86)
  | 1508 -> One (r87)
  | 531 -> One (r88)
  | 1507 -> One (r90)
  | 1506 -> One (r91)
  | 133 -> One (r92)
  | 132 -> One (r93)
  | 1505 -> One (r94)
  | 1504 -> One (r95)
  | 136 -> One (r96)
  | 1492 -> One (r97)
  | 328 -> One (r98)
  | 327 -> One (r99)
  | 155 -> One (r100)
  | 154 | 334 -> One (r101)
  | 140 | 333 -> One (r102)
  | 138 | 332 -> One (r103)
  | 137 | 331 -> One (r104)
  | 145 -> One (r105)
  | 148 -> One (r107)
  | 144 -> One (r108)
  | 153 -> One (r110)
  | 150 -> One (r112)
  | 149 -> One (r113)
  | 147 -> One (r114)
  | 152 -> One (r115)
  | 312 -> One (r116)
  | 274 -> One (r117)
  | 310 -> One (r119)
  | 309 -> One (r120)
  | 308 -> One (r121)
  | 307 -> One (r123)
  | 305 -> One (r125)
  | 161 -> One (r126)
  | 162 | 177 | 1227 -> One (r127)
  | 259 -> One (r128)
  | 258 -> One (r129)
  | 257 -> One (r130)
  | 166 -> One (r131)
  | 165 | 442 -> One (r132)
  | 169 -> One (r133)
  | 253 -> One (r134)
  | 252 -> One (r136)
  | 251 -> One (r137)
  | 250 -> One (r138)
  | 249 -> One (r139)
  | 171 -> One (r140)
  | 199 | 1232 -> One (r142)
  | 228 -> One (r144)
  | 238 -> One (r146)
  | 237 -> One (r147)
  | 176 -> One (r148)
  | 179 -> One (r149)
  | 236 -> One (r150)
  | 235 -> One (r151)
  | 201 -> One (r152)
  | 200 -> One (r153)
  | 182 -> One (r154)
  | 184 -> One (r155)
  | 183 -> One (r156)
  | 189 -> One (r157)
  | 198 | 1237 -> One (r158)
  | 197 | 1236 -> One (r159)
  | 191 | 1235 -> One (r160)
  | 194 -> One (r162)
  | 193 -> One (r163)
  | 196 -> One (r164)
  | 203 -> One (r165)
  | 227 -> One (r166)
  | 216 -> One (r167)
  | 226 -> One (r169)
  | 223 -> One (r170)
  | 207 -> One (r171)
  | 211 -> One (r172)
  | 213 -> One (r174)
  | 215 -> One (r175)
  | 219 -> One (r176)
  | 222 -> One (r177)
  | 221 -> One (r178)
  | 225 -> One (r179)
  | 230 -> One (r180)
  | 234 -> One (r181)
  | 233 -> One (r182)
  | 232 -> One (r183)
  | 243 -> One (r185)
  | 241 -> One (r187)
  | 240 -> One (r188)
  | 248 -> One (r189)
  | 247 -> One (r190)
  | 246 -> One (r191)
  | 261 -> One (r192)
  | 264 -> One (r193)
  | 266 -> One (r194)
  | 269 -> One (r195)
  | 271 -> One (r196)
  | 273 -> One (r197)
  | 303 -> One (r198)
  | 302 -> One (r199)
  | 279 -> One (r200)
  | 295 -> One (r201)
  | 296 -> One (r203)
  | 294 -> One (r211)
  | 291 -> One (r213)
  | 290 -> One (r214)
  | 289 -> One (r215)
  | 288 -> One (r216)
  | 287 -> One (r217)
  | 286 -> One (r218)
  | 285 -> One (r219)
  | 284 -> One (r220)
  | 298 -> One (r221)
  | 301 -> One (r222)
  | 300 -> One (r223)
  | 316 -> One (r224)
  | 315 -> One (r225)
  | 314 -> One (r226)
  | 320 -> One (r227)
  | 325 -> One (r228)
  | 324 -> One (r229)
  | 323 -> One (r230)
  | 1491 -> One (r231)
  | 1490 -> One (r232)
  | 330 -> One (r233)
  | 364 -> One (r234)
  | 363 -> One (r235)
  | 1502 -> One (r236)
  | 358 -> One (r238)
  | 352 -> One (r240)
  | 351 -> One (r241)
  | 337 -> One (r242)
  | 336 -> One (r243)
  | 350 -> One (r244)
  | 349 -> One (r245)
  | 1497 -> One (r246)
  | 1496 -> One (r247)
  | 344 -> One (r248)
  | 348 -> One (r250)
  | 347 -> One (r251)
  | 340 -> One (r252)
  | 343 -> One (r254)
  | 354 -> One (r255)
  | 362 -> One (r256)
  | 361 -> One (r257)
  | 360 -> One (r258)
  | 369 -> One (r259)
  | 1489 -> One (r260)
  | 387 -> One (r262)
  | 386 -> One (r263)
  | 385 -> One (r264)
  | 384 -> One (r265)
  | 1315 -> One (r266)
  | 1314 -> One (r267)
  | 1488 -> One (r269)
  | 1487 -> One (r270)
  | 389 -> One (r271)
  | 1483 -> One (r272)
  | 1482 -> One (r273)
  | 391 -> One (r274)
  | 393 -> One (r276)
  | 395 -> One (r277)
  | 398 -> One (r278)
  | 608 -> One (r279)
  | 607 -> One (r280)
  | 405 -> One (r281)
  | 404 -> One (r282)
  | 403 -> One (r283)
  | 1474 -> One (r284)
  | 1473 -> One (r285)
  | 1472 -> One (r286)
  | 410 -> One (r287)
  | 409 -> One (r288)
  | 408 -> One (r289)
  | 1471 -> One (r290)
  | 1470 -> One (r291)
  | 412 -> One (r292)
  | 1063 -> One (r293)
  | 494 -> One (r294)
  | 1069 -> One (r296)
  | 1068 -> One (r297)
  | 1067 -> One (r298)
  | 1066 -> One (r299)
  | 491 -> One (r301)
  | 490 -> One (r302)
  | 416 -> One (r303)
  | 415 -> One (r304)
  | 414 -> One (r305)
  | 489 -> One (r306)
  | 488 -> One (r307)
  | 487 -> One (r308)
  | 484 -> One (r309)
  | 483 -> One (r310)
  | 482 -> One (r311)
  | 420 -> One (r312)
  | 424 -> One (r313)
  | 472 -> One (r314)
  | 428 -> One (r315)
  | 426 -> One (r316)
  | 464 -> One (r317)
  | 463 -> One (r318)
  | 462 -> One (r319)
  | 461 -> One (r320)
  | 471 -> One (r322)
  | 468 -> One (r324)
  | 459 -> One (r325)
  | 458 -> One (r326)
  | 457 -> One (r327)
  | 441 -> One (r328)
  | 434 -> One (r329)
  | 433 -> One (r330)
  | 435 -> One (r332)
  | 432 -> One (r333)
  | 440 -> One (r335)
  | 437 -> One (r337)
  | 436 -> One (r338)
  | 439 -> One (r339)
  | 445 -> One (r340)
  | 444 -> One (r341)
  | 447 -> One (r342)
  | 451 -> One (r344)
  | 450 -> One (r345)
  | 449 -> One (r346)
  | 455 -> One (r347)
  | 454 -> One (r348)
  | 453 -> One (r349)
  | 456 -> One (r351)
  | 467 -> One (r352)
  | 466 -> One (r353)
  | 470 -> One (r354)
  | 474 -> One (r355)
  | 479 -> One (r356)
  | 477 -> One (r357)
  | 1065 -> One (r358)
  | 1064 -> One (r359)
  | 493 -> One (r360)
  | 518 -> One (r361)
  | 517 -> One (r362)
  | 1053 -> One (r363)
  | 1052 -> One (r364)
  | 497 -> One (r365)
  | 1051 -> One (r366)
  | 1050 -> One (r367)
  | 1049 -> One (r368)
  | 1048 -> One (r369)
  | 501 -> One (r370)
  | 1047 -> One (r371)
  | 1046 -> One (r372)
  | 1045 -> One (r373)
  | 1044 -> One (r374)
  | 503 -> One (r375)
  | 527 -> One (r376)
  | 526 -> One (r377)
  | 508 -> One (r378)
  | 514 -> One (r379)
  | 512 -> One (r380)
  | 511 -> One (r381)
  | 510 -> One (r382)
  | 516 -> One (r383)
  | 521 -> One (r384)
  | 520 -> One (r385)
  | 530 -> One (r386)
  | 529 -> One (r387)
  | 592 -> One (r388)
  | 591 -> One (r389)
  | 825 -> One (r391)
  | 815 -> One (r393)
  | 814 -> One (r394)
  | 813 -> One (r395)
  | 1032 -> One (r397)
  | 1031 -> One (r398)
  | 538 -> One (r399)
  | 536 -> One (r400)
  | 616 -> One (r401)
  | 604 -> One (r402)
  | 603 -> One (r404)
  | 602 -> One (r405)
  | 599 -> One (r406)
  | 542 -> One (r407)
  | 554 -> One (r409)
  | 551 -> One (r410)
  | 545 -> One (r412)
  | 544 -> One (r414)
  | 543 -> One (r415)
  | 549 -> One (r416)
  | 553 -> One (r417)
  | 615 -> One (r418)
  | 566 | 805 -> One (r420)
  | 612 -> One (r422)
  | 557 -> One (r423)
  | 560 -> One (r424)
  | 606 -> One (r425)
  | 563 -> One (r426)
  | 572 -> One (r428)
  | 571 -> One (r430)
  | 576 -> One (r431)
  | 578 -> One (r432)
  | 581 -> One (r433)
  | 598 -> One (r434)
  | 597 -> One (r435)
  | 583 -> One (r436)
  | 585 -> One (r437)
  | 587 -> One (r438)
  | 589 -> One (r439)
  | 594 -> One (r440)
  | 596 -> One (r441)
  | 611 -> One (r442)
  | 610 -> One (r443)
  | 619 -> One (r444)
  | 618 -> One (r445)
  | 1010 -> One (r446)
  | 1009 -> One (r447)
  | 1014 -> One (r449)
  | 1030 -> One (r451)
  | 623 -> One (r452)
  | 629 -> One (r454)
  | 624 -> One (r455)
  | 628 -> One (r457)
  | 627 -> One (r458)
  | 626 -> One (r459)
  | 1004 -> One (r460)
  | 1003 -> One (r461)
  | 1002 -> One (r462)
  | 632 -> One (r463)
  | 1001 -> One (r464)
  | 996 -> One (r465)
  | 995 -> One (r466)
  | 994 -> One (r467)
  | 993 -> One (r469)
  | 992 -> One (r470)
  | 991 -> One (r471)
  | 990 -> One (r472)
  | 989 -> One (r473)
  | 1000 -> One (r474)
  | 999 -> One (r475)
  | 998 -> One (r476)
  | 997 -> One (r477)
  | 1374 -> One (r478)
  | 976 -> One (r479)
  | 639 -> One (r480)
  | 638 -> One (r481)
  | 637 -> One (r482)
  | 636 -> One (r483)
  | 972 -> One (r484)
  | 868 -> One (r485)
  | 971 -> One (r487)
  | 970 -> One (r488)
  | 969 -> One (r489)
  | 643 -> One (r490)
  | 644 -> One (r491)
  | 966 -> One (r492)
  | 896 -> One (r493)
  | 888 -> One (r494)
  | 885 -> One (r496)
  | 904 -> One (r498)
  | 965 -> One (r500)
  | 964 -> One (r501)
  | 963 -> One (r502)
  | 962 -> One (r503)
  | 961 -> One (r504)
  | 960 -> One (r505)
  | 651 -> One (r506)
  | 957 -> One (r507)
  | 653 -> One (r508)
  | 954 -> One (r509)
  | 953 -> One (r510)
  | 952 -> One (r511)
  | 655 -> One (r512)
  | 948 -> One (r513)
  | 658 -> One (r514)
  | 657 -> One (r515)
  | 947 -> One (r516)
  | 945 -> One (r517)
  | 659 -> One (r518)
  | 944 -> One (r519)
  | 943 -> One (r520)
  | 942 -> One (r521)
  | 935 -> One (r522)
  | 924 -> One (r524)
  | 680 -> One (r525)
  | 941 -> One (r527)
  | 940 -> One (r528)
  | 662 -> One (r529)
  | 664 -> One (r530)
  | 673 -> One (r532)
  | 671 -> One (r533)
  | 670 -> One (r534)
  | 669 -> One (r535)
  | 668 -> One (r536)
  | 676 -> One (r537)
  | 939 -> One (r539)
  | 679 -> One (r540)
  | 678 -> One (r541)
  | 916 -> One (r542)
  | 915 -> One (r543)
  | 914 -> One (r544)
  | 913 -> One (r545)
  | 684 -> One (r546)
  | 683 -> One (r547)
  | 682 -> One (r548)
  | 907 -> One (r549)
  | 912 -> One (r551)
  | 911 -> One (r552)
  | 910 -> One (r553)
  | 909 -> One (r554)
  | 908 -> One (r555)
  | 905 -> One (r556)
  | 688 -> One (r557)
  | 687 -> One (r558)
  | 686 -> One (r559)
  | 691 -> One (r560)
  | 696 -> One (r561)
  | 695 -> One (r562)
  | 694 | 902 -> One (r563)
  | 901 -> One (r564)
  | 702 -> One (r565)
  | 701 -> One (r566)
  | 700 -> One (r567)
  | 699 -> One (r568)
  | 698 -> One (r569)
  | 709 -> One (r570)
  | 708 -> One (r571)
  | 711 -> One (r572)
  | 713 -> One (r573)
  | 838 | 857 -> One (r574)
  | 837 | 856 -> One (r575)
  | 836 | 855 -> One (r576)
  | 714 | 727 -> One (r577)
  | 730 | 851 -> One (r578)
  | 729 | 850 -> One (r579)
  | 715 | 728 -> One (r580)
  | 849 -> One (r581)
  | 719 -> One (r582)
  | 720 -> One (r584)
  | 722 -> One (r585)
  | 724 -> One (r586)
  | 726 -> One (r587)
  | 830 -> One (r588)
  | 736 -> One (r589)
  | 735 -> One (r590)
  | 740 -> One (r591)
  | 739 -> One (r592)
  | 792 -> One (r593)
  | 743 -> One (r594)
  | 746 -> One (r595)
  | 748 -> One (r596)
  | 750 -> One (r597)
  | 755 -> One (r598)
  | 757 -> One (r599)
  | 759 -> One (r600)
  | 761 -> One (r601)
  | 763 -> One (r602)
  | 765 -> One (r603)
  | 767 -> One (r604)
  | 769 -> One (r605)
  | 771 -> One (r606)
  | 773 -> One (r607)
  | 775 -> One (r608)
  | 777 -> One (r609)
  | 779 -> One (r610)
  | 781 -> One (r611)
  | 783 -> One (r612)
  | 785 -> One (r613)
  | 787 -> One (r614)
  | 789 -> One (r615)
  | 791 -> One (r616)
  | 794 -> One (r617)
  | 796 -> One (r618)
  | 801 -> One (r619)
  | 800 -> One (r620)
  | 827 -> One (r621)
  | 826 -> One (r622)
  | 804 -> One (r623)
  | 809 -> One (r624)
  | 808 -> One (r625)
  | 807 -> One (r626)
  | 812 -> One (r627)
  | 817 -> One (r628)
  | 819 -> One (r629)
  | 821 -> One (r630)
  | 823 -> One (r631)
  | 833 | 854 -> One (r632)
  | 832 | 853 -> One (r633)
  | 831 | 852 -> One (r634)
  | 835 -> One (r635)
  | 840 -> One (r636)
  | 843 -> One (r637)
  | 860 -> One (r638)
  | 862 -> One (r639)
  | 867 -> One (r640)
  | 866 -> One (r641)
  | 870 -> One (r642)
  | 873 -> One (r643)
  | 872 -> One (r644)
  | 880 -> One (r646)
  | 877 -> One (r647)
  | 876 -> One (r649)
  | 875 -> One (r650)
  | 900 -> One (r651)
  | 899 -> One (r652)
  | 884 -> One (r653)
  | 883 -> One (r654)
  | 890 -> One (r655)
  | 892 -> One (r656)
  | 891 | 1018 -> One (r657)
  | 894 -> One (r658)
  | 898 -> One (r659)
  | 923 -> One (r660)
  | 922 -> One (r661)
  | 921 -> One (r662)
  | 920 -> One (r663)
  | 919 -> One (r664)
  | 918 -> One (r665)
  | 938 -> One (r666)
  | 928 -> One (r667)
  | 927 -> One (r668)
  | 930 -> One (r669)
  | 934 -> One (r670)
  | 933 -> One (r671)
  | 932 -> One (r672)
  | 937 -> One (r673)
  | 950 -> One (r675)
  | 956 -> One (r676)
  | 959 -> One (r677)
  | 968 -> One (r678)
  | 975 -> One (r679)
  | 974 -> One (r680)
  | 986 -> One (r681)
  | 985 -> One (r682)
  | 984 -> One (r683)
  | 979 -> One (r684)
  | 978 -> One (r685)
  | 983 -> One (r686)
  | 982 -> One (r687)
  | 981 -> One (r688)
  | 988 -> One (r689)
  | 1017 -> One (r690)
  | 1008 -> One (r691)
  | 1007 -> One (r692)
  | 1006 -> One (r693)
  | 1013 -> One (r694)
  | 1012 -> One (r695)
  | 1016 -> One (r696)
  | 1024 -> One (r697)
  | 1023 -> One (r698)
  | 1022 -> One (r699)
  | 1021 -> One (r700)
  | 1020 -> One (r701)
  | 1029 -> One (r702)
  | 1028 -> One (r703)
  | 1027 -> One (r704)
  | 1026 -> One (r705)
  | 1039 -> One (r706)
  | 1038 -> One (r707)
  | 1037 -> One (r708)
  | 1036 -> One (r709)
  | 1035 -> One (r710)
  | 1034 -> One (r711)
  | 1043 -> One (r712)
  | 1056 -> One (r713)
  | 1055 -> One (r714)
  | 1059 -> One (r715)
  | 1058 -> One (r716)
  | 1062 -> One (r717)
  | 1061 -> One (r718)
  | 1469 -> One (r719)
  | 1468 -> One (r720)
  | 1071 -> One (r721)
  | 1073 -> One (r722)
  | 1075 -> One (r723)
  | 1467 -> One (r724)
  | 1466 -> One (r725)
  | 1077 -> One (r726)
  | 1081 -> One (r727)
  | 1080 -> One (r728)
  | 1079 -> One (r729)
  | 1088 -> One (r730)
  | 1091 -> One (r732)
  | 1090 -> One (r733)
  | 1087 -> One (r734)
  | 1086 -> One (r735)
  | 1085 -> One (r736)
  | 1084 -> One (r737)
  | 1083 -> One (r738)
  | 1098 -> One (r739)
  | 1097 -> One (r740)
  | 1096 -> One (r741)
  | 1095 -> One (r742)
  | 1101 -> One (r745)
  | 1100 -> One (r746)
  | 1099 -> One (r747)
  | 1133 -> One (r748)
  | 1132 -> One (r749)
  | 1131 -> One (r750)
  | 1302 -> One (r751)
  | 1301 -> One (r752)
  | 1126 -> One (r753)
  | 1125 -> One (r754)
  | 1124 -> One (r755)
  | 1123 -> One (r756)
  | 1122 -> One (r757)
  | 1105 -> One (r758)
  | 1113 -> One (r759)
  | 1112 -> One (r760)
  | 1121 -> One (r762)
  | 1120 -> One (r763)
  | 1116 -> One (r764)
  | 1115 -> One (r765)
  | 1114 -> One (r766)
  | 1111 -> One (r767)
  | 1110 -> One (r768)
  | 1109 -> One (r769)
  | 1119 -> One (r770)
  | 1118 -> One (r771)
  | 1130 -> One (r772)
  | 1129 -> One (r773)
  | 1128 -> One (r774)
  | 1188 -> One (r775)
  | 1197 -> One (r777)
  | 1213 -> One (r779)
  | 1212 -> One (r780)
  | 1144 -> One (r781)
  | 1143 -> One (r782)
  | 1142 -> One (r783)
  | 1138 -> One (r784)
  | 1136 -> One (r785)
  | 1135 -> One (r786)
  | 1141 -> One (r787)
  | 1140 -> One (r788)
  | 1153 -> One (r789)
  | 1152 -> One (r790)
  | 1151 -> One (r792)
  | 1150 -> One (r793)
  | 1146 -> One (r794)
  | 1149 -> One (r795)
  | 1148 -> One (r796)
  | 1170 -> One (r797)
  | 1169 -> One (r798)
  | 1168 -> One (r799)
  | 1167 -> One (r801)
  | 1166 -> One (r802)
  | 1155 -> One (r803)
  | 1160 -> One (r804)
  | 1159 -> One (r805)
  | 1158 -> One (r806)
  | 1157 -> One (r807)
  | 1165 -> One (r808)
  | 1164 -> One (r809)
  | 1163 -> One (r810)
  | 1162 -> One (r811)
  | 1185 -> One (r812)
  | 1184 -> One (r814)
  | 1183 -> One (r815)
  | 1179 -> One (r816)
  | 1178 -> One (r817)
  | 1177 -> One (r818)
  | 1172 -> One (r819)
  | 1182 -> One (r820)
  | 1181 -> One (r821)
  | 1199 -> One (r822)
  | 1198 -> One (r823)
  | 1187 -> One (r824)
  | 1195 -> One (r826)
  | 1191 -> One (r827)
  | 1190 -> One (r828)
  | 1194 -> One (r829)
  | 1193 -> One (r830)
  | 1205 -> One (r831)
  | 1204 -> One (r832)
  | 1203 -> One (r833)
  | 1207 -> One (r835)
  | 1206 -> One (r836)
  | 1202 -> One (r837)
  | 1209 -> One (r838)
  | 1240 -> One (r839)
  | 1245 -> One (r841)
  | 1244 -> One (r842)
  | 1218 -> One (r843)
  | 1217 -> One (r844)
  | 1216 -> One (r845)
  | 1215 -> One (r846)
  | 1243 -> One (r847)
  | 1223 -> One (r848)
  | 1222 -> One (r849)
  | 1221 -> One (r850)
  | 1220 -> One (r851)
  | 1242 -> One (r852)
  | 1226 -> One (r853)
  | 1225 -> One (r854)
  | 1241 -> One (r855)
  | 1230 -> One (r856)
  | 1229 -> One (r857)
  | 1239 -> One (r858)
  | 1234 -> One (r859)
  | 1254 -> One (r860)
  | 1253 -> One (r861)
  | 1252 -> One (r862)
  | 1251 -> One (r863)
  | 1250 -> One (r864)
  | 1249 -> One (r865)
  | 1258 -> One (r866)
  | 1268 -> One (r867)
  | 1267 -> One (r868)
  | 1266 -> One (r869)
  | 1265 -> One (r870)
  | 1264 -> One (r871)
  | 1277 -> One (r872)
  | 1287 -> One (r873)
  | 1286 -> One (r874)
  | 1285 -> One (r875)
  | 1284 -> One (r876)
  | 1283 -> One (r877)
  | 1282 -> One (r878)
  | 1281 -> One (r879)
  | 1298 -> One (r880)
  | 1297 -> One (r881)
  | 1296 -> One (r882)
  | 1295 -> One (r883)
  | 1294 -> One (r884)
  | 1293 -> One (r885)
  | 1292 -> One (r886)
  | 1388 -> One (r887)
  | 1386 -> One (r889)
  | 1412 -> One (r891)
  | 1309 -> One (r892)
  | 1420 -> One (r894)
  | 1419 -> One (r895)
  | 1308 -> One (r896)
  | 1307 -> One (r897)
  | 1306 -> One (r898)
  | 1313 -> One (r899)
  | 1312 -> One (r900)
  | 1311 -> One (r901)
  | 1334 -> One (r902)
  | 1333 -> One (r903)
  | 1332 -> One (r904)
  | 1331 -> One (r905)
  | 1320 -> One (r906)
  | 1319 -> One (r907)
  | 1318 -> One (r909)
  | 1317 -> One (r910)
  | 1325 -> One (r911)
  | 1324 -> One (r912)
  | 1323 -> One (r913)
  | 1322 -> One (r914)
  | 1330 -> One (r915)
  | 1329 -> One (r916)
  | 1328 -> One (r917)
  | 1337 -> One (r918)
  | 1336 -> One (r919)
  | 1363 -> One (r920)
  | 1352 -> One (r921)
  | 1351 -> One (r922)
  | 1340 -> One (r923)
  | 1339 -> One (r924)
  | 1365 -> One (r926)
  | 1364 -> One (r927)
  | 1345 -> One (r928)
  | 1344 -> One (r929)
  | 1343 -> One (r930)
  | 1342 -> One (r931)
  | 1350 -> One (r932)
  | 1349 -> One (r933)
  | 1348 -> One (r934)
  | 1362 -> One (r935)
  | 1361 -> One (r936)
  | 1360 -> One (r937)
  | 1359 -> One (r938)
  | 1358 -> One (r939)
  | 1357 -> One (r940)
  | 1356 -> One (r941)
  | 1355 -> One (r942)
  | 1369 -> One (r943)
  | 1368 -> One (r944)
  | 1367 -> One (r945)
  | 1403 -> One (r946)
  | 1402 -> One (r947)
  | 1399 -> One (r948)
  | 1372 -> One (r949)
  | 1371 -> One (r950)
  | 1395 -> One (r951)
  | 1394 -> One (r952)
  | 1378 -> One (r953)
  | 1377 -> One (r954)
  | 1376 -> One (r955)
  | 1391 -> One (r956)
  | 1382 -> One (r957)
  | 1381 -> One (r958)
  | 1393 -> One (r960)
  | 1380 -> One (r961)
  | 1389 -> One (r962)
  | 1384 -> One (r963)
  | 1398 -> One (r964)
  | 1397 -> One (r965)
  | 1401 -> One (r966)
  | 1407 -> One (r967)
  | 1406 -> One (r968)
  | 1405 -> One (r969)
  | 1409 -> One (r970)
  | 1416 -> One (r971)
  | 1415 -> One (r972)
  | 1414 -> One (r973)
  | 1418 -> One (r974)
  | 1459 -> One (r975)
  | 1426 -> One (r976)
  | 1436 -> One (r977)
  | 1435 -> One (r978)
  | 1434 -> One (r979)
  | 1433 -> One (r980)
  | 1446 -> One (r981)
  | 1456 -> One (r982)
  | 1455 -> One (r983)
  | 1454 -> One (r984)
  | 1453 -> One (r985)
  | 1452 -> One (r986)
  | 1451 -> One (r987)
  | 1462 -> One (r988)
  | 1461 -> One (r989)
  | 1481 -> One (r990)
  | 1480 -> One (r991)
  | 1479 -> One (r992)
  | 1478 -> One (r993)
  | 1477 -> One (r994)
  | 1476 -> One (r995)
  | 1486 -> One (r996)
  | 1485 -> One (r997)
  | 1501 -> One (r998)
  | 1499 -> One (r1000)
  | 1498 -> One (r1001)
  | 1495 -> One (r1002)
  | 1494 -> One (r1003)
  | 1526 -> One (r1004)
  | 1525 -> One (r1005)
  | 1528 -> One (r1006)
  | 1530 -> One (r1007)
  | 1544 -> One (r1008)
  | 1548 -> One (r1009)
  | 1552 -> One (r1010)
  | 693 -> Select (function
    | -1 -> [R 97]
    | _ -> r564)
  | 413 -> Select (function
    | -1 -> S (T T_TYPE) :: r305
    | _ -> R 191 :: r300)
  | 1092 -> Select (function
    | -1 -> r750
    | _ -> R 191 :: r744)
  | 492 -> Select (function
    | -1 -> S (T T_UIDENT) :: r360
    | _ -> r300)
  | 504 -> Select (function
    | -1 -> S (T T_RPAREN) :: r47
    | _ -> r46)
  | 641 -> Select (function
    | -1 -> S (T T_RBRACKET) :: r254
    | _ -> Sub (r486) :: r489)
  | 634 -> Select (function
    | 59 | 89 | 330 | 379 | 412 | 1071 | 1077 | 1423 -> r478
    | _ -> S (T T_EXCEPTION) :: r473)
  | 172 -> Select (function
    | 1018 -> r79
    | _ -> Sub (r77) :: r141)
  | 338 -> Select (function
    | 351 -> r247
    | _ -> Sub (r118) :: r253)
  | 173 -> Select (function
    | 1018 -> r78
    | _ -> r141)
  | 339 -> Select (function
    | 337 -> r253
    | _ -> r246)
  | 1094 -> Select (function
    | -1 -> r748
    | _ -> r743)
  | 1093 -> Select (function
    | -1 -> r749
    | _ -> r744)
  | _ -> raise Not_found
