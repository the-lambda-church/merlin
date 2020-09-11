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
  [|0;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;2;3;2;1;2;1;2;1;1;2;1;2;1;1;1;1;1;2;1;1;2;3;3;3;1;2;1;2;1;1;2;1;1;1;2;1;2;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;2;3;3;4;1;1;1;2;1;1;1;2;1;2;3;1;1;2;3;1;1;2;1;2;1;3;1;1;1;1;1;1;2;2;2;3;2;3;1;4;5;1;1;1;2;1;2;1;2;1;1;2;1;2;3;1;2;1;2;1;1;2;1;1;2;2;1;2;1;2;1;1;1;2;3;2;1;2;3;4;2;3;2;3;1;3;4;2;3;1;2;1;3;1;1;2;1;1;3;2;3;1;1;2;3;2;3;4;4;2;3;5;2;1;2;3;2;3;4;5;3;4;3;4;4;5;6;2;1;1;2;3;2;3;3;4;5;6;1;1;2;1;3;4;5;2;3;1;2;3;4;5;4;2;3;2;1;1;2;1;1;1;1;1;2;1;1;1;1;2;3;4;5;6;7;2;3;1;2;1;1;2;1;2;2;3;4;3;4;3;3;2;1;1;2;3;1;2;2;3;4;5;2;3;1;4;4;5;6;7;5;2;6;7;1;2;1;2;3;4;5;6;7;1;2;3;1;1;2;1;1;2;4;5;3;4;8;9;1;2;2;2;1;1;1;2;3;4;2;3;1;1;1;1;2;3;3;3;3;3;1;3;2;3;1;1;1;1;1;2;3;4;5;1;2;1;1;1;2;1;2;2;1;2;2;1;1;2;3;4;5;5;1;2;3;4;1;2;1;2;3;4;1;1;1;2;1;1;1;2;2;3;1;4;2;1;1;1;1;2;2;2;3;2;3;1;2;1;1;2;3;3;1;2;4;5;6;2;1;2;3;3;3;4;5;1;1;2;3;4;2;3;4;3;2;3;1;5;2;3;2;1;2;3;3;1;1;3;4;5;2;1;2;3;2;5;6;2;3;1;1;2;3;1;1;1;2;1;2;1;1;1;2;3;1;2;3;4;5;2;3;3;4;2;1;1;4;5;5;6;7;1;1;1;1;1;2;1;3;1;1;1;1;2;3;1;2;3;1;4;3;1;1;2;2;3;1;2;1;1;1;1;1;2;1;1;1;1;1;1;2;3;1;1;1;2;3;2;3;2;1;2;1;2;3;4;4;5;2;3;1;1;2;2;3;2;3;3;4;2;2;3;3;4;1;3;3;2;3;3;4;5;3;1;1;4;2;2;3;4;1;1;2;1;1;2;3;3;4;2;1;2;3;1;1;2;3;4;5;6;1;1;1;2;1;2;1;1;1;1;1;2;1;2;1;2;1;2;3;1;2;1;2;1;1;2;1;2;3;3;4;5;1;2;3;4;1;2;5;6;1;1;2;3;4;1;2;3;4;1;1;2;1;1;2;3;4;4;5;6;7;8;9;1;1;1;1;1;2;3;2;3;2;3;2;3;1;1;1;1;2;1;2;1;2;1;2;2;3;4;5;6;1;2;1;2;3;1;1;2;3;1;1;2;1;2;3;2;3;2;3;2;1;2;1;2;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;3;2;3;2;3;1;2;3;4;5;3;1;2;1;2;3;4;5;1;2;3;1;2;3;2;3;2;3;2;3;2;3;2;1;3;4;2;2;7;3;4;5;6;7;3;4;5;6;7;3;4;5;2;1;2;1;1;2;4;5;3;4;5;3;4;5;3;4;5;3;4;4;2;3;5;6;1;3;4;4;5;6;3;4;5;1;2;3;2;3;1;2;3;1;2;3;1;1;2;1;2;3;4;1;2;3;4;4;5;1;2;3;1;5;1;1;6;7;8;9;10;5;6;7;8;4;5;6;7;8;9;10;2;1;2;3;4;1;2;1;2;3;4;1;1;2;5;7;3;4;3;4;5;2;3;3;4;2;3;1;3;4;5;6;7;3;3;4;3;2;3;2;2;3;3;2;3;2;3;4;2;2;3;4;7;2;3;4;1;2;3;4;5;6;7;1;2;2;3;4;5;6;1;2;3;2;3;4;5;2;4;5;2;1;2;3;4;1;2;1;2;3;1;1;2;5;2;3;4;5;6;7;8;3;4;5;6;7;2;4;5;2;3;4;5;6;7;8;2;3;3;4;3;4;5;6;3;4;5;6;4;5;5;6;7;5;6;7;7;8;9;2;3;3;4;5;2;4;5;3;4;5;6;1;2;1;2;3;4;1;2;1;2;3;4;1;2;3;4;5;6;1;2;7;8;1;2;3;4;5;6;7;4;5;6;1;1;1;2;3;1;2;3;4;5;1;2;6;2;3;4;5;6;4;5;3;4;5;6;7;1;2;3;4;1;2;3;1;2;3;1;4;1;2;3;5;6;7;1;2;1;2;3;3;4;1;2;1;2;1;2;3;4;5;1;2;3;4;5;3;4;1;2;3;1;2;1;2;1;2;3;4;5;1;2;3;6;7;1;1;2;1;1;2;3;1;2;3;4;1;1;3;4;2;1;2;1;2;3;3;4;1;2;1;2;8;9;2;3;4;5;6;1;2;3;4;5;1;2;3;1;2;3;4;1;1;1;2;1;2;3;1;3;1;5;4;6;7;8;1;1;1;2;3;4;5;6;7;2;1;1;2;1;1;1;1;1;2;3;4;5;6;2;1;1;1;1;1;1;1;2;1;1;1;2;3;4;5;6;7;8;2;1;1;1;2;3;4;5;6;7;8;2;1;2;2;2;1;2;3;4;5;1;1;2;3;4;1;2;1;2;3;1;2;3;4;5;6;7;3;4;5;6;7;3;4;5;6;5;6;7;1;1;2;3;4;5;6;7;3;4;5;6;7;3;4;5;6;7;8;9;10;11;6;7;8;5;2;3;1;2;3;4;1;2;3;1;1;1;2;3;4;1;2;1;2;1;2;1;1;2;1;3;2;3;2;3;2;3;3;4;5;4;1;2;5;6;1;2;3;4;1;2;1;2;2;1;2;3;4;1;2;6;7;1;1;1;1;1;2;1;1;1;1;1;1;2;3;4;5;2;1;1;1;1;1;1;1;1;2;1;1;1;1;2;3;4;5;6;7;2;1;2;1;2;3;1;1;1;3;4;3;4;3;4;5;6;7;2;3;4;5;6;7;8;2;3;3;4;5;3;4;2;3;4;8;5;6;7;1;2;8;9;2;1;1;1;3;4;4;5;2;3;4;4;5;6;5;6;3;4;2;3;4;5;6;3;4;5;6;7;8;4;5;3;4;5;6;3;4;3;2;3;4;5;6;1;2;1;0;1;2;1;0;1;2;1;|]

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
  let r0 = [R 452] in
  let r1 = S (N N_expr) :: r0 in
  let r2 = [R 133] in
  let r3 = S (T T_DONE) :: r2 in
  let r4 = Sub (r1) :: r3 in
  let r5 = S (T T_DO) :: r4 in
  let r6 = Sub (r1) :: r5 in
  let r7 = [R 572] in
  let r8 = S (T T_AND) :: r7 in
  let r9 = [R 14] in
  let r10 = Sub (r8) :: r9 in
  let r11 = [R 193] in
  let r12 = R 17 :: r11 in
  let r13 = [R 15] in
  let r14 = [R 416] in
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
  let r29 = R 366 :: r28 in
  let r30 = S (T T_WITH) :: r29 in
  let r31 = Sub (r1) :: r30 in
  let r32 = [R 549] in
  let r33 = S (T T_QUESTIONQUESTION) :: r32 in
  let r34 = [R 537] in
  let r35 = [R 48] in
  let r36 = S (T T_LIDENT) :: r35 in
  let r37 = [R 539] in
  let r38 = Sub (r36) :: r37 in
  let r39 = [R 49] in
  let r40 = S (T T_LIDENT) :: r39 in
  let r41 = [R 296] in
  let r42 = [R 192] in
  let r43 = [R 18] in
  let r44 = [R 518] in
  let r45 = S (T T_RPAREN) :: r44 in
  let r46 = Sub (r1) :: r45 in
  let r47 = [R 99] in
  let r48 = [R 697] in
  let r49 = [R 194] in
  let r50 = S (T T_RBRACKET) :: r49 in
  let r51 = Sub (r15) :: r50 in
  let r52 = S (T T_LIDENT) :: r48 in
  let r53 = [R 488] in
  let r54 = S (T T_UNDERSCORE) :: r53 in
  let r55 = [R 485] in
  let r56 = Sub (r54) :: r55 in
  let r57 = [R 506] in
  let r58 = Sub (r56) :: r57 in
  let r59 = [R 114] in
  let r60 = Sub (r58) :: r59 in
  let r61 = [R 123] in
  let r62 = Sub (r60) :: r61 in
  let r63 = [R 112] in
  let r64 = Sub (r62) :: r63 in
  let r65 = [R 705] in
  let r66 = R 426 :: r65 in
  let r67 = Sub (r64) :: r66 in
  let r68 = S (T T_COLON) :: r67 in
  let r69 = Sub (r52) :: r68 in
  let r70 = [R 360] in
  let r71 = S (T T_AMPERAMPER) :: r70 in
  let r72 = [R 698] in
  let r73 = S (T T_RPAREN) :: r72 in
  let r74 = [R 292] in
  let r75 = [R 494] in
  let r76 = [R 221] in
  let r77 = S (T T_LIDENT) :: r76 in
  let r78 = [R 487] in
  let r79 = Sub (r77) :: r78 in
  let r80 = [R 115] in
  let r81 = Sub (r60) :: r80 in
  let r82 = S (T T_MINUSGREATER) :: r81 in
  let r83 = Sub (r60) :: r82 in
  let r84 = S (T T_COLON) :: r83 in
  let r85 = [R 116] in
  let r86 = Sub (r60) :: r85 in
  let r87 = S (T T_MINUSGREATER) :: r86 in
  let r88 = [R 386] in
  let r89 = S (N N_module_type) :: r88 in
  let r90 = [R 504] in
  let r91 = S (T T_RPAREN) :: r90 in
  let r92 = Sub (r89) :: r91 in
  let r93 = R 191 :: r92 in
  let r94 = [R 320] in
  let r95 = S (T T_END) :: r94 in
  let r96 = R 462 :: r95 in
  let r97 = [R 670] in
  let r98 = R 426 :: r97 in
  let r99 = R 105 :: r98 in
  let r100 = R 673 :: r99 in
  let r101 = S (T T_LIDENT) :: r100 in
  let r102 = R 379 :: r101 in
  let r103 = R 338 :: r102 in
  let r104 = R 191 :: r103 in
  let r105 = [R 383] in
  let r106 = S (T T_UNDERSCORE) :: r105 in
  let r107 = [R 376] in
  let r108 = Sub (r106) :: r107 in
  let r109 = R 692 :: r108 in
  let r110 = [R 377] in
  let r111 = Sub (r109) :: r110 in
  let r112 = [R 381] in
  let r113 = S (T T_RPAREN) :: r112 in
  let r114 = [R 382] in
  let r115 = [R 378] in
  let r116 = [R 678] in
  let r117 = [R 95] in
  let r118 = S (T T_FALSE) :: r117 in
  let r119 = [R 108] in
  let r120 = R 17 :: r119 in
  let r121 = R 216 :: r120 in
  let r122 = Sub (r118) :: r121 in
  let r123 = [R 109] in
  let r124 = Sub (r122) :: r123 in
  let r125 = [R 677] in
  let r126 = [R 93] in
  let r127 = [R 683] in
  let r128 = [R 117] in
  let r129 = Sub (r60) :: r128 in
  let r130 = S (T T_MINUSGREATER) :: r129 in
  let r131 = [R 493] in
  let r132 = [R 225] in
  let r133 = [R 492] in
  let r134 = [R 423] in
  let r135 = Sub (r62) :: r134 in
  let r136 = [R 202] in
  let r137 = R 17 :: r136 in
  let r138 = S (T T_SEMI) :: r137 in
  let r139 = R 17 :: r138 in
  let r140 = Sub (r135) :: r139 in
  let r141 = [R 695] in
  let r142 = [R 449] in
  let r143 = Sub (r56) :: r142 in
  let r144 = [R 450] in
  let r145 = Sub (r143) :: r144 in
  let r146 = [R 502] in
  let r147 = S (T T_RBRACKET) :: r146 in
  let r148 = Sub (r145) :: r147 in
  let r149 = [R 501] in
  let r150 = [R 500] in
  let r151 = S (T T_RBRACKET) :: r150 in
  let r152 = [R 498] in
  let r153 = S (T T_RBRACKET) :: r152 in
  let r154 = Sub (r145) :: r153 in
  let r155 = [R 335] in
  let r156 = Sub (r77) :: r155 in
  let r157 = [R 495] in
  let r158 = [R 684] in
  let r159 = S (T T_LIDENT) :: r158 in
  let r160 = S (T T_DOT) :: r159 in
  let r161 = S (T T_UIDENT) :: r74 in
  let r162 = [R 294] in
  let r163 = S (T T_RPAREN) :: r162 in
  let r164 = [R 293] in
  let r165 = [R 451] in
  let r166 = [R 659] in
  let r167 = [R 5] in
  let r168 = Sub (r62) :: r167 in
  let r169 = [R 658] in
  let r170 = R 17 :: r169 in
  let r171 = Sub (r168) :: r170 in
  let r172 = [R 121] in
  let r173 = Sub (r56) :: r172 in
  let r174 = [R 507] in
  let r175 = [R 122] in
  let r176 = [R 118] in
  let r177 = [R 124] in
  let r178 = Sub (r77) :: r177 in
  let r179 = [R 6] in
  let r180 = [R 497] in
  let r181 = [R 499] in
  let r182 = S (T T_RBRACKET) :: r181 in
  let r183 = Sub (r145) :: r182 in
  let r184 = S (T T_BACKQUOTE) :: r156 in
  let r185 = [R 336] in
  let r186 = Sub (r184) :: r185 in
  let r187 = [R 503] in
  let r188 = S (T T_RBRACKET) :: r187 in
  let r189 = [R 424] in
  let r190 = Sub (r62) :: r189 in
  let r191 = [R 696] in
  let r192 = [R 94] in
  let r193 = [R 486] in
  let r194 = [R 496] in
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
  let r210 = R 333 :: r209 in
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
  let r224 = [R 679] in
  let r225 = S (T T_RBRACE) :: r224 in
  let r226 = Sub (r212) :: r225 in
  let r227 = [R 681] in
  let r228 = [R 680] in
  let r229 = [R 682] in
  let r230 = S (T T_RBRACE) :: r229 in
  let r231 = [R 425] in
  let r232 = S (T T_RBRACKET) :: r231 in
  let r233 = Sub (r15) :: r232 in
  let r234 = [R 195] in
  let r235 = R 17 :: r234 in
  let r236 = R 216 :: r235 in
  let r237 = Sub (r118) :: r236 in
  let r238 = [R 623] in
  let r239 = Sub (r237) :: r238 in
  let r240 = [R 630] in
  let r241 = R 426 :: r240 in
  let r242 = Sub (r239) :: r241 in
  let r243 = R 431 :: r242 in
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
  let r255 = [R 427] in
  let r256 = [R 196] in
  let r257 = R 17 :: r256 in
  let r258 = Sub (r249) :: r257 in
  let r259 = [R 660] in
  let r260 = [R 654] in
  let r261 = S (T T_UIDENT) :: r24 in
  let r262 = [R 340] in
  let r263 = R 426 :: r262 in
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
  let r274 = [R 554] in
  let r275 = S (T T_LIDENT) :: r127 in
  let r276 = [R 559] in
  let r277 = [R 483] in
  let r278 = [R 481] in
  let r279 = [R 565] in
  let r280 = S (T T_RPAREN) :: r279 in
  let r281 = [R 567] in
  let r282 = S (T T_RPAREN) :: r281 in
  let r283 = S (T T_UIDENT) :: r282 in
  let r284 = [R 568] in
  let r285 = S (T T_RPAREN) :: r284 in
  let r286 = [R 324] in
  let r287 = S (N N_module_expr) :: r286 in
  let r288 = R 17 :: r287 in
  let r289 = S (T T_OF) :: r288 in
  let r290 = [R 307] in
  let r291 = S (T T_END) :: r290 in
  let r292 = S (N N_structure) :: r291 in
  let r293 = [R 299] in
  let r294 = S (N N_module_expr) :: r293 in
  let r295 = S (T T_EQUAL) :: r294 in
  let r296 = [R 440] in
  let r297 = R 426 :: r296 in
  let r298 = Sub (r295) :: r297 in
  let r299 = S (T T_UIDENT) :: r298 in
  let r300 = S (T T_REC) :: r299 in
  let r301 = [R 328] in
  let r302 = R 426 :: r301 in
  let r303 = R 329 :: r302 in
  let r304 = Sub (r77) :: r303 in
  let r305 = R 191 :: r304 in
  let r306 = [R 330] in
  let r307 = [R 325] in
  let r308 = S (T T_RPAREN) :: r307 in
  let r309 = [R 321] in
  let r310 = S (N N_module_type) :: r309 in
  let r311 = S (T T_MINUSGREATER) :: r310 in
  let r312 = S (N N_functor_args) :: r311 in
  let r313 = [R 210] in
  let r314 = [R 211] in
  let r315 = S (T T_RPAREN) :: r314 in
  let r316 = S (N N_module_type) :: r315 in
  let r317 = [R 714] in
  let r318 = Sub (r161) :: r317 in
  let r319 = S (T T_COLONEQUAL) :: r318 in
  let r320 = S (T T_UIDENT) :: r319 in
  let r321 = S (T T_MODULE) :: r320 in
  let r322 = [R 715] in
  let r323 = Sub (r321) :: r322 in
  let r324 = [R 323] in
  let r325 = [R 712] in
  let r326 = Sub (r62) :: r325 in
  let r327 = S (T T_COLONEQUAL) :: r326 in
  let r328 = Sub (r204) :: r327 in
  let r329 = [R 691] in
  let r330 = Sub (r77) :: r329 in
  let r331 = S (T T_QUOTE) :: r330 in
  let r332 = [R 685] in
  let r333 = Sub (r331) :: r332 in
  let r334 = R 692 :: r333 in
  let r335 = [R 686] in
  let r336 = Sub (r334) :: r335 in
  let r337 = [R 690] in
  let r338 = S (T T_RPAREN) :: r337 in
  let r339 = [R 687] in
  let r340 = [R 239] in
  let r341 = S (T T_LIDENT) :: r340 in
  let r342 = [R 717] in
  let r343 = S (T T_EQUAL) :: r342 in
  let r344 = [R 711] in
  let r345 = R 105 :: r344 in
  let r346 = Sub (r62) :: r345 in
  let r347 = [R 102] in
  let r348 = Sub (r64) :: r347 in
  let r349 = S (T T_EQUAL) :: r348 in
  let r350 = Sub (r64) :: r349 in
  let r351 = [R 104] in
  let r352 = [R 713] in
  let r353 = Sub (r161) :: r352 in
  let r354 = [R 716] in
  let r355 = [R 322] in
  let r356 = [R 332] in
  let r357 = Sub (r77) :: r356 in
  let r358 = [R 298] in
  let r359 = R 426 :: r358 in
  let r360 = Sub (r295) :: r359 in
  let r361 = [R 312] in
  let r362 = S (T T_RPAREN) :: r361 in
  let r363 = [R 313] in
  let r364 = S (T T_RPAREN) :: r363 in
  let r365 = S (N N_expr) :: r364 in
  let r366 = [R 128] in
  let r367 = S (N N_match_cases) :: r366 in
  let r368 = R 366 :: r367 in
  let r369 = S (T T_WITH) :: r368 in
  let r370 = Sub (r1) :: r369 in
  let r371 = [R 145] in
  let r372 = S (N N_match_cases) :: r371 in
  let r373 = R 366 :: r372 in
  let r374 = S (T T_WITH) :: r373 in
  let r375 = Sub (r1) :: r374 in
  let r376 = [R 545] in
  let r377 = S (T T_RPAREN) :: r376 in
  let r378 = S (N N_module_expr) :: r377 in
  let r379 = [R 308] in
  let r380 = S (N N_module_expr) :: r379 in
  let r381 = S (T T_MINUSGREATER) :: r380 in
  let r382 = S (N N_functor_args) :: r381 in
  let r383 = [R 310] in
  let r384 = [R 309] in
  let r385 = [R 546] in
  let r386 = S (T T_RPAREN) :: r385 in
  let r387 = [R 400] in
  let r388 = S (N N_pattern) :: r387 in
  let r389 = Sub (r249) :: r388 in
  let r390 = [R 409] in
  let r391 = Sub (r389) :: r390 in
  let r392 = [R 266] in
  let r393 = Sub (r1) :: r392 in
  let r394 = S (T T_EQUAL) :: r393 in
  let r395 = Sub (r391) :: r394 in
  let r396 = [R 275] in
  let r397 = R 426 :: r396 in
  let r398 = Sub (r395) :: r397 in
  let r399 = R 438 :: r398 in
  let r400 = [R 511] in
  let r401 = [R 412] in
  let r402 = S (N N_pattern) :: r401 in
  let r403 = [R 509] in
  let r404 = S (T T_RBRACKET) :: r403 in
  let r405 = R 372 :: r404 in
  let r406 = [R 238] in
  let r407 = S (T T_LIDENT) :: r406 in
  let r408 = [R 257] in
  let r409 = R 371 :: r408 in
  let r410 = Sub (r407) :: r409 in
  let r411 = [R 258] in
  let r412 = Sub (r410) :: r411 in
  let r413 = [R 508] in
  let r414 = S (T T_RBRACE) :: r413 in
  let r415 = [R 260] in
  let r416 = [R 370] in
  let r417 = [R 256] in
  let r418 = S (T T_UNDERSCORE) :: r274 in
  let r419 = [R 553] in
  let r420 = Sub (r418) :: r419 in
  let r421 = [R 403] in
  let r422 = Sub (r420) :: r421 in
  let r423 = [R 87] in
  let r424 = [R 394] in
  let r425 = S (N N_pattern) :: r424 in
  let r426 = S (T T_INT) :: r423 in
  let r427 = [R 480] in
  let r428 = Sub (r426) :: r427 in
  let r429 = [R 556] in
  let r430 = [R 397] in
  let r431 = [R 392] in
  let r432 = [R 401] in
  let r433 = [R 562] in
  let r434 = S (T T_RBRACKET) :: r433 in
  let r435 = S (T T_LBRACKET) :: r434 in
  let r436 = [R 563] in
  let r437 = [R 564] in
  let r438 = [R 398] in
  let r439 = [R 393] in
  let r440 = [R 390] in
  let r441 = [R 566] in
  let r442 = S (T T_RPAREN) :: r441 in
  let r443 = [R 510] in
  let r444 = S (T T_BARRBRACKET) :: r443 in
  let r445 = [R 631] in
  let r446 = Sub (r1) :: r445 in
  let r447 = S (T T_EQUAL) :: r446 in
  let r448 = [R 204] in
  let r449 = Sub (r447) :: r448 in
  let r450 = [R 263] in
  let r451 = [R 240] in
  let r452 = S (T T_LIDENT) :: r451 in
  let r453 = [R 248] in
  let r454 = [R 236] in
  let r455 = Sub (r452) :: r454 in
  let r456 = [R 247] in
  let r457 = S (T T_RPAREN) :: r456 in
  let r458 = [R 237] in
  let r459 = [R 244] in
  let r460 = [R 243] in
  let r461 = S (T T_RPAREN) :: r460 in
  let r462 = R 368 :: r461 in
  let r463 = [R 369] in
  let r464 = [R 270] in
  let r465 = R 17 :: r464 in
  let r466 = R 216 :: r465 in
  let r467 = Sub (r118) :: r466 in
  let r468 = [R 140] in
  let r469 = Sub (r1) :: r468 in
  let r470 = S (T T_IN) :: r469 in
  let r471 = Sub (r467) :: r470 in
  let r472 = R 191 :: r471 in
  let r473 = [R 262] in
  let r474 = R 426 :: r473 in
  let r475 = Sub (r395) :: r474 in
  let r476 = R 438 :: r475 in
  let r477 = R 191 :: r476 in
  let r478 = [R 141] in
  let r479 = Sub (r1) :: r478 in
  let r480 = S (T T_IN) :: r479 in
  let r481 = Sub (r261) :: r480 in
  let r482 = R 191 :: r481 in
  let r483 = [R 531] in
  let r484 = [R 189] in
  let r485 = S (N N_expr) :: r484 in
  let r486 = [R 534] in
  let r487 = S (T T_RBRACKET) :: r486 in
  let r488 = R 372 :: r487 in
  let r489 = [R 541] in
  let r490 = [R 199] in
  let r491 = [R 198] in
  let r492 = [R 252] in
  let r493 = R 375 :: r492 in
  let r494 = Sub (r407) :: r493 in
  let r495 = [R 253] in
  let r496 = Sub (r494) :: r495 in
  let r497 = [R 447] in
  let r498 = Sub (r496) :: r497 in
  let r499 = [R 528] in
  let r500 = S (T T_RBRACE) :: r499 in
  let r501 = [R 513] in
  let r502 = [R 512] in
  let r503 = S (T T_GREATERDOT) :: r502 in
  let r504 = [R 184] in
  let r505 = Sub (r33) :: r504 in
  let r506 = [R 520] in
  let r507 = S (T T_END) :: r506 in
  let r508 = [R 151] in
  let r509 = S (N N_expr) :: r508 in
  let r510 = S (T T_THEN) :: r509 in
  let r511 = Sub (r1) :: r510 in
  let r512 = [R 142] in
  let r513 = S (N N_match_cases) :: r512 in
  let r514 = R 366 :: r513 in
  let r515 = [R 278] in
  let r516 = Sub (r1) :: r515 in
  let r517 = S (T T_MINUSGREATER) :: r516 in
  let r518 = [R 279] in
  let r519 = Sub (r1) :: r518 in
  let r520 = S (T T_MINUSGREATER) :: r519 in
  let r521 = [R 250] in
  let r522 = Sub (r420) :: r521 in
  let r523 = [R 206] in
  let r524 = Sub (r1) :: r523 in
  let r525 = S (T T_MINUSGREATER) :: r524 in
  let r526 = [R 143] in
  let r527 = Sub (r525) :: r526 in
  let r528 = Sub (r522) :: r527 in
  let r529 = [R 415] in
  let r530 = S (T T_UNDERSCORE) :: r529 in
  let r531 = [R 246] in
  let r532 = [R 245] in
  let r533 = S (T T_RPAREN) :: r532 in
  let r534 = R 368 :: r533 in
  let r535 = [R 272] in
  let r536 = [R 273] in
  let r537 = S (T T_LIDENT) :: r536 in
  let r538 = [R 144] in
  let r539 = Sub (r525) :: r538 in
  let r540 = S (T T_RPAREN) :: r539 in
  let r541 = [R 135] in
  let r542 = S (T T_DONE) :: r541 in
  let r543 = Sub (r1) :: r542 in
  let r544 = S (T T_DO) :: r543 in
  let r545 = Sub (r1) :: r544 in
  let r546 = S (T T_IN) :: r545 in
  let r547 = S (N N_pattern) :: r546 in
  let r548 = [R 126] in
  let r549 = S (T T_DOWNTO) :: r548 in
  let r550 = [R 153] in
  let r551 = S (T T_DONE) :: r550 in
  let r552 = Sub (r1) :: r551 in
  let r553 = S (T T_DO) :: r552 in
  let r554 = Sub (r1) :: r553 in
  let r555 = Sub (r549) :: r554 in
  let r556 = Sub (r1) :: r555 in
  let r557 = S (T T_EQUAL) :: r556 in
  let r558 = S (N N_pattern) :: r557 in
  let r559 = [R 538] in
  let r560 = [R 524] in
  let r561 = S (T T_RPAREN) :: r560 in
  let r562 = S (T T_LPAREN) :: r561 in
  let r563 = S (T T_DOT) :: r562 in
  let r564 = [R 547] in
  let r565 = S (T T_RPAREN) :: r564 in
  let r566 = Sub (r89) :: r565 in
  let r567 = S (T T_COLON) :: r566 in
  let r568 = S (N N_module_expr) :: r567 in
  let r569 = [R 183] in
  let r570 = Sub (r33) :: r569 in
  let r571 = [R 543] in
  let r572 = [R 544] in
  let r573 = [R 527] in
  let r574 = S (T T_RBRACE) :: r573 in
  let r575 = S (N N_expr) :: r574 in
  let r576 = S (T T_LBRACE) :: r575 in
  let r577 = [R 525] in
  let r578 = S (T T_RPAREN) :: r577 in
  let r579 = Sub (r1) :: r578 in
  let r580 = [R 176] in
  let r581 = [R 235] in
  let r582 = S (T T_LIDENT) :: r581 in
  let r583 = [R 232] in
  let r584 = [R 233] in
  let r585 = [R 234] in
  let r586 = [R 231] in
  let r587 = [R 179] in
  let r588 = [R 127] in
  let r589 = Sub (r1) :: r588 in
  let r590 = [R 138] in
  let r591 = Sub (r1) :: r590 in
  let r592 = [R 182] in
  let r593 = S (N N_expr) :: r592 in
  let r594 = [R 187] in
  let r595 = [R 166] in
  let r596 = [R 160] in
  let r597 = [R 177] in
  let r598 = [R 163] in
  let r599 = [R 167] in
  let r600 = [R 159] in
  let r601 = [R 162] in
  let r602 = [R 161] in
  let r603 = [R 171] in
  let r604 = [R 165] in
  let r605 = [R 164] in
  let r606 = [R 169] in
  let r607 = [R 158] in
  let r608 = [R 157] in
  let r609 = [R 154] in
  let r610 = [R 156] in
  let r611 = [R 170] in
  let r612 = [R 168] in
  let r613 = [R 172] in
  let r614 = [R 173] in
  let r615 = [R 174] in
  let r616 = [R 188] in
  let r617 = [R 175] in
  let r618 = [R 455] in
  let r619 = Sub (r1) :: r618 in
  let r620 = [R 10] in
  let r621 = R 426 :: r620 in
  let r622 = Sub (r395) :: r621 in
  let r623 = [R 267] in
  let r624 = Sub (r1) :: r623 in
  let r625 = S (T T_EQUAL) :: r624 in
  let r626 = [R 410] in
  let r627 = [R 411] in
  let r628 = [R 406] in
  let r629 = [R 407] in
  let r630 = [R 404] in
  let r631 = [R 526] in
  let r632 = S (T T_RBRACKET) :: r631 in
  let r633 = Sub (r1) :: r632 in
  let r634 = [R 180] in
  let r635 = [R 181] in
  let r636 = [R 178] in
  let r637 = [R 523] in
  let r638 = [R 533] in
  let r639 = [R 532] in
  let r640 = S (T T_BARRBRACKET) :: r639 in
  let r641 = [R 536] in
  let r642 = [R 535] in
  let r643 = S (T T_RBRACKET) :: r642 in
  let r644 = Sub (r204) :: r490 in
  let r645 = [R 200] in
  let r646 = R 372 :: r645 in
  let r647 = Sub (r644) :: r646 in
  let r648 = [R 542] in
  let r649 = S (T T_GREATERRBRACE) :: r648 in
  let r650 = [R 529] in
  let r651 = S (T T_RBRACE) :: r650 in
  let r652 = [R 446] in
  let r653 = Sub (r496) :: r652 in
  let r654 = [R 669] in
  let r655 = [R 667] in
  let r656 = Sub (r64) :: r655 in
  let r657 = [R 668] in
  let r658 = [R 251] in
  let r659 = [R 134] in
  let r660 = S (T T_DONE) :: r659 in
  let r661 = Sub (r1) :: r660 in
  let r662 = S (T T_DO) :: r661 in
  let r663 = Sub (r1) :: r662 in
  let r664 = Sub (r549) :: r663 in
  let r665 = [R 209] in
  let r666 = Sub (r525) :: r665 in
  let r667 = S (T T_RPAREN) :: r666 in
  let r668 = [R 249] in
  let r669 = [R 207] in
  let r670 = Sub (r1) :: r669 in
  let r671 = S (T T_MINUSGREATER) :: r670 in
  let r672 = [R 208] in
  let r673 = S (N N_pattern) :: r517 in
  let r674 = [R 282] in
  let r675 = [R 150] in
  let r676 = [R 519] in
  let r677 = [R 540] in
  let r678 = [R 530] in
  let r679 = S (T T_BARRBRACKET) :: r678 in
  let r680 = [R 139] in
  let r681 = Sub (r1) :: r680 in
  let r682 = S (T T_IN) :: r681 in
  let r683 = Sub (r295) :: r682 in
  let r684 = S (T T_UIDENT) :: r683 in
  let r685 = [R 300] in
  let r686 = S (N N_module_expr) :: r685 in
  let r687 = S (T T_EQUAL) :: r686 in
  let r688 = [R 301] in
  let r689 = [R 633] in
  let r690 = Sub (r449) :: r689 in
  let r691 = S (T T_RPAREN) :: r690 in
  let r692 = Sub (r537) :: r691 in
  let r693 = [R 205] in
  let r694 = Sub (r1) :: r693 in
  let r695 = [R 632] in
  let r696 = [R 265] in
  let r697 = Sub (r1) :: r696 in
  let r698 = S (T T_EQUAL) :: r697 in
  let r699 = Sub (r64) :: r698 in
  let r700 = S (T T_DOT) :: r699 in
  let r701 = [R 264] in
  let r702 = Sub (r1) :: r701 in
  let r703 = S (T T_EQUAL) :: r702 in
  let r704 = Sub (r64) :: r703 in
  let r705 = [R 155] in
  let r706 = S (T T_RPAREN) :: r705 in
  let r707 = S (N N_expr) :: r706 in
  let r708 = S (T T_COMMA) :: r707 in
  let r709 = S (N N_expr) :: r708 in
  let r710 = S (T T_LPAREN) :: r709 in
  let r711 = [R 521] in
  let r712 = [R 316] in
  let r713 = S (T T_RPAREN) :: r712 in
  let r714 = [R 314] in
  let r715 = S (T T_RPAREN) :: r714 in
  let r716 = [R 315] in
  let r717 = S (T T_RPAREN) :: r716 in
  let r718 = [R 311] in
  let r719 = S (T T_RPAREN) :: r718 in
  let r720 = [R 224] in
  let r721 = S (T T_RBRACKET) :: r720 in
  let r722 = Sub (r15) :: r721 in
  let r723 = [R 419] in
  let r724 = [R 420] in
  let r725 = [R 203] in
  let r726 = S (T T_RBRACKET) :: r725 in
  let r727 = Sub (r15) :: r726 in
  let r728 = [R 629] in
  let r729 = R 426 :: r728 in
  let r730 = S (N N_module_expr) :: r729 in
  let r731 = [R 429] in
  let r732 = S (T T_STRING) :: r731 in
  let r733 = [R 428] in
  let r734 = R 426 :: r733 in
  let r735 = Sub (r732) :: r734 in
  let r736 = S (T T_EQUAL) :: r735 in
  let r737 = Sub (r64) :: r736 in
  let r738 = S (T T_COLON) :: r737 in
  let r739 = Sub (r52) :: r738 in
  let r740 = [R 622] in
  let r741 = R 426 :: r740 in
  let r742 = R 17 :: r741 in
  let r743 = Sub (r249) :: r742 in
  let r744 = S (T T_EQUAL) :: r743 in
  let r745 = Sub (r118) :: r744 in
  let r746 = [R 456] in
  let r747 = R 426 :: r746 in
  let r748 = R 17 :: r747 in
  let r749 = R 216 :: r748 in
  let r750 = Sub (r118) :: r749 in
  let r751 = R 191 :: r750 in
  let r752 = [R 417] in
  let r753 = [R 463] in
  let r754 = [R 443] in
  let r755 = R 426 :: r754 in
  let r756 = S (N N_module_type) :: r755 in
  let r757 = S (T T_COLON) :: r756 in
  let r758 = S (T T_UIDENT) :: r757 in
  let r759 = S (T T_REC) :: r758 in
  let r760 = [R 303] in
  let r761 = S (N N_module_type) :: r760 in
  let r762 = S (T T_COLON) :: r761 in
  let r763 = [R 302] in
  let r764 = R 426 :: r763 in
  let r765 = [R 305] in
  let r766 = Sub (r762) :: r765 in
  let r767 = [R 304] in
  let r768 = Sub (r762) :: r767 in
  let r769 = S (T T_RPAREN) :: r768 in
  let r770 = S (N N_module_type) :: r769 in
  let r771 = [R 297] in
  let r772 = R 426 :: r771 in
  let r773 = [R 460] in
  let r774 = R 426 :: r773 in
  let r775 = S (N N_module_type) :: r774 in
  let r776 = [R 85] in
  let r777 = S (T T_LIDENT) :: r776 in
  let r778 = [R 65] in
  let r779 = Sub (r777) :: r778 in
  let r780 = [R 80] in
  let r781 = R 426 :: r780 in
  let r782 = Sub (r779) :: r781 in
  let r783 = S (T T_EQUAL) :: r782 in
  let r784 = S (T T_LIDENT) :: r783 in
  let r785 = R 83 :: r784 in
  let r786 = R 709 :: r785 in
  let r787 = R 191 :: r786 in
  let r788 = [R 84] in
  let r789 = S (T T_RBRACKET) :: r788 in
  let r790 = [R 55] in
  let r791 = R 62 :: r790 in
  let r792 = R 54 :: r791 in
  let r793 = [R 66] in
  let r794 = S (T T_END) :: r793 in
  let r795 = Sub (r792) :: r794 in
  let r796 = [R 53] in
  let r797 = S (T T_RPAREN) :: r796 in
  let r798 = [R 708] in
  let r799 = Sub (r64) :: r798 in
  let r800 = S (T T_COLON) :: r799 in
  let r801 = Sub (r204) :: r800 in
  let r802 = [R 57] in
  let r803 = R 426 :: r802 in
  let r804 = Sub (r801) :: r803 in
  let r805 = [R 706] in
  let r806 = Sub (r64) :: r805 in
  let r807 = S (T T_COLON) :: r806 in
  let r808 = Sub (r204) :: r807 in
  let r809 = [R 707] in
  let r810 = Sub (r64) :: r809 in
  let r811 = S (T T_COLON) :: r810 in
  let r812 = Sub (r204) :: r811 in
  let r813 = [R 421] in
  let r814 = Sub (r64) :: r813 in
  let r815 = [R 58] in
  let r816 = R 426 :: r815 in
  let r817 = Sub (r814) :: r816 in
  let r818 = S (T T_COLON) :: r817 in
  let r819 = Sub (r204) :: r818 in
  let r820 = R 433 :: r819 in
  let r821 = [R 422] in
  let r822 = Sub (r64) :: r821 in
  let r823 = [R 56] in
  let r824 = R 426 :: r823 in
  let r825 = Sub (r779) :: r824 in
  let r826 = Sub (r64) :: r196 in
  let r827 = [R 64] in
  let r828 = Sub (r777) :: r827 in
  let r829 = S (T T_RBRACKET) :: r828 in
  let r830 = [R 86] in
  let r831 = S (T T_LIDENT) :: r830 in
  let r832 = [R 103] in
  let r833 = Sub (r64) :: r832 in
  let r834 = S (T T_EQUAL) :: r833 in
  let r835 = Sub (r64) :: r834 in
  let r836 = [R 59] in
  let r837 = R 426 :: r836 in
  let r838 = Sub (r835) :: r837 in
  let r839 = [R 60] in
  let r840 = [R 75] in
  let r841 = Sub (r779) :: r840 in
  let r842 = [R 25] in
  let r843 = R 426 :: r842 in
  let r844 = Sub (r841) :: r843 in
  let r845 = S (T T_COLON) :: r844 in
  let r846 = S (T T_LIDENT) :: r845 in
  let r847 = R 83 :: r846 in
  let r848 = [R 76] in
  let r849 = Sub (r841) :: r848 in
  let r850 = S (T T_MINUSGREATER) :: r849 in
  let r851 = Sub (r58) :: r850 in
  let r852 = S (T T_COLON) :: r851 in
  let r853 = [R 77] in
  let r854 = Sub (r841) :: r853 in
  let r855 = S (T T_MINUSGREATER) :: r854 in
  let r856 = [R 78] in
  let r857 = Sub (r841) :: r856 in
  let r858 = S (T T_MINUSGREATER) :: r857 in
  let r859 = [R 79] in
  let r860 = Sub (r841) :: r859 in
  let r861 = [R 13] in
  let r862 = R 426 :: r861 in
  let r863 = R 105 :: r862 in
  let r864 = R 673 :: r863 in
  let r865 = S (T T_LIDENT) :: r864 in
  let r866 = R 379 :: r865 in
  let r867 = [R 464] in
  let r868 = [R 12] in
  let r869 = R 426 :: r868 in
  let r870 = S (N N_module_type) :: r869 in
  let r871 = S (T T_COLON) :: r870 in
  let r872 = S (T T_UIDENT) :: r871 in
  let r873 = [R 478] in
  let r874 = [R 9] in
  let r875 = R 426 :: r874 in
  let r876 = Sub (r779) :: r875 in
  let r877 = S (T T_EQUAL) :: r876 in
  let r878 = S (T T_LIDENT) :: r877 in
  let r879 = R 83 :: r878 in
  let r880 = R 709 :: r879 in
  let r881 = [R 8] in
  let r882 = R 426 :: r881 in
  let r883 = Sub (r841) :: r882 in
  let r884 = S (T T_COLON) :: r883 in
  let r885 = S (T T_LIDENT) :: r884 in
  let r886 = R 83 :: r885 in
  let r887 = R 709 :: r886 in
  let r888 = [R 70] in
  let r889 = Sub (r36) :: r888 in
  let r890 = [R 28] in
  let r891 = Sub (r889) :: r890 in
  let r892 = [R 43] in
  let r893 = Sub (r891) :: r892 in
  let r894 = S (T T_EQUAL) :: r893 in
  let r895 = [R 22] in
  let r896 = R 426 :: r895 in
  let r897 = Sub (r894) :: r896 in
  let r898 = S (T T_LIDENT) :: r897 in
  let r899 = R 83 :: r898 in
  let r900 = [R 71] in
  let r901 = S (T T_END) :: r900 in
  let r902 = Sub (r268) :: r901 in
  let r903 = [R 703] in
  let r904 = Sub (r1) :: r903 in
  let r905 = S (T T_EQUAL) :: r904 in
  let r906 = Sub (r204) :: r905 in
  let r907 = R 333 :: r906 in
  let r908 = R 17 :: r907 in
  let r909 = R 384 :: r908 in
  let r910 = [R 35] in
  let r911 = R 426 :: r910 in
  let r912 = [R 702] in
  let r913 = Sub (r64) :: r912 in
  let r914 = S (T T_COLON) :: r913 in
  let r915 = Sub (r204) :: r914 in
  let r916 = [R 701] in
  let r917 = Sub (r64) :: r916 in
  let r918 = S (T T_COLON) :: r917 in
  let r919 = [R 704] in
  let r920 = Sub (r1) :: r919 in
  let r921 = [R 289] in
  let r922 = Sub (r447) :: r921 in
  let r923 = Sub (r204) :: r922 in
  let r924 = R 431 :: r923 in
  let r925 = R 17 :: r924 in
  let r926 = R 384 :: r925 in
  let r927 = [R 36] in
  let r928 = R 426 :: r927 in
  let r929 = [R 288] in
  let r930 = Sub (r814) :: r929 in
  let r931 = S (T T_COLON) :: r930 in
  let r932 = Sub (r204) :: r931 in
  let r933 = [R 287] in
  let r934 = Sub (r814) :: r933 in
  let r935 = S (T T_COLON) :: r934 in
  let r936 = [R 290] in
  let r937 = Sub (r1) :: r936 in
  let r938 = S (T T_EQUAL) :: r937 in
  let r939 = [R 291] in
  let r940 = Sub (r1) :: r939 in
  let r941 = S (T T_EQUAL) :: r940 in
  let r942 = Sub (r64) :: r941 in
  let r943 = S (T T_DOT) :: r942 in
  let r944 = [R 38] in
  let r945 = R 426 :: r944 in
  let r946 = Sub (r1) :: r945 in
  let r947 = [R 34] in
  let r948 = R 426 :: r947 in
  let r949 = R 388 :: r948 in
  let r950 = Sub (r891) :: r949 in
  let r951 = R 17 :: r950 in
  let r952 = [R 73] in
  let r953 = S (T T_RPAREN) :: r952 in
  let r954 = [R 69] in
  let r955 = Sub (r36) :: r954 in
  let r956 = S (T T_RBRACKET) :: r955 in
  let r957 = [R 46] in
  let r958 = Sub (r891) :: r957 in
  let r959 = S (T T_MINUSGREATER) :: r958 in
  let r960 = Sub (r522) :: r959 in
  let r961 = [R 29] in
  let r962 = Sub (r960) :: r961 in
  let r963 = [R 31] in
  let r964 = Sub (r891) :: r963 in
  let r965 = [R 72] in
  let r966 = S (T T_RPAREN) :: r965 in
  let r967 = [R 387] in
  let r968 = [R 37] in
  let r969 = R 426 :: r968 in
  let r970 = Sub (r835) :: r969 in
  let r971 = [R 39] in
  let r972 = [R 44] in
  let r973 = Sub (r891) :: r972 in
  let r974 = S (T T_EQUAL) :: r973 in
  let r975 = [R 45] in
  let r976 = [R 635] in
  let r977 = [R 655] in
  let r978 = [R 11] in
  let r979 = R 426 :: r978 in
  let r980 = Sub (r295) :: r979 in
  let r981 = S (T T_UIDENT) :: r980 in
  let r982 = [R 651] in
  let r983 = [R 7] in
  let r984 = R 426 :: r983 in
  let r985 = Sub (r894) :: r984 in
  let r986 = S (T T_LIDENT) :: r985 in
  let r987 = R 83 :: r986 in
  let r988 = R 709 :: r987 in
  let r989 = [R 634] in
  let r990 = R 653 :: r989 in
  let r991 = [R 402] in
  let r992 = S (T T_RPAREN) :: r991 in
  let r993 = S (N N_pattern) :: r992 in
  let r994 = S (T T_COMMA) :: r993 in
  let r995 = S (N N_pattern) :: r994 in
  let r996 = S (T T_LPAREN) :: r995 in
  let r997 = [R 51] in
  let r998 = S (T T_RPAREN) :: r997 in
  let r999 = [R 457] in
  let r1000 = Sub (r237) :: r999 in
  let r1001 = [R 461] in
  let r1002 = R 426 :: r1001 in
  let r1003 = Sub (r1000) :: r1002 in
  let r1004 = R 431 :: r1003 in
  let r1005 = [R 130] in
  let r1006 = S (N N_match_cases) :: r1005 in
  let r1007 = [R 132] in
  let r1008 = [R 131] in
  let r1009 = [R 222] in
  let r1010 = [R 223] in
  let r1011 = [R 389] in
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
  | 798 -> One (R 17 :: r622)
  | 1145 -> One (R 17 :: r795)
  | 1154 -> One (R 17 :: r804)
  | 1171 -> One (R 17 :: r820)
  | 1186 -> One (R 17 :: r825)
  | 1201 -> One (R 17 :: r838)
  | 1248 -> One (R 17 :: r866)
  | 1263 -> One (R 17 :: r872)
  | 1280 -> One (R 17 :: r880)
  | 1291 -> One (R 17 :: r887)
  | 1310 -> One (R 17 :: r902)
  | 1366 -> One (R 17 :: r946)
  | 1379 -> One (R 17 :: r962)
  | 1404 -> One (R 17 :: r970)
  | 1432 -> One (R 17 :: r981)
  | 1450 -> One (R 17 :: r988)
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
  | 559 -> One ([R 88])
  | 68 -> One ([R 89])
  | 556 -> One ([R 90])
  | 157 | 276 -> One ([R 91])
  | 158 -> One ([R 96])
  | 345 -> One ([R 97])
  | 67 -> One ([R 101])
  | 311 -> One ([R 110])
  | 306 -> One ([R 111])
  | 270 -> One ([R 113])
  | 901 -> One ([R 125])
  | 712 -> One ([R 136])
  | 839 -> One ([R 137])
  | 739 -> One ([R 147])
  | 748 -> One ([R 148])
  | 728 -> One ([R 149])
  | 746 -> One ([R 186])
  | 860 -> One ([R 190])
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
  | 530 -> One (R 191 :: r399)
  | 551 -> One (R 191 :: r422)
  | 557 -> One (R 191 :: r425)
  | 645 -> One (R 191 :: r505)
  | 647 -> One (R 191 :: r507)
  | 649 -> One (R 191 :: r511)
  | 651 -> One (R 191 :: r514)
  | 656 -> One (R 191 :: r528)
  | 676 -> One (R 191 :: r547)
  | 680 -> One (R 191 :: r558)
  | 692 -> One (R 191 :: r568)
  | 702 -> One (R 191 :: r570)
  | 972 -> One (R 191 :: r684)
  | 1078 -> One (R 191 :: r730)
  | 1082 -> One (R 191 :: r739)
  | 1104 -> One (R 191 :: r759)
  | 1127 -> One (R 191 :: r775)
  | 874 -> One ([R 201])
  | 423 -> One ([R 212])
  | 422 -> One ([R 213])
  | 485 -> One ([R 214])
  | 486 -> One ([R 215])
  | 124 | 478 -> One ([R 220])
  | 292 -> One ([R 229])
  | 293 -> One ([R 230])
  | 840 -> One ([R 241])
  | 842 -> One ([R 242])
  | 882 -> One ([R 254])
  | 881 -> One ([R 255])
  | 541 -> One ([R 259])
  | 545 -> One ([R 261])
  | 736 -> One ([R 268])
  | 823 -> One ([R 269])
  | 661 -> One ([R 271])
  | 672 -> One ([R 274])
  | 732 -> One ([R 276])
  | 824 -> One ([R 277])
  | 941 -> One ([R 280])
  | 946 -> One ([R 281])
  | 255 -> One ([R 283])
  | 254 -> One ([R 284])
  | 256 -> One ([R 285])
  | 167 -> One ([R 286])
  | 519 -> One ([R 306])
  | 518 -> One ([R 317])
  | 520 -> One ([R 318])
  | 427 -> One ([R 319])
  | 481 -> One ([R 326])
  | 475 -> One ([R 327])
  | 480 -> One ([R 331])
  | 1156 -> One (R 333 :: r808)
  | 1321 -> One (R 333 :: r915)
  | 282 | 1326 -> One ([R 334])
  | 242 -> One ([R 337])
  | 139 -> One ([R 339])
  | 86 | 93 -> One ([R 341])
  | 106 -> One ([R 342])
  | 105 -> One ([R 343])
  | 104 -> One ([R 344])
  | 103 -> One ([R 345])
  | 102 -> One ([R 346])
  | 107 -> One ([R 347])
  | 112 | 698 -> One ([R 348])
  | 96 | 401 | 506 -> One ([R 349])
  | 95 | 505 -> One ([R 350])
  | 100 | 528 | 554 -> One ([R 351])
  | 99 | 527 -> One ([R 352])
  | 84 -> One ([R 353])
  | 109 -> One ([R 354])
  | 101 -> One ([R 355])
  | 108 -> One ([R 356])
  | 98 -> One ([R 357])
  | 111 -> One ([R 358])
  | 113 -> One ([R 359])
  | 110 -> One ([R 361])
  | 94 -> One ([R 362])
  | 97 -> One ([R 363])
  | 206 -> One ([R 364])
  | 205 -> One (R 365 :: r171)
  | 174 -> One (R 366 :: r148)
  | 1524 -> One (R 366 :: r1006)
  | 175 -> One ([R 367])
  | 542 -> One (R 372 :: r415)
  | 612 -> One (R 372 :: r444)
  | 858 -> One (R 372 :: r640)
  | 866 -> One (R 372 :: r643)
  | 968 -> One (R 372 :: r679)
  | 543 | 595 | 859 | 873 -> One ([R 373])
  | 890 -> One ([R 374])
  | 367 -> One ([R 380])
  | 382 -> One (R 384 :: r265)
  | 630 -> One (R 384 :: r482)
  | 1370 -> One (R 384 :: r951)
  | 383 -> One ([R 385])
  | 569 -> One ([R 391])
  | 574 -> One ([R 395])
  | 568 -> One ([R 396])
  | 562 -> One ([R 399])
  | 805 -> One ([R 405])
  | 819 -> One ([R 408])
  | 596 -> One ([R 413])
  | 667 -> One ([R 414])
  | 1303 -> One ([R 418])
  | 353 -> One (R 426 :: r255)
  | 1208 -> One (R 426 :: r839)
  | 1276 -> One (R 426 :: r873)
  | 1408 -> One (R 426 :: r971)
  | 1445 -> One (R 426 :: r982)
  | 1460 -> One (R 426 :: r990)
  | 1089 -> One ([R 430])
  | 1341 -> One (R 431 :: r932)
  | 319 | 1346 -> One ([R 432])
  | 1175 -> One ([R 434])
  | 1173 -> One ([R 435])
  | 1176 -> One ([R 436])
  | 1174 -> One ([R 437])
  | 532 -> One ([R 439])
  | 1438 -> One ([R 441])
  | 1437 -> One ([R 442])
  | 1270 -> One ([R 444])
  | 1269 -> One ([R 445])
  | 186 -> One ([R 448])
  | 793 -> One ([R 453])
  | 797 -> One ([R 454])
  | 1503 -> One ([R 458])
  | 1500 -> One ([R 459])
  | 1102 -> One (R 462 :: r752)
  | 1103 -> One (R 462 :: r753)
  | 1257 -> One (R 462 :: r867)
  | 1246 -> One ([R 465])
  | 1271 -> One ([R 466])
  | 1247 -> One ([R 467])
  | 1259 -> One ([R 468])
  | 1261 -> One ([R 469])
  | 1274 -> One ([R 470])
  | 1275 -> One ([R 471])
  | 1262 -> One ([R 472])
  | 1273 -> One ([R 473])
  | 1272 -> One ([R 474])
  | 1260 -> One ([R 475])
  | 1290 -> One ([R 476])
  | 1279 -> One ([R 477])
  | 1278 -> One ([R 479])
  | 399 -> One ([R 482])
  | 396 -> One ([R 484])
  | 185 -> One ([R 489])
  | 190 -> One ([R 490])
  | 267 -> One ([R 491])
  | 212 | 1238 -> One ([R 505])
  | 685 -> One ([R 514])
  | 701 -> One ([R 515])
  | 700 | 747 -> One ([R 516])
  | 687 | 727 -> One ([R 517])
  | 836 | 853 -> One ([R 522])
  | 699 -> One ([R 548])
  | 843 -> One ([R 550])
  | 841 -> One ([R 551])
  | 560 -> One ([R 552])
  | 564 -> One ([R 555])
  | 609 -> One ([R 557])
  | 608 -> One ([R 558])
  | 563 -> One ([R 560])
  | 600 -> One ([R 561])
  | 585 -> One ([R 569])
  | 28 -> One ([R 570])
  | 8 -> One ([R 571])
  | 52 -> One ([R 573])
  | 51 -> One ([R 574])
  | 50 -> One ([R 575])
  | 49 -> One ([R 576])
  | 48 -> One ([R 577])
  | 47 -> One ([R 578])
  | 46 -> One ([R 579])
  | 45 -> One ([R 580])
  | 44 -> One ([R 581])
  | 43 -> One ([R 582])
  | 42 -> One ([R 583])
  | 41 -> One ([R 584])
  | 40 -> One ([R 585])
  | 39 -> One ([R 586])
  | 38 -> One ([R 587])
  | 37 -> One ([R 588])
  | 36 -> One ([R 589])
  | 35 -> One ([R 590])
  | 34 -> One ([R 591])
  | 33 -> One ([R 592])
  | 32 -> One ([R 593])
  | 31 -> One ([R 594])
  | 30 -> One ([R 595])
  | 29 -> One ([R 596])
  | 27 -> One ([R 597])
  | 26 -> One ([R 598])
  | 25 -> One ([R 599])
  | 24 -> One ([R 600])
  | 23 -> One ([R 601])
  | 22 -> One ([R 602])
  | 21 -> One ([R 603])
  | 20 -> One ([R 604])
  | 19 -> One ([R 605])
  | 18 -> One ([R 606])
  | 17 -> One ([R 607])
  | 16 -> One ([R 608])
  | 15 -> One ([R 609])
  | 14 -> One ([R 610])
  | 13 -> One ([R 611])
  | 12 -> One ([R 612])
  | 11 -> One ([R 613])
  | 10 -> One ([R 614])
  | 9 -> One ([R 615])
  | 7 -> One ([R 616])
  | 6 -> One ([R 617])
  | 5 -> One ([R 618])
  | 4 -> One ([R 619])
  | 3 -> One ([R 620])
  | 1430 -> One ([R 621])
  | 366 -> One ([R 624])
  | 357 -> One ([R 625])
  | 365 -> One ([R 626])
  | 356 -> One ([R 627])
  | 355 -> One ([R 628])
  | 1424 -> One ([R 636])
  | 1443 | 1463 -> One ([R 637])
  | 1444 | 1464 -> One ([R 638])
  | 1439 -> One ([R 639])
  | 1421 -> One ([R 640])
  | 1422 -> One ([R 641])
  | 1427 -> One ([R 642])
  | 1429 -> One ([R 643])
  | 1442 -> One ([R 644])
  | 1431 -> One ([R 645])
  | 1441 -> One ([R 646])
  | 1440 -> One ([R 647])
  | 1449 -> One ([R 648])
  | 1448 -> One ([R 649])
  | 1428 -> One ([R 650])
  | 1447 -> One ([R 652])
  | 1425 -> One (R 653 :: r977)
  | 499 -> One ([R 656])
  | 498 -> One ([R 657])
  | 371 -> One ([R 661])
  | 372 -> One ([R 662])
  | 374 -> One ([R 663])
  | 376 -> One ([R 664])
  | 373 -> One ([R 665])
  | 370 -> One ([R 666])
  | 1256 -> One ([R 671])
  | 1255 -> One ([R 672])
  | 317 -> One ([R 674])
  | 304 -> One ([R 675])
  | 326 -> One ([R 676])
  | 430 -> One (R 688 :: r328)
  | 460 -> One ([R 689])
  | 141 -> One ([R 693])
  | 142 -> One ([R 694])
  | 375 -> One ([R 699])
  | 378 -> One ([R 700])
  | 1161 -> One (R 709 :: r812)
  | 1214 -> One (R 709 :: r847)
  | 1305 -> One (R 709 :: r899)
  | 1137 -> One ([R 710])
  | 448 -> One ([R 718])
  | 877 -> One (S (T T_WITH) :: r653)
  | 346 | 377 -> One (S (T T_UIDENT) :: r41)
  | 195 -> One (S (T T_UIDENT) :: r164)
  | 407 -> One (S (T T_TYPE) :: r289)
  | 1000 -> One (S (T T_TYPE) :: r692)
  | 1134 | 1304 -> One (S (T T_TYPE) :: r787)
  | 341 -> One (S (T T_RPAREN) :: r47)
  | 160 | 277 -> One (S (T T_RPAREN) :: r126)
  | 260 -> One (S (T T_RPAREN) :: r192)
  | 263 -> One (S (T T_RPAREN) :: r193)
  | 421 -> One (S (T T_RPAREN) :: r313)
  | 514 -> One (S (T T_RPAREN) :: r383)
  | 516 -> One (S (T T_RPAREN) :: r384)
  | 579 -> One (S (T T_RPAREN) :: r436)
  | 581 -> One (S (T T_RPAREN) :: r437)
  | 854 -> One (S (T T_RPAREN) :: r637)
  | 1028 -> One (S (T T_RPAREN) :: r710)
  | 1037 -> One (S (T T_RPAREN) :: r711)
  | 1107 -> One (S (T T_RPAREN) :: r766)
  | 1475 -> One (S (T T_RPAREN) :: r996)
  | 178 -> One (S (T T_RBRACKET) :: r149)
  | 229 -> One (S (T T_RBRACKET) :: r180)
  | 272 | 278 -> One (S (T T_RBRACKET) :: r197)
  | 342 -> One (S (T T_RBRACKET) :: r254)
  | 864 -> One (S (T T_RBRACKET) :: r641)
  | 220 -> One (S (T T_QUOTE) :: r178)
  | 335 -> One (S (T T_PLUSEQ) :: r243)
  | 1493 -> One (S (T T_PLUSEQ) :: r1004)
  | 131 -> One (S (T T_MODULE) :: r93)
  | 299 -> One (S (T T_MINUSGREATER) :: r223)
  | 1233 -> One (S (T T_MINUSGREATER) :: r860)
  | 127 -> One (S (T T_LIDENT) :: r84)
  | 1219 -> One (S (T T_LIDENT) :: r852)
  | 1400 -> One (S (T T_LIDENT) :: r967)
  | 737 -> One (S (T T_LESSMINUS) :: r593)
  | 313 -> One (S (T T_LBRACE) :: r226)
  | 394 -> One (S (T T_INT) :: r277)
  | 397 -> One (S (T T_INT) :: r278)
  | 729 -> One (S (T T_IN) :: r589)
  | 733 -> One (S (T T_IN) :: r591)
  | 1383 -> One (S (T T_IN) :: r964)
  | 637 -> One (S (T T_GREATERRBRACE) :: r489)
  | 962 -> One (S (T T_GREATERRBRACE) :: r677)
  | 164 -> One (S (T T_GREATER) :: r131)
  | 168 -> One (S (T T_GREATER) :: r133)
  | 359 -> One (S (T T_EQUAL) :: r258)
  | 465 -> One (S (T T_EQUAL) :: r353)
  | 1006 -> One (S (T T_EQUAL) :: r694)
  | 1335 -> One (S (T T_EQUAL) :: r920)
  | 1543 -> One (S (T T_EOF) :: r1009)
  | 1547 -> One (S (T T_EOF) :: r1010)
  | 1551 -> One (S (T T_EOF) :: r1011)
  | 953 -> One (S (T T_END) :: r676)
  | 156 -> One (S (T T_DOTDOT) :: r116)
  | 318 -> One (S (T T_DOTDOT) :: r227)
  | 74 -> One (S (T T_DOT) :: r40)
  | 244 -> One (S (T T_DOT) :: r190)
  | 443 -> One (S (T T_DOT) :: r341)
  | 476 -> One (S (T T_DOT) :: r357)
  | 577 -> One (S (T T_DOT) :: r435)
  | 1020 -> One (S (T T_DOT) :: r704)
  | 1180 -> One (S (T T_DOT) :: r822)
  | 1192 -> One (S (T T_DOT) :: r831)
  | 170 -> One (S (T T_COLON) :: r140)
  | 425 -> One (S (T T_COLON) :: r316)
  | 1108 -> One (S (T T_COLON) :: r770)
  | 534 -> One (S (T T_BARRBRACKET) :: r400)
  | 635 -> One (S (T T_BARRBRACKET) :: r483)
  | 856 -> One (S (T T_BARRBRACKET) :: r638)
  | 181 | 1231 -> One (S (T T_BAR) :: r154)
  | 231 -> One (S (T T_BAR) :: r183)
  | 379 -> One (S (N N_structure) :: r260)
  | 1423 -> One (S (N N_structure) :: r976)
  | 390 -> One (S (N N_pattern) :: r273)
  | 400 | 553 | 669 | 920 -> One (S (N N_pattern) :: r280)
  | 550 -> One (S (N N_pattern) :: r417)
  | 570 -> One (S (N N_pattern) :: r430)
  | 572 -> One (S (N N_pattern) :: r431)
  | 575 -> One (S (N N_pattern) :: r432)
  | 583 -> One (S (N N_pattern) :: r438)
  | 588 -> One (S (N N_pattern) :: r439)
  | 806 -> One (S (N N_pattern) :: r626)
  | 811 -> One (S (N N_pattern) :: r627)
  | 813 -> One (S (N N_pattern) :: r628)
  | 815 -> One (S (N N_pattern) :: r629)
  | 1072 -> One (S (N N_pattern) :: r723)
  | 417 -> One (S (N N_module_type) :: r306)
  | 418 -> One (S (N N_module_type) :: r308)
  | 473 -> One (S (N N_module_type) :: r355)
  | 975 -> One (S (N N_module_type) :: r687)
  | 1060 -> One (S (N N_module_type) :: r719)
  | 495 -> One (S (N N_module_expr) :: r362)
  | 660 -> One (S (N N_let_pattern) :: r534)
  | 640 -> One (S (N N_expr) :: r491)
  | 644 -> One (S (N N_expr) :: r503)
  | 711 -> One (S (N N_expr) :: r580)
  | 726 -> One (S (N N_expr) :: r587)
  | 740 -> One (S (N N_expr) :: r594)
  | 742 -> One (S (N N_expr) :: r595)
  | 744 -> One (S (N N_expr) :: r596)
  | 749 -> One (S (N N_expr) :: r597)
  | 751 -> One (S (N N_expr) :: r598)
  | 753 -> One (S (N N_expr) :: r599)
  | 755 -> One (S (N N_expr) :: r600)
  | 757 -> One (S (N N_expr) :: r601)
  | 759 -> One (S (N N_expr) :: r602)
  | 761 -> One (S (N N_expr) :: r603)
  | 763 -> One (S (N N_expr) :: r604)
  | 765 -> One (S (N N_expr) :: r605)
  | 767 -> One (S (N N_expr) :: r606)
  | 769 -> One (S (N N_expr) :: r607)
  | 771 -> One (S (N N_expr) :: r608)
  | 773 -> One (S (N N_expr) :: r609)
  | 775 -> One (S (N N_expr) :: r610)
  | 777 -> One (S (N N_expr) :: r611)
  | 779 -> One (S (N N_expr) :: r612)
  | 781 -> One (S (N N_expr) :: r613)
  | 783 -> One (S (N N_expr) :: r614)
  | 785 -> One (S (N N_expr) :: r615)
  | 788 -> One (S (N N_expr) :: r616)
  | 790 -> One (S (N N_expr) :: r617)
  | 829 -> One (S (N N_expr) :: r634)
  | 834 -> One (S (N N_expr) :: r635)
  | 837 -> One (S (N N_expr) :: r636)
  | 892 -> One (S (N N_expr) :: r658)
  | 950 -> One (S (N N_expr) :: r675)
  | 628 -> One (Sub (r1) :: r463)
  | 655 -> One (Sub (r1) :: r520)
  | 912 -> One (Sub (r1) :: r664)
  | 1074 -> One (Sub (r1) :: r724)
  | 1527 -> One (Sub (r1) :: r1007)
  | 1529 -> One (Sub (r1) :: r1008)
  | 2 -> One (Sub (r10) :: r12)
  | 55 -> One (Sub (r10) :: r13)
  | 58 -> One (Sub (r10) :: r18)
  | 88 -> One (Sub (r10) :: r51)
  | 329 -> One (Sub (r10) :: r233)
  | 794 -> One (Sub (r10) :: r619)
  | 1070 -> One (Sub (r10) :: r722)
  | 1076 -> One (Sub (r10) :: r727)
  | 70 -> One (Sub (r33) :: r34)
  | 643 -> One (Sub (r33) :: r501)
  | 684 -> One (Sub (r33) :: r559)
  | 707 -> One (Sub (r33) :: r572)
  | 718 -> One (Sub (r33) :: r585)
  | 720 -> One (Sub (r33) :: r586)
  | 121 -> One (Sub (r36) :: r75)
  | 188 -> One (Sub (r36) :: r157)
  | 265 -> One (Sub (r36) :: r194)
  | 590 -> One (Sub (r52) :: r440)
  | 817 -> One (Sub (r52) :: r630)
  | 214 -> One (Sub (r56) :: r175)
  | 297 -> One (Sub (r56) :: r221)
  | 926 -> One (Sub (r56) :: r671)
  | 1224 -> One (Sub (r58) :: r855)
  | 1228 -> One (Sub (r58) :: r858)
  | 130 -> One (Sub (r60) :: r87)
  | 163 -> One (Sub (r60) :: r130)
  | 218 -> One (Sub (r60) :: r176)
  | 224 -> One (Sub (r62) :: r179)
  | 268 -> One (Sub (r64) :: r195)
  | 547 -> One (Sub (r64) :: r416)
  | 604 -> One (Sub (r64) :: r442)
  | 620 -> One (Sub (r64) :: r458)
  | 662 -> One (Sub (r64) :: r535)
  | 801 -> One (Sub (r64) :: r625)
  | 884 -> One (Sub (r64) :: r654)
  | 888 -> One (Sub (r64) :: r657)
  | 1147 -> One (Sub (r64) :: r797)
  | 1484 -> One (Sub (r64) :: r998)
  | 92 -> One (Sub (r71) :: r73)
  | 146 -> One (Sub (r77) :: r114)
  | 245 -> One (Sub (r77) :: r191)
  | 368 -> One (Sub (r77) :: r259)
  | 406 -> One (Sub (r89) :: r285)
  | 523 -> One (Sub (r89) :: r386)
  | 1049 -> One (Sub (r89) :: r713)
  | 1052 -> One (Sub (r89) :: r715)
  | 1055 -> One (Sub (r89) :: r717)
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
  | 705 -> One (Sub (r204) :: r571)
  | 1327 -> One (Sub (r204) :: r918)
  | 1347 -> One (Sub (r204) :: r935)
  | 281 -> One (Sub (r212) :: r214)
  | 322 -> One (Sub (r212) :: r230)
  | 1117 -> One (Sub (r261) :: r772)
  | 392 -> One (Sub (r275) :: r276)
  | 982 -> One (Sub (r295) :: r688)
  | 469 -> One (Sub (r321) :: r354)
  | 429 -> One (Sub (r323) :: r324)
  | 438 -> One (Sub (r334) :: r339)
  | 431 -> One (Sub (r336) :: r338)
  | 1139 -> One (Sub (r336) :: r789)
  | 446 -> One (Sub (r343) :: r346)
  | 452 -> One (Sub (r350) :: r351)
  | 535 -> One (Sub (r402) :: r405)
  | 536 -> One (Sub (r412) :: r414)
  | 924 -> One (Sub (r420) :: r668)
  | 565 -> One (Sub (r428) :: r429)
  | 615 -> One (Sub (r449) :: r450)
  | 1010 -> One (Sub (r449) :: r695)
  | 616 -> One (Sub (r452) :: r453)
  | 625 -> One (Sub (r452) :: r459)
  | 617 -> One (Sub (r455) :: r457)
  | 626 -> One (Sub (r455) :: r462)
  | 641 -> One (Sub (r498) :: r500)
  | 876 -> One (Sub (r498) :: r651)
  | 931 -> One (Sub (r525) :: r672)
  | 658 -> One (Sub (r530) :: r531)
  | 670 -> One (Sub (r537) :: r540)
  | 921 -> One (Sub (r537) :: r667)
  | 1014 -> One (Sub (r537) :: r700)
  | 1354 -> One (Sub (r537) :: r943)
  | 713 -> One (Sub (r582) :: r583)
  | 716 -> One (Sub (r582) :: r584)
  | 869 -> One (Sub (r647) :: r649)
  | 944 -> One (Sub (r673) :: r674)
  | 1106 -> One (Sub (r762) :: r764)
  | 1353 -> One (Sub (r814) :: r938)
  | 1189 -> One (Sub (r826) :: r829)
  | 1375 -> One (Sub (r826) :: r956)
  | 1396 -> One (Sub (r841) :: r966)
  | 1413 -> One (Sub (r841) :: r974)
  | 1373 -> One (Sub (r891) :: r953)
  | 1417 -> One (Sub (r894) :: r975)
  | 1316 -> One (Sub (r909) :: r911)
  | 1338 -> One (Sub (r926) :: r928)
  | 792 -> One (r0)
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
  | 1036 -> One (r44)
  | 1035 -> One (r45)
  | 83 -> One (r46)
  | 85 -> One (r47)
  | 87 | 529 | 642 | 898 -> One (r48)
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
  | 526 -> One (r88)
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
  | 603 -> One (r279)
  | 602 -> One (r280)
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
  | 1059 -> One (r361)
  | 1058 -> One (r362)
  | 1048 -> One (r363)
  | 1047 -> One (r364)
  | 497 -> One (r365)
  | 1046 -> One (r366)
  | 1045 -> One (r367)
  | 1044 -> One (r368)
  | 1043 -> One (r369)
  | 501 -> One (r370)
  | 1042 -> One (r371)
  | 1041 -> One (r372)
  | 1040 -> One (r373)
  | 1039 -> One (r374)
  | 503 -> One (r375)
  | 522 -> One (r376)
  | 521 -> One (r377)
  | 508 -> One (r378)
  | 513 -> One (r379)
  | 512 -> One (r380)
  | 511 -> One (r381)
  | 510 -> One (r382)
  | 515 -> One (r383)
  | 517 -> One (r384)
  | 525 -> One (r385)
  | 524 -> One (r386)
  | 587 -> One (r387)
  | 586 -> One (r388)
  | 820 -> One (r390)
  | 810 -> One (r392)
  | 809 -> One (r393)
  | 808 -> One (r394)
  | 1027 -> One (r396)
  | 1026 -> One (r397)
  | 533 -> One (r398)
  | 531 -> One (r399)
  | 611 -> One (r400)
  | 599 -> One (r401)
  | 598 -> One (r403)
  | 597 -> One (r404)
  | 594 -> One (r405)
  | 537 -> One (r406)
  | 549 -> One (r408)
  | 546 -> One (r409)
  | 540 -> One (r411)
  | 539 -> One (r413)
  | 538 -> One (r414)
  | 544 -> One (r415)
  | 548 -> One (r416)
  | 610 -> One (r417)
  | 561 | 800 -> One (r419)
  | 607 -> One (r421)
  | 552 -> One (r422)
  | 555 -> One (r423)
  | 601 -> One (r424)
  | 558 -> One (r425)
  | 567 -> One (r427)
  | 566 -> One (r429)
  | 571 -> One (r430)
  | 573 -> One (r431)
  | 576 -> One (r432)
  | 593 -> One (r433)
  | 592 -> One (r434)
  | 578 -> One (r435)
  | 580 -> One (r436)
  | 582 -> One (r437)
  | 584 -> One (r438)
  | 589 -> One (r439)
  | 591 -> One (r440)
  | 606 -> One (r441)
  | 605 -> One (r442)
  | 614 -> One (r443)
  | 613 -> One (r444)
  | 1005 -> One (r445)
  | 1004 -> One (r446)
  | 1009 -> One (r448)
  | 1025 -> One (r450)
  | 618 -> One (r451)
  | 624 -> One (r453)
  | 619 -> One (r454)
  | 623 -> One (r456)
  | 622 -> One (r457)
  | 621 -> One (r458)
  | 999 -> One (r459)
  | 998 -> One (r460)
  | 997 -> One (r461)
  | 627 -> One (r462)
  | 996 -> One (r463)
  | 991 -> One (r464)
  | 990 -> One (r465)
  | 989 -> One (r466)
  | 988 -> One (r468)
  | 987 -> One (r469)
  | 986 -> One (r470)
  | 985 -> One (r471)
  | 984 -> One (r472)
  | 995 -> One (r473)
  | 994 -> One (r474)
  | 993 -> One (r475)
  | 992 -> One (r476)
  | 1374 -> One (r477)
  | 971 -> One (r478)
  | 634 -> One (r479)
  | 633 -> One (r480)
  | 632 -> One (r481)
  | 631 -> One (r482)
  | 967 -> One (r483)
  | 863 -> One (r484)
  | 966 -> One (r486)
  | 965 -> One (r487)
  | 964 -> One (r488)
  | 638 -> One (r489)
  | 639 -> One (r490)
  | 961 -> One (r491)
  | 891 -> One (r492)
  | 883 -> One (r493)
  | 880 -> One (r495)
  | 899 -> One (r497)
  | 960 -> One (r499)
  | 959 -> One (r500)
  | 958 -> One (r501)
  | 957 -> One (r502)
  | 956 -> One (r503)
  | 955 -> One (r504)
  | 646 -> One (r505)
  | 952 -> One (r506)
  | 648 -> One (r507)
  | 949 -> One (r508)
  | 948 -> One (r509)
  | 947 -> One (r510)
  | 650 -> One (r511)
  | 943 -> One (r512)
  | 653 -> One (r513)
  | 652 -> One (r514)
  | 942 -> One (r515)
  | 940 -> One (r516)
  | 654 -> One (r517)
  | 939 -> One (r518)
  | 938 -> One (r519)
  | 937 -> One (r520)
  | 930 -> One (r521)
  | 919 -> One (r523)
  | 675 -> One (r524)
  | 936 -> One (r526)
  | 935 -> One (r527)
  | 657 -> One (r528)
  | 659 -> One (r529)
  | 668 -> One (r531)
  | 666 -> One (r532)
  | 665 -> One (r533)
  | 664 -> One (r534)
  | 663 -> One (r535)
  | 671 -> One (r536)
  | 934 -> One (r538)
  | 674 -> One (r539)
  | 673 -> One (r540)
  | 911 -> One (r541)
  | 910 -> One (r542)
  | 909 -> One (r543)
  | 908 -> One (r544)
  | 679 -> One (r545)
  | 678 -> One (r546)
  | 677 -> One (r547)
  | 902 -> One (r548)
  | 907 -> One (r550)
  | 906 -> One (r551)
  | 905 -> One (r552)
  | 904 -> One (r553)
  | 903 -> One (r554)
  | 900 -> One (r555)
  | 683 -> One (r556)
  | 682 -> One (r557)
  | 681 -> One (r558)
  | 686 -> One (r559)
  | 691 -> One (r560)
  | 690 -> One (r561)
  | 689 | 897 -> One (r562)
  | 896 -> One (r563)
  | 697 -> One (r564)
  | 696 -> One (r565)
  | 695 -> One (r566)
  | 694 -> One (r567)
  | 693 -> One (r568)
  | 704 -> One (r569)
  | 703 -> One (r570)
  | 706 -> One (r571)
  | 708 -> One (r572)
  | 833 | 852 -> One (r573)
  | 832 | 851 -> One (r574)
  | 831 | 850 -> One (r575)
  | 709 | 722 -> One (r576)
  | 725 | 846 -> One (r577)
  | 724 | 845 -> One (r578)
  | 710 | 723 -> One (r579)
  | 844 -> One (r580)
  | 714 -> One (r581)
  | 715 -> One (r583)
  | 717 -> One (r584)
  | 719 -> One (r585)
  | 721 -> One (r586)
  | 825 -> One (r587)
  | 731 -> One (r588)
  | 730 -> One (r589)
  | 735 -> One (r590)
  | 734 -> One (r591)
  | 787 -> One (r592)
  | 738 -> One (r593)
  | 741 -> One (r594)
  | 743 -> One (r595)
  | 745 -> One (r596)
  | 750 -> One (r597)
  | 752 -> One (r598)
  | 754 -> One (r599)
  | 756 -> One (r600)
  | 758 -> One (r601)
  | 760 -> One (r602)
  | 762 -> One (r603)
  | 764 -> One (r604)
  | 766 -> One (r605)
  | 768 -> One (r606)
  | 770 -> One (r607)
  | 772 -> One (r608)
  | 774 -> One (r609)
  | 776 -> One (r610)
  | 778 -> One (r611)
  | 780 -> One (r612)
  | 782 -> One (r613)
  | 784 -> One (r614)
  | 786 -> One (r615)
  | 789 -> One (r616)
  | 791 -> One (r617)
  | 796 -> One (r618)
  | 795 -> One (r619)
  | 822 -> One (r620)
  | 821 -> One (r621)
  | 799 -> One (r622)
  | 804 -> One (r623)
  | 803 -> One (r624)
  | 802 -> One (r625)
  | 807 -> One (r626)
  | 812 -> One (r627)
  | 814 -> One (r628)
  | 816 -> One (r629)
  | 818 -> One (r630)
  | 828 | 849 -> One (r631)
  | 827 | 848 -> One (r632)
  | 826 | 847 -> One (r633)
  | 830 -> One (r634)
  | 835 -> One (r635)
  | 838 -> One (r636)
  | 855 -> One (r637)
  | 857 -> One (r638)
  | 862 -> One (r639)
  | 861 -> One (r640)
  | 865 -> One (r641)
  | 868 -> One (r642)
  | 867 -> One (r643)
  | 875 -> One (r645)
  | 872 -> One (r646)
  | 871 -> One (r648)
  | 870 -> One (r649)
  | 895 -> One (r650)
  | 894 -> One (r651)
  | 879 -> One (r652)
  | 878 -> One (r653)
  | 885 -> One (r654)
  | 887 -> One (r655)
  | 886 | 1013 -> One (r656)
  | 889 -> One (r657)
  | 893 -> One (r658)
  | 918 -> One (r659)
  | 917 -> One (r660)
  | 916 -> One (r661)
  | 915 -> One (r662)
  | 914 -> One (r663)
  | 913 -> One (r664)
  | 933 -> One (r665)
  | 923 -> One (r666)
  | 922 -> One (r667)
  | 925 -> One (r668)
  | 929 -> One (r669)
  | 928 -> One (r670)
  | 927 -> One (r671)
  | 932 -> One (r672)
  | 945 -> One (r674)
  | 951 -> One (r675)
  | 954 -> One (r676)
  | 963 -> One (r677)
  | 970 -> One (r678)
  | 969 -> One (r679)
  | 981 -> One (r680)
  | 980 -> One (r681)
  | 979 -> One (r682)
  | 974 -> One (r683)
  | 973 -> One (r684)
  | 978 -> One (r685)
  | 977 -> One (r686)
  | 976 -> One (r687)
  | 983 -> One (r688)
  | 1012 -> One (r689)
  | 1003 -> One (r690)
  | 1002 -> One (r691)
  | 1001 -> One (r692)
  | 1008 -> One (r693)
  | 1007 -> One (r694)
  | 1011 -> One (r695)
  | 1019 -> One (r696)
  | 1018 -> One (r697)
  | 1017 -> One (r698)
  | 1016 -> One (r699)
  | 1015 -> One (r700)
  | 1024 -> One (r701)
  | 1023 -> One (r702)
  | 1022 -> One (r703)
  | 1021 -> One (r704)
  | 1034 -> One (r705)
  | 1033 -> One (r706)
  | 1032 -> One (r707)
  | 1031 -> One (r708)
  | 1030 -> One (r709)
  | 1029 -> One (r710)
  | 1038 -> One (r711)
  | 1051 -> One (r712)
  | 1050 -> One (r713)
  | 1054 -> One (r714)
  | 1053 -> One (r715)
  | 1057 -> One (r716)
  | 1056 -> One (r717)
  | 1062 -> One (r718)
  | 1061 -> One (r719)
  | 1469 -> One (r720)
  | 1468 -> One (r721)
  | 1071 -> One (r722)
  | 1073 -> One (r723)
  | 1075 -> One (r724)
  | 1467 -> One (r725)
  | 1466 -> One (r726)
  | 1077 -> One (r727)
  | 1081 -> One (r728)
  | 1080 -> One (r729)
  | 1079 -> One (r730)
  | 1088 -> One (r731)
  | 1091 -> One (r733)
  | 1090 -> One (r734)
  | 1087 -> One (r735)
  | 1086 -> One (r736)
  | 1085 -> One (r737)
  | 1084 -> One (r738)
  | 1083 -> One (r739)
  | 1098 -> One (r740)
  | 1097 -> One (r741)
  | 1096 -> One (r742)
  | 1095 -> One (r743)
  | 1101 -> One (r746)
  | 1100 -> One (r747)
  | 1099 -> One (r748)
  | 1133 -> One (r749)
  | 1132 -> One (r750)
  | 1131 -> One (r751)
  | 1302 -> One (r752)
  | 1301 -> One (r753)
  | 1126 -> One (r754)
  | 1125 -> One (r755)
  | 1124 -> One (r756)
  | 1123 -> One (r757)
  | 1122 -> One (r758)
  | 1105 -> One (r759)
  | 1113 -> One (r760)
  | 1112 -> One (r761)
  | 1121 -> One (r763)
  | 1120 -> One (r764)
  | 1116 -> One (r765)
  | 1115 -> One (r766)
  | 1114 -> One (r767)
  | 1111 -> One (r768)
  | 1110 -> One (r769)
  | 1109 -> One (r770)
  | 1119 -> One (r771)
  | 1118 -> One (r772)
  | 1130 -> One (r773)
  | 1129 -> One (r774)
  | 1128 -> One (r775)
  | 1188 -> One (r776)
  | 1197 -> One (r778)
  | 1213 -> One (r780)
  | 1212 -> One (r781)
  | 1144 -> One (r782)
  | 1143 -> One (r783)
  | 1142 -> One (r784)
  | 1138 -> One (r785)
  | 1136 -> One (r786)
  | 1135 -> One (r787)
  | 1141 -> One (r788)
  | 1140 -> One (r789)
  | 1153 -> One (r790)
  | 1152 -> One (r791)
  | 1151 -> One (r793)
  | 1150 -> One (r794)
  | 1146 -> One (r795)
  | 1149 -> One (r796)
  | 1148 -> One (r797)
  | 1170 -> One (r798)
  | 1169 -> One (r799)
  | 1168 -> One (r800)
  | 1167 -> One (r802)
  | 1166 -> One (r803)
  | 1155 -> One (r804)
  | 1160 -> One (r805)
  | 1159 -> One (r806)
  | 1158 -> One (r807)
  | 1157 -> One (r808)
  | 1165 -> One (r809)
  | 1164 -> One (r810)
  | 1163 -> One (r811)
  | 1162 -> One (r812)
  | 1185 -> One (r813)
  | 1184 -> One (r815)
  | 1183 -> One (r816)
  | 1179 -> One (r817)
  | 1178 -> One (r818)
  | 1177 -> One (r819)
  | 1172 -> One (r820)
  | 1182 -> One (r821)
  | 1181 -> One (r822)
  | 1199 -> One (r823)
  | 1198 -> One (r824)
  | 1187 -> One (r825)
  | 1195 -> One (r827)
  | 1191 -> One (r828)
  | 1190 -> One (r829)
  | 1194 -> One (r830)
  | 1193 -> One (r831)
  | 1205 -> One (r832)
  | 1204 -> One (r833)
  | 1203 -> One (r834)
  | 1207 -> One (r836)
  | 1206 -> One (r837)
  | 1202 -> One (r838)
  | 1209 -> One (r839)
  | 1240 -> One (r840)
  | 1245 -> One (r842)
  | 1244 -> One (r843)
  | 1218 -> One (r844)
  | 1217 -> One (r845)
  | 1216 -> One (r846)
  | 1215 -> One (r847)
  | 1243 -> One (r848)
  | 1223 -> One (r849)
  | 1222 -> One (r850)
  | 1221 -> One (r851)
  | 1220 -> One (r852)
  | 1242 -> One (r853)
  | 1226 -> One (r854)
  | 1225 -> One (r855)
  | 1241 -> One (r856)
  | 1230 -> One (r857)
  | 1229 -> One (r858)
  | 1239 -> One (r859)
  | 1234 -> One (r860)
  | 1254 -> One (r861)
  | 1253 -> One (r862)
  | 1252 -> One (r863)
  | 1251 -> One (r864)
  | 1250 -> One (r865)
  | 1249 -> One (r866)
  | 1258 -> One (r867)
  | 1268 -> One (r868)
  | 1267 -> One (r869)
  | 1266 -> One (r870)
  | 1265 -> One (r871)
  | 1264 -> One (r872)
  | 1277 -> One (r873)
  | 1287 -> One (r874)
  | 1286 -> One (r875)
  | 1285 -> One (r876)
  | 1284 -> One (r877)
  | 1283 -> One (r878)
  | 1282 -> One (r879)
  | 1281 -> One (r880)
  | 1298 -> One (r881)
  | 1297 -> One (r882)
  | 1296 -> One (r883)
  | 1295 -> One (r884)
  | 1294 -> One (r885)
  | 1293 -> One (r886)
  | 1292 -> One (r887)
  | 1388 -> One (r888)
  | 1386 -> One (r890)
  | 1412 -> One (r892)
  | 1309 -> One (r893)
  | 1420 -> One (r895)
  | 1419 -> One (r896)
  | 1308 -> One (r897)
  | 1307 -> One (r898)
  | 1306 -> One (r899)
  | 1313 -> One (r900)
  | 1312 -> One (r901)
  | 1311 -> One (r902)
  | 1334 -> One (r903)
  | 1333 -> One (r904)
  | 1332 -> One (r905)
  | 1331 -> One (r906)
  | 1320 -> One (r907)
  | 1319 -> One (r908)
  | 1318 -> One (r910)
  | 1317 -> One (r911)
  | 1325 -> One (r912)
  | 1324 -> One (r913)
  | 1323 -> One (r914)
  | 1322 -> One (r915)
  | 1330 -> One (r916)
  | 1329 -> One (r917)
  | 1328 -> One (r918)
  | 1337 -> One (r919)
  | 1336 -> One (r920)
  | 1363 -> One (r921)
  | 1352 -> One (r922)
  | 1351 -> One (r923)
  | 1340 -> One (r924)
  | 1339 -> One (r925)
  | 1365 -> One (r927)
  | 1364 -> One (r928)
  | 1345 -> One (r929)
  | 1344 -> One (r930)
  | 1343 -> One (r931)
  | 1342 -> One (r932)
  | 1350 -> One (r933)
  | 1349 -> One (r934)
  | 1348 -> One (r935)
  | 1362 -> One (r936)
  | 1361 -> One (r937)
  | 1360 -> One (r938)
  | 1359 -> One (r939)
  | 1358 -> One (r940)
  | 1357 -> One (r941)
  | 1356 -> One (r942)
  | 1355 -> One (r943)
  | 1369 -> One (r944)
  | 1368 -> One (r945)
  | 1367 -> One (r946)
  | 1403 -> One (r947)
  | 1402 -> One (r948)
  | 1399 -> One (r949)
  | 1372 -> One (r950)
  | 1371 -> One (r951)
  | 1395 -> One (r952)
  | 1394 -> One (r953)
  | 1378 -> One (r954)
  | 1377 -> One (r955)
  | 1376 -> One (r956)
  | 1391 -> One (r957)
  | 1382 -> One (r958)
  | 1381 -> One (r959)
  | 1393 -> One (r961)
  | 1380 -> One (r962)
  | 1389 -> One (r963)
  | 1384 -> One (r964)
  | 1398 -> One (r965)
  | 1397 -> One (r966)
  | 1401 -> One (r967)
  | 1407 -> One (r968)
  | 1406 -> One (r969)
  | 1405 -> One (r970)
  | 1409 -> One (r971)
  | 1416 -> One (r972)
  | 1415 -> One (r973)
  | 1414 -> One (r974)
  | 1418 -> One (r975)
  | 1459 -> One (r976)
  | 1426 -> One (r977)
  | 1436 -> One (r978)
  | 1435 -> One (r979)
  | 1434 -> One (r980)
  | 1433 -> One (r981)
  | 1446 -> One (r982)
  | 1456 -> One (r983)
  | 1455 -> One (r984)
  | 1454 -> One (r985)
  | 1453 -> One (r986)
  | 1452 -> One (r987)
  | 1451 -> One (r988)
  | 1462 -> One (r989)
  | 1461 -> One (r990)
  | 1481 -> One (r991)
  | 1480 -> One (r992)
  | 1479 -> One (r993)
  | 1478 -> One (r994)
  | 1477 -> One (r995)
  | 1476 -> One (r996)
  | 1486 -> One (r997)
  | 1485 -> One (r998)
  | 1501 -> One (r999)
  | 1499 -> One (r1001)
  | 1498 -> One (r1002)
  | 1495 -> One (r1003)
  | 1494 -> One (r1004)
  | 1526 -> One (r1005)
  | 1525 -> One (r1006)
  | 1528 -> One (r1007)
  | 1530 -> One (r1008)
  | 1544 -> One (r1009)
  | 1548 -> One (r1010)
  | 1552 -> One (r1011)
  | 688 -> Select (function
    | -1 -> [R 97]
    | _ -> r563)
  | 413 -> Select (function
    | -1 -> S (T T_TYPE) :: r305
    | _ -> R 191 :: r300)
  | 1092 -> Select (function
    | -1 -> r751
    | _ -> R 191 :: r745)
  | 492 -> Select (function
    | -1 -> S (T T_UIDENT) :: r360
    | _ -> r300)
  | 504 -> Select (function
    | -1 -> S (T T_RPAREN) :: r47
    | _ -> r46)
  | 636 -> Select (function
    | -1 -> S (T T_RBRACKET) :: r254
    | _ -> Sub (r485) :: r488)
  | 629 -> Select (function
    | 59 | 89 | 330 | 379 | 412 | 1071 | 1077 | 1423 -> r477
    | _ -> S (T T_EXCEPTION) :: r472)
  | 172 -> Select (function
    | 1013 -> r79
    | _ -> Sub (r77) :: r141)
  | 338 -> Select (function
    | 351 -> r247
    | _ -> Sub (r118) :: r253)
  | 173 -> Select (function
    | 1013 -> r78
    | _ -> r141)
  | 339 -> Select (function
    | 337 -> r253
    | _ -> r246)
  | 1094 -> Select (function
    | -1 -> r749
    | _ -> r744)
  | 1093 -> Select (function
    | -1 -> r750
    | _ -> r745)
  | _ -> raise Not_found
