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
  [|0;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;2;3;2;1;2;1;2;1;1;2;1;2;1;1;1;1;1;2;1;1;2;3;3;3;1;2;1;2;1;1;2;1;1;1;2;1;2;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;2;3;3;4;1;1;1;2;1;1;1;2;1;2;3;1;1;2;3;1;1;2;1;2;1;3;1;1;1;1;1;1;2;2;2;3;2;3;1;4;5;1;1;1;2;1;2;1;2;1;1;2;1;2;3;1;2;1;2;1;1;2;1;1;2;2;1;2;1;2;1;1;1;2;3;2;1;2;3;4;2;3;2;3;1;3;4;2;3;1;2;1;3;1;1;2;1;1;3;2;3;1;1;2;3;2;3;4;4;2;3;5;2;1;2;3;2;3;4;5;3;4;3;4;4;5;6;2;1;1;2;3;2;3;3;4;5;6;1;1;2;1;3;4;5;2;3;1;2;3;4;5;4;2;3;2;1;1;2;1;1;1;1;1;2;1;1;1;1;2;3;4;5;6;7;2;3;1;2;1;1;2;1;2;2;3;4;3;4;3;3;2;1;1;2;3;1;2;2;3;4;5;2;3;1;4;4;5;6;7;5;2;6;7;1;2;1;2;3;4;5;6;7;1;2;3;1;1;2;1;1;2;4;5;3;4;8;9;1;2;2;2;1;1;1;2;3;4;2;3;1;1;1;1;2;3;3;3;3;3;1;3;2;3;1;1;1;1;1;2;3;4;5;1;2;1;1;1;2;1;2;2;1;2;2;1;1;2;3;4;5;5;1;2;3;4;1;2;1;2;3;4;1;1;1;2;1;1;1;2;2;3;1;4;2;1;1;1;1;2;2;2;3;2;3;1;2;1;1;2;3;3;1;2;4;5;6;2;1;2;3;3;3;4;5;1;1;2;3;4;2;3;4;3;2;3;1;5;2;3;2;1;2;3;3;1;1;3;4;5;2;1;2;3;2;5;6;2;3;1;1;2;3;1;1;1;2;1;2;1;1;1;2;3;1;2;3;4;5;2;3;3;4;2;1;1;4;5;5;6;7;1;1;1;1;1;2;1;3;1;1;1;1;2;3;1;2;3;1;4;3;1;1;2;2;3;1;2;1;1;1;1;1;2;1;1;1;1;1;2;3;1;1;2;3;2;3;2;1;2;1;1;2;3;2;3;2;3;3;4;5;2;3;2;3;3;1;1;4;2;2;3;3;4;1;2;2;3;4;1;1;2;1;1;2;3;3;4;2;1;2;3;1;1;2;3;4;5;6;1;1;1;2;1;2;1;1;1;1;1;2;1;2;1;2;1;2;3;1;2;1;2;1;1;2;1;2;3;3;4;5;1;2;3;4;1;2;5;6;1;1;2;3;4;1;2;3;4;1;1;2;1;1;2;3;4;4;5;6;7;8;9;1;1;1;1;1;2;3;2;3;2;3;2;3;1;1;1;1;2;1;2;1;2;1;2;2;3;4;5;6;1;2;1;2;3;1;1;2;3;1;1;2;1;2;3;2;3;2;3;2;1;2;1;2;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;3;2;3;2;3;1;2;3;1;2;1;2;3;4;5;1;2;3;3;4;2;2;7;3;4;5;6;7;3;4;5;6;7;3;4;5;2;1;2;1;1;2;4;5;3;4;5;3;4;5;3;4;5;3;4;4;2;3;5;6;1;3;4;4;5;6;3;4;5;1;2;3;2;3;1;2;3;1;2;3;1;1;2;1;2;3;4;1;2;3;4;4;5;1;2;3;1;5;1;1;6;7;8;9;10;5;6;7;8;4;5;6;7;8;9;10;2;1;2;3;4;1;2;1;2;3;4;1;1;2;5;7;3;4;3;4;5;2;3;3;4;2;3;1;3;4;5;6;7;3;3;4;3;2;3;2;2;3;3;2;3;2;3;4;2;2;3;4;7;2;3;4;1;2;3;4;5;6;7;1;2;2;3;4;5;2;4;5;2;1;2;3;4;1;2;1;2;3;1;1;2;5;2;3;4;5;6;7;8;3;4;5;6;7;2;4;5;2;3;4;5;6;7;8;2;3;3;4;3;4;5;6;3;4;5;6;4;5;5;6;7;5;6;7;7;8;9;2;3;3;4;5;2;4;5;3;4;5;6;1;2;1;2;3;4;1;2;1;2;3;4;1;2;3;4;5;6;1;2;7;8;1;2;3;4;5;6;7;4;5;6;1;1;1;2;3;1;2;3;4;5;1;2;6;2;3;4;5;6;4;5;3;4;5;6;7;1;2;3;4;1;2;3;1;2;3;1;4;1;2;3;5;6;7;1;2;1;2;3;3;4;1;2;1;2;1;2;3;4;5;1;2;3;4;5;3;4;1;2;3;1;2;1;2;1;2;3;4;5;1;2;3;6;7;1;1;2;1;1;2;3;1;2;3;4;1;1;3;4;2;1;2;1;2;3;3;4;1;2;1;2;8;9;2;3;4;5;6;1;2;3;4;5;1;2;3;1;2;3;4;1;1;1;2;1;2;3;1;3;1;5;4;6;7;8;1;1;1;2;3;4;5;6;7;2;1;1;2;1;1;1;1;1;2;3;4;5;6;2;1;1;1;1;1;1;1;2;1;1;1;2;3;4;5;6;7;8;2;1;1;1;2;3;4;5;6;7;8;2;1;2;2;2;1;2;3;4;5;1;1;2;3;4;1;2;1;2;3;1;2;3;4;5;6;7;3;4;5;6;7;3;4;5;6;5;6;7;1;1;2;3;4;5;6;7;3;4;5;6;7;3;4;5;6;7;8;9;10;11;6;7;8;5;2;3;1;2;3;4;1;2;3;1;1;1;2;3;4;1;2;1;2;1;2;1;1;2;1;3;2;3;2;3;2;3;3;4;5;4;1;2;5;6;1;2;3;4;1;2;1;2;2;1;2;3;4;1;2;6;7;1;1;1;1;1;2;1;1;1;1;1;1;2;3;4;5;2;1;1;1;1;1;1;1;1;2;1;1;1;1;2;3;4;5;6;7;2;1;2;1;2;3;1;1;1;3;4;3;4;3;4;5;6;7;2;3;4;5;6;7;8;2;3;3;4;5;3;4;2;3;4;8;5;6;7;1;2;8;9;2;1;1;1;3;4;4;5;2;3;4;4;5;6;5;6;3;4;2;3;4;5;6;3;4;5;6;7;8;4;5;3;4;5;6;3;4;3;2;3;4;5;6;1;2;1;0;1;2;1;0;1;2;1;|]

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
  let r0 = [R 441] in
  let r1 = S (N N_expr) :: r0 in
  let r2 = [R 133] in
  let r3 = S (T T_DONE) :: r2 in
  let r4 = Sub (r1) :: r3 in
  let r5 = S (T T_DO) :: r4 in
  let r6 = Sub (r1) :: r5 in
  let r7 = [R 555] in
  let r8 = S (T T_AND) :: r7 in
  let r9 = [R 14] in
  let r10 = Sub (r8) :: r9 in
  let r11 = [R 192] in
  let r12 = R 17 :: r11 in
  let r13 = [R 15] in
  let r14 = [R 405] in
  let r15 = S (N N_structure) :: r14 in
  let r16 = [R 16] in
  let r17 = S (T T_RBRACKET) :: r16 in
  let r18 = Sub (r15) :: r17 in
  let r19 = [R 151] in
  let r20 = S (T T_DONE) :: r19 in
  let r21 = Sub (r1) :: r20 in
  let r22 = S (T T_DO) :: r21 in
  let r23 = Sub (r1) :: r22 in
  let r24 = [R 293] in
  let r25 = [R 129] in
  let r26 = Sub (r1) :: r25 in
  let r27 = [R 145] in
  let r28 = S (N N_match_cases) :: r27 in
  let r29 = R 364 :: r28 in
  let r30 = S (T T_WITH) :: r29 in
  let r31 = Sub (r1) :: r30 in
  let r32 = [R 533] in
  let r33 = S (T T_QUESTIONQUESTION) :: r32 in
  let r34 = [R 521] in
  let r35 = [R 48] in
  let r36 = S (T T_LIDENT) :: r35 in
  let r37 = [R 523] in
  let r38 = Sub (r36) :: r37 in
  let r39 = [R 49] in
  let r40 = S (T T_LIDENT) :: r39 in
  let r41 = [R 294] in
  let r42 = [R 191] in
  let r43 = [R 18] in
  let r44 = [R 502] in
  let r45 = S (T T_RPAREN) :: r44 in
  let r46 = Sub (r1) :: r45 in
  let r47 = [R 99] in
  let r48 = [R 680] in
  let r49 = [R 193] in
  let r50 = S (T T_RBRACKET) :: r49 in
  let r51 = Sub (r15) :: r50 in
  let r52 = S (T T_LIDENT) :: r48 in
  let r53 = [R 476] in
  let r54 = S (T T_UNDERSCORE) :: r53 in
  let r55 = [R 473] in
  let r56 = Sub (r54) :: r55 in
  let r57 = [R 494] in
  let r58 = Sub (r56) :: r57 in
  let r59 = [R 114] in
  let r60 = Sub (r58) :: r59 in
  let r61 = [R 123] in
  let r62 = Sub (r60) :: r61 in
  let r63 = [R 112] in
  let r64 = Sub (r62) :: r63 in
  let r65 = [R 688] in
  let r66 = R 415 :: r65 in
  let r67 = Sub (r64) :: r66 in
  let r68 = S (T T_COLON) :: r67 in
  let r69 = Sub (r52) :: r68 in
  let r70 = [R 358] in
  let r71 = S (T T_AMPERAMPER) :: r70 in
  let r72 = [R 681] in
  let r73 = S (T T_RPAREN) :: r72 in
  let r74 = [R 290] in
  let r75 = [R 482] in
  let r76 = [R 220] in
  let r77 = S (T T_LIDENT) :: r76 in
  let r78 = [R 475] in
  let r79 = Sub (r77) :: r78 in
  let r80 = [R 115] in
  let r81 = Sub (r60) :: r80 in
  let r82 = S (T T_MINUSGREATER) :: r81 in
  let r83 = Sub (r60) :: r82 in
  let r84 = S (T T_COLON) :: r83 in
  let r85 = [R 116] in
  let r86 = Sub (r60) :: r85 in
  let r87 = S (T T_MINUSGREATER) :: r86 in
  let r88 = [R 384] in
  let r89 = S (N N_module_type) :: r88 in
  let r90 = [R 492] in
  let r91 = S (T T_RPAREN) :: r90 in
  let r92 = Sub (r89) :: r91 in
  let r93 = R 190 :: r92 in
  let r94 = [R 318] in
  let r95 = S (T T_END) :: r94 in
  let r96 = R 450 :: r95 in
  let r97 = [R 653] in
  let r98 = R 415 :: r97 in
  let r99 = R 105 :: r98 in
  let r100 = R 656 :: r99 in
  let r101 = S (T T_LIDENT) :: r100 in
  let r102 = R 377 :: r101 in
  let r103 = R 336 :: r102 in
  let r104 = R 190 :: r103 in
  let r105 = [R 381] in
  let r106 = S (T T_UNDERSCORE) :: r105 in
  let r107 = [R 374] in
  let r108 = Sub (r106) :: r107 in
  let r109 = R 675 :: r108 in
  let r110 = [R 375] in
  let r111 = Sub (r109) :: r110 in
  let r112 = [R 379] in
  let r113 = S (T T_RPAREN) :: r112 in
  let r114 = [R 380] in
  let r115 = [R 376] in
  let r116 = [R 661] in
  let r117 = [R 95] in
  let r118 = S (T T_FALSE) :: r117 in
  let r119 = [R 108] in
  let r120 = R 17 :: r119 in
  let r121 = R 215 :: r120 in
  let r122 = Sub (r118) :: r121 in
  let r123 = [R 109] in
  let r124 = Sub (r122) :: r123 in
  let r125 = [R 660] in
  let r126 = [R 93] in
  let r127 = [R 666] in
  let r128 = [R 117] in
  let r129 = Sub (r60) :: r128 in
  let r130 = S (T T_MINUSGREATER) :: r129 in
  let r131 = [R 481] in
  let r132 = [R 224] in
  let r133 = [R 480] in
  let r134 = [R 412] in
  let r135 = Sub (r62) :: r134 in
  let r136 = [R 201] in
  let r137 = R 17 :: r136 in
  let r138 = S (T T_SEMI) :: r137 in
  let r139 = R 17 :: r138 in
  let r140 = Sub (r135) :: r139 in
  let r141 = [R 678] in
  let r142 = [R 438] in
  let r143 = Sub (r56) :: r142 in
  let r144 = [R 439] in
  let r145 = Sub (r143) :: r144 in
  let r146 = [R 490] in
  let r147 = S (T T_RBRACKET) :: r146 in
  let r148 = Sub (r145) :: r147 in
  let r149 = [R 489] in
  let r150 = [R 488] in
  let r151 = S (T T_RBRACKET) :: r150 in
  let r152 = [R 486] in
  let r153 = S (T T_RBRACKET) :: r152 in
  let r154 = Sub (r145) :: r153 in
  let r155 = [R 333] in
  let r156 = Sub (r77) :: r155 in
  let r157 = [R 483] in
  let r158 = [R 667] in
  let r159 = S (T T_LIDENT) :: r158 in
  let r160 = S (T T_DOT) :: r159 in
  let r161 = S (T T_UIDENT) :: r74 in
  let r162 = [R 292] in
  let r163 = S (T T_RPAREN) :: r162 in
  let r164 = [R 291] in
  let r165 = [R 440] in
  let r166 = [R 642] in
  let r167 = [R 5] in
  let r168 = Sub (r62) :: r167 in
  let r169 = [R 641] in
  let r170 = R 17 :: r169 in
  let r171 = Sub (r168) :: r170 in
  let r172 = [R 121] in
  let r173 = Sub (r56) :: r172 in
  let r174 = [R 495] in
  let r175 = [R 122] in
  let r176 = [R 118] in
  let r177 = [R 124] in
  let r178 = Sub (r77) :: r177 in
  let r179 = [R 6] in
  let r180 = [R 485] in
  let r181 = [R 487] in
  let r182 = S (T T_RBRACKET) :: r181 in
  let r183 = Sub (r145) :: r182 in
  let r184 = S (T T_BACKQUOTE) :: r156 in
  let r185 = [R 334] in
  let r186 = Sub (r184) :: r185 in
  let r187 = [R 491] in
  let r188 = S (T T_RBRACKET) :: r187 in
  let r189 = [R 413] in
  let r190 = Sub (r62) :: r189 in
  let r191 = [R 679] in
  let r192 = [R 94] in
  let r193 = [R 474] in
  let r194 = [R 484] in
  let r195 = [R 120] in
  let r196 = [R 119] in
  let r197 = [R 92] in
  let r198 = [R 19] in
  let r199 = R 17 :: r198 in
  let r200 = R 215 :: r199 in
  let r201 = [R 106] in
  let r202 = Sub (r173) :: r201 in
  let r203 = [R 216] in
  let r204 = S (T T_LIDENT) :: r132 in
  let r205 = [R 225] in
  let r206 = R 17 :: r205 in
  let r207 = Sub (r135) :: r206 in
  let r208 = S (T T_COLON) :: r207 in
  let r209 = Sub (r204) :: r208 in
  let r210 = R 331 :: r209 in
  let r211 = [R 227] in
  let r212 = Sub (r210) :: r211 in
  let r213 = [R 107] in
  let r214 = S (T T_RBRACE) :: r213 in
  let r215 = [R 226] in
  let r216 = R 17 :: r215 in
  let r217 = S (T T_SEMI) :: r216 in
  let r218 = R 17 :: r217 in
  let r219 = Sub (r135) :: r218 in
  let r220 = S (T T_COLON) :: r219 in
  let r221 = [R 218] in
  let r222 = [R 217] in
  let r223 = Sub (r56) :: r222 in
  let r224 = [R 662] in
  let r225 = S (T T_RBRACE) :: r224 in
  let r226 = Sub (r212) :: r225 in
  let r227 = [R 664] in
  let r228 = [R 663] in
  let r229 = [R 665] in
  let r230 = S (T T_RBRACE) :: r229 in
  let r231 = [R 414] in
  let r232 = S (T T_RBRACKET) :: r231 in
  let r233 = Sub (r15) :: r232 in
  let r234 = [R 194] in
  let r235 = R 17 :: r234 in
  let r236 = R 215 :: r235 in
  let r237 = Sub (r118) :: r236 in
  let r238 = [R 606] in
  let r239 = Sub (r237) :: r238 in
  let r240 = [R 613] in
  let r241 = R 415 :: r240 in
  let r242 = Sub (r239) :: r241 in
  let r243 = R 420 :: r242 in
  let r244 = [R 20] in
  let r245 = R 17 :: r244 in
  let r246 = R 215 :: r245 in
  let r247 = Sub (r118) :: r246 in
  let r248 = [R 100] in
  let r249 = S (T T_FALSE) :: r248 in
  let r250 = [R 21] in
  let r251 = R 17 :: r250 in
  let r252 = Sub (r249) :: r251 in
  let r253 = S (T T_EQUAL) :: r252 in
  let r254 = [R 98] in
  let r255 = [R 416] in
  let r256 = [R 195] in
  let r257 = R 17 :: r256 in
  let r258 = Sub (r249) :: r257 in
  let r259 = [R 643] in
  let r260 = [R 637] in
  let r261 = S (T T_UIDENT) :: r24 in
  let r262 = [R 338] in
  let r263 = R 415 :: r262 in
  let r264 = Sub (r261) :: r263 in
  let r265 = R 190 :: r264 in
  let r266 = [R 74] in
  let r267 = R 41 :: r266 in
  let r268 = R 52 :: r267 in
  let r269 = [R 184] in
  let r270 = S (T T_END) :: r269 in
  let r271 = Sub (r268) :: r270 in
  let r272 = [R 50] in
  let r273 = S (T T_RPAREN) :: r272 in
  let r274 = [R 538] in
  let r275 = S (T T_LIDENT) :: r127 in
  let r276 = [R 543] in
  let r277 = [R 471] in
  let r278 = [R 469] in
  let r279 = [R 548] in
  let r280 = S (T T_RPAREN) :: r279 in
  let r281 = [R 550] in
  let r282 = S (T T_RPAREN) :: r281 in
  let r283 = S (T T_UIDENT) :: r282 in
  let r284 = [R 551] in
  let r285 = S (T T_RPAREN) :: r284 in
  let r286 = [R 322] in
  let r287 = S (N N_module_expr) :: r286 in
  let r288 = R 17 :: r287 in
  let r289 = S (T T_OF) :: r288 in
  let r290 = [R 305] in
  let r291 = S (T T_END) :: r290 in
  let r292 = S (N N_structure) :: r291 in
  let r293 = [R 297] in
  let r294 = S (N N_module_expr) :: r293 in
  let r295 = S (T T_EQUAL) :: r294 in
  let r296 = [R 429] in
  let r297 = R 415 :: r296 in
  let r298 = Sub (r295) :: r297 in
  let r299 = S (T T_UIDENT) :: r298 in
  let r300 = S (T T_REC) :: r299 in
  let r301 = [R 326] in
  let r302 = R 415 :: r301 in
  let r303 = R 327 :: r302 in
  let r304 = Sub (r77) :: r303 in
  let r305 = R 190 :: r304 in
  let r306 = [R 328] in
  let r307 = [R 323] in
  let r308 = S (T T_RPAREN) :: r307 in
  let r309 = [R 319] in
  let r310 = S (N N_module_type) :: r309 in
  let r311 = S (T T_MINUSGREATER) :: r310 in
  let r312 = S (N N_functor_args) :: r311 in
  let r313 = [R 209] in
  let r314 = [R 210] in
  let r315 = S (T T_RPAREN) :: r314 in
  let r316 = S (N N_module_type) :: r315 in
  let r317 = [R 697] in
  let r318 = Sub (r161) :: r317 in
  let r319 = S (T T_COLONEQUAL) :: r318 in
  let r320 = S (T T_UIDENT) :: r319 in
  let r321 = S (T T_MODULE) :: r320 in
  let r322 = [R 698] in
  let r323 = Sub (r321) :: r322 in
  let r324 = [R 321] in
  let r325 = [R 695] in
  let r326 = Sub (r62) :: r325 in
  let r327 = S (T T_COLONEQUAL) :: r326 in
  let r328 = Sub (r204) :: r327 in
  let r329 = [R 674] in
  let r330 = Sub (r77) :: r329 in
  let r331 = S (T T_QUOTE) :: r330 in
  let r332 = [R 668] in
  let r333 = Sub (r331) :: r332 in
  let r334 = R 675 :: r333 in
  let r335 = [R 669] in
  let r336 = Sub (r334) :: r335 in
  let r337 = [R 673] in
  let r338 = S (T T_RPAREN) :: r337 in
  let r339 = [R 670] in
  let r340 = [R 238] in
  let r341 = S (T T_LIDENT) :: r340 in
  let r342 = [R 700] in
  let r343 = S (T T_EQUAL) :: r342 in
  let r344 = [R 694] in
  let r345 = R 105 :: r344 in
  let r346 = Sub (r62) :: r345 in
  let r347 = [R 102] in
  let r348 = Sub (r64) :: r347 in
  let r349 = S (T T_EQUAL) :: r348 in
  let r350 = Sub (r64) :: r349 in
  let r351 = [R 104] in
  let r352 = [R 696] in
  let r353 = Sub (r161) :: r352 in
  let r354 = [R 699] in
  let r355 = [R 320] in
  let r356 = [R 330] in
  let r357 = Sub (r77) :: r356 in
  let r358 = [R 296] in
  let r359 = R 415 :: r358 in
  let r360 = Sub (r295) :: r359 in
  let r361 = [R 310] in
  let r362 = S (T T_RPAREN) :: r361 in
  let r363 = [R 311] in
  let r364 = S (T T_RPAREN) :: r363 in
  let r365 = S (N N_expr) :: r364 in
  let r366 = [R 128] in
  let r367 = S (N N_match_cases) :: r366 in
  let r368 = R 364 :: r367 in
  let r369 = S (T T_WITH) :: r368 in
  let r370 = Sub (r1) :: r369 in
  let r371 = [R 144] in
  let r372 = S (N N_match_cases) :: r371 in
  let r373 = R 364 :: r372 in
  let r374 = S (T T_WITH) :: r373 in
  let r375 = Sub (r1) :: r374 in
  let r376 = [R 529] in
  let r377 = S (T T_RPAREN) :: r376 in
  let r378 = S (N N_module_expr) :: r377 in
  let r379 = [R 306] in
  let r380 = S (N N_module_expr) :: r379 in
  let r381 = S (T T_MINUSGREATER) :: r380 in
  let r382 = S (N N_functor_args) :: r381 in
  let r383 = [R 308] in
  let r384 = [R 307] in
  let r385 = [R 530] in
  let r386 = S (T T_RPAREN) :: r385 in
  let r387 = [R 265] in
  let r388 = Sub (r1) :: r387 in
  let r389 = S (T T_EQUAL) :: r388 in
  let r390 = S (N N_pattern) :: r389 in
  let r391 = [R 273] in
  let r392 = R 415 :: r391 in
  let r393 = Sub (r390) :: r392 in
  let r394 = R 427 :: r393 in
  let r395 = [R 547] in
  let r396 = [R 401] in
  let r397 = S (N N_pattern) :: r396 in
  let r398 = [R 545] in
  let r399 = S (T T_RBRACKET) :: r398 in
  let r400 = R 370 :: r399 in
  let r401 = [R 237] in
  let r402 = S (T T_LIDENT) :: r401 in
  let r403 = [R 256] in
  let r404 = R 369 :: r403 in
  let r405 = Sub (r402) :: r404 in
  let r406 = [R 257] in
  let r407 = Sub (r405) :: r406 in
  let r408 = [R 544] in
  let r409 = S (T T_RBRACE) :: r408 in
  let r410 = [R 259] in
  let r411 = [R 368] in
  let r412 = [R 255] in
  let r413 = S (T T_UNDERSCORE) :: r274 in
  let r414 = [R 537] in
  let r415 = Sub (r413) :: r414 in
  let r416 = [R 396] in
  let r417 = Sub (r415) :: r416 in
  let r418 = [R 87] in
  let r419 = [R 397] in
  let r420 = S (N N_pattern) :: r419 in
  let r421 = S (T T_INT) :: r418 in
  let r422 = [R 468] in
  let r423 = Sub (r421) :: r422 in
  let r424 = [R 540] in
  let r425 = [R 399] in
  let r426 = [R 393] in
  let r427 = [R 392] in
  let r428 = [R 391] in
  let r429 = [R 400] in
  let r430 = [R 549] in
  let r431 = S (T T_RPAREN) :: r430 in
  let r432 = [R 395] in
  let r433 = [R 389] in
  let r434 = [R 546] in
  let r435 = S (T T_BARRBRACKET) :: r434 in
  let r436 = [R 239] in
  let r437 = S (T T_LIDENT) :: r436 in
  let r438 = [R 247] in
  let r439 = [R 235] in
  let r440 = Sub (r437) :: r439 in
  let r441 = [R 246] in
  let r442 = S (T T_RPAREN) :: r441 in
  let r443 = [R 236] in
  let r444 = [R 243] in
  let r445 = [R 242] in
  let r446 = S (T T_RPAREN) :: r445 in
  let r447 = R 366 :: r446 in
  let r448 = [R 367] in
  let r449 = [R 261] in
  let r450 = R 415 :: r449 in
  let r451 = Sub (r390) :: r450 in
  let r452 = R 427 :: r451 in
  let r453 = R 190 :: r452 in
  let r454 = [R 140] in
  let r455 = Sub (r1) :: r454 in
  let r456 = S (T T_IN) :: r455 in
  let r457 = Sub (r261) :: r456 in
  let r458 = R 190 :: r457 in
  let r459 = R 382 :: r458 in
  let r460 = [R 515] in
  let r461 = [R 188] in
  let r462 = S (N N_expr) :: r461 in
  let r463 = [R 518] in
  let r464 = S (T T_RBRACKET) :: r463 in
  let r465 = R 370 :: r464 in
  let r466 = [R 525] in
  let r467 = [R 198] in
  let r468 = [R 197] in
  let r469 = [R 251] in
  let r470 = R 373 :: r469 in
  let r471 = Sub (r402) :: r470 in
  let r472 = [R 252] in
  let r473 = Sub (r471) :: r472 in
  let r474 = [R 436] in
  let r475 = Sub (r473) :: r474 in
  let r476 = [R 512] in
  let r477 = S (T T_RBRACE) :: r476 in
  let r478 = [R 497] in
  let r479 = [R 496] in
  let r480 = S (T T_GREATERDOT) :: r479 in
  let r481 = [R 183] in
  let r482 = Sub (r33) :: r481 in
  let r483 = [R 504] in
  let r484 = S (T T_END) :: r483 in
  let r485 = [R 150] in
  let r486 = S (N N_expr) :: r485 in
  let r487 = S (T T_THEN) :: r486 in
  let r488 = Sub (r1) :: r487 in
  let r489 = [R 141] in
  let r490 = S (N N_match_cases) :: r489 in
  let r491 = R 364 :: r490 in
  let r492 = [R 276] in
  let r493 = Sub (r1) :: r492 in
  let r494 = S (T T_MINUSGREATER) :: r493 in
  let r495 = [R 277] in
  let r496 = Sub (r1) :: r495 in
  let r497 = S (T T_MINUSGREATER) :: r496 in
  let r498 = [R 249] in
  let r499 = Sub (r415) :: r498 in
  let r500 = [R 205] in
  let r501 = Sub (r1) :: r500 in
  let r502 = S (T T_MINUSGREATER) :: r501 in
  let r503 = [R 142] in
  let r504 = Sub (r502) :: r503 in
  let r505 = Sub (r499) :: r504 in
  let r506 = [R 404] in
  let r507 = S (T T_UNDERSCORE) :: r506 in
  let r508 = [R 245] in
  let r509 = [R 244] in
  let r510 = S (T T_RPAREN) :: r509 in
  let r511 = R 366 :: r510 in
  let r512 = [R 270] in
  let r513 = [R 271] in
  let r514 = S (T T_LIDENT) :: r513 in
  let r515 = [R 143] in
  let r516 = Sub (r502) :: r515 in
  let r517 = S (T T_RPAREN) :: r516 in
  let r518 = [R 135] in
  let r519 = S (T T_DONE) :: r518 in
  let r520 = Sub (r1) :: r519 in
  let r521 = S (T T_DO) :: r520 in
  let r522 = Sub (r1) :: r521 in
  let r523 = S (T T_IN) :: r522 in
  let r524 = S (N N_pattern) :: r523 in
  let r525 = [R 126] in
  let r526 = S (T T_DOWNTO) :: r525 in
  let r527 = [R 152] in
  let r528 = S (T T_DONE) :: r527 in
  let r529 = Sub (r1) :: r528 in
  let r530 = S (T T_DO) :: r529 in
  let r531 = Sub (r1) :: r530 in
  let r532 = Sub (r526) :: r531 in
  let r533 = Sub (r1) :: r532 in
  let r534 = S (T T_EQUAL) :: r533 in
  let r535 = S (N N_pattern) :: r534 in
  let r536 = [R 522] in
  let r537 = [R 508] in
  let r538 = S (T T_RPAREN) :: r537 in
  let r539 = S (T T_LPAREN) :: r538 in
  let r540 = S (T T_DOT) :: r539 in
  let r541 = [R 531] in
  let r542 = S (T T_RPAREN) :: r541 in
  let r543 = Sub (r89) :: r542 in
  let r544 = S (T T_COLON) :: r543 in
  let r545 = S (N N_module_expr) :: r544 in
  let r546 = [R 182] in
  let r547 = Sub (r33) :: r546 in
  let r548 = [R 527] in
  let r549 = [R 528] in
  let r550 = [R 511] in
  let r551 = S (T T_RBRACE) :: r550 in
  let r552 = S (N N_expr) :: r551 in
  let r553 = S (T T_LBRACE) :: r552 in
  let r554 = [R 509] in
  let r555 = S (T T_RPAREN) :: r554 in
  let r556 = Sub (r1) :: r555 in
  let r557 = [R 175] in
  let r558 = [R 234] in
  let r559 = S (T T_LIDENT) :: r558 in
  let r560 = [R 231] in
  let r561 = [R 232] in
  let r562 = [R 233] in
  let r563 = [R 230] in
  let r564 = [R 178] in
  let r565 = [R 127] in
  let r566 = Sub (r1) :: r565 in
  let r567 = [R 138] in
  let r568 = Sub (r1) :: r567 in
  let r569 = [R 181] in
  let r570 = S (N N_expr) :: r569 in
  let r571 = [R 186] in
  let r572 = [R 165] in
  let r573 = [R 159] in
  let r574 = [R 176] in
  let r575 = [R 162] in
  let r576 = [R 166] in
  let r577 = [R 158] in
  let r578 = [R 161] in
  let r579 = [R 160] in
  let r580 = [R 170] in
  let r581 = [R 164] in
  let r582 = [R 163] in
  let r583 = [R 168] in
  let r584 = [R 157] in
  let r585 = [R 156] in
  let r586 = [R 153] in
  let r587 = [R 155] in
  let r588 = [R 169] in
  let r589 = [R 167] in
  let r590 = [R 171] in
  let r591 = [R 172] in
  let r592 = [R 173] in
  let r593 = [R 187] in
  let r594 = [R 174] in
  let r595 = [R 10] in
  let r596 = R 415 :: r595 in
  let r597 = Sub (r390) :: r596 in
  let r598 = [R 266] in
  let r599 = Sub (r1) :: r598 in
  let r600 = S (T T_EQUAL) :: r599 in
  let r601 = [R 510] in
  let r602 = S (T T_RBRACKET) :: r601 in
  let r603 = Sub (r1) :: r602 in
  let r604 = [R 179] in
  let r605 = [R 180] in
  let r606 = [R 177] in
  let r607 = [R 507] in
  let r608 = [R 517] in
  let r609 = [R 516] in
  let r610 = S (T T_BARRBRACKET) :: r609 in
  let r611 = [R 520] in
  let r612 = [R 519] in
  let r613 = S (T T_RBRACKET) :: r612 in
  let r614 = Sub (r204) :: r467 in
  let r615 = [R 199] in
  let r616 = R 370 :: r615 in
  let r617 = Sub (r614) :: r616 in
  let r618 = [R 526] in
  let r619 = S (T T_GREATERRBRACE) :: r618 in
  let r620 = [R 513] in
  let r621 = S (T T_RBRACE) :: r620 in
  let r622 = [R 435] in
  let r623 = Sub (r473) :: r622 in
  let r624 = [R 652] in
  let r625 = [R 650] in
  let r626 = Sub (r64) :: r625 in
  let r627 = [R 651] in
  let r628 = [R 250] in
  let r629 = [R 134] in
  let r630 = S (T T_DONE) :: r629 in
  let r631 = Sub (r1) :: r630 in
  let r632 = S (T T_DO) :: r631 in
  let r633 = Sub (r1) :: r632 in
  let r634 = Sub (r526) :: r633 in
  let r635 = [R 208] in
  let r636 = Sub (r502) :: r635 in
  let r637 = S (T T_RPAREN) :: r636 in
  let r638 = [R 248] in
  let r639 = [R 206] in
  let r640 = Sub (r1) :: r639 in
  let r641 = S (T T_MINUSGREATER) :: r640 in
  let r642 = [R 207] in
  let r643 = S (N N_pattern) :: r494 in
  let r644 = [R 280] in
  let r645 = [R 149] in
  let r646 = [R 503] in
  let r647 = [R 524] in
  let r648 = [R 514] in
  let r649 = S (T T_BARRBRACKET) :: r648 in
  let r650 = [R 139] in
  let r651 = Sub (r1) :: r650 in
  let r652 = S (T T_IN) :: r651 in
  let r653 = Sub (r295) :: r652 in
  let r654 = S (T T_UIDENT) :: r653 in
  let r655 = [R 298] in
  let r656 = S (N N_module_expr) :: r655 in
  let r657 = S (T T_EQUAL) :: r656 in
  let r658 = [R 299] in
  let r659 = [R 614] in
  let r660 = Sub (r1) :: r659 in
  let r661 = S (T T_EQUAL) :: r660 in
  let r662 = [R 203] in
  let r663 = Sub (r661) :: r662 in
  let r664 = [R 616] in
  let r665 = Sub (r663) :: r664 in
  let r666 = S (T T_RPAREN) :: r665 in
  let r667 = Sub (r514) :: r666 in
  let r668 = [R 204] in
  let r669 = Sub (r1) :: r668 in
  let r670 = [R 615] in
  let r671 = [R 264] in
  let r672 = Sub (r1) :: r671 in
  let r673 = S (T T_EQUAL) :: r672 in
  let r674 = Sub (r64) :: r673 in
  let r675 = S (T T_DOT) :: r674 in
  let r676 = [R 263] in
  let r677 = Sub (r1) :: r676 in
  let r678 = S (T T_EQUAL) :: r677 in
  let r679 = Sub (r64) :: r678 in
  let r680 = [R 154] in
  let r681 = S (T T_RPAREN) :: r680 in
  let r682 = S (N N_expr) :: r681 in
  let r683 = S (T T_COMMA) :: r682 in
  let r684 = S (N N_expr) :: r683 in
  let r685 = S (T T_LPAREN) :: r684 in
  let r686 = [R 505] in
  let r687 = [R 314] in
  let r688 = S (T T_RPAREN) :: r687 in
  let r689 = [R 312] in
  let r690 = S (T T_RPAREN) :: r689 in
  let r691 = [R 313] in
  let r692 = S (T T_RPAREN) :: r691 in
  let r693 = [R 309] in
  let r694 = S (T T_RPAREN) :: r693 in
  let r695 = [R 223] in
  let r696 = S (T T_RBRACKET) :: r695 in
  let r697 = Sub (r15) :: r696 in
  let r698 = [R 408] in
  let r699 = [R 409] in
  let r700 = [R 202] in
  let r701 = S (T T_RBRACKET) :: r700 in
  let r702 = Sub (r15) :: r701 in
  let r703 = [R 612] in
  let r704 = R 415 :: r703 in
  let r705 = S (N N_module_expr) :: r704 in
  let r706 = [R 418] in
  let r707 = S (T T_STRING) :: r706 in
  let r708 = [R 417] in
  let r709 = R 415 :: r708 in
  let r710 = Sub (r707) :: r709 in
  let r711 = S (T T_EQUAL) :: r710 in
  let r712 = Sub (r64) :: r711 in
  let r713 = S (T T_COLON) :: r712 in
  let r714 = Sub (r52) :: r713 in
  let r715 = [R 605] in
  let r716 = R 415 :: r715 in
  let r717 = R 17 :: r716 in
  let r718 = Sub (r249) :: r717 in
  let r719 = S (T T_EQUAL) :: r718 in
  let r720 = Sub (r118) :: r719 in
  let r721 = [R 444] in
  let r722 = R 415 :: r721 in
  let r723 = R 17 :: r722 in
  let r724 = R 215 :: r723 in
  let r725 = Sub (r118) :: r724 in
  let r726 = R 190 :: r725 in
  let r727 = [R 406] in
  let r728 = [R 451] in
  let r729 = [R 432] in
  let r730 = R 415 :: r729 in
  let r731 = S (N N_module_type) :: r730 in
  let r732 = S (T T_COLON) :: r731 in
  let r733 = S (T T_UIDENT) :: r732 in
  let r734 = S (T T_REC) :: r733 in
  let r735 = [R 301] in
  let r736 = S (N N_module_type) :: r735 in
  let r737 = S (T T_COLON) :: r736 in
  let r738 = [R 300] in
  let r739 = R 415 :: r738 in
  let r740 = [R 303] in
  let r741 = Sub (r737) :: r740 in
  let r742 = [R 302] in
  let r743 = Sub (r737) :: r742 in
  let r744 = S (T T_RPAREN) :: r743 in
  let r745 = S (N N_module_type) :: r744 in
  let r746 = [R 295] in
  let r747 = R 415 :: r746 in
  let r748 = [R 448] in
  let r749 = R 415 :: r748 in
  let r750 = S (N N_module_type) :: r749 in
  let r751 = [R 85] in
  let r752 = S (T T_LIDENT) :: r751 in
  let r753 = [R 65] in
  let r754 = Sub (r752) :: r753 in
  let r755 = [R 80] in
  let r756 = R 415 :: r755 in
  let r757 = Sub (r754) :: r756 in
  let r758 = S (T T_EQUAL) :: r757 in
  let r759 = S (T T_LIDENT) :: r758 in
  let r760 = R 83 :: r759 in
  let r761 = R 692 :: r760 in
  let r762 = R 190 :: r761 in
  let r763 = [R 84] in
  let r764 = S (T T_RBRACKET) :: r763 in
  let r765 = [R 55] in
  let r766 = R 62 :: r765 in
  let r767 = R 54 :: r766 in
  let r768 = [R 66] in
  let r769 = S (T T_END) :: r768 in
  let r770 = Sub (r767) :: r769 in
  let r771 = [R 53] in
  let r772 = S (T T_RPAREN) :: r771 in
  let r773 = [R 691] in
  let r774 = Sub (r64) :: r773 in
  let r775 = S (T T_COLON) :: r774 in
  let r776 = Sub (r204) :: r775 in
  let r777 = [R 57] in
  let r778 = R 415 :: r777 in
  let r779 = Sub (r776) :: r778 in
  let r780 = [R 689] in
  let r781 = Sub (r64) :: r780 in
  let r782 = S (T T_COLON) :: r781 in
  let r783 = Sub (r204) :: r782 in
  let r784 = [R 690] in
  let r785 = Sub (r64) :: r784 in
  let r786 = S (T T_COLON) :: r785 in
  let r787 = Sub (r204) :: r786 in
  let r788 = [R 410] in
  let r789 = Sub (r64) :: r788 in
  let r790 = [R 58] in
  let r791 = R 415 :: r790 in
  let r792 = Sub (r789) :: r791 in
  let r793 = S (T T_COLON) :: r792 in
  let r794 = Sub (r204) :: r793 in
  let r795 = R 422 :: r794 in
  let r796 = [R 411] in
  let r797 = Sub (r64) :: r796 in
  let r798 = [R 56] in
  let r799 = R 415 :: r798 in
  let r800 = Sub (r754) :: r799 in
  let r801 = Sub (r64) :: r196 in
  let r802 = [R 64] in
  let r803 = Sub (r752) :: r802 in
  let r804 = S (T T_RBRACKET) :: r803 in
  let r805 = [R 86] in
  let r806 = S (T T_LIDENT) :: r805 in
  let r807 = [R 103] in
  let r808 = Sub (r64) :: r807 in
  let r809 = S (T T_EQUAL) :: r808 in
  let r810 = Sub (r64) :: r809 in
  let r811 = [R 59] in
  let r812 = R 415 :: r811 in
  let r813 = Sub (r810) :: r812 in
  let r814 = [R 60] in
  let r815 = [R 75] in
  let r816 = Sub (r754) :: r815 in
  let r817 = [R 25] in
  let r818 = R 415 :: r817 in
  let r819 = Sub (r816) :: r818 in
  let r820 = S (T T_COLON) :: r819 in
  let r821 = S (T T_LIDENT) :: r820 in
  let r822 = R 83 :: r821 in
  let r823 = [R 76] in
  let r824 = Sub (r816) :: r823 in
  let r825 = S (T T_MINUSGREATER) :: r824 in
  let r826 = Sub (r58) :: r825 in
  let r827 = S (T T_COLON) :: r826 in
  let r828 = [R 77] in
  let r829 = Sub (r816) :: r828 in
  let r830 = S (T T_MINUSGREATER) :: r829 in
  let r831 = [R 78] in
  let r832 = Sub (r816) :: r831 in
  let r833 = S (T T_MINUSGREATER) :: r832 in
  let r834 = [R 79] in
  let r835 = Sub (r816) :: r834 in
  let r836 = [R 13] in
  let r837 = R 415 :: r836 in
  let r838 = R 105 :: r837 in
  let r839 = R 656 :: r838 in
  let r840 = S (T T_LIDENT) :: r839 in
  let r841 = R 377 :: r840 in
  let r842 = [R 452] in
  let r843 = [R 12] in
  let r844 = R 415 :: r843 in
  let r845 = S (N N_module_type) :: r844 in
  let r846 = S (T T_COLON) :: r845 in
  let r847 = S (T T_UIDENT) :: r846 in
  let r848 = [R 466] in
  let r849 = [R 9] in
  let r850 = R 415 :: r849 in
  let r851 = Sub (r754) :: r850 in
  let r852 = S (T T_EQUAL) :: r851 in
  let r853 = S (T T_LIDENT) :: r852 in
  let r854 = R 83 :: r853 in
  let r855 = R 692 :: r854 in
  let r856 = [R 8] in
  let r857 = R 415 :: r856 in
  let r858 = Sub (r816) :: r857 in
  let r859 = S (T T_COLON) :: r858 in
  let r860 = S (T T_LIDENT) :: r859 in
  let r861 = R 83 :: r860 in
  let r862 = R 692 :: r861 in
  let r863 = [R 70] in
  let r864 = Sub (r36) :: r863 in
  let r865 = [R 28] in
  let r866 = Sub (r864) :: r865 in
  let r867 = [R 43] in
  let r868 = Sub (r866) :: r867 in
  let r869 = S (T T_EQUAL) :: r868 in
  let r870 = [R 22] in
  let r871 = R 415 :: r870 in
  let r872 = Sub (r869) :: r871 in
  let r873 = S (T T_LIDENT) :: r872 in
  let r874 = R 83 :: r873 in
  let r875 = [R 71] in
  let r876 = S (T T_END) :: r875 in
  let r877 = Sub (r268) :: r876 in
  let r878 = [R 686] in
  let r879 = Sub (r1) :: r878 in
  let r880 = S (T T_EQUAL) :: r879 in
  let r881 = Sub (r204) :: r880 in
  let r882 = R 331 :: r881 in
  let r883 = R 17 :: r882 in
  let r884 = R 382 :: r883 in
  let r885 = [R 35] in
  let r886 = R 415 :: r885 in
  let r887 = [R 685] in
  let r888 = Sub (r64) :: r887 in
  let r889 = S (T T_COLON) :: r888 in
  let r890 = Sub (r204) :: r889 in
  let r891 = [R 684] in
  let r892 = Sub (r64) :: r891 in
  let r893 = S (T T_COLON) :: r892 in
  let r894 = [R 687] in
  let r895 = Sub (r1) :: r894 in
  let r896 = [R 287] in
  let r897 = Sub (r661) :: r896 in
  let r898 = Sub (r204) :: r897 in
  let r899 = R 420 :: r898 in
  let r900 = R 17 :: r899 in
  let r901 = R 382 :: r900 in
  let r902 = [R 36] in
  let r903 = R 415 :: r902 in
  let r904 = [R 286] in
  let r905 = Sub (r789) :: r904 in
  let r906 = S (T T_COLON) :: r905 in
  let r907 = Sub (r204) :: r906 in
  let r908 = [R 285] in
  let r909 = Sub (r789) :: r908 in
  let r910 = S (T T_COLON) :: r909 in
  let r911 = [R 288] in
  let r912 = Sub (r1) :: r911 in
  let r913 = S (T T_EQUAL) :: r912 in
  let r914 = [R 289] in
  let r915 = Sub (r1) :: r914 in
  let r916 = S (T T_EQUAL) :: r915 in
  let r917 = Sub (r64) :: r916 in
  let r918 = S (T T_DOT) :: r917 in
  let r919 = [R 38] in
  let r920 = R 415 :: r919 in
  let r921 = Sub (r1) :: r920 in
  let r922 = [R 34] in
  let r923 = R 415 :: r922 in
  let r924 = R 386 :: r923 in
  let r925 = Sub (r866) :: r924 in
  let r926 = R 17 :: r925 in
  let r927 = [R 73] in
  let r928 = S (T T_RPAREN) :: r927 in
  let r929 = [R 69] in
  let r930 = Sub (r36) :: r929 in
  let r931 = S (T T_RBRACKET) :: r930 in
  let r932 = [R 46] in
  let r933 = Sub (r866) :: r932 in
  let r934 = S (T T_MINUSGREATER) :: r933 in
  let r935 = Sub (r499) :: r934 in
  let r936 = [R 29] in
  let r937 = Sub (r935) :: r936 in
  let r938 = [R 31] in
  let r939 = Sub (r866) :: r938 in
  let r940 = [R 72] in
  let r941 = S (T T_RPAREN) :: r940 in
  let r942 = [R 385] in
  let r943 = [R 37] in
  let r944 = R 415 :: r943 in
  let r945 = Sub (r810) :: r944 in
  let r946 = [R 39] in
  let r947 = [R 44] in
  let r948 = Sub (r866) :: r947 in
  let r949 = S (T T_EQUAL) :: r948 in
  let r950 = [R 45] in
  let r951 = [R 618] in
  let r952 = [R 638] in
  let r953 = [R 11] in
  let r954 = R 415 :: r953 in
  let r955 = Sub (r295) :: r954 in
  let r956 = S (T T_UIDENT) :: r955 in
  let r957 = [R 634] in
  let r958 = [R 7] in
  let r959 = R 415 :: r958 in
  let r960 = Sub (r869) :: r959 in
  let r961 = S (T T_LIDENT) :: r960 in
  let r962 = R 83 :: r961 in
  let r963 = R 692 :: r962 in
  let r964 = [R 617] in
  let r965 = R 636 :: r964 in
  let r966 = [R 394] in
  let r967 = S (T T_RPAREN) :: r966 in
  let r968 = S (N N_pattern) :: r967 in
  let r969 = S (T T_COMMA) :: r968 in
  let r970 = S (N N_pattern) :: r969 in
  let r971 = S (T T_LPAREN) :: r970 in
  let r972 = [R 51] in
  let r973 = S (T T_RPAREN) :: r972 in
  let r974 = [R 445] in
  let r975 = Sub (r237) :: r974 in
  let r976 = [R 449] in
  let r977 = R 415 :: r976 in
  let r978 = Sub (r975) :: r977 in
  let r979 = R 420 :: r978 in
  let r980 = [R 130] in
  let r981 = S (N N_match_cases) :: r980 in
  let r982 = [R 132] in
  let r983 = [R 131] in
  let r984 = [R 221] in
  let r985 = [R 222] in
  let r986 = [R 387] in
  function
  | 0 | 1511 | 1515 -> Nothing
  | 1510 -> One ([R 0])
  | 1514 -> One ([R 1])
  | 1518 -> One ([R 2])
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
  | 784 -> One (R 17 :: r597)
  | 1110 -> One (R 17 :: r770)
  | 1119 -> One (R 17 :: r779)
  | 1136 -> One (R 17 :: r795)
  | 1151 -> One (R 17 :: r800)
  | 1166 -> One (R 17 :: r813)
  | 1213 -> One (R 17 :: r841)
  | 1228 -> One (R 17 :: r847)
  | 1245 -> One (R 17 :: r855)
  | 1256 -> One (R 17 :: r862)
  | 1275 -> One (R 17 :: r877)
  | 1331 -> One (R 17 :: r921)
  | 1344 -> One (R 17 :: r937)
  | 1369 -> One (R 17 :: r945)
  | 1397 -> One (R 17 :: r956)
  | 1415 -> One (R 17 :: r963)
  | 1423 -> One ([R 23])
  | 1422 -> One ([R 24])
  | 1265 -> One ([R 26])
  | 1264 -> One ([R 27])
  | 1352 -> One ([R 30])
  | 1355 -> One ([R 32])
  | 1350 -> One ([R 33])
  | 1375 -> One ([R 40])
  | 1376 -> One ([R 42])
  | 1357 -> One ([R 47])
  | 1175 -> One ([R 61])
  | 1176 -> One ([R 63])
  | 1165 -> One ([R 67])
  | 1161 -> One ([R 68])
  | 1254 -> One ([R 81])
  | 1253 -> One ([R 82])
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
  | 874 -> One ([R 125])
  | 701 -> One ([R 136])
  | 812 -> One ([R 137])
  | 728 -> One ([R 146])
  | 737 -> One ([R 147])
  | 717 -> One ([R 148])
  | 735 -> One ([R 185])
  | 833 -> One ([R 189])
  | 1 -> One (R 190 :: r6)
  | 60 -> One (R 190 :: r23)
  | 63 -> One (R 190 :: r26)
  | 65 -> One (R 190 :: r31)
  | 71 -> One (R 190 :: r38)
  | 90 -> One (R 190 :: r69)
  | 388 -> One (R 190 :: r271)
  | 402 -> One (R 190 :: r283)
  | 500 -> One (R 190 :: r370)
  | 502 -> One (R 190 :: r375)
  | 507 -> One (R 190 :: r378)
  | 530 -> One (R 190 :: r394)
  | 551 -> One (R 190 :: r417)
  | 557 -> One (R 190 :: r420)
  | 634 -> One (R 190 :: r482)
  | 636 -> One (R 190 :: r484)
  | 638 -> One (R 190 :: r488)
  | 640 -> One (R 190 :: r491)
  | 645 -> One (R 190 :: r505)
  | 665 -> One (R 190 :: r524)
  | 669 -> One (R 190 :: r535)
  | 681 -> One (R 190 :: r545)
  | 691 -> One (R 190 :: r547)
  | 945 -> One (R 190 :: r654)
  | 1043 -> One (R 190 :: r705)
  | 1047 -> One (R 190 :: r714)
  | 1069 -> One (R 190 :: r734)
  | 1092 -> One (R 190 :: r750)
  | 847 -> One ([R 200])
  | 423 -> One ([R 211])
  | 422 -> One ([R 212])
  | 485 -> One ([R 213])
  | 486 -> One ([R 214])
  | 124 | 478 -> One ([R 219])
  | 292 -> One ([R 228])
  | 293 -> One ([R 229])
  | 813 -> One ([R 240])
  | 815 -> One ([R 241])
  | 855 -> One ([R 253])
  | 854 -> One ([R 254])
  | 541 -> One ([R 258])
  | 545 -> One ([R 260])
  | 990 -> One ([R 262])
  | 725 -> One ([R 267])
  | 796 -> One ([R 268])
  | 650 -> One ([R 269])
  | 661 -> One ([R 272])
  | 721 -> One ([R 274])
  | 797 -> One ([R 275])
  | 914 -> One ([R 278])
  | 919 -> One ([R 279])
  | 255 -> One ([R 281])
  | 254 -> One ([R 282])
  | 256 -> One ([R 283])
  | 167 -> One ([R 284])
  | 519 -> One ([R 304])
  | 518 -> One ([R 315])
  | 520 -> One ([R 316])
  | 427 -> One ([R 317])
  | 481 -> One ([R 324])
  | 475 -> One ([R 325])
  | 480 -> One ([R 329])
  | 1121 -> One (R 331 :: r783)
  | 1286 -> One (R 331 :: r890)
  | 282 | 1291 -> One ([R 332])
  | 242 -> One ([R 335])
  | 139 -> One ([R 337])
  | 86 | 93 -> One ([R 339])
  | 106 -> One ([R 340])
  | 105 -> One ([R 341])
  | 104 -> One ([R 342])
  | 103 -> One ([R 343])
  | 102 -> One ([R 344])
  | 107 -> One ([R 345])
  | 112 | 687 -> One ([R 346])
  | 96 | 401 | 506 -> One ([R 347])
  | 95 | 505 -> One ([R 348])
  | 100 | 528 | 554 -> One ([R 349])
  | 99 | 527 -> One ([R 350])
  | 84 -> One ([R 351])
  | 109 -> One ([R 352])
  | 101 -> One ([R 353])
  | 108 -> One ([R 354])
  | 98 -> One ([R 355])
  | 111 -> One ([R 356])
  | 113 -> One ([R 357])
  | 110 -> One ([R 359])
  | 94 -> One ([R 360])
  | 97 -> One ([R 361])
  | 206 -> One ([R 362])
  | 205 -> One (R 363 :: r171)
  | 174 -> One (R 364 :: r148)
  | 1489 -> One (R 364 :: r981)
  | 175 -> One ([R 365])
  | 542 -> One (R 370 :: r410)
  | 601 -> One (R 370 :: r435)
  | 831 -> One (R 370 :: r610)
  | 839 -> One (R 370 :: r613)
  | 941 -> One (R 370 :: r649)
  | 543 | 595 | 832 | 846 -> One ([R 371])
  | 863 -> One ([R 372])
  | 367 -> One ([R 378])
  | 382 -> One (R 382 :: r265)
  | 1335 -> One (R 382 :: r926)
  | 383 -> One ([R 383])
  | 562 -> One ([R 388])
  | 567 -> One ([R 390])
  | 572 -> One ([R 398])
  | 596 -> One ([R 402])
  | 656 -> One ([R 403])
  | 1268 -> One ([R 407])
  | 353 -> One (R 415 :: r255)
  | 1173 -> One (R 415 :: r814)
  | 1241 -> One (R 415 :: r848)
  | 1373 -> One (R 415 :: r946)
  | 1410 -> One (R 415 :: r957)
  | 1425 -> One (R 415 :: r965)
  | 1054 -> One ([R 419])
  | 1306 -> One (R 420 :: r907)
  | 319 | 1311 -> One ([R 421])
  | 1140 -> One ([R 423])
  | 1138 -> One ([R 424])
  | 1141 -> One ([R 425])
  | 1139 -> One ([R 426])
  | 532 -> One ([R 428])
  | 1403 -> One ([R 430])
  | 1402 -> One ([R 431])
  | 1235 -> One ([R 433])
  | 1234 -> One ([R 434])
  | 186 -> One ([R 437])
  | 782 -> One ([R 442])
  | 783 -> One ([R 443])
  | 1468 -> One ([R 446])
  | 1465 -> One ([R 447])
  | 1067 -> One (R 450 :: r727)
  | 1068 -> One (R 450 :: r728)
  | 1222 -> One (R 450 :: r842)
  | 1211 -> One ([R 453])
  | 1236 -> One ([R 454])
  | 1212 -> One ([R 455])
  | 1224 -> One ([R 456])
  | 1226 -> One ([R 457])
  | 1239 -> One ([R 458])
  | 1240 -> One ([R 459])
  | 1227 -> One ([R 460])
  | 1238 -> One ([R 461])
  | 1237 -> One ([R 462])
  | 1225 -> One ([R 463])
  | 1255 -> One ([R 464])
  | 1244 -> One ([R 465])
  | 1243 -> One ([R 467])
  | 399 -> One ([R 470])
  | 396 -> One ([R 472])
  | 185 -> One ([R 477])
  | 190 -> One ([R 478])
  | 267 -> One ([R 479])
  | 212 | 1203 -> One ([R 493])
  | 674 -> One ([R 498])
  | 690 -> One ([R 499])
  | 689 | 736 -> One ([R 500])
  | 676 | 716 -> One ([R 501])
  | 809 | 826 -> One ([R 506])
  | 688 -> One ([R 532])
  | 816 -> One ([R 534])
  | 814 -> One ([R 535])
  | 560 | 604 -> One ([R 536])
  | 563 -> One ([R 539])
  | 592 -> One ([R 541])
  | 591 -> One ([R 542])
  | 575 -> One ([R 552])
  | 28 -> One ([R 553])
  | 8 -> One ([R 554])
  | 52 -> One ([R 556])
  | 51 -> One ([R 557])
  | 50 -> One ([R 558])
  | 49 -> One ([R 559])
  | 48 -> One ([R 560])
  | 47 -> One ([R 561])
  | 46 -> One ([R 562])
  | 45 -> One ([R 563])
  | 44 -> One ([R 564])
  | 43 -> One ([R 565])
  | 42 -> One ([R 566])
  | 41 -> One ([R 567])
  | 40 -> One ([R 568])
  | 39 -> One ([R 569])
  | 38 -> One ([R 570])
  | 37 -> One ([R 571])
  | 36 -> One ([R 572])
  | 35 -> One ([R 573])
  | 34 -> One ([R 574])
  | 33 -> One ([R 575])
  | 32 -> One ([R 576])
  | 31 -> One ([R 577])
  | 30 -> One ([R 578])
  | 29 -> One ([R 579])
  | 27 -> One ([R 580])
  | 26 -> One ([R 581])
  | 25 -> One ([R 582])
  | 24 -> One ([R 583])
  | 23 -> One ([R 584])
  | 22 -> One ([R 585])
  | 21 -> One ([R 586])
  | 20 -> One ([R 587])
  | 19 -> One ([R 588])
  | 18 -> One ([R 589])
  | 17 -> One ([R 590])
  | 16 -> One ([R 591])
  | 15 -> One ([R 592])
  | 14 -> One ([R 593])
  | 13 -> One ([R 594])
  | 12 -> One ([R 595])
  | 11 -> One ([R 596])
  | 10 -> One ([R 597])
  | 9 -> One ([R 598])
  | 7 -> One ([R 599])
  | 6 -> One ([R 600])
  | 5 -> One ([R 601])
  | 4 -> One ([R 602])
  | 3 -> One ([R 603])
  | 1395 -> One ([R 604])
  | 366 -> One ([R 607])
  | 357 -> One ([R 608])
  | 365 -> One ([R 609])
  | 356 -> One ([R 610])
  | 355 -> One ([R 611])
  | 1389 -> One ([R 619])
  | 1408 | 1428 -> One ([R 620])
  | 1409 | 1429 -> One ([R 621])
  | 1404 -> One ([R 622])
  | 1386 -> One ([R 623])
  | 1387 -> One ([R 624])
  | 1392 -> One ([R 625])
  | 1394 -> One ([R 626])
  | 1407 -> One ([R 627])
  | 1396 -> One ([R 628])
  | 1406 -> One ([R 629])
  | 1405 -> One ([R 630])
  | 1414 -> One ([R 631])
  | 1413 -> One ([R 632])
  | 1393 -> One ([R 633])
  | 1412 -> One ([R 635])
  | 1390 -> One (R 636 :: r952)
  | 499 -> One ([R 639])
  | 498 -> One ([R 640])
  | 371 -> One ([R 644])
  | 372 -> One ([R 645])
  | 374 -> One ([R 646])
  | 376 -> One ([R 647])
  | 373 -> One ([R 648])
  | 370 -> One ([R 649])
  | 1221 -> One ([R 654])
  | 1220 -> One ([R 655])
  | 317 -> One ([R 657])
  | 304 -> One ([R 658])
  | 326 -> One ([R 659])
  | 430 -> One (R 671 :: r328)
  | 460 -> One ([R 672])
  | 141 -> One ([R 676])
  | 142 -> One ([R 677])
  | 375 -> One ([R 682])
  | 378 -> One ([R 683])
  | 1126 -> One (R 692 :: r787)
  | 1179 -> One (R 692 :: r822)
  | 1270 -> One (R 692 :: r874)
  | 1102 -> One ([R 693])
  | 448 -> One ([R 701])
  | 850 -> One (S (T T_WITH) :: r623)
  | 346 | 377 -> One (S (T T_UIDENT) :: r41)
  | 195 -> One (S (T T_UIDENT) :: r164)
  | 407 -> One (S (T T_TYPE) :: r289)
  | 965 -> One (S (T T_TYPE) :: r667)
  | 1099 | 1269 -> One (S (T T_TYPE) :: r762)
  | 341 -> One (S (T T_RPAREN) :: r47)
  | 160 | 277 -> One (S (T T_RPAREN) :: r126)
  | 260 -> One (S (T T_RPAREN) :: r192)
  | 263 -> One (S (T T_RPAREN) :: r193)
  | 421 -> One (S (T T_RPAREN) :: r313)
  | 514 -> One (S (T T_RPAREN) :: r383)
  | 516 -> One (S (T T_RPAREN) :: r384)
  | 827 -> One (S (T T_RPAREN) :: r607)
  | 993 -> One (S (T T_RPAREN) :: r685)
  | 1002 -> One (S (T T_RPAREN) :: r686)
  | 1072 -> One (S (T T_RPAREN) :: r741)
  | 1440 -> One (S (T T_RPAREN) :: r971)
  | 178 -> One (S (T T_RBRACKET) :: r149)
  | 229 -> One (S (T T_RBRACKET) :: r180)
  | 272 | 278 -> One (S (T T_RBRACKET) :: r197)
  | 342 -> One (S (T T_RBRACKET) :: r254)
  | 837 -> One (S (T T_RBRACKET) :: r611)
  | 220 -> One (S (T T_QUOTE) :: r178)
  | 335 -> One (S (T T_PLUSEQ) :: r243)
  | 1458 -> One (S (T T_PLUSEQ) :: r979)
  | 131 -> One (S (T T_MODULE) :: r93)
  | 299 -> One (S (T T_MINUSGREATER) :: r223)
  | 1198 -> One (S (T T_MINUSGREATER) :: r835)
  | 127 -> One (S (T T_LIDENT) :: r84)
  | 1184 -> One (S (T T_LIDENT) :: r827)
  | 1365 -> One (S (T T_LIDENT) :: r942)
  | 726 -> One (S (T T_LESSMINUS) :: r570)
  | 313 -> One (S (T T_LBRACE) :: r226)
  | 394 -> One (S (T T_INT) :: r277)
  | 397 -> One (S (T T_INT) :: r278)
  | 718 -> One (S (T T_IN) :: r566)
  | 722 -> One (S (T T_IN) :: r568)
  | 1348 -> One (S (T T_IN) :: r939)
  | 626 -> One (S (T T_GREATERRBRACE) :: r466)
  | 935 -> One (S (T T_GREATERRBRACE) :: r647)
  | 164 -> One (S (T T_GREATER) :: r131)
  | 168 -> One (S (T T_GREATER) :: r133)
  | 359 -> One (S (T T_EQUAL) :: r258)
  | 465 -> One (S (T T_EQUAL) :: r353)
  | 971 -> One (S (T T_EQUAL) :: r669)
  | 1300 -> One (S (T T_EQUAL) :: r895)
  | 1508 -> One (S (T T_EOF) :: r984)
  | 1512 -> One (S (T T_EOF) :: r985)
  | 1516 -> One (S (T T_EOF) :: r986)
  | 926 -> One (S (T T_END) :: r646)
  | 156 -> One (S (T T_DOTDOT) :: r116)
  | 318 -> One (S (T T_DOTDOT) :: r227)
  | 74 -> One (S (T T_DOT) :: r40)
  | 244 -> One (S (T T_DOT) :: r190)
  | 443 -> One (S (T T_DOT) :: r341)
  | 476 -> One (S (T T_DOT) :: r357)
  | 985 -> One (S (T T_DOT) :: r679)
  | 1145 -> One (S (T T_DOT) :: r797)
  | 1157 -> One (S (T T_DOT) :: r806)
  | 170 -> One (S (T T_COLON) :: r140)
  | 425 -> One (S (T T_COLON) :: r316)
  | 1073 -> One (S (T T_COLON) :: r745)
  | 534 -> One (S (T T_BARRBRACKET) :: r395)
  | 624 -> One (S (T T_BARRBRACKET) :: r460)
  | 829 -> One (S (T T_BARRBRACKET) :: r608)
  | 181 | 1196 -> One (S (T T_BAR) :: r154)
  | 231 -> One (S (T T_BAR) :: r183)
  | 379 -> One (S (N N_structure) :: r260)
  | 1388 -> One (S (N N_structure) :: r951)
  | 390 -> One (S (N N_pattern) :: r273)
  | 400 | 553 | 658 | 893 -> One (S (N N_pattern) :: r280)
  | 550 -> One (S (N N_pattern) :: r412)
  | 568 -> One (S (N N_pattern) :: r425)
  | 570 -> One (S (N N_pattern) :: r426)
  | 573 -> One (S (N N_pattern) :: r427)
  | 576 -> One (S (N N_pattern) :: r428)
  | 581 -> One (S (N N_pattern) :: r429)
  | 586 -> One (S (N N_pattern) :: r432)
  | 1037 -> One (S (N N_pattern) :: r698)
  | 417 -> One (S (N N_module_type) :: r306)
  | 418 -> One (S (N N_module_type) :: r308)
  | 473 -> One (S (N N_module_type) :: r355)
  | 948 -> One (S (N N_module_type) :: r657)
  | 1025 -> One (S (N N_module_type) :: r694)
  | 495 -> One (S (N N_module_expr) :: r362)
  | 649 -> One (S (N N_let_pattern) :: r511)
  | 629 -> One (S (N N_expr) :: r468)
  | 633 -> One (S (N N_expr) :: r480)
  | 700 -> One (S (N N_expr) :: r557)
  | 715 -> One (S (N N_expr) :: r564)
  | 729 -> One (S (N N_expr) :: r571)
  | 731 -> One (S (N N_expr) :: r572)
  | 733 -> One (S (N N_expr) :: r573)
  | 738 -> One (S (N N_expr) :: r574)
  | 740 -> One (S (N N_expr) :: r575)
  | 742 -> One (S (N N_expr) :: r576)
  | 744 -> One (S (N N_expr) :: r577)
  | 746 -> One (S (N N_expr) :: r578)
  | 748 -> One (S (N N_expr) :: r579)
  | 750 -> One (S (N N_expr) :: r580)
  | 752 -> One (S (N N_expr) :: r581)
  | 754 -> One (S (N N_expr) :: r582)
  | 756 -> One (S (N N_expr) :: r583)
  | 758 -> One (S (N N_expr) :: r584)
  | 760 -> One (S (N N_expr) :: r585)
  | 762 -> One (S (N N_expr) :: r586)
  | 764 -> One (S (N N_expr) :: r587)
  | 766 -> One (S (N N_expr) :: r588)
  | 768 -> One (S (N N_expr) :: r589)
  | 770 -> One (S (N N_expr) :: r590)
  | 772 -> One (S (N N_expr) :: r591)
  | 774 -> One (S (N N_expr) :: r592)
  | 777 -> One (S (N N_expr) :: r593)
  | 779 -> One (S (N N_expr) :: r594)
  | 802 -> One (S (N N_expr) :: r604)
  | 807 -> One (S (N N_expr) :: r605)
  | 810 -> One (S (N N_expr) :: r606)
  | 865 -> One (S (N N_expr) :: r628)
  | 923 -> One (S (N N_expr) :: r645)
  | 617 -> One (Sub (r1) :: r448)
  | 644 -> One (Sub (r1) :: r497)
  | 885 -> One (Sub (r1) :: r634)
  | 1039 -> One (Sub (r1) :: r699)
  | 1492 -> One (Sub (r1) :: r982)
  | 1494 -> One (Sub (r1) :: r983)
  | 2 -> One (Sub (r10) :: r12)
  | 55 -> One (Sub (r10) :: r13)
  | 58 -> One (Sub (r10) :: r18)
  | 88 -> One (Sub (r10) :: r51)
  | 329 -> One (Sub (r10) :: r233)
  | 1035 -> One (Sub (r10) :: r697)
  | 1041 -> One (Sub (r10) :: r702)
  | 70 -> One (Sub (r33) :: r34)
  | 632 -> One (Sub (r33) :: r478)
  | 673 -> One (Sub (r33) :: r536)
  | 696 -> One (Sub (r33) :: r549)
  | 707 -> One (Sub (r33) :: r562)
  | 709 -> One (Sub (r33) :: r563)
  | 121 -> One (Sub (r36) :: r75)
  | 188 -> One (Sub (r36) :: r157)
  | 265 -> One (Sub (r36) :: r194)
  | 588 -> One (Sub (r52) :: r433)
  | 214 -> One (Sub (r56) :: r175)
  | 297 -> One (Sub (r56) :: r221)
  | 899 -> One (Sub (r56) :: r641)
  | 1189 -> One (Sub (r58) :: r830)
  | 1193 -> One (Sub (r58) :: r833)
  | 130 -> One (Sub (r60) :: r87)
  | 163 -> One (Sub (r60) :: r130)
  | 218 -> One (Sub (r60) :: r176)
  | 224 -> One (Sub (r62) :: r179)
  | 268 -> One (Sub (r64) :: r195)
  | 547 -> One (Sub (r64) :: r411)
  | 583 -> One (Sub (r64) :: r431)
  | 609 -> One (Sub (r64) :: r443)
  | 651 -> One (Sub (r64) :: r512)
  | 787 -> One (Sub (r64) :: r600)
  | 857 -> One (Sub (r64) :: r624)
  | 861 -> One (Sub (r64) :: r627)
  | 1112 -> One (Sub (r64) :: r772)
  | 1449 -> One (Sub (r64) :: r973)
  | 92 -> One (Sub (r71) :: r73)
  | 146 -> One (Sub (r77) :: r114)
  | 245 -> One (Sub (r77) :: r191)
  | 368 -> One (Sub (r77) :: r259)
  | 406 -> One (Sub (r89) :: r285)
  | 523 -> One (Sub (r89) :: r386)
  | 1014 -> One (Sub (r89) :: r688)
  | 1017 -> One (Sub (r89) :: r690)
  | 1020 -> One (Sub (r89) :: r692)
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
  | 694 -> One (Sub (r204) :: r548)
  | 1292 -> One (Sub (r204) :: r893)
  | 1312 -> One (Sub (r204) :: r910)
  | 281 -> One (Sub (r212) :: r214)
  | 322 -> One (Sub (r212) :: r230)
  | 1082 -> One (Sub (r261) :: r747)
  | 392 -> One (Sub (r275) :: r276)
  | 955 -> One (Sub (r295) :: r658)
  | 469 -> One (Sub (r321) :: r354)
  | 429 -> One (Sub (r323) :: r324)
  | 438 -> One (Sub (r334) :: r339)
  | 431 -> One (Sub (r336) :: r338)
  | 1104 -> One (Sub (r336) :: r764)
  | 446 -> One (Sub (r343) :: r346)
  | 452 -> One (Sub (r350) :: r351)
  | 535 -> One (Sub (r397) :: r400)
  | 536 -> One (Sub (r407) :: r409)
  | 897 -> One (Sub (r415) :: r638)
  | 564 -> One (Sub (r423) :: r424)
  | 605 -> One (Sub (r437) :: r438)
  | 614 -> One (Sub (r437) :: r444)
  | 606 -> One (Sub (r440) :: r442)
  | 615 -> One (Sub (r440) :: r447)
  | 630 -> One (Sub (r475) :: r477)
  | 849 -> One (Sub (r475) :: r621)
  | 904 -> One (Sub (r502) :: r642)
  | 647 -> One (Sub (r507) :: r508)
  | 659 -> One (Sub (r514) :: r517)
  | 894 -> One (Sub (r514) :: r637)
  | 979 -> One (Sub (r514) :: r675)
  | 1319 -> One (Sub (r514) :: r918)
  | 702 -> One (Sub (r559) :: r560)
  | 705 -> One (Sub (r559) :: r561)
  | 842 -> One (Sub (r617) :: r619)
  | 917 -> One (Sub (r643) :: r644)
  | 975 -> One (Sub (r663) :: r670)
  | 1071 -> One (Sub (r737) :: r739)
  | 1318 -> One (Sub (r789) :: r913)
  | 1154 -> One (Sub (r801) :: r804)
  | 1340 -> One (Sub (r801) :: r931)
  | 1361 -> One (Sub (r816) :: r941)
  | 1378 -> One (Sub (r816) :: r949)
  | 1338 -> One (Sub (r866) :: r928)
  | 1382 -> One (Sub (r869) :: r950)
  | 1281 -> One (Sub (r884) :: r886)
  | 1303 -> One (Sub (r901) :: r903)
  | 781 -> One (r0)
  | 1507 -> One (r2)
  | 1506 -> One (r3)
  | 1505 -> One (r4)
  | 1504 -> One (r5)
  | 1503 -> One (r6)
  | 53 -> One (r7)
  | 54 -> One (r9)
  | 1502 -> One (r11)
  | 57 -> One (r12)
  | 56 -> One (r13)
  | 1430 -> One (r14)
  | 1501 -> One (r16)
  | 1500 -> One (r17)
  | 59 -> One (r18)
  | 1499 -> One (r19)
  | 1498 -> One (r20)
  | 1497 -> One (r21)
  | 1496 -> One (r22)
  | 61 -> One (r23)
  | 62 -> One (r24)
  | 1488 -> One (r25)
  | 64 -> One (r26)
  | 1487 -> One (r27)
  | 1486 -> One (r28)
  | 1485 -> One (r29)
  | 1484 -> One (r30)
  | 66 -> One (r31)
  | 69 -> One (r32)
  | 1483 -> One (r34)
  | 73 -> One (r35)
  | 78 -> One (r37)
  | 72 -> One (r38)
  | 77 -> One (r39)
  | 75 -> One (r40)
  | 76 -> One (r41)
  | 80 -> One (r42)
  | 82 -> One (r43)
  | 1001 -> One (r44)
  | 1000 -> One (r45)
  | 83 -> One (r46)
  | 85 -> One (r47)
  | 87 | 529 | 631 | 871 -> One (r48)
  | 1482 -> One (r49)
  | 1481 -> One (r50)
  | 89 -> One (r51)
  | 119 -> One (r53)
  | 187 -> One (r55)
  | 209 -> One (r57)
  | 208 -> One (r59)
  | 217 -> One (r61)
  | 262 -> One (r63)
  | 1480 -> One (r65)
  | 1479 -> One (r66)
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
  | 1478 -> One (r80)
  | 1477 -> One (r81)
  | 1476 -> One (r82)
  | 129 -> One (r83)
  | 128 -> One (r84)
  | 1475 -> One (r85)
  | 1474 -> One (r86)
  | 1473 -> One (r87)
  | 526 -> One (r88)
  | 1472 -> One (r90)
  | 1471 -> One (r91)
  | 133 -> One (r92)
  | 132 -> One (r93)
  | 1470 -> One (r94)
  | 1469 -> One (r95)
  | 136 -> One (r96)
  | 1457 -> One (r97)
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
  | 162 | 177 | 1192 -> One (r127)
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
  | 199 | 1197 -> One (r142)
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
  | 198 | 1202 -> One (r158)
  | 197 | 1201 -> One (r159)
  | 191 | 1200 -> One (r160)
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
  | 1456 -> One (r231)
  | 1455 -> One (r232)
  | 330 -> One (r233)
  | 364 -> One (r234)
  | 363 -> One (r235)
  | 1467 -> One (r236)
  | 358 -> One (r238)
  | 352 -> One (r240)
  | 351 -> One (r241)
  | 337 -> One (r242)
  | 336 -> One (r243)
  | 350 -> One (r244)
  | 349 -> One (r245)
  | 1462 -> One (r246)
  | 1461 -> One (r247)
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
  | 1454 -> One (r260)
  | 387 -> One (r262)
  | 386 -> One (r263)
  | 385 -> One (r264)
  | 384 -> One (r265)
  | 1280 -> One (r266)
  | 1279 -> One (r267)
  | 1453 -> One (r269)
  | 1452 -> One (r270)
  | 389 -> One (r271)
  | 1448 -> One (r272)
  | 1447 -> One (r273)
  | 391 -> One (r274)
  | 393 -> One (r276)
  | 395 -> One (r277)
  | 398 -> One (r278)
  | 580 -> One (r279)
  | 579 -> One (r280)
  | 405 -> One (r281)
  | 404 -> One (r282)
  | 403 -> One (r283)
  | 1439 -> One (r284)
  | 1438 -> One (r285)
  | 1437 -> One (r286)
  | 410 -> One (r287)
  | 409 -> One (r288)
  | 408 -> One (r289)
  | 1436 -> One (r290)
  | 1435 -> One (r291)
  | 412 -> One (r292)
  | 1028 -> One (r293)
  | 494 -> One (r294)
  | 1034 -> One (r296)
  | 1033 -> One (r297)
  | 1032 -> One (r298)
  | 1031 -> One (r299)
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
  | 1030 -> One (r358)
  | 1029 -> One (r359)
  | 493 -> One (r360)
  | 1024 -> One (r361)
  | 1023 -> One (r362)
  | 1013 -> One (r363)
  | 1012 -> One (r364)
  | 497 -> One (r365)
  | 1011 -> One (r366)
  | 1010 -> One (r367)
  | 1009 -> One (r368)
  | 1008 -> One (r369)
  | 501 -> One (r370)
  | 1007 -> One (r371)
  | 1006 -> One (r372)
  | 1005 -> One (r373)
  | 1004 -> One (r374)
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
  | 793 -> One (r387)
  | 792 -> One (r388)
  | 791 -> One (r389)
  | 992 -> One (r391)
  | 991 -> One (r392)
  | 533 -> One (r393)
  | 531 -> One (r394)
  | 600 -> One (r395)
  | 599 -> One (r396)
  | 598 -> One (r398)
  | 597 -> One (r399)
  | 594 -> One (r400)
  | 537 -> One (r401)
  | 549 -> One (r403)
  | 546 -> One (r404)
  | 540 -> One (r406)
  | 539 -> One (r408)
  | 538 -> One (r409)
  | 544 -> One (r410)
  | 548 -> One (r411)
  | 593 -> One (r412)
  | 561 | 786 -> One (r414)
  | 590 -> One (r416)
  | 552 -> One (r417)
  | 555 -> One (r418)
  | 578 -> One (r419)
  | 558 -> One (r420)
  | 566 -> One (r422)
  | 565 -> One (r424)
  | 569 -> One (r425)
  | 571 -> One (r426)
  | 574 -> One (r427)
  | 577 -> One (r428)
  | 582 -> One (r429)
  | 585 -> One (r430)
  | 584 -> One (r431)
  | 587 -> One (r432)
  | 589 -> One (r433)
  | 603 -> One (r434)
  | 602 -> One (r435)
  | 607 -> One (r436)
  | 613 -> One (r438)
  | 608 -> One (r439)
  | 612 -> One (r441)
  | 611 -> One (r442)
  | 610 -> One (r443)
  | 964 -> One (r444)
  | 963 -> One (r445)
  | 962 -> One (r446)
  | 616 -> One (r447)
  | 961 -> One (r448)
  | 960 -> One (r449)
  | 959 -> One (r450)
  | 958 -> One (r451)
  | 957 -> One (r452)
  | 1339 -> One (r453)
  | 944 -> One (r454)
  | 623 -> One (r455)
  | 622 -> One (r456)
  | 621 -> One (r457)
  | 620 -> One (r458)
  | 619 -> One (r459)
  | 940 -> One (r460)
  | 836 -> One (r461)
  | 939 -> One (r463)
  | 938 -> One (r464)
  | 937 -> One (r465)
  | 627 -> One (r466)
  | 628 -> One (r467)
  | 934 -> One (r468)
  | 864 -> One (r469)
  | 856 -> One (r470)
  | 853 -> One (r472)
  | 872 -> One (r474)
  | 933 -> One (r476)
  | 932 -> One (r477)
  | 931 -> One (r478)
  | 930 -> One (r479)
  | 929 -> One (r480)
  | 928 -> One (r481)
  | 635 -> One (r482)
  | 925 -> One (r483)
  | 637 -> One (r484)
  | 922 -> One (r485)
  | 921 -> One (r486)
  | 920 -> One (r487)
  | 639 -> One (r488)
  | 916 -> One (r489)
  | 642 -> One (r490)
  | 641 -> One (r491)
  | 915 -> One (r492)
  | 913 -> One (r493)
  | 643 -> One (r494)
  | 912 -> One (r495)
  | 911 -> One (r496)
  | 910 -> One (r497)
  | 903 -> One (r498)
  | 892 -> One (r500)
  | 664 -> One (r501)
  | 909 -> One (r503)
  | 908 -> One (r504)
  | 646 -> One (r505)
  | 648 -> One (r506)
  | 657 -> One (r508)
  | 655 -> One (r509)
  | 654 -> One (r510)
  | 653 -> One (r511)
  | 652 -> One (r512)
  | 660 -> One (r513)
  | 907 -> One (r515)
  | 663 -> One (r516)
  | 662 -> One (r517)
  | 884 -> One (r518)
  | 883 -> One (r519)
  | 882 -> One (r520)
  | 881 -> One (r521)
  | 668 -> One (r522)
  | 667 -> One (r523)
  | 666 -> One (r524)
  | 875 -> One (r525)
  | 880 -> One (r527)
  | 879 -> One (r528)
  | 878 -> One (r529)
  | 877 -> One (r530)
  | 876 -> One (r531)
  | 873 -> One (r532)
  | 672 -> One (r533)
  | 671 -> One (r534)
  | 670 -> One (r535)
  | 675 -> One (r536)
  | 680 -> One (r537)
  | 679 -> One (r538)
  | 678 | 870 -> One (r539)
  | 869 -> One (r540)
  | 686 -> One (r541)
  | 685 -> One (r542)
  | 684 -> One (r543)
  | 683 -> One (r544)
  | 682 -> One (r545)
  | 693 -> One (r546)
  | 692 -> One (r547)
  | 695 -> One (r548)
  | 697 -> One (r549)
  | 806 | 825 -> One (r550)
  | 805 | 824 -> One (r551)
  | 804 | 823 -> One (r552)
  | 698 | 711 -> One (r553)
  | 714 | 819 -> One (r554)
  | 713 | 818 -> One (r555)
  | 699 | 712 -> One (r556)
  | 817 -> One (r557)
  | 703 -> One (r558)
  | 704 -> One (r560)
  | 706 -> One (r561)
  | 708 -> One (r562)
  | 710 -> One (r563)
  | 798 -> One (r564)
  | 720 -> One (r565)
  | 719 -> One (r566)
  | 724 -> One (r567)
  | 723 -> One (r568)
  | 776 -> One (r569)
  | 727 -> One (r570)
  | 730 -> One (r571)
  | 732 -> One (r572)
  | 734 -> One (r573)
  | 739 -> One (r574)
  | 741 -> One (r575)
  | 743 -> One (r576)
  | 745 -> One (r577)
  | 747 -> One (r578)
  | 749 -> One (r579)
  | 751 -> One (r580)
  | 753 -> One (r581)
  | 755 -> One (r582)
  | 757 -> One (r583)
  | 759 -> One (r584)
  | 761 -> One (r585)
  | 763 -> One (r586)
  | 765 -> One (r587)
  | 767 -> One (r588)
  | 769 -> One (r589)
  | 771 -> One (r590)
  | 773 -> One (r591)
  | 775 -> One (r592)
  | 778 -> One (r593)
  | 780 -> One (r594)
  | 795 -> One (r595)
  | 794 -> One (r596)
  | 785 -> One (r597)
  | 790 -> One (r598)
  | 789 -> One (r599)
  | 788 -> One (r600)
  | 801 | 822 -> One (r601)
  | 800 | 821 -> One (r602)
  | 799 | 820 -> One (r603)
  | 803 -> One (r604)
  | 808 -> One (r605)
  | 811 -> One (r606)
  | 828 -> One (r607)
  | 830 -> One (r608)
  | 835 -> One (r609)
  | 834 -> One (r610)
  | 838 -> One (r611)
  | 841 -> One (r612)
  | 840 -> One (r613)
  | 848 -> One (r615)
  | 845 -> One (r616)
  | 844 -> One (r618)
  | 843 -> One (r619)
  | 868 -> One (r620)
  | 867 -> One (r621)
  | 852 -> One (r622)
  | 851 -> One (r623)
  | 858 -> One (r624)
  | 860 -> One (r625)
  | 859 | 978 -> One (r626)
  | 862 -> One (r627)
  | 866 -> One (r628)
  | 891 -> One (r629)
  | 890 -> One (r630)
  | 889 -> One (r631)
  | 888 -> One (r632)
  | 887 -> One (r633)
  | 886 -> One (r634)
  | 906 -> One (r635)
  | 896 -> One (r636)
  | 895 -> One (r637)
  | 898 -> One (r638)
  | 902 -> One (r639)
  | 901 -> One (r640)
  | 900 -> One (r641)
  | 905 -> One (r642)
  | 918 -> One (r644)
  | 924 -> One (r645)
  | 927 -> One (r646)
  | 936 -> One (r647)
  | 943 -> One (r648)
  | 942 -> One (r649)
  | 954 -> One (r650)
  | 953 -> One (r651)
  | 952 -> One (r652)
  | 947 -> One (r653)
  | 946 -> One (r654)
  | 951 -> One (r655)
  | 950 -> One (r656)
  | 949 -> One (r657)
  | 956 -> One (r658)
  | 970 -> One (r659)
  | 969 -> One (r660)
  | 974 -> One (r662)
  | 977 -> One (r664)
  | 968 -> One (r665)
  | 967 -> One (r666)
  | 966 -> One (r667)
  | 973 -> One (r668)
  | 972 -> One (r669)
  | 976 -> One (r670)
  | 984 -> One (r671)
  | 983 -> One (r672)
  | 982 -> One (r673)
  | 981 -> One (r674)
  | 980 -> One (r675)
  | 989 -> One (r676)
  | 988 -> One (r677)
  | 987 -> One (r678)
  | 986 -> One (r679)
  | 999 -> One (r680)
  | 998 -> One (r681)
  | 997 -> One (r682)
  | 996 -> One (r683)
  | 995 -> One (r684)
  | 994 -> One (r685)
  | 1003 -> One (r686)
  | 1016 -> One (r687)
  | 1015 -> One (r688)
  | 1019 -> One (r689)
  | 1018 -> One (r690)
  | 1022 -> One (r691)
  | 1021 -> One (r692)
  | 1027 -> One (r693)
  | 1026 -> One (r694)
  | 1434 -> One (r695)
  | 1433 -> One (r696)
  | 1036 -> One (r697)
  | 1038 -> One (r698)
  | 1040 -> One (r699)
  | 1432 -> One (r700)
  | 1431 -> One (r701)
  | 1042 -> One (r702)
  | 1046 -> One (r703)
  | 1045 -> One (r704)
  | 1044 -> One (r705)
  | 1053 -> One (r706)
  | 1056 -> One (r708)
  | 1055 -> One (r709)
  | 1052 -> One (r710)
  | 1051 -> One (r711)
  | 1050 -> One (r712)
  | 1049 -> One (r713)
  | 1048 -> One (r714)
  | 1063 -> One (r715)
  | 1062 -> One (r716)
  | 1061 -> One (r717)
  | 1060 -> One (r718)
  | 1066 -> One (r721)
  | 1065 -> One (r722)
  | 1064 -> One (r723)
  | 1098 -> One (r724)
  | 1097 -> One (r725)
  | 1096 -> One (r726)
  | 1267 -> One (r727)
  | 1266 -> One (r728)
  | 1091 -> One (r729)
  | 1090 -> One (r730)
  | 1089 -> One (r731)
  | 1088 -> One (r732)
  | 1087 -> One (r733)
  | 1070 -> One (r734)
  | 1078 -> One (r735)
  | 1077 -> One (r736)
  | 1086 -> One (r738)
  | 1085 -> One (r739)
  | 1081 -> One (r740)
  | 1080 -> One (r741)
  | 1079 -> One (r742)
  | 1076 -> One (r743)
  | 1075 -> One (r744)
  | 1074 -> One (r745)
  | 1084 -> One (r746)
  | 1083 -> One (r747)
  | 1095 -> One (r748)
  | 1094 -> One (r749)
  | 1093 -> One (r750)
  | 1153 -> One (r751)
  | 1162 -> One (r753)
  | 1178 -> One (r755)
  | 1177 -> One (r756)
  | 1109 -> One (r757)
  | 1108 -> One (r758)
  | 1107 -> One (r759)
  | 1103 -> One (r760)
  | 1101 -> One (r761)
  | 1100 -> One (r762)
  | 1106 -> One (r763)
  | 1105 -> One (r764)
  | 1118 -> One (r765)
  | 1117 -> One (r766)
  | 1116 -> One (r768)
  | 1115 -> One (r769)
  | 1111 -> One (r770)
  | 1114 -> One (r771)
  | 1113 -> One (r772)
  | 1135 -> One (r773)
  | 1134 -> One (r774)
  | 1133 -> One (r775)
  | 1132 -> One (r777)
  | 1131 -> One (r778)
  | 1120 -> One (r779)
  | 1125 -> One (r780)
  | 1124 -> One (r781)
  | 1123 -> One (r782)
  | 1122 -> One (r783)
  | 1130 -> One (r784)
  | 1129 -> One (r785)
  | 1128 -> One (r786)
  | 1127 -> One (r787)
  | 1150 -> One (r788)
  | 1149 -> One (r790)
  | 1148 -> One (r791)
  | 1144 -> One (r792)
  | 1143 -> One (r793)
  | 1142 -> One (r794)
  | 1137 -> One (r795)
  | 1147 -> One (r796)
  | 1146 -> One (r797)
  | 1164 -> One (r798)
  | 1163 -> One (r799)
  | 1152 -> One (r800)
  | 1160 -> One (r802)
  | 1156 -> One (r803)
  | 1155 -> One (r804)
  | 1159 -> One (r805)
  | 1158 -> One (r806)
  | 1170 -> One (r807)
  | 1169 -> One (r808)
  | 1168 -> One (r809)
  | 1172 -> One (r811)
  | 1171 -> One (r812)
  | 1167 -> One (r813)
  | 1174 -> One (r814)
  | 1205 -> One (r815)
  | 1210 -> One (r817)
  | 1209 -> One (r818)
  | 1183 -> One (r819)
  | 1182 -> One (r820)
  | 1181 -> One (r821)
  | 1180 -> One (r822)
  | 1208 -> One (r823)
  | 1188 -> One (r824)
  | 1187 -> One (r825)
  | 1186 -> One (r826)
  | 1185 -> One (r827)
  | 1207 -> One (r828)
  | 1191 -> One (r829)
  | 1190 -> One (r830)
  | 1206 -> One (r831)
  | 1195 -> One (r832)
  | 1194 -> One (r833)
  | 1204 -> One (r834)
  | 1199 -> One (r835)
  | 1219 -> One (r836)
  | 1218 -> One (r837)
  | 1217 -> One (r838)
  | 1216 -> One (r839)
  | 1215 -> One (r840)
  | 1214 -> One (r841)
  | 1223 -> One (r842)
  | 1233 -> One (r843)
  | 1232 -> One (r844)
  | 1231 -> One (r845)
  | 1230 -> One (r846)
  | 1229 -> One (r847)
  | 1242 -> One (r848)
  | 1252 -> One (r849)
  | 1251 -> One (r850)
  | 1250 -> One (r851)
  | 1249 -> One (r852)
  | 1248 -> One (r853)
  | 1247 -> One (r854)
  | 1246 -> One (r855)
  | 1263 -> One (r856)
  | 1262 -> One (r857)
  | 1261 -> One (r858)
  | 1260 -> One (r859)
  | 1259 -> One (r860)
  | 1258 -> One (r861)
  | 1257 -> One (r862)
  | 1353 -> One (r863)
  | 1351 -> One (r865)
  | 1377 -> One (r867)
  | 1274 -> One (r868)
  | 1385 -> One (r870)
  | 1384 -> One (r871)
  | 1273 -> One (r872)
  | 1272 -> One (r873)
  | 1271 -> One (r874)
  | 1278 -> One (r875)
  | 1277 -> One (r876)
  | 1276 -> One (r877)
  | 1299 -> One (r878)
  | 1298 -> One (r879)
  | 1297 -> One (r880)
  | 1296 -> One (r881)
  | 1285 -> One (r882)
  | 1284 -> One (r883)
  | 1283 -> One (r885)
  | 1282 -> One (r886)
  | 1290 -> One (r887)
  | 1289 -> One (r888)
  | 1288 -> One (r889)
  | 1287 -> One (r890)
  | 1295 -> One (r891)
  | 1294 -> One (r892)
  | 1293 -> One (r893)
  | 1302 -> One (r894)
  | 1301 -> One (r895)
  | 1328 -> One (r896)
  | 1317 -> One (r897)
  | 1316 -> One (r898)
  | 1305 -> One (r899)
  | 1304 -> One (r900)
  | 1330 -> One (r902)
  | 1329 -> One (r903)
  | 1310 -> One (r904)
  | 1309 -> One (r905)
  | 1308 -> One (r906)
  | 1307 -> One (r907)
  | 1315 -> One (r908)
  | 1314 -> One (r909)
  | 1313 -> One (r910)
  | 1327 -> One (r911)
  | 1326 -> One (r912)
  | 1325 -> One (r913)
  | 1324 -> One (r914)
  | 1323 -> One (r915)
  | 1322 -> One (r916)
  | 1321 -> One (r917)
  | 1320 -> One (r918)
  | 1334 -> One (r919)
  | 1333 -> One (r920)
  | 1332 -> One (r921)
  | 1368 -> One (r922)
  | 1367 -> One (r923)
  | 1364 -> One (r924)
  | 1337 -> One (r925)
  | 1336 -> One (r926)
  | 1360 -> One (r927)
  | 1359 -> One (r928)
  | 1343 -> One (r929)
  | 1342 -> One (r930)
  | 1341 -> One (r931)
  | 1356 -> One (r932)
  | 1347 -> One (r933)
  | 1346 -> One (r934)
  | 1358 -> One (r936)
  | 1345 -> One (r937)
  | 1354 -> One (r938)
  | 1349 -> One (r939)
  | 1363 -> One (r940)
  | 1362 -> One (r941)
  | 1366 -> One (r942)
  | 1372 -> One (r943)
  | 1371 -> One (r944)
  | 1370 -> One (r945)
  | 1374 -> One (r946)
  | 1381 -> One (r947)
  | 1380 -> One (r948)
  | 1379 -> One (r949)
  | 1383 -> One (r950)
  | 1424 -> One (r951)
  | 1391 -> One (r952)
  | 1401 -> One (r953)
  | 1400 -> One (r954)
  | 1399 -> One (r955)
  | 1398 -> One (r956)
  | 1411 -> One (r957)
  | 1421 -> One (r958)
  | 1420 -> One (r959)
  | 1419 -> One (r960)
  | 1418 -> One (r961)
  | 1417 -> One (r962)
  | 1416 -> One (r963)
  | 1427 -> One (r964)
  | 1426 -> One (r965)
  | 1446 -> One (r966)
  | 1445 -> One (r967)
  | 1444 -> One (r968)
  | 1443 -> One (r969)
  | 1442 -> One (r970)
  | 1441 -> One (r971)
  | 1451 -> One (r972)
  | 1450 -> One (r973)
  | 1466 -> One (r974)
  | 1464 -> One (r976)
  | 1463 -> One (r977)
  | 1460 -> One (r978)
  | 1459 -> One (r979)
  | 1491 -> One (r980)
  | 1490 -> One (r981)
  | 1493 -> One (r982)
  | 1495 -> One (r983)
  | 1509 -> One (r984)
  | 1513 -> One (r985)
  | 1517 -> One (r986)
  | 677 -> Select (function
    | -1 -> [R 97]
    | _ -> r540)
  | 413 -> Select (function
    | -1 -> S (T T_TYPE) :: r305
    | _ -> R 190 :: r300)
  | 1057 -> Select (function
    | -1 -> r726
    | _ -> R 190 :: r720)
  | 492 -> Select (function
    | -1 -> S (T T_UIDENT) :: r360
    | _ -> r300)
  | 504 -> Select (function
    | -1 -> S (T T_RPAREN) :: r47
    | _ -> r46)
  | 625 -> Select (function
    | -1 -> S (T T_RBRACKET) :: r254
    | _ -> Sub (r462) :: r465)
  | 618 -> Select (function
    | -1 | 59 | 89 | 330 | 379 | 412 | 1036 | 1042 | 1388 -> r453
    | _ -> S (T T_OPEN) :: r459)
  | 172 -> Select (function
    | 978 -> r79
    | _ -> Sub (r77) :: r141)
  | 338 -> Select (function
    | 351 -> r247
    | _ -> Sub (r118) :: r253)
  | 173 -> Select (function
    | 978 -> r78
    | _ -> r141)
  | 339 -> Select (function
    | 337 -> r253
    | _ -> r246)
  | 1059 -> Select (function
    | -1 -> r724
    | _ -> r719)
  | 1058 -> Select (function
    | -1 -> r725
    | _ -> r720)
  | _ -> raise Not_found
