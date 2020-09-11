open Parser_raw
let named_item_at = function
  | _ -> raise Not_found

let nullable (type a) : a MenhirInterpreter.nonterminal -> bool =
  let open MenhirInterpreter in function
  | N_virtual_flag -> true
  | N_type_variance -> true
  | N_type_parameters -> true
  | N_type_kind -> true
  | N_structure -> true
  | N_signature -> true
  | N_reversed_llist_preceded_CONSTRAINT_constrain__ -> true
  | N_rec_flag -> true
  | N_private_virtual_flags -> true
  | N_private_flag -> true
  | N_payload -> true
  | N_option_type_constraint_ -> true
  | N_option_preceded_EQUAL_seq_expr__ -> true
  | N_option_preceded_EQUAL_pattern__ -> true
  | N_option_preceded_EQUAL_module_type__ -> true
  | N_option_preceded_EQUAL_expr__ -> true
  | N_option_preceded_COLON_core_type__ -> true
  | N_option_preceded_AS_mkrhs_LIDENT___ -> true
  | N_option_SEMI_ -> true
  | N_option_BAR_ -> true
  | N_opt_ampersand -> true
  | N_mutable_virtual_flags -> true
  | N_mutable_flag -> true
  | N_list_use_file_element_ -> true
  | N_list_text_str_structure_item__ -> true
  | N_list_text_cstr_class_field__ -> true
  | N_list_text_csig_class_sig_field__ -> true
  | N_list_structure_element_ -> true
  | N_list_signature_element_ -> true
  | N_list_post_item_attribute_ -> true
  | N_list_generic_and_type_declaration_type_subst_kind__ -> true
  | N_list_generic_and_type_declaration_type_kind__ -> true
  | N_list_attribute_ -> true
  | N_list_and_module_declaration_ -> true
  | N_list_and_module_binding_ -> true
  | N_list_and_class_type_declaration_ -> true
  | N_list_and_class_description_ -> true
  | N_list_and_class_declaration_ -> true
  | N_index_mod -> true
  | N_generalized_constructor_arguments -> true
  | N_formal_class_parameters -> true
  | N_ext -> true
  | N_class_self_type -> true
  | N_class_self_pattern -> true
  | _ -> false
