open Parser_raw
let named_item_at = function
  | _ -> raise Not_found

let nullable (type a) : a MenhirInterpreter.nonterminal -> bool =
  let open MenhirInterpreter in function
  | N_virtual_flag -> true
  | N_type_variance -> true
  | N_type_kind -> true
  | N_structure_tail -> true
  | N_structure -> true
  | N_signature -> true
  | N_rec_flag -> true
  | N_private_virtual_flags -> true
  | N_private_flag -> true
  | N_post_item_attributes -> true
  | N_payload -> true
  | N_parent_binder -> true
  | N_override_flag -> true
  | N_optional_type_parameters -> true
  | N_opt_type_constraint -> true
  | N_opt_semi -> true
  | N_opt_pattern_type_constraint -> true
  | N_opt_default -> true
  | N_opt_bar -> true
  | N_opt_ampersand -> true
  | N_nonrec_flag -> true
  | N_mutable_flag -> true
  | N_module_type_declaration_body -> true
  | N_generalized_constructor_arguments -> true
  | N_ext_attributes -> true
  | N_constraints -> true
  | N_class_type_parameters -> true
  | N_class_structure -> true
  | N_class_sig_fields -> true
  | N_class_sig_body -> true
  | N_class_self_type -> true
  | N_class_self_pattern -> true
  | N_class_fields -> true
  | N_attributes -> true
  | _ -> false
