open Parser_raw
let named_item_at = function
  | 2 -> "while_lwt"
  | 58 -> "attribute"
  | 152 -> "extension"
  | 360 -> "while"
  | 362 -> "try_lwt"
  | 364 -> "try"
  | 378 -> "object"
  | 399 -> "attribute"
  | 417 -> "match_lwt"
  | 419 -> "match"
  | 431 -> "lwt"
  | 450 -> "let"
  | 451 -> "let open"
  | 470 -> "if"
  | 472 -> "function"
  | 475 -> "pattern"
  | 476 -> "when guard"
  | 477 -> "fun"
  | 499 -> "for_lwt"
  | 503 -> "for"
  | 524 -> "val"
  | 529 -> "type"
  | 531 -> "type nonrec"
  | 641 -> "open"
  | 645 -> "module"
  | 720 -> "module type"
  | 726 -> "recursive module"
  | 735 -> "extension"
  | 739 -> "attribute"
  | 743 -> "include"
  | 746 -> "external"
  | 755 -> "exception"
  | 759 -> "class"
  | 761 -> "class type"
  | 769 -> "object"
  | 931 -> "meta-let"
  | 1078 -> "for body"
  | 1082 -> "for body"
  | 1089 -> "for body"
  | 1104 -> "match action"
  | 1106 -> "match action"
  | 1113 -> "then clause"
  | 1115 -> "else clause"
  | 1134 -> "let module"
  | 1164 -> "type constraint"
  | 1166 -> "type constraint"
  | 1177 -> "type constraint"
  | 1271 -> "object"
  | 1330 -> "while body"
  | 1339 -> "while_lwt body"
  | 1342 -> "type"
  | 1343 -> "type nonrec"
  | 1376 -> "module"
  | 1380 -> "module type"
  | 1386 -> "recursive module"
  | 1392 -> "lwt"
  | 1396 -> "let"
  | 1400 -> "include"
  | 1403 -> "external"
  | 1410 -> "exception"
  | 1416 -> "class"
  | 1417 -> "class type"
  | 1438 -> "lwt"
  | 1442 -> "let"
  | _ -> raise Not_found

let nullable (type a) : a MenhirInterpreter.nonterminal -> bool =
  let open MenhirInterpreter in function
  | N_virtual_flag -> true
  | N_type_variance -> true
  | N_type_parameters -> true
  | N_type_kind -> true
  | N_toplevel_directives -> true
  | N_structure_tail -> true
  | N_structure_head -> true
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
  | N_opt_semi -> true
  | N_opt_default -> true
  | N_opt_bar -> true
  | N_opt_ampersand -> true
  | N_mutable_flag -> true
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
