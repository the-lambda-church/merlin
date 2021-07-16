open Types

let rec find_in_type_sig uid = function
  | [] -> None
  | signature_item :: tl -> 
    match signature_item with
    | Sig_value (_, { val_loc = loc; val_uid = uid2; _ }, _) 
    | Sig_type (_, { type_loc = loc; type_uid = uid2; _ } , _, _)
    | Sig_typext (_, { ext_loc = loc; ext_uid = uid2; _ } , _, _)
    | Sig_modtype 
        (_, { mtd_type = None; mtd_loc = loc; mtd_uid = uid2; _ }, _) ->
      if Uid.equal uid uid2 then Some loc else find_in_type_sig uid tl 
    | Sig_modtype (_, 
        { mtd_type = Some module_type; mtd_loc = loc; mtd_uid = uid2; _ }, _)
    | Sig_module (_, _, 
        { md_type = module_type; md_loc = loc; md_uid = uid2; _ }, _, _) -> 
        if Uid.equal uid uid2 then Some loc else 
        begin match find_in_module_type uid module_type with
        | Some loc -> Some loc
        | None -> find_in_type_sig uid tl end
    | Sig_class_type  (_, 
        { clty_type = class_type; clty_loc = loc; clty_uid = uid2; _ }, _, _)
    | Sig_class (_, 
        { cty_type = class_type; cty_loc = loc; cty_uid = uid2; _ }, _, _) ->
      if Uid.equal uid uid2 then Some loc else begin 
        match find_in_class_type uid class_type with
        | Some loc -> Some loc
        | None -> find_in_type_sig uid tl
        end
and find_in_module_type uid = function
  | Mty_signature signature -> find_in_type_sig uid signature
  | Mty_functor (param, mod_type) -> find_in_module_type uid mod_type
  | Mty_alias _
  | Mty_ident _ 
  | Mty_for_hole -> None
and find_in_class_type uid = function
  | Cty_constr (_, _, class_type)
  | Cty_arrow (_, _, class_type) -> find_in_class_type uid class_type
  | Cty_signature _class_signature -> None
  
let in_annots uid (binary_annots : Cmt_format.binary_annots) =
  match binary_annots with
  | Interface signature -> find_in_type_sig uid signature.sig_type
  | Implementation structure -> find_in_type_sig uid structure.str_type
  | _ -> None (* TODO What are "Partials" ? *)

let rec find_in_summary uid = function
  | Env.Env_empty -> None
  | Env_value (summary, _, { val_loc = loc; val_uid = uid2; _ }) 
  | Env_type (summary, _, { type_loc = loc; type_uid = uid2;  }) 
  | Env_extension (summary, _, { ext_loc = loc; ext_uid = uid2; _ })
  | Env_modtype (summary, _, 
    { mtd_loc = loc; mtd_uid = uid2; mtd_type = None; _ }) -> 
    if Uid.equal uid uid2 then Some loc
    else find_in_summary uid summary
  | Env_module (summary, _, _, { md_loc = loc; md_uid = uid2; md_type; _ }) 
  | Env_modtype (summary, _, 
    { mtd_loc = loc; mtd_uid = uid2; mtd_type = Some md_type; _ }) -> 
    if Uid.equal uid uid2 then Some loc
    else 
      begin match find_in_module_type uid md_type with
      | Some loc -> Some loc
      | None -> find_in_summary uid summary 
      end
  | Env_class (summary, _, { cty_loc = loc; cty_uid = uid2; cty_type; _ }) 
  | Env_cltype (summary, _, 
    { clty_loc = loc; clty_uid = uid2; clty_type = cty_type; _ }) -> 
    if Uid.equal uid uid2 then Some loc
    else 
      begin match find_in_class_type uid cty_type with
      | Some loc -> Some loc
      | None -> find_in_summary uid summary 
      end
  | Env_open (summary, _) 
  | Env_functor_arg (summary, _)
  | Env_constraints (summary, _)
  | Env_copy_types summary
  | Env_persistent (summary, _) 
  | Env_value_unbound (summary, _, _) 
  | Env_module_unbound (summary, _, _) -> 
    find_in_summary uid summary

let in_env uid env =
  find_in_summary uid (Env.summary env)