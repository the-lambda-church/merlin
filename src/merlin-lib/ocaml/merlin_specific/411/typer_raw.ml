(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2015  Frédéric Bour  <frederic.bour(_)lakaban.net>
                             Thomas Refis  <refis.thomas(_)gmail.com>
                             Simon Castellan  <simon.castellan(_)iuwt.fr>

  Permission is hereby granted, free of charge, to any person obtaining a
  copy of this software and associated documentation files (the "Software"),
  to deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  The Software is provided "as is", without warranty of any kind, express or
  implied, including but not limited to the warranties of merchantability,
  fitness for a particular purpose and noninfringement. In no event shall
  the authors or copyright holders be liable for any claim, damages or other
  liability, whether in an action of contract, tort or otherwise, arising
  from, out of or in connection with the software or the use or other dealings
  in the Software.

)* }}} *)

open Std
open Location
open Parsetree

let fresh_env () =
  (*Ident.reinit();*)
  let initially_opened_module =
    if !Clflags.nopervasives then
      None
    else
      Some "Stdlib"
  in
  Typemod.initial_env
    ~loc:(Location.in_file "command line")
    ~safe_string:(not !Clflags.unsafe_string)
    ~initially_opened_module
    ~open_implicit_modules:(List.rev !Clflags.open_modules)

module Rewrite_loc = struct
  let queue = ref []

  let update l =
    if l <> none then
      match !queue with
      | [] -> assert false
      | l' :: ls -> queue := Location_aux.union l l' :: ls

  let enter () = queue := Location.none :: !queue
  let leave l0 = match !queue with
    | [] -> assert false
    | [l] -> queue := []; Location_aux.extend l0 l
    | l :: l' :: ls ->
      let l = Location_aux.extend l0 l in
      queue := Location_aux.union l l' :: ls;
      l

  let start () = assert (!queue = []); enter ()
  let exit () = match !queue with
    | [_] -> queue := []
    | _ -> assert false

  let u_option f = function
    | None -> None
    | Some x -> Some (f x)

  let u_loc (loc : _ Location.loc) =
    update loc.loc; loc

  let rec u_attribute { attr_name = loc ; attr_payload; attr_loc } =
    let loc = if Location_aux.is_relaxed_location loc then loc else u_loc loc in
    { attr_name = loc
    ; attr_payload = u_payload attr_payload
    ; attr_loc }

  and u_extension (loc, payload) =
    let loc = if Location_aux.is_relaxed_location loc then loc else u_loc loc in
    (loc, u_payload payload)

  and u_attributes l = List.map ~f:u_attribute l

  and u_payload = function
    | PStr str -> PStr (u_structure str)
    | PSig sg -> PSig (u_signature sg)
    | PTyp ct  -> PTyp (u_core_type ct)
    | PPat (p, eo) -> PPat (u_pattern p, u_option u_expression eo)

  and u_core_type {ptyp_desc; ptyp_attributes; ptyp_loc; ptyp_loc_stack} =
    enter ();
    let ptyp_desc = u_core_type_desc ptyp_desc in
    let ptyp_attributes = u_attributes ptyp_attributes in
    let ptyp_loc = leave ptyp_loc in
    {ptyp_desc; ptyp_loc; ptyp_attributes; ptyp_loc_stack}

  and u_core_type_desc = function
    | Ptyp_any | Ptyp_var _ as desc -> desc
    | Ptyp_arrow (l, t1, t2) -> Ptyp_arrow (l, u_core_type t1, u_core_type t2)
    | Ptyp_tuple ts -> Ptyp_tuple (List.map ~f:u_core_type ts)
    | Ptyp_constr (loc, ts) -> Ptyp_constr (u_loc loc, List.map ~f:u_core_type ts)
    | Ptyp_object (fields, flag) ->
      let object_field_desc = function
        | Otag (lbl, ct) -> Otag (lbl, u_core_type ct)
        | Oinherit ct -> Oinherit (u_core_type ct)
      in
      let object_field { pof_desc; pof_loc; pof_attributes } =
        { pof_desc = object_field_desc pof_desc
        ; pof_attributes = u_attributes pof_attributes
        ; pof_loc }
      in
      Ptyp_object (List.map ~f:object_field fields, flag)
    | Ptyp_class (loc, ts) -> Ptyp_class (u_loc loc, List.map ~f:u_core_type ts)
    | Ptyp_alias (ct, name) -> Ptyp_alias (u_core_type ct, name)
    | Ptyp_variant (fields, flag, label) -> Ptyp_variant (List.map ~f:u_row_field fields, flag, label)
    | Ptyp_poly (ss,ct) -> Ptyp_poly (ss, u_core_type ct)
    | Ptyp_package pt -> Ptyp_package (u_package_type pt)
    | Ptyp_extension ext -> Ptyp_extension (u_extension ext)

  and u_package_type (loc, cts) =
    (u_loc loc, List.map ~f:(fun (l,ct) -> u_loc l, u_core_type ct) cts)

  and u_row_field { prf_desc; prf_loc; prf_attributes } =
    let desc = function
      | Rtag (l,has_const,cts) ->
        Rtag (l, has_const, List.map ~f:u_core_type cts)
      | Rinherit ct -> Rinherit (u_core_type ct)
    in
    { prf_desc = desc prf_desc
    ; prf_loc
    ; prf_attributes = u_attributes prf_attributes }

  and u_pattern {ppat_desc; ppat_loc; ppat_attributes; ppat_loc_stack} =
    enter ();
    let ppat_desc = u_pattern_desc ppat_desc in
    let ppat_attributes = u_attributes ppat_attributes in
    let ppat_loc = leave ppat_loc in
    {ppat_desc; ppat_loc; ppat_attributes; ppat_loc_stack}

  and u_pattern_desc = function
    | Ppat_any | Ppat_constant _ | Ppat_interval _ as p -> p
    | Ppat_var l -> Ppat_var (u_loc l)
    | Ppat_alias (p, l) -> Ppat_alias (u_pattern p, u_loc l)
    | Ppat_tuple ps -> Ppat_tuple (List.map ~f:u_pattern ps)
    | Ppat_construct (loc, po) -> Ppat_construct (u_loc loc, u_option u_pattern po)
    | Ppat_variant (lbl, po) -> Ppat_variant (lbl, u_option u_pattern po)
    | Ppat_record (fields, flag) -> Ppat_record (List.map ~f:(fun (l,p) -> (u_loc l, u_pattern p)) fields, flag)
    | Ppat_array ps -> Ppat_array (List.map ~f:u_pattern ps)
    | Ppat_or (p1, p2) -> Ppat_or (u_pattern p1, u_pattern p2)
    | Ppat_constraint (p, ct) -> Ppat_constraint (u_pattern p, u_core_type ct)
    | Ppat_type loc -> Ppat_type (u_loc loc)
    | Ppat_lazy p -> Ppat_lazy (u_pattern p)
    | Ppat_unpack loc -> Ppat_unpack (u_loc loc)
    | Ppat_exception p -> Ppat_exception (u_pattern p)
    | Ppat_extension ext -> Ppat_extension (u_extension ext)
    | Ppat_open (l,p) -> Ppat_open (u_loc l, u_pattern p)

  and u_expression {pexp_desc; pexp_loc; pexp_attributes; pexp_loc_stack} =
    enter ();
    let pexp_desc = u_expression_desc pexp_desc in
    let pexp_attributes = u_attributes pexp_attributes in
    let pexp_loc = leave pexp_loc in
    {pexp_desc; pexp_loc; pexp_attributes; pexp_loc_stack}

  and u_expression_desc = function
    | Pexp_ident loc -> Pexp_ident (u_loc loc)
    | Pexp_constant _ as e -> e
    | Pexp_let (flag, vs, e) ->
      Pexp_let (flag, List.map ~f:u_value_binding vs, u_expression e)
    | Pexp_function cs ->
      Pexp_function (List.map ~f:u_case cs)
    | Pexp_fun (lbl, eo, pattern, expr) ->
      Pexp_fun (lbl, u_option u_expression eo, u_pattern pattern, u_expression expr)
    | Pexp_apply (e, les) ->
      Pexp_apply (u_expression e, List.map ~f:(fun (l,e) -> (l, u_expression e)) les)
    | Pexp_match (e, cs) -> Pexp_match (u_expression e, List.map ~f:u_case cs)
    | Pexp_try (e, cs) -> Pexp_try (u_expression e, List.map ~f:u_case cs)
    | Pexp_tuple es -> Pexp_tuple (List.map ~f:u_expression es)
    | Pexp_construct (loc, eo) ->
      Pexp_construct (u_loc loc, u_option u_expression eo)
    | Pexp_variant (lbl, eo) ->
      Pexp_variant (lbl, u_option u_expression eo)
    | Pexp_record (les, eo) ->
      Pexp_record (List.map ~f:(fun (loc,e) -> (u_loc loc, u_expression e)) les, u_option u_expression eo)
    | Pexp_field (e, loc) -> Pexp_field (u_expression e, u_loc loc)
    | Pexp_setfield (e1, loc, e2) -> Pexp_setfield (u_expression e1, u_loc loc, u_expression e2)
    | Pexp_array es -> Pexp_array (List.map ~f:u_expression es)
    | Pexp_ifthenelse (e1,e2,e3) -> Pexp_ifthenelse (u_expression e1, u_expression e2, u_option u_expression e3)
    | Pexp_sequence (e1, e2) -> Pexp_sequence (u_expression e1, u_expression e2)
    | Pexp_while (e1, e2) -> Pexp_while (u_expression e1, u_expression e2)
    | Pexp_for (p, e1, e2, flag, e3) -> Pexp_for (u_pattern p, u_expression e1, u_expression e2, flag, u_expression e3)
    | Pexp_constraint (e, ct) -> Pexp_constraint (u_expression e, u_core_type ct)
    | Pexp_coerce (e, cto, ct) -> Pexp_coerce (u_expression e, u_option u_core_type cto, u_core_type ct)
    | Pexp_send (e, s) -> Pexp_send (u_expression e, s)
    | Pexp_new loc -> Pexp_new (u_loc loc)
    | Pexp_setinstvar (s, e) -> Pexp_setinstvar (u_loc s, u_expression e)
    | Pexp_override es -> Pexp_override (List.map ~f:(fun (loc,e) -> (u_loc loc, u_expression e)) es)
    | Pexp_letmodule (s, me, e) -> Pexp_letmodule (u_loc s, u_module_expr me, u_expression e)
    | Pexp_letexception (c, e) -> Pexp_letexception (u_extension_constructor c, u_expression e)
    | Pexp_assert e -> Pexp_assert (u_expression e)
    | Pexp_lazy e -> Pexp_lazy (u_expression e)
    | Pexp_poly (e, cto) -> Pexp_poly (u_expression e, u_option u_core_type cto)
    | Pexp_object cs -> Pexp_object (u_class_structure cs)
    | Pexp_newtype (s, e) -> Pexp_newtype (s, u_expression e)
    | Pexp_pack me -> Pexp_pack (u_module_expr me)
    | Pexp_open (od, e) -> Pexp_open (u_open_declaration od, u_expression e)
    | Pexp_extension ext -> Pexp_extension (u_extension ext)
    | Pexp_unreachable -> Pexp_unreachable
    | Pexp_letop { let_; ands; body } ->
      Pexp_letop {
        let_ = u_binding_op let_;
        ands = List.map ~f:u_binding_op ands;
        body = u_expression body;
      }

  and u_binding_op { pbop_op; pbop_pat; pbop_exp; pbop_loc } =
    { pbop_op = u_loc pbop_op
    ; pbop_pat = u_pattern pbop_pat
    ; pbop_exp = u_expression pbop_exp
    ; pbop_loc }

  and u_case {pc_lhs; pc_guard; pc_rhs} = {
    pc_lhs = u_pattern pc_lhs;
    pc_guard = u_option u_expression pc_guard;
    pc_rhs = u_expression pc_rhs;
  }

  and u_value_description {pval_name; pval_type; pval_prim; pval_attributes; pval_loc} =
    enter ();
    let pval_name = u_loc pval_name in
    let pval_type = u_core_type pval_type in
    let pval_attributes = u_attributes pval_attributes in
    let pval_loc = leave pval_loc in
    {pval_name; pval_type; pval_prim; pval_attributes; pval_loc}

  and u_type_declaration {ptype_name; ptype_params; ptype_cstrs; ptype_kind;
                        ptype_private; ptype_manifest; ptype_attributes; ptype_loc} =
    enter ();
    let ptype_name = u_loc ptype_name
    and ptype_params = List.map ~f:(fun (ct,v) -> (u_core_type ct, v)) ptype_params
    and ptype_cstrs = List.map ~f:(fun (ct1,ct2,l) ->
        update l; (u_core_type ct1, u_core_type ct2, l)) ptype_cstrs
    and ptype_kind = u_type_kind ptype_kind
    and ptype_manifest = u_option u_core_type ptype_manifest
    and ptype_attributes = u_attributes ptype_attributes
    in
    let ptype_loc = leave ptype_loc in
    {ptype_name; ptype_params; ptype_cstrs; ptype_kind;
     ptype_private; ptype_manifest; ptype_attributes; ptype_loc}

  and u_type_kind = function
    | Ptype_abstract | Ptype_open as k -> k
    | Ptype_variant cstrs -> Ptype_variant (List.map ~f:u_constructor_declaration cstrs)
    | Ptype_record lbls -> Ptype_record (List.map ~f:u_label_declaration lbls)

  and u_label_declaration {pld_name; pld_mutable; pld_type; pld_loc; pld_attributes} =
    enter ();
    let pld_name = u_loc pld_name in
    let pld_type = u_core_type pld_type in
    let pld_attributes = u_attributes pld_attributes in
    let pld_loc = leave pld_loc in
    {pld_name; pld_mutable; pld_type; pld_loc; pld_attributes}

  and u_constructor_declaration {pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes} =
    enter ();
    let pcd_name = u_loc pcd_name in
    let pcd_args = u_constructor_arguments pcd_args in
    let pcd_res = u_option u_core_type pcd_res in
    let pcd_attributes = u_attributes pcd_attributes in
    let pcd_loc = leave pcd_loc in
    {pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes}

  and u_constructor_arguments = function
    | Pcstr_tuple cts -> Pcstr_tuple (List.map ~f:u_core_type cts)
    | Pcstr_record lbls -> Pcstr_record (List.map ~f:u_label_declaration lbls)

  and u_type_extension
        {ptyext_path; ptyext_params; ptyext_constructors; ptyext_private
        ; ptyext_attributes; ptyext_loc } =
    let ptyext_path = u_loc ptyext_path in
    let ptyext_params = List.map ~f:(fun (ct,v) -> (u_core_type ct, v)) ptyext_params in
    let ptyext_constructors = List.map ~f:u_extension_constructor ptyext_constructors in
    let ptyext_attributes = u_attributes ptyext_attributes in
    {ptyext_path; ptyext_params; ptyext_constructors; ptyext_private
    ; ptyext_attributes; ptyext_loc }

  and u_extension_constructor {pext_name; pext_kind; pext_loc; pext_attributes} =
    enter ();
    let pext_name = u_loc pext_name in
    let pext_kind = u_extension_constructor_kind pext_kind in
    let pext_attributes = u_attributes pext_attributes in
    let pext_loc = leave pext_loc in
    {pext_name; pext_kind; pext_loc; pext_attributes}

  and u_extension_constructor_kind = function
    | Pext_decl (cargs, cto) ->
      Pext_decl (u_constructor_arguments cargs, u_option u_core_type cto)
    | Pext_rebind loc -> Pext_rebind (u_loc loc)

  (** {2 Class language} *)

  (* Type expressions for the class language *)

  and u_class_type {pcty_desc; pcty_loc; pcty_attributes} =
    enter ();
    let pcty_desc = u_class_type_desc pcty_desc in
    let pcty_attributes = u_attributes pcty_attributes in
    let pcty_loc = leave pcty_loc in
    {pcty_desc; pcty_loc; pcty_attributes}

  and u_class_type_desc = function
    | Pcty_constr (loc, cts) ->
      Pcty_constr (u_loc loc, List.map ~f:u_core_type cts)
    | Pcty_signature cs -> Pcty_signature (u_class_signature cs)
    | Pcty_arrow (lbl, ct, clt) ->
      Pcty_arrow (lbl, u_core_type ct, u_class_type clt)
    | Pcty_extension ext ->
      Pcty_extension (u_extension ext)
    | Pcty_open (od, cty) ->
      Pcty_open (u_open_description od, u_class_type cty)

  and u_class_signature {pcsig_self; pcsig_fields} =
    let pcsig_self = u_core_type pcsig_self in
    let pcsig_fields = List.map ~f:u_class_type_field pcsig_fields in
    {pcsig_self; pcsig_fields}

  and u_class_type_field {pctf_desc; pctf_loc; pctf_attributes} =
    enter ();
    let pctf_desc = u_class_type_field_desc pctf_desc in
    let pctf_attributes = u_attributes pctf_attributes in
    let pctf_loc = leave pctf_loc in
    {pctf_desc; pctf_loc; pctf_attributes}

  and u_class_type_field_desc = function
    | Pctf_inherit clt -> Pctf_inherit (u_class_type clt)
    | Pctf_val (s, fl1, fl2, ct) -> Pctf_val (s, fl1, fl2, u_core_type ct)
    | Pctf_method (s, fl1, fl2, ct) -> Pctf_method (s, fl1, fl2, u_core_type ct)
    | Pctf_constraint (ct1, ct2) -> Pctf_constraint (u_core_type ct1, u_core_type ct2)
    | Pctf_attribute attr ->
      Pctf_attribute (u_attribute attr)
    | Pctf_extension ext -> Pctf_extension (u_extension ext)

  and u_class_infos : 'a 'b. ('a -> 'b) -> 'a class_infos -> 'b class_infos =
    fun u_a {pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes} ->
    enter ();
    let pci_params = List.map ~f:(fun (ct,v) -> (u_core_type ct, v)) pci_params in
    let pci_name = u_loc pci_name in
    let pci_expr = u_a pci_expr in
    let pci_attributes = u_attributes pci_attributes in
    let pci_loc = leave pci_loc in
    {pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes}

  and u_class_description clt = u_class_infos u_class_type clt

  and u_class_type_declaration clt = u_class_infos u_class_type clt

  and u_class_expr {pcl_desc; pcl_loc; pcl_attributes} =
    enter ();
    let pcl_desc = u_class_expr_desc pcl_desc in
    let pcl_attributes = u_attributes pcl_attributes in
    let pcl_loc = leave pcl_loc in
    {pcl_desc; pcl_loc; pcl_attributes}

  and u_class_expr_desc = function
    | Pcl_constr (loc, cts) -> Pcl_constr (u_loc loc, List.map ~f:u_core_type cts)
    | Pcl_structure cs -> Pcl_structure (u_class_structure cs)
    | Pcl_fun (lbl, eo, p, ce) ->
      Pcl_fun (lbl, u_option u_expression eo, u_pattern p, u_class_expr ce)
    | Pcl_apply (ce, les) ->
      Pcl_apply (u_class_expr ce, List.map ~f:(fun (l,e) -> (l, u_expression e)) les)
    | Pcl_let (rf, vbs, ce) ->
      Pcl_let (rf, List.map ~f:u_value_binding vbs, u_class_expr ce)
    | Pcl_constraint (ce, ct) -> Pcl_constraint (u_class_expr ce, u_class_type ct)
    | Pcl_extension ext -> Pcl_extension (u_extension ext)
    | Pcl_open (od, ce) ->
      Pcl_open (u_open_description od, u_class_expr ce)

  and u_class_structure {pcstr_self; pcstr_fields} =
    let pcstr_self = u_pattern pcstr_self in
    let pcstr_fields = List.map ~f:u_class_field pcstr_fields in
    {pcstr_self; pcstr_fields}

  and u_class_field {pcf_desc; pcf_loc; pcf_attributes} =
    enter ();
    let pcf_desc = u_class_field_desc pcf_desc in
    let pcf_attributes = u_attributes pcf_attributes in
    let pcf_loc = leave pcf_loc in
    {pcf_desc; pcf_loc; pcf_attributes}

  and u_class_field_desc = function
    | Pcf_inherit (fl, ce, so) -> Pcf_inherit (fl, u_class_expr ce, so)
    | Pcf_val (loc, fl, cfk) -> Pcf_val (u_loc loc, fl, u_class_field_kind cfk)
    | Pcf_method (loc, fl, cfk) -> Pcf_method (u_loc loc, fl, u_class_field_kind cfk)
    | Pcf_constraint (c1, c2) -> Pcf_constraint (u_core_type c1, u_core_type c2)
    | Pcf_initializer e -> Pcf_initializer (u_expression e)
    | Pcf_attribute attr -> Pcf_attribute (u_attribute attr)
    | Pcf_extension ext -> Pcf_extension (u_extension ext)

  and u_class_field_kind = function
    | Cfk_virtual ct -> Cfk_virtual (u_core_type ct)
    | Cfk_concrete (fl,e) -> Cfk_concrete (fl, u_expression e)

  and u_class_declaration cd = u_class_infos u_class_expr cd

  and u_module_type {pmty_desc; pmty_loc; pmty_attributes} =
    enter ();
    let pmty_desc = u_module_type_desc pmty_desc in
    let pmty_attributes = u_attributes pmty_attributes in
    let pmty_loc = leave pmty_loc in
    {pmty_desc; pmty_loc; pmty_attributes}

  and u_module_type_desc = function
    | Pmty_ident loc -> Pmty_ident (u_loc loc)
    | Pmty_signature sg -> Pmty_signature (u_signature sg)
    | Pmty_functor (fp, mt) -> Pmty_functor (u_functor_parameter fp, u_module_type mt)
    | Pmty_with (mt, wts) -> Pmty_with (u_module_type mt, List.map ~f:u_with_constraint wts)
    | Pmty_typeof me -> Pmty_typeof (u_module_expr me)
    | Pmty_extension ext -> Pmty_extension (u_extension ext)
    | Pmty_alias loc -> Pmty_alias (u_loc loc)

  and u_functor_parameter = function
    | Unit -> Unit
    | Named (name, mt) -> Named (u_loc name, u_module_type mt)

  and u_signature l = List.map ~f:u_signature_item l

  and u_signature_item {psig_desc; psig_loc} =
    enter ();
    let psig_desc = u_signature_item_desc psig_desc in
    let psig_loc = leave psig_loc in
    {psig_desc; psig_loc}

  and u_signature_item_desc = function
    | Psig_value vd -> Psig_value (u_value_description vd)
    | Psig_type (fl, tds) -> Psig_type (fl, List.map ~f:u_type_declaration tds)
    | Psig_typext text -> Psig_typext (u_type_extension text)
    | Psig_exception texn -> Psig_exception (u_type_exception texn)
    | Psig_module md -> Psig_module (u_module_declaration md)
    | Psig_recmodule mds -> Psig_recmodule (List.map ~f:u_module_declaration mds)
    | Psig_modtype mtd -> Psig_modtype (u_module_type_declaration mtd)
    | Psig_open od -> Psig_open (u_open_description od)
    | Psig_include id -> Psig_include (u_include_description id)
    | Psig_class cds -> Psig_class (List.map ~f:u_class_description cds)
    | Psig_class_type cts -> Psig_class_type (List.map ~f:u_class_type_declaration cts)
    | Psig_attribute attr -> Psig_attribute (u_attribute attr)
    | Psig_extension (ext, attrs) -> Psig_extension (u_extension ext, u_attributes attrs)
    | Psig_typesubst tds -> Psig_typesubst (List.map ~f:u_type_declaration tds)
    | Psig_modsubst ms -> Psig_modsubst (u_module_substitution ms)

  and u_type_exception {ptyexn_constructor; ptyexn_loc; ptyexn_attributes } =
    { ptyexn_constructor = u_extension_constructor ptyexn_constructor
    ; ptyexn_loc
    ; ptyexn_attributes = u_attributes ptyexn_attributes }

  and u_module_declaration {pmd_name; pmd_type; pmd_attributes; pmd_loc} =
    enter ();
    let pmd_name = u_loc pmd_name in
    let pmd_type = u_module_type pmd_type in
    let pmd_attributes = u_attributes pmd_attributes in
    let pmd_loc = leave pmd_loc in
    {pmd_name; pmd_type; pmd_attributes; pmd_loc}

  and u_module_substitution {pms_name; pms_manifest; pms_attributes; pms_loc} =
    let pms_name = u_loc pms_name in
    let pms_manifest = u_loc pms_manifest in
    let pms_attributes = u_attributes pms_attributes in
    { pms_name; pms_manifest; pms_attributes; pms_loc }

  and u_module_type_declaration {pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc} =
    enter ();
    let pmtd_name = u_loc pmtd_name in
    let pmtd_type = u_option u_module_type pmtd_type in
    let pmtd_attributes = u_attributes pmtd_attributes in
    let pmtd_loc = leave pmtd_loc in
    {pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc}

  and u_open_declaration {popen_expr; popen_override; popen_loc; popen_attributes} =
    enter ();
    let popen_expr = u_module_expr popen_expr in
    let popen_attributes = u_attributes popen_attributes in
    let popen_loc = leave popen_loc in
    {popen_expr; popen_override; popen_loc; popen_attributes}

  and u_open_description {popen_expr; popen_override; popen_loc; popen_attributes} =
    enter ();
    let popen_expr = u_loc popen_expr in
    let popen_attributes = u_attributes popen_attributes in
    let popen_loc = leave popen_loc in
    {popen_expr; popen_override; popen_loc; popen_attributes}

  and u_include_infos : 'a 'b . ('a -> 'b) -> 'a include_infos -> 'b include_infos =
    fun u_a {pincl_mod; pincl_loc; pincl_attributes} ->
    enter ();
    let pincl_mod = u_a pincl_mod in
    let pincl_attributes = u_attributes pincl_attributes in
    let pincl_loc = leave pincl_loc in
    {pincl_mod; pincl_loc; pincl_attributes}

  and u_include_description id = u_include_infos u_module_type id
  and u_include_declaration id = u_include_infos u_module_expr id

  and u_with_constraint = function
    | Pwith_type (loc, td) -> Pwith_type (u_loc loc, u_type_declaration td)
    | Pwith_module (loc1, loc2) -> Pwith_module (u_loc loc1, u_loc loc2)
    | Pwith_typesubst (loc, td) -> Pwith_typesubst (u_loc loc, u_type_declaration td)
    | Pwith_modsubst (loc1, loc2) -> Pwith_modsubst (u_loc loc1, u_loc loc2)

  and u_module_expr {pmod_desc; pmod_loc; pmod_attributes} =
    enter ();
    let pmod_desc = u_module_expr_desc pmod_desc in
    let pmod_attributes = u_attributes pmod_attributes in
    let pmod_loc = leave pmod_loc in
    {pmod_desc; pmod_loc; pmod_attributes}

  and u_module_expr_desc = function
    | Pmod_ident loc -> Pmod_ident (u_loc loc)
    | Pmod_structure str -> Pmod_structure (u_structure str)
    | Pmod_functor (fp, me) ->
      Pmod_functor (u_functor_parameter fp, u_module_expr me)
    | Pmod_apply (me1, me2) ->
      Pmod_apply (u_module_expr me1, u_module_expr me2)
    | Pmod_constraint (me, mt) ->
      Pmod_constraint (u_module_expr me, u_module_type mt)
    | Pmod_unpack e -> Pmod_unpack (u_expression e)
    | Pmod_extension ext -> Pmod_extension (u_extension ext)

  and u_structure l = List.map ~f:u_structure_item l

  and u_structure_item {pstr_desc; pstr_loc} =
    enter ();
    let pstr_desc = u_structure_item_desc pstr_desc in
    let pstr_loc = leave pstr_loc in
    {pstr_desc; pstr_loc}

  and u_structure_item_desc = function
    | Pstr_eval (expr, attrs) -> Pstr_eval (u_expression expr, u_attributes attrs)
    | Pstr_value (fl, vbs) -> Pstr_value (fl, List.map ~f:u_value_binding vbs)
    | Pstr_primitive vd -> Pstr_primitive (u_value_description vd)
    | Pstr_type (fl, tds) -> Pstr_type (fl, List.map ~f:u_type_declaration tds)
    | Pstr_typext text -> Pstr_typext (u_type_extension text)
    | Pstr_exception texn -> Pstr_exception (u_type_exception texn)
    | Pstr_module mb -> Pstr_module (u_module_binding mb)
    | Pstr_recmodule mbs -> Pstr_recmodule (List.map ~f:u_module_binding mbs)
    | Pstr_modtype mtd -> Pstr_modtype (u_module_type_declaration mtd)
    | Pstr_open od -> Pstr_open (u_open_declaration od)
    | Pstr_class cds -> Pstr_class (List.map ~f:u_class_declaration cds)
    | Pstr_class_type ctds -> Pstr_class_type (List.map ~f:u_class_type_declaration ctds)
    | Pstr_include id -> Pstr_include (u_include_declaration id)
    | Pstr_attribute attr -> Pstr_attribute (u_attribute attr)
    | Pstr_extension (ext, attrs) -> Pstr_extension (u_extension ext, u_attributes attrs)

  and u_value_binding {pvb_pat; pvb_expr; pvb_attributes; pvb_loc} =
    enter ();
    let pvb_pat = u_pattern pvb_pat in
    let pvb_expr = u_expression pvb_expr in
    let pvb_attributes = u_attributes pvb_attributes in
    let pvb_loc = leave pvb_loc in
    {pvb_pat; pvb_expr; pvb_attributes; pvb_loc}

  and u_module_binding {pmb_name; pmb_expr; pmb_attributes; pmb_loc} =
    enter ();
    let pmb_name = u_loc pmb_name in
    let pmb_expr = u_module_expr pmb_expr in
    let pmb_attributes = u_attributes pmb_attributes in
    let pmb_loc = leave pmb_loc in
    {pmb_name; pmb_expr; pmb_attributes; pmb_loc}
end

let rewrite_loc t =
  Rewrite_loc.start ();
  let t = match t with
    | `str str -> `str (Rewrite_loc.u_structure str)
    | `fake str -> `fake (Rewrite_loc.u_structure str)
    | `sg sg -> `sg (Rewrite_loc.u_signature sg)
  in
  Rewrite_loc.exit ();
  t
