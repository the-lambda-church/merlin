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
open Option.Infix

(* Réglisse la police *)
open Typedtree

open Browse_raw
open Browse_tree

let id_of_patt = function
  | { pat_desc = Tpat_var (id, _) ; _ } -> Some id
  | _ -> None

let mk ?(children=[]) ~location ~deprecated outline_kind outline_type id =
  { Query_protocol.  outline_kind; outline_type; location; children;
    outline_name = Ident.name id ; deprecated }

let get_class_field_desc_infos = function
  | Typedtree.Tcf_val (str_loc,_,_,_,_) -> Some (str_loc, `Value)
  | Typedtree.Tcf_method (str_loc,_,_)  -> Some (str_loc, `Method)
  | _ -> None

let outline_type ~env typ =
  let ppf, to_string = Format.to_string () in
  Printtyp.wrap_printing_env env (fun () ->
  Type_utils.print_type_with_decl ~verbosity:0 env ppf typ);
  Some (to_string ())

let rec summarize node =
  let location = node.t_loc in
  match node.t_node with
  | Value_binding vb      ->
    let deprecated = Type_utils.is_deprecated vb.vb_attributes in
    begin match id_of_patt vb.vb_pat with
    | None -> None
    | Some ident ->
      let typ = outline_type ~env:node.t_env vb.vb_pat.pat_type in
      Some (mk ~location ~deprecated `Value typ ident)
    end
  | Value_description vd  ->
    let deprecated = Type_utils.is_deprecated vd.val_attributes in
    let typ = outline_type ~env:node.t_env vd.val_val.val_type in
    Some (mk ~location ~deprecated `Value typ vd.val_id)

  | Module_declaration md ->
    let children = get_mod_children node in
    begin match Raw_compat.md_id md with
    | None -> None
    | Some id ->
      let deprecated = Type_utils.is_deprecated md.md_attributes in
      Some (mk ~children ~location ~deprecated `Module None id)
    end

  | Module_binding mb ->
    let children = get_mod_children node in
    begin match Raw_compat.mb_id mb with
    | None -> None
    | Some id ->
      let deprecated = Type_utils.is_deprecated mb.mb_attributes in
      Some (mk ~children ~location ~deprecated `Module None id)
    end

  | Module_type_declaration mtd ->
    let children = get_mod_children node in
    let deprecated = Type_utils.is_deprecated mtd.mtd_attributes in
    Some (mk ~deprecated ~children ~location `Modtype None mtd.mtd_id)

  | Type_declaration td ->
    let children =
      List.concat_map (Lazy.force node.t_children) ~f:(fun child ->
        match child.t_node with
        | Type_kind _ ->
          List.map (Lazy.force child.t_children) ~f:(fun x ->
            match x.t_node with
            | Constructor_declaration c ->
              let deprecated = Type_utils.is_deprecated c.cd_attributes in
              mk `Constructor None c.cd_id ~deprecated ~location:c.cd_loc
            | Label_declaration ld ->
              let deprecated = Type_utils.is_deprecated ld.ld_attributes in
              mk `Label None ld.ld_id ~deprecated ~location:ld.ld_loc
            | _ -> assert false (* ! *)
          )
        | _ -> []
      )
    in
    let deprecated = Type_utils.is_deprecated td.typ_attributes in
    Some (mk ~children ~location ~deprecated `Type None td.typ_id)

  | Type_extension te ->
    let name = Path.name te.tyext_path in
    let children =
      List.filter_map (Lazy.force node.t_children) ~f:(fun x ->
        summarize x >>| fun x -> { x with Query_protocol.outline_kind = `Constructor }
      )
    in
    let deprecated = Type_utils.is_deprecated te.tyext_attributes in
    Some { Query_protocol. outline_name = name; outline_kind = `Type
         ; outline_type = None; location; children; deprecated }

  | Extension_constructor ec ->
    let deprecated = Type_utils.is_deprecated ec.ext_attributes in
    Some (mk ~location `Exn None ec.ext_id ~deprecated)

  | Class_declaration cd ->
    let children =
      List.concat_map (Lazy.force node.t_children) ~f:get_class_elements
    in
    let deprecated = Type_utils.is_deprecated cd.ci_attributes in
    Some (mk ~children ~location `Class None cd.ci_id_class_type ~deprecated)

  | _ -> None

and get_class_elements node =
  match node.t_node with
  | Class_expr _ ->
    List.concat_map (Lazy.force node.t_children) ~f:get_class_elements
  | Class_structure _ ->
    List.filter_map (Lazy.force node.t_children) ~f:(fun child ->
      match child.t_node with
      | Class_field cf ->
        begin match get_class_field_desc_infos cf.cf_desc with
        | Some (str_loc, outline_kind) ->
          let deprecated = Type_utils.is_deprecated cf.cf_attributes in
          Some { Query_protocol.
            outline_name = str_loc.Location.txt;
            outline_kind;
            outline_type = None;
            location = str_loc.Location.loc;
            children = [];
            deprecated
          }
        | None -> None
        end
      | _ -> None
    )
  | _ -> []

and get_mod_children node =
  List.concat_map (Lazy.force node.t_children) ~f:remove_mod_indir

and remove_mod_indir node =
  match node.t_node with
  | Module_expr _
  | Module_type _ ->
    List.concat_map (Lazy.force node.t_children) ~f:remove_mod_indir
  | _ -> remove_top_indir node

and remove_top_indir t =
  match t.t_node with
  | Structure _
  | Signature _ -> List.concat_map ~f:remove_top_indir (Lazy.force t.t_children)
  | Signature_item _
  | Structure_item _ -> List.filter_map (Lazy.force t.t_children) ~f:summarize
  | _ -> []

let get browses = List.concat @@ List.rev_map ~f:remove_top_indir browses

let shape cursor nodes =
  let rec aux node =
    (* A node is selected if:
       - part of the module language
       - or under the cursor *)
    let selected = match node.t_node with
      | Module_expr _
      | Module_type_constraint _
      | Structure _
      | Structure_item _
      | Module_binding _
      | Module_type _
      | Signature _
      | Signature_item _
      | Module_declaration _
      | Module_type_declaration _
      | Module_binding_name _
      | Module_declaration_name _
      | Module_type_declaration_name _ -> not node.t_loc.Location.loc_ghost
      | _ -> Location_aux.compare_pos cursor node.t_loc = 0 &&
             Lexing.compare_pos node.t_loc.Location.loc_start cursor <> 0 &&
             Lexing.compare_pos node.t_loc.Location.loc_end cursor <> 0
    in
    if selected then [{
        Query_protocol.
        shape_loc = node.t_loc;
        shape_sub = List.concat_map ~f:aux (Lazy.force node.t_children)
      }]
    else []
  in
  List.concat_map ~f:aux nodes
