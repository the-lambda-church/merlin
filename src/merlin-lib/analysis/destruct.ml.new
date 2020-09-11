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
open Browse_raw

exception Not_allowed of string
exception Useless_refine
exception Nothing_to_do
exception Wrong_parent of string

let {Logger. log} = Logger.for_section "destruct"

let () =
  Location.register_error_of_exn (function
    | Not_allowed s  -> Some (Location.error ("Destruct not allowed on " ^ s))
    | Useless_refine -> Some (Location.error "Cannot refine an useless branch")
    | Nothing_to_do  -> Some (Location.error "Nothing to do")
    | _ -> None
  )

let mk_id s  = Location.mknoloc (Longident.Lident s)
let mk_var s = Location.mknoloc s

module Predef_types = struct
  let char_ env ty =
    let a = Tast_helper.Pat.constant env ty (Asttypes.Const_char 'a') in
    let z = Parmatch.omega in
    [ a ; z ]

  let int_ env ty =
    let zero = Tast_helper.Pat.constant env ty (Asttypes.Const_int 0) in
    let n = Parmatch.omega in
    [ zero ; n ]

  let string_ env ty =
    let empty = Tast_helper.Pat.constant env ty (Raw_compat.const_string ("", None)) in
    let s = Parmatch.omega in
    [ empty ; s ]

  let tbl = Hashtbl.create 3

  let () =
    List.iter ~f:(fun (k, v) -> Hashtbl.add tbl k v) [
      Predef.path_char, char_ ;
      Predef.path_int, int_ ;
      Predef.path_string, string_ ;
    ]
end

let placeholder =
  Ast_helper.Exp.ident (mk_id "??")

let rec gen_patterns ?(recurse=true) env type_expr =
  let open Types in
  let type_expr = Btype.repr type_expr in
  match type_expr.desc with
  | Tlink _    -> assert false (* impossible after [Btype.repr] *)
  | Tvar _     -> raise (Not_allowed "non-immediate type")
  | Tarrow _   -> raise (Not_allowed "arrow type")
  | Tobject _  -> raise (Not_allowed "object type")
  | Tpackage _ -> raise (Not_allowed "modules")
  | Ttuple lst ->
    let patterns = Parmatch.omega_list lst in
    [ Tast_helper.Pat.tuple env type_expr patterns ]
  | Tconstr (path, _params, _) ->
    begin match Env.find_type_descrs path env with
    | [], [] ->
      if recurse then from_type_decl env path type_expr else
      raise (Not_allowed (sprintf "non-destructible type: %s" (Path.last path)))
    | [], labels ->
      let lst =
        List.map labels ~f:(fun lbl_descr ->
          let lidloc = mk_id lbl_descr.lbl_name in
          lidloc, lbl_descr,
          Tast_helper.Pat.var env type_expr (mk_var lbl_descr.lbl_name)
        )
      in
      [ Tast_helper.Pat.record env type_expr lst Asttypes.Closed ]
    | constructors, _ ->
      let prefix =
        let path = Printtyp.shorten_type_path env path in
        match Path.to_string_list path with
        | [] -> assert false
        | p :: ps ->
          fun name ->
            let open Longident in
            match
              List.fold_left ps ~init:(Lident p) ~f:(fun lid p -> Ldot (lid, p))
            with
            | Lident _ -> Lident name
            | Ldot (lid, _) -> Ldot (lid, name)
            | _ -> assert false
      in
      let are_types_unifiable typ =
        let snap = Btype.snapshot () in
        let res =
          try
            Ctype.unify_gadt 0 (ref env) type_expr typ [@ocaml.warning "-6"]
            (* The label was called "newtype_level" up to 4.06 (incl) and became
               "equation_level" after that. *) ;
            true
          with Ctype.Unify _trace -> false
        in
        Btype.backtrack snap ;
        res
      in
      List.filter_map constructors ~f:(fun cstr_descr ->
        if cstr_descr.cstr_generalized &&
           not (are_types_unifiable cstr_descr.cstr_res)
        then (
          log ~title:"gen_patterns" "%a"
            Logger.fmt (fun fmt ->
              Format.fprintf fmt
                "Eliminating '%s' branch, its return type is not\
                \ compatible with the expected type (%a)"
                cstr_descr.cstr_name Printtyp.type_expr type_expr);
          None
        ) else
          let args =
            if cstr_descr.cstr_arity <= 0 then [] else
              Parmatch.omegas cstr_descr.cstr_arity
          in
          let lidl = Location.mknoloc (prefix cstr_descr.cstr_name) in
          Some (Tast_helper.Pat.construct env type_expr lidl cstr_descr args)
      )
    end
  | Tvariant row_desc ->
    List.filter_map row_desc.row_fields ~f:(function
      | lbl, Rpresent param_opt ->
        let popt = Option.map param_opt ~f:(fun _ -> Parmatch.omega) in
        Some (Tast_helper.Pat.variant env type_expr lbl popt (ref row_desc))
      | _, _ -> None
    )
  | _ ->
    let fmt, to_string = Format.to_string () in
    Printtyp.type_expr fmt type_expr ;
    raise (Not_allowed (to_string ()))

and from_type_decl env path texpr =
  let tdecl = Env.find_type path env in
  match tdecl.Types.type_manifest with
  | Some te -> gen_patterns ~recurse:false env te
  | None ->
    try Hashtbl.find Predef_types.tbl path env texpr
    with Not_found ->
      raise (Not_allowed (sprintf "non-destructible type: %s" (Path.last path)))


let rec needs_parentheses = function
  | [] -> false
  | t :: ts ->
    match t with
    | Structure _
    | Structure_item _
    | Value_binding _ -> false
    | Expression e ->
      begin match e.Typedtree.exp_desc with
      | Typedtree.Texp_for _
      | Typedtree.Texp_while _ -> false
      | Typedtree.Texp_let _ ->
        (* We are after the "in" keyword, we need to look at the parent of the
           binding. *)
        needs_parentheses ts
      | Typedtree.Texp_function _ as desc
        when List.length (Raw_compat.texp_function_cases desc) = 1 ->
        (* The assumption here is that we're not in a [function ... | ...]
            situation but either in [fun param] or [let name param]. *)
        needs_parentheses ts
      | _ -> true
      end
    | _ -> needs_parentheses ts

let rec get_every_pattern = function
  | [] -> assert false
  | parent :: parents ->
    match parent with
    | Case _
    | Pattern _ ->
      (* We are still in the same branch, going up. *)
      get_every_pattern parents
    | Expression _ ->
      (* We are on the right node *)
      let patterns : Typedtree.pattern list =
        Mbrowse.fold_node (fun env node acc ->
          match node with
          | Pattern _ -> (* Not expected here *) assert false
          | Case _ ->
              Mbrowse.fold_node (fun _env node acc ->
                 match node with
              | Pattern p ->
                begin match Typedtree.classify_pattern p with
                | Value -> let p : Typedtree.pattern = p in p :: acc
                | Computation -> let val_p, _ = Typedtree.split_pattern p in
                  (* We ignore computation patterns *)
                  begin match val_p with
                  | Some val_p ->  val_p :: acc
                  | None -> acc
                  end
                end
              | _ -> acc
              ) env node acc
          | _ -> acc
        ) Env.empty parent []
      in
      let loc =
        Mbrowse.fold_node (fun _ node acc ->
          let open Location in
          let loc = Mbrowse.node_loc node in
          if Lexing.compare_pos loc.loc_end acc.loc_end > 0 then loc else acc
        ) Env.empty parent Location.none
      in
      loc, patterns
    | _ ->
      let s = Json.to_string (Browse_misc.dump_browse parent) in
      raise  (Wrong_parent (sprintf "get_every_pattern: %s" s)) (* Something went wrong. *)

let rec destructible patt =
  let open Typedtree in
  match patt.pat_desc with
  | Tpat_any | Tpat_var _ -> true
  | Tpat_alias (p, _, _)  -> destructible p
  | _ -> false


let is_package ty =
  match ty.Types.desc with
  | Types.Tpackage _ -> true
  | _ -> false

let filter_attr =
  let default = Ast_mapper.default_mapper in
  let keep attr =
    let ({Location.txt;_},_) = Ast_helper.Attr.as_tuple attr in
    not (String.is_prefixed ~by:"merlin." txt)
  in
  let attributes mapper attrs =
    default.Ast_mapper.attributes mapper (List.filter ~f:keep attrs)
  in
  {default with Ast_mapper.attributes}

let filter_expr_attr expr =
  filter_attr.Ast_mapper.expr filter_attr expr

let filter_pat_attr pat =
  filter_attr.Ast_mapper.pat filter_attr pat

let rec subst_patt initial ~by patt =
  let f = subst_patt initial ~by in
  if patt == initial then by else
  let open Typedtree in
  match patt.pat_desc with
  | Tpat_any
  | Tpat_var _
  | Tpat_constant _ -> patt
  | Tpat_alias (p,x,y) ->
    { patt with pat_desc = Tpat_alias (f p, x, y) }
  | Tpat_tuple lst ->
    { patt with pat_desc = Tpat_tuple (List.map lst ~f) }
  | Tpat_construct (lid, cd, lst) ->
    { patt with pat_desc = Tpat_construct (lid, cd, List.map lst ~f) }
  | Tpat_variant (lbl, pat_opt, row_desc) ->
    { patt with pat_desc = Tpat_variant (lbl, Option.map pat_opt ~f, row_desc) }
  | Tpat_record (sub, flg) ->
    let sub' =
      List.map sub ~f:(fun (lid, lbl_descr, patt) -> lid, lbl_descr, f patt)
    in
    { patt with pat_desc = Tpat_record (sub', flg) }
  | Tpat_array lst ->
    { patt with pat_desc = Tpat_array (List.map lst ~f) }
  | Tpat_or (p1, p2, row) ->
    { patt with pat_desc = Tpat_or (f p1, f p2, row) }
  | Tpat_lazy p ->
    { patt with pat_desc = Tpat_lazy (f p) }

let rec rm_sub patt sub =
  let f p = rm_sub p sub in
  let open Typedtree in
  match patt.pat_desc with
  | Tpat_any
  | Tpat_var _
  | Tpat_constant _ -> patt
  | Tpat_alias (p,x,y) ->
    { patt with pat_desc = Tpat_alias (f p, x, y)  }
  | Tpat_tuple lst ->
    { patt with pat_desc = Tpat_tuple (List.map lst ~f) }
  | Tpat_construct (lid, cd, lst) ->
    { patt with pat_desc = Tpat_construct (lid, cd, List.map lst ~f) }
  | Tpat_variant (lbl, pat_opt, row_desc) ->
    { patt with pat_desc = Tpat_variant (lbl, Option.map pat_opt ~f, row_desc) }
  | Tpat_record (sub, flg) ->
    let sub' =
      List.map sub ~f:(fun (lid, lbl_descr, patt) -> lid, lbl_descr, f patt)
    in
    { patt with pat_desc = Tpat_record (sub', flg) }
  | Tpat_array lst ->
    { patt with pat_desc = Tpat_array (List.map lst ~f) }
  | Tpat_or (p1, p2, row) ->
    if p1 == sub then p2 else if p2 == sub then p1 else
      { patt with pat_desc = Tpat_or (f p1, f p2, row) }
  | Tpat_lazy p ->
    { patt with pat_desc = Tpat_lazy (f p) }

let rec qualify_constructors f pat =
  let open Typedtree in
  let pat_desc =
    match pat.pat_desc with
    | Tpat_alias (p, id, loc) -> Tpat_alias (qualify_constructors f p, id, loc)
    | Tpat_tuple ps -> Tpat_tuple (List.map ps ~f:(qualify_constructors f))
    | Tpat_record (labels, closed) ->
      let labels =
        List.map labels
          ~f:(fun (lid, descr, pat) -> lid, descr, qualify_constructors f pat)
      in
      Tpat_record (labels, closed)
    | Tpat_construct (lid, cstr_desc, ps) ->
      let lid =
        match lid.Asttypes.txt with
        | Longident.Lident name ->
          begin match (Btype.repr pat.pat_type).Types.desc with
          | Types.Tconstr (path, _, _) ->
            let path = f pat.pat_env path in
            begin match Path.to_string_list path with
            | [] -> assert false
            | p :: ps ->
              let open Longident in
              match
                List.fold_left ps ~init:(Lident p)
                  ~f:(fun lid p -> Ldot (lid, p))
              with
              | Lident _ -> { lid with Asttypes.txt = Lident name }
              | Ldot (path, _) -> { lid with Asttypes.txt = Ldot (path, name) }
              | _ -> assert false
            end
          | _ -> lid
          end
        | _ -> lid (* already qualified *)
      in
      Tpat_construct (lid, cstr_desc, List.map ps ~f:(qualify_constructors f))
    | Tpat_array ps -> Tpat_array (List.map ps ~f:(qualify_constructors f))
    | Tpat_or (p1, p2, row_desc) ->
      Tpat_or (qualify_constructors f p1, qualify_constructors f p2, row_desc)
    | Tpat_lazy p -> Tpat_lazy (qualify_constructors f p)
    | desc -> desc
  in
  { pat with pat_desc = pat_desc }

let find_branch patterns sub =
  let rec is_sub_patt patt ~sub =
    if patt == sub then true else
      let open Typedtree in
      match patt.pat_desc with
      | Tpat_any
      | Tpat_var _
      | Tpat_constant _
      | Tpat_variant (_, None, _) -> false
      | Tpat_alias (p,_,_)
      | Tpat_variant (_, Some p, _)
      | Tpat_lazy p ->
        is_sub_patt p ~sub
      | Tpat_tuple lst
      | Tpat_construct (_, _, lst)
      | Tpat_array lst ->
        List.exists lst ~f:(is_sub_patt ~sub)
      | Tpat_record (subs, _) ->
        List.exists subs ~f:(fun (_, _, p) -> is_sub_patt p ~sub)
      | Tpat_or (p1, p2, _) ->
        is_sub_patt p1 ~sub || is_sub_patt p2 ~sub
  in
  let rec aux before = function
    | [] -> raise Not_found
    | p :: after when is_sub_patt p ~sub -> before, after, p
    | p :: ps -> aux (p :: before) ps
  in
  aux [] patterns

let node config source node parents =
  let open Extend_protocol.Reader in
  let loc = Mbrowse.node_loc node in
  match node with
  | Expression expr ->
    let ty = expr.Typedtree.exp_type in
    let pexp = filter_expr_attr (Untypeast.untype_expression expr) in
    let needs_parentheses, result =
      if is_package ty then (
        let mode = Ast_helper.Mod.unpack pexp in
        false, Ast_helper.Exp.letmodule_no_opt "M" mode placeholder
      ) else (
        let ps = gen_patterns expr.Typedtree.exp_env ty in
        let cases  =
          List.map ps ~f:(fun patt ->
            let pc_lhs = filter_pat_attr (Untypeast.untype_pattern patt) in
            { Parsetree. pc_lhs ; pc_guard = None ; pc_rhs = placeholder }
          )
        in
        needs_parentheses parents, Ast_helper.Exp.match_ pexp cases
      )
    in
    let str = Mreader.print_pretty
        config source (Pretty_expression result) in
    let str = if needs_parentheses then "(" ^ str ^ ")" else str in
    loc, str
  | Pattern patt ->
    begin let last_case_loc, patterns = get_every_pattern parents in
      (* Printf.eprintf "tot %d o%!"(List.length patterns); *)
      List.iter patterns ~f:(fun p ->
        let p = filter_pat_attr (Untypeast.untype_pattern p) in
        log ~title:"EXISTING" "%t"
          (fun () -> Mreader.print_pretty config source (Pretty_pattern p))
      ) ;
      let pss = List.map patterns ~f:(fun x -> [ x ]) in
      begin match Parmatch.complete_partial pss with
      | Some pat ->
        let pat  = qualify_constructors Printtyp.shorten_type_path pat in
        let ppat = filter_pat_attr (Untypeast.untype_pattern pat) in
        let case = Ast_helper.Exp.case ppat placeholder in
        let loc =
          let open Location in
          { last_case_loc with loc_start = last_case_loc.loc_end }
        in
        let str = Mreader.print_pretty
            config source (Pretty_case_list [ case ]) in
        loc, str
      | None ->
        begin match Typedtree.classify_pattern patt with
        | Computation -> raise (Not_allowed ("computation pattern"));
        | Value ->
          let _patt : Typedtree.value Typedtree.general_pattern = patt in
          if not (destructible patt) then raise Nothing_to_do else
            let ty = patt.Typedtree.pat_type in
            begin match gen_patterns patt.Typedtree.pat_env ty with
            | [] -> assert false (* we raise Not_allowed, but never return [] *)
            | [ more_precise ] ->
              (* If only one pattern is generated, then we're only refining the
                current pattern, not generating new branches. *)
              let ppat = filter_pat_attr (Untypeast.untype_pattern more_precise) in
              let str = Mreader.print_pretty
                  config source (Pretty_pattern ppat) in
              patt.Typedtree.pat_loc, str
            | sub_patterns ->
              let rev_before, after, top_patt =
                find_branch patterns patt
              in
              let new_branches =
                List.map sub_patterns ~f:(fun by ->
                  subst_patt patt ~by top_patt
                )
              in
              let patterns =
                List.rev_append rev_before
                  (List.append new_branches after)
              in
              let unused = Parmatch.return_unused patterns in
              let new_branches =
                List.fold_left unused ~init:new_branches ~f:(fun branches u ->
                  match u with
                  | `Unused p -> List.remove ~phys:true p branches
                  | `Unused_subs (p, lst) ->
                    List.map branches ~f:(fun branch ->
                      if branch != p then branch else
                      List.fold_left lst ~init:branch ~f:rm_sub
                    )
                )
              in
              match new_branches with
              | [] -> raise Useless_refine
              | p :: ps ->
                let p =
                  List.fold_left ps ~init:p ~f:(fun acc p ->
                    Tast_helper.Pat.pat_or top_patt.Typedtree.pat_env
                      top_patt.Typedtree.pat_type acc p
                  )
                in
                let ppat = filter_pat_attr (Untypeast.untype_pattern p) in
                let str = Mreader.print_pretty
                    config source (Pretty_pattern ppat) in
                top_patt.Typedtree.pat_loc, str
            end
        end
      end
    end
  | node ->
    raise (Not_allowed (string_of_node node))
