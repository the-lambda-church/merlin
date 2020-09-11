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

[@@@ocaml.warning "-9"] (* yolo *)

open Std

(* For Browse_misc *)

let signature_of_summary =
  let open Env in
  let open Types in
  function
  | Env_value (_,i,v)      -> Some (Sig_value (i,v))
  (* Trec_not == bluff, FIXME *)
  | Env_type (_,i,t)       -> Some (Sig_type (i,t,Trec_not))
  (* Texp_first == bluff, FIXME *)
  | Env_extension (_,i,e)  ->
    begin match e.ext_type_path with
    | Path.Pident id when Ident.name id = "exn" ->
      Some (Sig_typext (i,e, Text_exception))
    | _ ->
      Some (Sig_typext (i,e, Text_first))
    end
  | Env_module (_,i,m)     -> Some (Sig_module (i,m,Trec_not))
  | Env_modtype (_,i,m)    -> Some (Sig_modtype (i,m))
  | Env_class (_,i,c)      -> Some (Sig_class (i,c,Trec_not))
  | Env_cltype (_,i,c)     -> Some (Sig_class_type (i,c,Trec_not))
  | Env_open _ | Env_empty | Env_functor_arg _ | Env_constraints _ -> None

let summary_prev = function
  | Env.Env_empty -> None
  | Env.Env_open (s,_)     | Env.Env_value (s,_,_)
  | Env.Env_type (s,_,_)   | Env.Env_extension (s,_,_)
  | Env.Env_module (s,_,_) | Env.Env_modtype (s,_,_)
  | Env.Env_class (s,_,_)  | Env.Env_cltype (s,_,_)
  | Env.Env_functor_arg (s,_)
  | Env.Env_constraints (s,_) ->
    Some s

let summary_module_ident_opt = function
  | Env.Env_module (_,i,_) -> Some i
  | _ -> None

(* For Type_utils *)

let dest_tstr_eval str =
  let open Typedtree in
  match str.str_items with
  | [ { str_desc = Tstr_eval (exp,_) }] -> exp
  | _ -> failwith "unhandled expression"

(* For Completion *)

let labels_of_application ~prefix = function
  | {Typedtree. exp_desc = Typedtree.Texp_apply (f, args); exp_env; _} ->
    let open Typedtree in
    let rec labels t =
      let t = Ctype.repr t in
      match t.Types.desc with
      | Types.Tarrow (label, lhs, rhs, _) ->
        (label, lhs) :: labels rhs
      | _ ->
        let t' = Ctype.full_expand exp_env t in
        if Types.TypeOps.equal t t' then
          []
        else
          labels t'
    in
    let labels = labels f.exp_type in
    let is_application_of label (label',expr) =
      match expr with
      | Some {exp_loc = {Location. loc_ghost; loc_start; loc_end}} ->
        label = label'
        && (Btype.prefixed_label_name label <> prefix)
        && not loc_ghost
        && not (loc_start = loc_end)
      | None -> false
    in
    List.filter_map ~f:(fun (label, ty) ->
        match label with
        | Asttypes.Nolabel -> None
        | label when List.exists ~f:(is_application_of label) args -> None
        | Asttypes.Labelled str -> Some ("~" ^ str, ty)
        | Asttypes.Optional str ->
          let ty = match (Ctype.repr ty).Types.desc with
            | Types.Tconstr (path, [ty], _)
              when Path.same path Predef.path_option -> ty
            | _ -> ty
          in
          Some ("?" ^ str, ty)
      ) labels
  | _ -> []

(* Select open nodes *)

let rec select_open_node =
  let open Typedtree in
  let open Browse_raw in
  function
  | (_, ( Structure_item ({str_desc = Tstr_open op}, _)
        | Signature_item ({sig_desc = Tsig_open op}, _)))
    :: ancestors ->
    Some (op.open_path, ancestors)
  | (_, Pattern {pat_extra; _}) :: ancestors
    when List.exists pat_extra
        ~f:(function (Tpat_open _, _ ,_) -> true | _ -> false) ->
    let p = List.find_map pat_extra
        ~f:(function | Tpat_open (p,_,_), _ ,_ -> Some p
                     | _ -> None)
    in
    Some (p, ancestors)
  | (_, Expression {exp_extra; _}) :: _ as ancestors
    when List.exists exp_extra
        ~f:(function (Texp_open _, _ ,_) -> true | _ -> false) ->
    let p = List.find_map exp_extra
        ~f:(function | Texp_open (_,p,_,_), _ ,_ -> Some p
                     | _ -> None)
    in
    Some (p, ancestors)
  | [] -> None
  | _ :: ancestors -> select_open_node ancestors

let texp_function_cases = function
  | Typedtree.Texp_function (_,cs,_) -> cs
  | _ -> assert false

let const_string (s, o) = Asttypes.Const_string (s, o)

let dummy_type_scheme desc =
  { Types. level = 0 ; id = 0 ; desc }

let ctype_instance env scheme =
  Ctype.instance env scheme

let si_modtype_opt : Types.signature_item -> Types.module_type option = function
  | Sig_modtype (_, m) -> m.mtd_type
  | Sig_module (_, m, _) -> Some m.md_type
  | _ -> None

module Pattern = struct
  open Asttypes

  type pattern = Typedtree.pattern

  type desc_view =
    | Tpat_any
    | Tpat_var of Ident.t * string loc
    | Tpat_alias of pattern * Ident.t * string loc
    | Tpat_constant of constant
    | Tpat_tuple of pattern list
    | Tpat_construct of
        Longident.t loc * Types.constructor_description * pattern list
    | Tpat_variant of label * pattern option * Types.row_desc ref
    | Tpat_record of
        (Longident.t loc * Types.label_description * pattern) list *
        closed_flag
    | Tpat_array of pattern list
    | Tpat_or of pattern * pattern * Types.row_desc option
    | Tpat_lazy of pattern
    | Tpat_exception of pattern

  let view p = Obj.magic p.Typedtree.pat_desc (* ben quoi ? c'est vrai *)

  exception Not_supported

  let update_desc_exn p desc_view =
    let pat_desc : Typedtree.pattern_desc =
      match desc_view with
      | Tpat_exception _ -> raise Not_supported
      | _ -> Obj.magic desc_view (* yolo. *)
    in
    { p with Typedtree. pat_desc }
end

let md_id { Typedtree.md_id; _ } = Some md_id
let mb_id { Typedtree.mb_id; _ } = Some mb_id
