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

let default_loc = Location.none
let default_env = Env.empty

type t = {
  t_node: Mbrowse.node;
  t_loc : Location.t;
  t_env : Env.t;
  t_children: t list lazy_t;
}

let of_node ?(env=default_env) node =
  let rec one t_env t_node =
    let t_loc = Mbrowse.node_loc t_node in
    let rec t = {t_node; t_env; t_loc; t_children = lazy (aux t)} in
    t
  and aux t =
    Mbrowse.fold_node (fun env node acc -> one env node :: acc)
      t.t_env t.t_node []
  in
  one (Browse_raw.node_update_env env node) node

let of_browse b =
  let env, node = Mbrowse.leaf_node b in
  of_node ~env node

let dummy = {
  t_node = Browse_raw.Dummy;
  t_loc = default_loc;
  t_env = default_env;
  t_children = lazy []
}

let rec normalize_type_expr env = function
  | {Types.desc = Types.Tconstr (path,_,_); _ } ->
    normalize_type_decl env (Env.find_type path env)
  | _ -> raise Not_found

and normalize_type_decl env decl = match decl.Types.type_manifest with
  | Some expr -> normalize_type_expr env expr
  | None -> decl

let id_of_constr_decl c = c.Types.cd_id

let same_constructor env a b =
  let name = function
    | `Description d -> d.Types.cstr_name
    | `Declaration d -> Ident.name d.Typedtree.cd_id
  in
  if name a <> name b then false
  else begin
    let get_decls = function
      | `Description d ->
        let ty = normalize_type_expr env d.Types.cstr_res in
        begin match ty.Types.type_kind with
        | Types.Type_variant decls ->
          List.map decls ~f:id_of_constr_decl
        | _ -> assert false
        end
      | `Declaration d ->
        [d.Typedtree.cd_id]
    in
    let a = get_decls a in
    let b = get_decls b in
    List.exists a ~f:(fun id -> List.exists b ~f:(Ident.same id))
  end

let all_occurrences path =
  let rec aux acc t =
    let acc =
      let paths = Browse_raw.node_paths t.t_node in
      let same l = Path.same path l.Location.txt in
      match List.filter ~f:same paths with
      | [] -> acc
      | paths -> (t, paths) :: acc
    in
    List.fold_left ~f:aux ~init:acc (Lazy.force t.t_children)
  in
  aux []

let all_constructor_occurrences ({t_env = env; _},d) t =
  let rec aux acc t =
    let acc =
      match Browse_raw.node_is_constructor t.t_node with
      | Some d' when (
          (* Don't try this at home kids. *)
          try same_constructor env d d'.Location.txt
          with Not_found -> same_constructor t.t_env d d'.Location.txt
        ) ->
        {d' with Location.txt = t} :: acc
      | _ -> acc
    in
    List.fold_left ~f:aux ~init:acc (Lazy.force t.t_children)
  in
  aux [] t

let all_occurrences_of_prefix ~strict_prefix path node =
  let rec path_prefix ~prefix path =
    Path.same prefix path ||
    match Path.Nopos.view path with
    | Pdot (p,_) -> path_prefix ~prefix p
    | _ -> false
  in
  let rec aux env node acc =
    let acc =
      let paths = Browse_raw.node_paths node in
      let has_prefix {Location. txt; _} =
        if not strict_prefix then path_prefix ~prefix:path txt
        else match Path.Nopos.view txt with
          | Pdot (p, _) -> path_prefix ~prefix:path p
          | _ -> false
      in
      match List.filter ~f:has_prefix paths with
      | [] -> acc
      | paths -> (node, paths) :: acc
    in
    Browse_raw.fold_node aux env node acc
  in
  aux Env.empty node []
