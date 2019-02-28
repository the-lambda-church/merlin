(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2019  Merlin contributors

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

let {Logger. log} = Logger.for_section "Mtyper"

type ('p,'t) item = {
  parsetree_item: 'p;
  typedtree_items: 't list * Types.signature_item list;
  part_snapshot  : Btype.snapshot;
  part_env       : Env.t;
  part_errors    : exn list;
  part_checks    : Typecore.delayed_check list;
  part_warnings  : Warnings.state;
}

type typedtree = [
  | `Interface of Typedtree.signature
  | `Implementation of Typedtree.structure
]

type result = {
  config    : Mconfig.t;
  state     : Mocaml.typer_state;
  initial_env : Env.t;
  initial_snapshot : Btype.snapshot;
  typedtree : [
    | `Interface of
        (Parsetree.signature_item, Typedtree.signature_item) item list
    | `Implementation of
        (Parsetree.structure_item, Typedtree.structure_item) item list
  ];
}

let cache = ref []

let cache_key config =
  Mconfig.(config.query.directory, config.query.filename, config.ocaml)

let pop_cache config =
  let title = "pop_cache" in
  let key = cache_key config in
  match List.assoc key !cache with
  | (state, result) ->
    cache := List.remove_assoc key !cache;
    log ~title "found entry for this configuration";
    if Mocaml.with_state state Env.check_state_consistency then (
      log ~title "consistent state, reusing";
      `Cached (state, result)
    )
    else (
      log ~title "inconsistent state, dropping";
      `Inconsistent
    )
  | exception Not_found -> (
      log ~title "nothing cached for this configuration";
      `None
    )

let push_cache config state t =
  let t = (t.initial_env, t.initial_snapshot, t.typedtree) in
  cache := List.take_n 5 ((cache_key config, (state, t)) :: !cache)

let compatible_prefix result_items tree_items =
  let rec aux acc = function
    | (ritem :: ritems, pitem :: pitems)
      when Btype.is_valid ritem.part_snapshot
        && compare ritem.parsetree_item pitem = 0 ->
      aux (ritem :: acc) (ritems, pitems)
    | (_, pitems) ->
      log ~title:"compatible_prefix" "reusing %d items, %d new items to type"
        (List.length acc) (List.length pitems);
      acc, pitems
  in
  aux [] (result_items, tree_items)

let fresh_env config =
  let env0 = Typer_raw.fresh_env () in
  let env0 = Extension.register Mconfig.(config.merlin.extensions) env0 in
  let snap0 = Btype.snapshot () in
  (env0, snap0)

let rec type_structure caught env = function
  | parsetree_item :: rest ->
    let items, _, part_env =
      Typemod.merlin_type_structure env [parsetree_item]
        parsetree_item.Parsetree.pstr_loc in
    let typedtree_items =
      (items.Typedtree.str_items, items.Typedtree.str_type) in
    let item = {
      parsetree_item; typedtree_items; part_env;
      part_snapshot = Btype.snapshot ();
      part_errors   = !caught;
      part_checks   = !Typecore.delayed_checks;
      part_warnings = Warnings.backup ();
    } in
    item :: type_structure caught part_env rest
  | [] -> []

let rec type_signature caught env = function
  | parsetree_item :: rest ->
    let {Typedtree. sig_final_env = part_env; sig_items; sig_type} =
      Typemod.merlin_transl_signature env [parsetree_item] in
    let item = {
      parsetree_item; typedtree_items = (sig_items, sig_type); part_env;
      part_snapshot = Btype.snapshot ();
      part_errors   = !caught;
      part_checks   = !Typecore.delayed_checks;
      part_warnings = Warnings.backup ();
    } in
    item :: type_signature caught part_env rest
  | [] -> []

let type_implementation config caught cached parsetree =
  let env0, snap0, (prefix, parsetree)  =
    match cached with
    | Some (env0, snap0, `Implementation items) when Btype.is_valid snap0 ->
      env0, snap0, compatible_prefix items parsetree
    | Some (env0, snap0, `Interface _)  when Btype.is_valid snap0 ->
      env0, snap0, ([], parsetree)
    | Some _ | None ->
      let env0, snap0 = fresh_env config in
      env0, snap0, ([], parsetree)
  in
  let env', snap', warn' = match prefix with
    | [] -> (env0, snap0, Warnings.backup ())
    | x :: _ ->
      caught := x.part_errors;
      Typecore.delayed_checks := x.part_checks;
      (x.part_env, x.part_snapshot, x.part_warnings)
  in
  Btype.backtrack snap';
  Warnings.restore warn';
  let suffix = type_structure caught env' parsetree in
  env0, snap0, List.rev_append prefix suffix

let type_interface config caught cached parsetree =
  let env0, snap0, (prefix, parsetree)  =
    match cached with
    | Some (env0, snap0, `Interface items) when
        Btype.is_valid snap0 ->
      env0, snap0, compatible_prefix items parsetree
    | Some (env0, snap0, `Implementation _)  when Btype.is_valid snap0 ->
      env0, snap0, ([], parsetree)
    | Some _ | None ->
      let env0, snap0 = fresh_env config in
      env0, snap0, ([], parsetree)
  in
  let env', snap', warn' = match prefix with
    | [] -> (env0, snap0, Warnings.backup ())
    | x :: _ ->
      caught := x.part_errors;
      Typecore.delayed_checks := x.part_checks;
      (x.part_env, x.part_snapshot, x.part_warnings)
  in
  Btype.backtrack snap';
  Warnings.restore warn';
  let suffix = type_signature caught env' parsetree in
  env0, snap0, List.rev_append prefix suffix

let run config parsetree =
  Mocaml.setup_config config;
  let state, cached = match pop_cache config with
    | `Cached (state, entry) -> (state, Some entry)
    | `None | `Inconsistent ->
      Mocaml.flush_caches ();
      (Mocaml.new_state ~unit_name:(Mconfig.unitname config), None)
  in
  Mocaml.with_state state @@ fun () ->
  let caught = ref [] in
  Msupport.catch_errors Mconfig.(config.ocaml.warnings) caught @@ fun () ->
  Typecore.reset_delayed_checks ();
  let result = match parsetree with
    | `Implementation parsetree ->
      let initial_env, initial_snapshot, items =
        type_implementation config caught cached parsetree in
      { config; state; initial_env; initial_snapshot;
        typedtree = `Implementation items }
    | `Interface parsetree ->
      let initial_env, initial_snapshot, items =
        type_interface config caught cached parsetree in
      { config; state; initial_env; initial_snapshot;
        typedtree = `Interface items }
  in
  Typecore.reset_delayed_checks ();
  push_cache config state result;
  result

let with_typer t f =
  Mocaml.with_state t.state f

let get_env ?pos:_ t =
  assert (Mocaml.is_current_state t.state);
  Option.value ~default:t.initial_env (
    match t.typedtree with
    | `Implementation l -> Option.map ~f:(fun x -> x.part_env) (List.last l)
    | `Interface l ->  Option.map ~f:(fun x -> x.part_env) (List.last l)
  )

let get_errors t =
  assert (Mocaml.is_current_state t.state);
  let errors, checks = Option.value ~default:([],[]) (
      let f x = x.part_errors, x.part_checks in
      match t.typedtree with
      | `Implementation l -> Option.map ~f (List.last l)
      | `Interface l ->  Option.map ~f (List.last l)
    )
  in
  let caught = ref errors in
  Typecore.delayed_checks := checks;
  Msupport.catch_errors Mconfig.(t.config.ocaml.warnings) caught
    Typecore.force_delayed_checks;
  Typecore.reset_delayed_checks ();
  (!caught)

let get_typedtree t =
  assert (Mocaml.is_current_state t.state);
  let split_items l =
    let typd, typs = List.split (List.map ~f:(fun x -> x.typedtree_items) l) in
    (List.concat typd, List.concat typs)
  in
  match t.typedtree with
  | `Implementation l ->
    let str_items, str_type = split_items l in
    `Implementation {Typedtree. str_items; str_type; str_final_env = get_env t}
  | `Interface l ->
    let sig_items, sig_type = split_items l in
    `Interface {Typedtree. sig_items; sig_type; sig_final_env = get_env t}

let node_at ?(skip_recovered=false) t pos_cursor =
  assert (Mocaml.is_current_state t.state);
  let node = Mbrowse.of_typedtree (get_typedtree t) in
  let rec select = function
    (* If recovery happens, the incorrect node is kept and a recovery node
       is introduced, so the node to check for recovery is the second one. *)
    | (_,_) :: ((_,node') :: _ as ancestors)
      when Mbrowse.is_recovered node' -> select ancestors
    | l -> l
  in
  match Mbrowse.deepest_before pos_cursor [node] with
  | [] -> [get_env t, Browse_raw.Dummy]
  | path when skip_recovered -> select path
  | path -> path
