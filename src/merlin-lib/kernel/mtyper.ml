open Std
open Local_store.Compiler

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

let cache = srefk None

let fresh_env config =
  let env0 = Typer_raw.fresh_env () in
  let env0 = Extension.register Mconfig.(config.merlin.extensions) env0 in
  let snap0 = Btype.snapshot () in
  (env0, snap0)

let get_cache config =
  match !cache with
  | Some (env0, snap0, items) when Btype.is_valid snap0 ->
    env0, snap0, Some items
  | Some _ | None ->
    let env0, snap0 = fresh_env config in
    env0, snap0, None

let return_and_cache status =
  cache := Some status;
  status

type result = {
  config : Mconfig.t;
  initial_env : Env.t;
  initial_snapshot : Btype.snapshot;
  typedtree : [
    | `Interface of
        (Parsetree.signature_item, Typedtree.signature_item) item list
    | `Implementation of
        (Parsetree.structure_item, Typedtree.structure_item) item list
  ];
}

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

let type_implementation config caught parsetree =
  let env0, snap0, prefix = get_cache config in
  let prefix, parsetree =
    match prefix with
    | Some (`Implementation items) -> compatible_prefix items parsetree
    | Some (`Interface _) | None -> ([], parsetree)
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
  return_and_cache
    (env0, snap0, `Implementation (List.rev_append prefix suffix))

let type_interface config caught parsetree =
  let env0, snap0, prefix = get_cache config in
  let prefix, parsetree =
    match prefix with
    | Some (`Interface items) -> compatible_prefix items parsetree
    | Some (`Implementation _) | None -> ([], parsetree)
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
  return_and_cache
    (env0, snap0, `Interface (List.rev_append prefix suffix))

let run config parsetree =
  if not (Env.check_state_consistency ()) then (
    Mocaml.flush_caches ();
    Local_store.reset compiler_state;
  );
  Mocaml.setup_config config;
  let caught = ref [] in
  Msupport.catch_errors Mconfig.(config.ocaml.warnings) caught @@ fun () ->
  Typecore.reset_delayed_checks ();
  let initial_env, initial_snapshot, typedtree = match parsetree with
    | `Implementation parsetree -> type_implementation config caught parsetree
    | `Interface parsetree -> type_interface config caught parsetree
  in
  Typecore.reset_delayed_checks ();
  { config; initial_env; initial_snapshot; typedtree }

let get_env ?pos:_ t =
  Option.value ~default:t.initial_env (
    match t.typedtree with
    | `Implementation l -> Option.map ~f:(fun x -> x.part_env) (List.last l)
    | `Interface l ->  Option.map ~f:(fun x -> x.part_env) (List.last l)
  )

let get_errors t =
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
  let node = Mbrowse.of_typedtree (get_typedtree t) in
  log ~title:"node_at" "Node: %s" (Mbrowse.print () node);
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
  | path ->
    log ~title:"node_at" "Deepest before %s"
      (Mbrowse.print () path);
    path
