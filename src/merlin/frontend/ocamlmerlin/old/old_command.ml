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
open Old_protocol
module Printtyp = Type_utils.Printtyp

type customization = [
  | `Ext of [`Enabled | `Disabled] * string
  | `Flags of string list
  | `Use of string list
  | `Path of [`Build | `Source] * [`Add | `Rem] * string list
]

let customize config =
  let open Mconfig in
  function
  | `Ext (`Enabled, ext) ->
    let extensions = ext :: config.merlin.extensions in
    {config with merlin = {config.merlin with extensions}};
  | `Ext (`Disabled, ext) ->
    let extensions = List.remove_all ext config.merlin.extensions in
    {config with merlin = {config.merlin with extensions}};
  | `Flags flags ->
    let flags_to_apply = [{workdir = config.query.directory; workval = flags}] in
    {config with merlin = {config.merlin with flags_to_apply}}
  | `Use _pkgs ->
    config
  | `Path (var, action, paths) ->
    let f l = match action with
      | `Add -> List.filter_dup (paths @ l)
      | `Rem -> List.filter l ~f:(fun x -> not (List.mem x ~set:paths))
    in
    let merlin = config.merlin in
    let merlin =
      match var with
      | `Build -> {merlin with build_path = f merlin.build_path}
      | `Source -> {merlin with source_path = f merlin.source_path}
    in
    {config with merlin}


type buffer = {
  path: string option;
  dot_merlins: string list option;
  mutable customization : customization list;
  mutable source : Msource.t;
}

type state = {
  mutable buffer : buffer;
}

let normalize_document doc =
  doc.Context.path, doc.Context.dot_merlins

let new_buffer (path, dot_merlins) =
  { path; dot_merlins; customization = [];
    source = Msource.make "" }

let default_config = ref Mconfig.initial

let configure (state : buffer) =
  let config = !default_config in
  let config = {config with Mconfig.query = match state.path with
      | None -> config.Mconfig.query
      | Some path -> {
          config.Mconfig.query with
          Mconfig.
          filename = Filename.basename path;
          directory = Misc.canonicalize_filename (Filename.dirname path);
        }
    } in
  let config =
    match state.dot_merlins with
    | Some (first :: _) -> (* ignore anything but the first one... *)
      Mconfig.get_external_config first config
    | None | Some [] ->
      match state.path with
      | None -> config
      | Some p -> Mconfig.get_external_config p config
  in
  List.fold_left ~f:customize ~init:config state.customization

let new_state document =
  { buffer = new_buffer document }

let checkout_buffer_cache = ref []
let checkout_buffer =
  let cache_size = 8 in
  fun document ->
    let document = normalize_document document in
    try List.assoc document !checkout_buffer_cache
    with Not_found ->
      let buffer = new_buffer document in
      begin match document with
        | Some _, _ ->
          checkout_buffer_cache :=
            (document, buffer) :: List.take_n cache_size !checkout_buffer_cache
        | None, _ -> ()
      end;
      buffer

let make_pipeline config buffer =
  Mpipeline.make config buffer.source

let dispatch_sync config state (type a) : a sync_command -> a = function
  | Idle_job -> false

  | Tell (pos_start, pos_end, text) ->
    let source = Msource.substitute state.source pos_start pos_end text in
    state.source <- source

  | Refresh ->
    checkout_buffer_cache := [];
    Cmi_cache.flush ()

  | Flags_set flags ->
    state.customization <-
      (`Flags flags) ::
      List.filter ~f:(function `Flags _ -> false | _ -> true)
        state.customization;
    `Ok

  | Findlib_use packages ->
    state.customization <-
      (`Use packages) ::
      List.filter ~f:(function `Use _ -> false | _ -> true)
        state.customization;
    `Ok

  | Extension_set (action,exts) ->
    state.customization <-
      List.map ~f:(fun ext -> `Ext (action, ext)) exts @
      List.filter ~f:(function
          | `Ext (_, ext) when List.mem ext ~set:exts -> false
          | _ -> true
        ) state.customization;
    `Ok

  | Path (var,_,paths) ->
    state.customization <-
      List.filter_map ~f:(function
          | `Path (var', action', paths') when var = var' ->
            let paths' = List.filter paths'
                ~f:(fun path -> not (List.mem path ~set:paths))
            in
            if paths' = [] then None else Some (`Path (var', action', paths'))
          | x -> Some x
        ) state.customization

  | Path_reset ->
    state.customization <-
      List.filter ~f:(function | `Path _ -> false
          | _ -> true
        ) state.customization;

  | Protocol_version version ->
    begin match version with
      | None -> ()
      | Some 2 -> Old_IO.current_version := `V2
      | Some 3 -> Old_IO.current_version := `V3
      | Some _ -> ()
    end;
    (`Selected !Old_IO.current_version,
     `Latest Old_IO.latest_version,
     Printf.sprintf "The Merlin toolkit version %s, for Ocaml %s\n"
       My_config.version Sys.ocaml_version)

  | Flags_get ->
    let pipeline = make_pipeline config state in
    let config = Mpipeline.final_config pipeline in
    List.concat_map ~f:(fun f -> f.workval)
      Mconfig.(config.merlin.flags_to_apply)

  | Project_get ->
    let failures = match  Mconfig.(config.merlin.failures) with
      | [] -> `Ok
      | failures -> `Failures failures in

    (Option.cons Mconfig.(config.merlin.config_path) [], failures)

  | Checkout _ -> failwith "invalid arguments"

let default_state = lazy (new_state (None, None))

let document_states
  : (string option * string list option, state) Hashtbl.t
  = Hashtbl.create 7

let dispatch (type a) (context : Context.t) (cmd : a command) =
  let open Context in
  (* Document selection *)
  let state = match context.document with
    | None -> Lazy.force default_state
    | Some document ->
      let document = normalize_document document in
      try Hashtbl.find document_states document
      with Not_found ->
        let state = new_state document in
        Hashtbl.add document_states document state;
        state
  in
  let config = configure state.buffer in
  (* Printer verbosity *)
  let config = match context.printer_verbosity with
    | None -> config
    | Some verbosity ->
      Mconfig.({config with query = {config.query with verbosity}})
  in
  let config = match context.printer_width with
    | None -> config
    | Some printer_width ->
      Mconfig.({config with query = {config.query with printer_width}})
  in
  (* Printer width *)
  Format.default_width := Option.value ~default:0 context.printer_width;
  (* Actual dispatch *)
  match cmd with
  | Query q ->
    let pipeline = make_pipeline config state.buffer in
    Mpipeline.with_pipeline pipeline @@ fun () ->
    Query_commands.dispatch pipeline q
  | Sync (Checkout context) when state == Lazy.force default_state ->
    let buffer = checkout_buffer context in
    state.buffer <- buffer
  | Sync s -> dispatch_sync config state.buffer s
