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

let {Logger. log} = Logger.for_section "Mconfig_dot"

type directive = Dot_protocol.directive

type config = {
  build_path   : string list;
  source_path  : string list;
  cmi_path     : string list;
  cmt_path     : string list;
  flags        : string list with_workdir list;
  extensions   : string list;
  suffixes     : (string * string) list;
  stdlib       : string option;
  reader       : string list;
  exclude_query_dir : bool;
}

let empty_config = {
  build_path   = [];
  source_path  = [];
  cmi_path     = [];
  cmt_path     = [];
  extensions   = [];
  suffixes     = [];
  flags        = [];
  stdlib       = None;
  reader       = [];
  exclude_query_dir = false;
}

let white_regexp = Str.regexp "[ \t]+"

(* Parses suffixes pairs that were supplied as whitespace separated pairs
   designating implementation/interface suffixes. These would be supplied in
   the .merlin file as:

   SUFFIX .sfx .sfxi   *)
let parse_suffix str =
  let trimmed = String.trim str in
  let split_on_white = Str.split white_regexp trimmed in
  if List.length split_on_white != 2 then []
  else
    let (first, second) = (List.nth split_on_white 0, List.nth split_on_white 1) in
    if String.get first 0 != '.' || String.get second 0 != '.' then []
    else [(first, second)]

let prepend_config ~dir:cwd (directives : directive list) config =
  List.fold_left ~init:(config, []) ~f:(fun (config, errors) ->
    function
    | `B path -> {config with build_path = path :: config.build_path}, errors
    | `S path -> {config with source_path = path :: config.source_path}, errors
    | `CMI path -> {config with cmi_path = path :: config.cmi_path}, errors
    | `CMT path -> {config with cmt_path = path :: config.cmt_path}, errors
    | `EXT exts ->
      {config with extensions = exts @ config.extensions}, errors
    | `SUFFIX suffix ->
      {config with suffixes = (parse_suffix suffix) @ config.suffixes}, errors
    | `FLG flags ->
      let flags = {workdir = cwd; workval = flags} in
      {config with flags = flags :: config.flags}, errors
    | `STDLIB path ->
      {config with stdlib = Some path}, errors
    | `READER reader ->
      {config with reader}, errors
    | `EXCLUDE_QUERY_DIR ->
      {config with exclude_query_dir = true}, errors
    | `ERROR_MSG str ->
      config, str :: errors
  ) directives

module Configurator = struct
  type t =
    | Dot_merlin
    | Dune

  let of_string_opt = function
    | ".merlin" ->
      Some Dot_merlin
    | "dune-project" | "dune-workspace" ->
      Some Dune
    | _ -> None

  let to_string = function
    | Dot_merlin -> "dot-merlin-reader"
    | Dune -> "dune"

  module Process = struct
    type nonrec t = {
      pid : int;
      kind : t;
      initial_cwd : string;
      stdin: out_channel;
      stdout: in_channel;
      stderr: in_channel;
    }

    let start ~dir cfg =
      let prog, args =
        match cfg with
        | Dot_merlin ->
          let prog = "dot-merlin-reader" in
          prog, [| prog |]
        | Dune ->
          let prog = "dune" in
          prog, [| prog; "ocaml-merlin"; "--no-print-directory" |]
      in
      let cwd = Sys.getcwd () in
      let stdin_r, stdin_w = Unix.pipe () in
      let stdout_r, stdout_w = Unix.pipe () in
      let stderr_r, stderr_w = Unix.pipe () in
      Unix.chdir dir;
      Unix.set_close_on_exec stdin_w;
      (* Set the windows equivalent of close on exec for and stdin stderr

         Most processes spawned by merlin are supposed to inherit stderr to
         output their debug information. This is fine because these processes
         are short-lived.
         However the dune helper we are about to spawn is long-lived, which can
         cause issues with inherited descriptors because it will outlive
         merlin's client process.
         This is not an issue under Unix because file descriptors are replaced
         (stdin/out/err are new), but under Windows, handle can accumulate.
         This makes emacs block, synchronously waiting for the inherited (but
         unused) stdout/stderr to be closed.

         Os_ipc.merlin_dont_inherit_stdio is a no-op under Unix.
      *)
      Os_ipc.merlin_dont_inherit_stdio true;
      log ~title:"get_config" "Starting %s configuration provider from dir %s."
        (to_string cfg)
        dir;
      let pid = Unix.create_process prog args stdin_r stdout_w stderr_w in
      Os_ipc.merlin_dont_inherit_stdio false;
      Unix.chdir cwd;
      Unix.close stdin_r;
      Unix.close stdout_w;
      Unix.close stderr_w;
      let stdin = Unix.out_channel_of_descr stdin_w in
      let stdout = Unix.in_channel_of_descr stdout_r in
      let stderr = Unix.in_channel_of_descr stderr_r in
      let initial_cwd =  Misc.canonicalize_filename dir in
      { pid; kind = cfg; initial_cwd; stdin; stdout; stderr }
  end

  let running_processes : (string * t, Process.t) Hashtbl.t = Hashtbl.create 0

  let get_process ~dir configurator =
    try
      let p = Hashtbl.find running_processes (dir, configurator) in
      let i, _ = Unix.waitpid [ WNOHANG ] p.pid in
      if i = 0 then
        p
      else
        let p = Process.start ~dir configurator in
        Hashtbl.replace running_processes (dir, configurator) p;
        p
    with Not_found ->
      let p = Process.start ~dir configurator in
      Hashtbl.add running_processes (dir, configurator) p;
      p
end

let postprocess_config config =
  let clean list = List.rev (List.filter_dup list) in
  {
    build_path   = clean config.build_path;
    source_path  = clean config.source_path;
    cmi_path     = clean config.cmi_path;
    cmt_path     = clean config.cmt_path;
    extensions   = clean config.extensions;
    suffixes     = clean config.suffixes;
    flags        = clean config.flags;
    stdlib      = config.stdlib;
    reader      = config.reader;
    exclude_query_dir = config.exclude_query_dir;
  }

type context = {
  workdir: string;
  configurator: Configurator.t;
  process_dir: string;
}

exception Process_exited
exception End_of_input

let get_config { workdir; process_dir; configurator } path_abs =
  let log_query path =
    log
      ~title:"get_config"
      "Querying %s (inital cwd: %s) for file: %s.\nWorkdir: %s"
      (Configurator.to_string configurator)
      process_dir
      path
      workdir
  in
  let query path (p : Configurator.Process.t) =
    log_query path;
    Dot_protocol.Commands.send_file
      ~out_channel:p.stdin
      path;
    flush p.stdin;
    Dot_protocol.read ~in_channel:p.stdout
  in
  try
    let p = Configurator.get_process ~dir:process_dir configurator in
    if fst (Unix.waitpid [ WNOHANG ] p.pid) <> 0 then
      raise Process_exited;

    (* Both [p.initial_cwd] and [path_abs] have gone through
    [canonicalize_filename] *)
    let path_rel =
      String.chop_prefix ~prefix:p.initial_cwd path_abs
      |> Option.map ~f:(fun path ->
        (* We need to remove the leading path separator after chopping.
        There is one case where no separator is left: when [initial_cwd]
        was the root of the filesystem *)
        if String.length path > 0 && path.[0] = Filename.dir_sep.[0] then
           String.drop 1 path
        else path)
    in

    let path =
      match p.kind, path_rel with
      | Dune, Some path_rel -> path_rel
      | _, _ -> path_abs
    in

    (* Starting with Dune 2.8.3 relative paths are prefered. However to maintain
    compatibility with 2.8 <= Dune <= 2.8.2  we always retry with an absolute
    path if using a relative one failed *)
    let answer =
      match query path p with
      | Ok ([`ERROR_MSG _]) when p.kind = Dune ->
        query path_abs p
      | answer -> answer
    in

    match answer with
    | Ok directives ->
      let cfg, failures =
        prepend_config ~dir:workdir directives empty_config
      in
      postprocess_config cfg, failures
    | Error (Dot_protocol.Unexpected_output msg) -> empty_config, [ msg ]
    | Error (Dot_protocol.Csexp_parse_error _) -> raise End_of_input
  with
    | Process_exited ->
      (* This can happen
      - If `dot-merlin-reader` is not installed and the project use `.merlin`
        files
      - There was a bug in the external reader causing a crash *)
      let error = Printf.sprintf
        "A problem occured with merlin external configuration reader. %s If \
         the problem persists, please file an issue on Merlin's tracker."
        (match configurator with
        | Dot_merlin -> "Check that `dot-merlin-reader` is installed."
        | Dune -> "Check that `dune` is installed and up-to-date.")
      in
      empty_config, [ error ]
    | End_of_input ->
      (* This can happen
        - if a project using old-dune has not been built and Merlin wrongly tries to
          start `new-dune ocaml-merlin` in the absence of `.merlin` files
        - the process stopped in the middle of its answer (which is very unlikely) *)
      let error = Printf.sprintf
        "Merlin could not load its configuration from the external reader. %s"
        (match configurator with
        | Dot_merlin -> "If the problem persists, please file an issue on \
          Merlin's tracker."
        | Dune -> "Building your project with `dune` might solve this issue.")
      in
      empty_config, [ error ]

let find_project_context start_dir =
  (* The workdir is the first directory we find which contains a [dune] file.
    We need to keep track of this folder because [dune ocaml-merlin] might be
    started from a folder that is a parent of the [workdir]. Thus we cannot
    always use that starting folder as the workdir.  *)
  let map_workdir dir = function
    | Some dir -> Some dir
    | None -> 
      let fnames = List.map ~f:(Filename.concat dir) ["dune"; "dune-file"] in
      if List.exists ~f:(fun fname -> 
        Sys.file_exists fname && not (Sys.is_directory fname)) fnames
      then Some dir else None
  in

  let rec loop workdir dir =
    try
      Some (
        List.find_map [
            ".merlin"; "dune-project"; "dune-workspace"
          ]
          ~f:(fun f ->
            let fname = Filename.concat dir f in
            if Sys.file_exists fname && not (Sys.is_directory fname)
            then
              (* When starting [dot-merlin-reader] from [dir]
                the workdir is always [dir] *)
              let workdir = if f = ".merlin" then None else workdir in
              let workdir = Option.value ~default:dir workdir in
              Some ({
                workdir;
                process_dir = dir;
                configurator = Option.get (Configurator.of_string_opt f)
              }, fname)
            else None
          )
    )
    with Not_found ->
      let parent = Filename.dirname dir in
      if parent <> dir
      then
        (* Was this directory the workdir ? *)
        let workdir = map_workdir dir workdir in
        loop workdir parent
      else None
  in
  loop None start_dir
