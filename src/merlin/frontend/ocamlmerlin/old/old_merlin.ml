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

let version_spec =
  Printf.sprintf "The Merlin toolkit version %s, for Ocaml %s"
    My_config.version Sys.ocaml_version

let ocamlmerlin_args = [
  (
    "-ignore-sigint",
    " Ignore SIGINT, useful when invoked from editor",
    Marg.unit (fun acc ->
        (try ignore (Sys.(signal sigint Signal_ignore))
         with Invalid_argument _ -> ());
        acc
      )
  );
  (
    "-version",
    " Print version and exit",
    Marg.unit (fun _ ->
        print_endline version_spec;
        exit 0
      )
  );
  (
    "-vnum",
    " Print version number and exit",
    Marg.unit (fun _ ->
        Printf.printf "%s\n" My_config.version;
        exit 0
      )
  );
  (
    "-warn-help",
    " Show description of warning numbers",
    Marg.unit (fun _ ->
        Warnings.help_warnings ();
        exit 0
      )
  );
  (
    "-protocol",
    " Select frontend protocol ('json' or 'sexp')",
    Marg.param "protocol" (fun arg _ ->
        match arg with
        | "json" -> `Json
        | "sexp" -> `Sexp
        | _ ->
          prerr_endline "Valid protocols are 'json' and 'sexp'";
          exit 1
      )
  );
]

let signal sg behavior =
  try ignore (Sys.signal sg behavior)
  with Invalid_argument _ (*Sys.signal: unavailable signal*) -> ()

let rec merlin_loop input output =
  let notifications = ref [] in
  Logger.with_notifications notifications @@ fun () ->
  match
    match input () with
    | Some (Old_protocol.Request (context, request)) ->
      let answer = Old_command.dispatch context request in
      output ~notifications:(List.rev !notifications)
        (Old_protocol.Return (request, answer));
      true
    | None -> false
  with
  | exception exn ->
    let trace =
      { Logger.section = "backtrace"; msg = Printexc.get_backtrace () }
    in
    output ~notifications:(trace :: List.rev !notifications)
      (Old_protocol.Exception exn);
    merlin_loop input output
  | true -> merlin_loop input output
  | false -> ()

let setup_system () =
  (* Setup signals, unix is a disaster *)
  signal Sys.sigusr1 Sys.Signal_ignore;
  signal Sys.sigpipe Sys.Signal_ignore;
  signal Sys.sighup  Sys.Signal_ignore

let setup_merlin args =
  let config, protocol =
    Mconfig.parse_arguments
      ~wd:(Sys.getcwd ()) ~warning:prerr_endline ocamlmerlin_args args
      Mconfig.initial `Json
  in
  Old_command.default_config := config;
  let protocol = match protocol with
    | `Json -> Old_IO.make_json
    | `Sexp -> Old_IO.make_sexp
  in
  let input, output = protocol ~input:Unix.stdin ~output:Unix.stdout () in
  let input () = match input () with
    | None -> None
    | Some json ->
      Logger.log ~section:"frontend" ~title:"input" "%a"
        Logger.json (fun () -> json);
      Some (Old_IO.request_of_json json)
  in
  let output ~notifications x =
    let json = Old_IO.json_of_response notifications x in
    Logger.log ~section:"frontend" ~title:"output" "%a"
      Logger.json (fun () -> json);
    output json
  in
  (input, output)

let run args =
  setup_system ();
  let input, output = setup_merlin args in
  merlin_loop input output;
  exit 0
