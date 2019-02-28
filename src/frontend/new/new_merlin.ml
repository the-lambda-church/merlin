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

(** {1 Prepare command-line arguments} *)

let {Logger. log} = Logger.for_section "New_merlin"

let usage () =
  prerr_endline
    "Usage: ocamlmerlin command [options] -- [compiler flags]\n\
     Help commands are:\n\
    \  -version        Print version and exit\n\
    \  -vnum           Print version number and exit\n\
    \  -warn-help      Show description of warning numbers\n\
    \  -flags-help     Show description of accepted compiler flags\n\
    \  -commands-help  Describe all accepted commands\n"

let commands_help () =
  print_endline "Query commands are:";
  List.iter (fun (New_commands.Command (name, doc, args, _, _)) ->
      print_newline ();
      let args = List.map (fun (kind, (key0,desc,_)) ->
        let key1, desc =
          let len = String.length desc in
          match String.index desc ' ' with
          | 0 -> key0, String.sub desc 1 (len - 1)
          | idx -> key0 ^ " " ^ String.sub desc 0 idx,
                   String.sub desc (idx + 1) (len - idx - 1)
          | exception Not_found -> key0, desc
        in
        let key = match kind with
          | `Mandatory -> key1
          | `Optional  -> "[ " ^ key1 ^ " ]"
          | `Many      -> "[ " ^ key1 ^ " " ^ key0 ^ " ... ]"
        in
        key, (key1, desc)
      ) args in
      let args, descs = List.split args in
      print_endline ("### `" ^ String.concat " " (name :: args) ^ "`");
      print_newline ();
      let print_desc (k,d) = print_endline (Printf.sprintf "%24s  %s" k d) in
      List.iter print_desc descs;
      print_newline ();
      print_endline doc
    ) New_commands.all_commands

let run = function
  | [] ->
    usage ();
    1
  | "-version" :: _ ->
    Printf.printf "The Merlin toolkit version %s, for Ocaml %s\n"
      My_config.version Sys.ocaml_version;
    0
  | "-vnum" :: _ ->
    Printf.printf "%s\n" My_config.version;
    0
  | "-warn-help" :: _ ->
    Warnings.help_warnings ();
    0
  | "-flags-help" :: _ ->
    Mconfig.document_arguments stdout;
    0
  | "-commands-help" :: _ ->
    commands_help ();
    0
  | query :: raw_args ->
    match New_commands.find_command query New_commands.all_commands with
    | exception Not_found ->
      prerr_endline ("Unknown command " ^ query ^ ".\n");
      usage ();
      1
    | New_commands.Command (_name, _doc, spec, command_args, command_action) ->
      (* Setup notifications *)
      let notifications = ref [] in
      Logger.with_notifications notifications @@ fun () ->
      (* Parse commandline *)
      match begin
        let start_time = Misc.time_spent () in
        let config, command_args =
          let fails = ref [] in
          let config, command_args =
            Mconfig.parse_arguments
              ~wd:(Sys.getcwd ()) ~warning:(fun w -> fails := w :: !fails)
              (List.map snd spec) raw_args Mconfig.initial command_args
          in
          let config =
            Mconfig.({config with merlin = {config.merlin with failures = !fails}})
          in
          config, command_args
        in
        (* Start processing query *)
        Logger.with_log_file Mconfig.(config.merlin.log_file)
          ~sections:Mconfig.(config.merlin.log_sections) @@ fun () ->
        File_id.with_cache @@ fun () ->
        let source = Msource.make (Misc.string_of_file stdin) in
        let pipeline = Mpipeline.make config source in
        let json =
          let class_, message =
            Printexc.record_backtrace true;
            match command_action pipeline command_args with
            | result ->
              ("return", result)
            | exception (Failure str) ->
              ("failure", `String str)
            | exception exn ->
              let trace = Printexc.get_backtrace () in
              log ~title:"run" "Command error backtrace: %s" trace;
              match Location.error_of_exn exn with
              | None | Some `Already_displayed ->
                ("exception", `String (Printexc.to_string exn ^ "\n" ^ trace))
              | Some (`Ok err) ->
                Location.report_error Format.str_formatter err;
                ("error", `String (Format.flush_str_formatter ()))
          in
          let total_time = Misc.time_spent () -. start_time in
          let timing = Mpipeline.timing_information pipeline in
          let pipeline_time =
            List.fold_left (fun acc (_, k) -> k +. acc) 0.0 timing in
          let timing = ("total", total_time) ::
                       ("query", (total_time -. pipeline_time)) :: timing in
          let notify { Logger.section; msg } =
            `String (Printf.sprintf "%s: %s" section msg)
          in
          let format_timing (k,v) = (k, `Int (int_of_float (0.5 +. v))) in
          `Assoc [
            "class", `String class_; "value", message;
            "notifications", `List (List.rev_map notify !notifications);
            "timing", `Assoc (List.map format_timing timing)
          ]
        in
        log ~title:"run(result)" "%a" Logger.json (fun () -> json);
        begin match Mconfig.(config.merlin.protocol) with
          | `Sexp -> Sexp.tell_sexp print_string (Sexp.of_json json)
          | `Json -> Std.Json.to_channel stdout json
        end;
        print_newline ()
      end with
      | () -> 0
      | exception exn ->
        prerr_endline ("Exception: " ^ Printexc.to_string exn);
        1

let run env wd args =
  Os_ipc.merlin_set_environ env;
  Unix.putenv "__MERLIN_MASTER_PID" (string_of_int (Unix.getpid ()));
  let wd_msg = match wd with
    | None -> "No working directory specified"
    | Some wd ->
      try Sys.chdir wd; Printf.sprintf "changed directory to %S" wd
      with _ -> Printf.sprintf "cannot change working directory to %S" wd
  in
  let log_file, sections =
    match Std.String.split_on_char_ ',' (Sys.getenv "MERLIN_LOG") with
    | (value :: sections) -> (Some value, sections)
    | [] -> (None, [])
    | exception Not_found -> (None, [])
  in
  Logger.with_log_file log_file ~sections @@ fun () ->
  log ~title:"run" "%s" wd_msg;
  run args
