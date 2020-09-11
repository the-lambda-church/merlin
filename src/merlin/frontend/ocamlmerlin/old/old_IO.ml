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

let latest_version : Old_protocol.protocol_version = `V3
let current_version = ref `V2

let default_context =
  {Old_protocol.Context.
    document = None; printer_width = None; printer_verbosity = None}

let invalid_arguments () = failwith "invalid arguments"

open Query_protocol
open Old_protocol

let pos_of_json = function
  | `String "start" -> `Start
  | `String "end" -> `End
  | `Int offset -> `Offset offset
  | `Assoc props ->
    begin try match List.assoc "line" props, List.assoc "col" props with
      | `Int line, `Int col -> `Logical (line,col)
      | _ -> failwith "Incorrect position"
      with Not_found -> failwith "Incorrect position"
    end
  | _ -> failwith "Incorrect position"

let mandatory_position = function
  | [`String "at"; jpos] -> pos_of_json jpos
  | _ -> invalid_arguments ()

let optional_string = function
  | [`String name] -> Some name
  | [] -> None
  | _ -> invalid_arguments ()

let string_list l =
  List.map ~f:(function `String s -> s | _ -> invalid_arguments ()) l

let source_or_build = function
  | "source" -> `Source
  | "build"  -> `Build
  | _ -> invalid_arguments ()

let ml_or_mli = function
  | "ml" -> `ML
  | "mli"  -> `MLI
  | _ -> invalid_arguments ()

let auto_ml_or_mli = function
  | "auto" -> `Auto
  | x -> ml_or_mli x

let add_or_remove = function
  | "add"    -> `Add
  | "remove" -> `Rem
  | _ -> invalid_arguments ()

let with_failures failures assoc = match failures with
  | `Ok -> assoc
  | `Failures failures ->
    let flags, extensions =
      List.fold_left failures ~init:([],[]) ~f:(
        fun (flgs, exts) (str,exn) ->
          match exn with
          | Arg.Bad _ -> str :: flgs, exts
          | Extension.Unknown -> flgs, str :: exts
          | _ -> assert false
      )
    in
    let flags =
      match flags with
      | [] -> []
      | failures ->
        let str = String.concat ~sep:", " failures in
        [ `String ("Unknown flags " ^ str) ]
    in
    let extensions =
      match extensions with
      | [] -> []
      | failures ->
        let str = String.concat ~sep:", " failures in
        [ `String ("Unknown extensions " ^ str) ]
    in
    ("failures", `List (flags @ extensions)) :: assoc

let document_of_json =
  let make kind path dot_merlins =
    {Context.dot_merlins;
     kind = auto_ml_or_mli kind;
     path = optional_string path;
    }
  in function
    | (`String "dot_merlin" :: `List dot_merlins :: `String kind :: opt_name) ->
      make kind opt_name (Some (string_list dot_merlins))
    | (`String kind :: opt_name) ->
      make kind opt_name None
    | _ -> invalid_arguments ()

let request_of_json context =
  let request x = Request (context, x) in function
    | (`String "type" :: `String "expression" :: `String expr :: opt_pos) ->
      request (Query (Type_expr (expr, mandatory_position opt_pos)))
    | [`String "type"; `String "enclosing";
       `Assoc [ "expr", `String expr ; "offset", `Int offset] ; jpos] ->
      request (Query (Type_enclosing (Some (expr, offset), pos_of_json jpos, None)))
    | [`String "type"; `String "enclosing"; `String "at"; jpos] ->
      request (Query (Type_enclosing (None, pos_of_json jpos, None)))
    | [ `String "case"; `String "analysis"; `String "from"; x; `String "to"; y ] ->
      request (Query (Case_analysis (pos_of_json x, pos_of_json y)))
    | [`String "enclosing"; jpos] ->
      request (Query (Enclosing (pos_of_json jpos)))
    | [`String "complete"; `String "prefix"; `String prefix; `String "at"; jpos] ->
      request (Query (Complete_prefix (prefix, pos_of_json jpos, [], false, true)))
    | [`String "complete"; `String "prefix"; `String prefix; `String "at"; jpos;
       `String "with"; `String "doc"] ->
      request (Query (Complete_prefix (prefix, pos_of_json jpos, [], true, true)))
    | [`String "expand"; `String "prefix"; `String prefix; `String "at"; jpos] ->
      request (Query (Expand_prefix (prefix, pos_of_json jpos, [], true)))
    | [`String "search"; `String "polarity"; `String query; `String "at"; jpos] ->
      request (Query (Polarity_search (query, pos_of_json jpos)))
    | (`String "document" :: (`String "" | `Null) :: pos) ->
      request (Query (Document (None, mandatory_position pos)))
    | (`String "document" :: `String path :: pos) ->
      request (Query (Document (Some path, mandatory_position pos)))
    | (`String "locate" :: (`String "" | `Null) :: `String choice :: pos) ->
      request (Query (Locate (None, ml_or_mli choice, mandatory_position pos)))
    | (`String "locate" :: `String path :: `String choice :: pos) ->
      request (Query (Locate (Some path, ml_or_mli choice, mandatory_position pos)))
    | (`String "jump" :: `String target :: pos) ->
      request (Query (Jump (target, mandatory_position pos)))
    | [`String "outline"] ->
      request (Query Outline)
    | [`String "shape"; pos] ->
      request (Query (Shape (pos_of_json pos)))
    | [`String "occurrences"; `String "ident"; `String "at"; jpos] ->
      request (Query (Occurrences (`Ident_at (pos_of_json jpos))))
    | (`String ("reset"|"checkout") :: document) ->
      request (Sync (Checkout (document_of_json document)))
    | [`String "refresh"] ->
      request (Sync Refresh)
    | [`String "errors"] ->
      request (Query (Errors { lexing = true; parsing = true; typing = true }))
    | (`String "dump" :: args) ->
      request (Query (Dump args))
    | [`String "which"; `String "path"; `String name] ->
      request (Query (Path_of_source [name]))
    | [`String "which"; `String "path"; `List names] ->
      request (Query (Path_of_source (string_list names)))
    | [`String "which"; `String "with_ext"; `String ext] ->
      request (Query (List_modules [ext]))
    | [`String "which"; `String "with_ext"; `List exts] ->
      request (Query (List_modules (string_list exts)))
    | [`String "flags" ; `String "set" ; `List flags ] ->
      request (Sync (Flags_set (string_list flags)))
    | [`String "flags" ; `String "get" ] ->
      request (Sync (Flags_get))
    | [`String "find"; `String "use"; `List packages]
    | (`String "find" :: `String "use" :: packages) ->
      request (Sync (Findlib_use (string_list packages)))
    | [`String "find"; `String "list"] ->
      request (Query Findlib_list)
    | [`String "extension"; `String "enable"; `List extensions] ->
      request (Sync (Extension_set (`Enabled,string_list extensions)))
    | [`String "extension"; `String "disable"; `List extensions] ->
      request (Sync (Extension_set (`Disabled,string_list extensions)))
    | [`String "extension"; `String "list"] ->
      request (Query (Extension_list `All))
    | [`String "extension"; `String "list"; `String "enabled"] ->
      request (Query (Extension_list `Enabled))
    | [`String "extension"; `String "list"; `String "disabled"] ->
      request (Query (Extension_list `Disabled))
    | [`String "path"; `String "list";
       `String ("source"|"build" as var)] ->
      request (Query (Path_list (source_or_build var)))
    | [`String "path"; `String "reset"] ->
      request (Sync Path_reset)
    | (`String "path" :: `String ("add"|"remove" as action) ::
       `String ("source"|"build" as var) :: ((`List pathes :: []) | pathes)) ->
      request (Sync (Path (source_or_build var, add_or_remove action, string_list pathes)))
    | [`String "tell"; pos_start; pos_end; `String content] ->
      request (Sync (Tell (pos_of_json pos_start, pos_of_json pos_end, content)))
    | [`String "project"; `String "get"] ->
      request (Sync Project_get)
    | [`String "version"] ->
      request (Query Version)
    | [`String "protocol"; `String "version"] ->
      request (Sync (Protocol_version None))
    | [`String "protocol"; `String "version"; `Int n] ->
      request (Sync (Protocol_version (Some n)))
    | _ -> invalid_arguments ()

let json_of_protocol_version : Old_protocol.protocol_version -> _ = function
  | `V2 -> `Int 2
  | `V3 -> `Int 3

let json_of_sync_command (type a) (command : a sync_command) (response : a) : json =
  match command, response with
  | Tell _, () -> `Bool true
  | Checkout _, () -> `Bool true
  | Refresh, () -> `Bool true
  | Flags_get, flags ->
    `List (List.map ~f:Json.string flags)
  | Flags_set _, failures ->
    `Assoc (with_failures failures ["result", `Bool true])
  | Findlib_use _, failures ->
    `Assoc (with_failures failures ["result", `Bool true])
  | Extension_set _, failures ->
    `Assoc (with_failures failures ["result", `Bool true])
  | Path _, () -> `Bool true
  | Path_reset, () -> `Bool true
  | Protocol_version _, (`Selected v, `Latest vm, version) ->
    `Assoc ["selected", json_of_protocol_version v;
            "latest", json_of_protocol_version vm;
            "merlin",  `String version
           ]
  | Project_get, (strs, fails) ->
    let failures = match fails with
      | `Failures ((_::_) as fails) ->
        ["failures", `List (List.map ~f:Json.string fails)]
      | _ -> []
    in
    `Assoc (("result", `List (List.map ~f:Json.string strs))::failures)
  | Idle_job, b -> `Bool b

let classify_response = function
  | Failure s | Exception (Failure s) -> ("failure", `String s)
  | Error error -> ("error", error)
  | Exception exn ->
    begin match Location.error_of_exn exn with
      | Some (`Ok error) -> ("error", Query_json.json_of_error error)
      | None | Some `Already_displayed ->
        ("exception", `String (Printexc.to_string exn))
    end
  | Return (Query cmd, response) ->
    ("return", Query_json.json_of_response cmd response)
  | Return (Sync cmd, response) ->
    ("return", json_of_sync_command cmd response)

let json_of_response_v2 response =
  let class_, value = classify_response response in
  `List [`String class_; value]

let json_of_response_v3 ~notifications response =
  let class_, value = classify_response response in
  `Assoc [
    "class", `String class_;
    "value", value;
    "notifications",
    `List (List.map ~f:(fun { Logger.section; msg } ->
        `Assoc ["section", `String section; "message", `String msg])
        notifications);
  ]

let json_of_response notifications response =
  match !current_version with
  | `V2 -> json_of_response_v2 response
  | `V3 -> json_of_response_v3 ~notifications response

let request_of_json = function
  | `Assoc _ as json ->
    let open Json.Util in
    let document =
      let value = member "document" json in
      let value =
        if value = `Null then
          member "context" json
        else value
      in
      if value = `Null then
        None
      else Some (to_list value |> document_of_json)
    in
    let printer_width = member "printer_width" json |> to_int_option in
    let printer_verbosity = member "printer_verbosity" json |> to_int_option in
    let context = {Context. document; printer_verbosity; printer_width} in
    let query = member "query" json |> to_list in
    request_of_json context query
  | `List jsons -> request_of_json default_context jsons
  | _ -> invalid_arguments ()

let make_json ?(on_read=ignore) ~input ~output () =
  let rec read buf len =
    on_read input;
    try Unix.read input buf 0 len
    with Unix.Unix_error (Unix.EINTR,_,_) ->
      read buf len
  in
  let lexbuf  = Lexing.from_function read in
  let input   = Json.stream_from_lexbuf (Json.init_lexer ()) lexbuf in
  let input () = try Some (Stream.next input) with Stream.Failure -> None in
  let output  = Unix.out_channel_of_descr output in
  let output' = Json.to_channel output in
  let output json =
    output' json;
    output_char output '\n';
    flush output
  in
  input, output

let make_sexp ?on_read ~input ~output () =
  (* Fix for emacs: emacs start-process doesn't distinguish between stdout and
     stderr.  So we redirect stderr to /dev/null with sexp frontend. *)
  begin match
      begin
        try Some (Unix.openfile "/dev/null" [Unix.O_WRONLY] 0o600)
        with
        | Unix.Unix_error _  ->
          if Sys.os_type = "Win32" then
            try Some (Unix.openfile "NUL" [Unix.O_WRONLY] 0o600)
            with Unix.Unix_error _ -> None
          else None
      end
      with
      | None -> ()
      | Some fd ->
        Unix.dup2 fd Unix.stderr;
        Unix.close fd
  end;
  let input' = Sexp.of_file_descr ?on_read input in
  let input' () = Option.map ~f:Sexp.to_json (input' ()) in
  let buf = Buffer.create 8192 in
  let output json =
    let sexp = Sexp.of_json json in
    Sexp.to_buf sexp buf;
    Buffer.add_char buf '\n';
    let contents = Buffer.to_bytes buf in
    let rec write_contents n l =
      if l > 0 then
        let l' = Unix.write output contents n l in
        if l' > 0 then
          write_contents (n + l') (l - l')
    in
    write_contents 0 (Bytes.length contents);
    if Buffer.length buf > 100_000
    then Buffer.reset buf
    else Buffer.clear buf
  in
  input', output
