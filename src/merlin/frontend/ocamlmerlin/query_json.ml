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
open Query_protocol

let dump (type a) : a t -> json =
  let mk command args =
    `Assoc (
      ("command", `String command) ::
      args
    ) in
  let mk_position = function
    | `Start -> `String "start"
    | `End -> `String "end"
    | `Offset n ->
      `Assoc ["offset", `Int n]
    | `Logical (line,col) ->
      `Assoc ["line", `Int line; "column", `Int col]
  in
  let kinds_to_json kind =
    `List (List.map ~f:(function
        | `Constructor  -> `String "constructor"
        | `Labels       -> `String "label"
        | `Modules      -> `String "module"
        | `Modules_type -> `String "module-type"
        | `Types        -> `String "type"
        | `Values       -> `String "value"
        | `Variants     -> `String "variant"
      ) kind)
  in
  function
  | Type_expr (expr, pos) ->
    mk "type-expression" [
      "expression", `String expr;
      "position", mk_position pos;
    ]

  | Type_enclosing (opt_cursor, pos, index) ->
    mk "type-enclosing" [
      "cursor", (match opt_cursor with
          | None -> `Null
          | Some (text, offset) -> `Assoc [
              "text", `String text;
              "offset", `Int offset;
            ]
        );
      "index", (match index with
          | None -> `String "all"
          | Some n -> `Int n
        );
      "position", mk_position pos;
    ]

  | Locate_type pos ->
    mk "locate-type" [
      "position", mk_position pos
    ]

  | Enclosing pos ->
    mk "enclosing" [
      "position", mk_position pos;
    ]

  | Complete_prefix (prefix, pos, kind, doc, typ) ->
    mk "complete-prefix" [
      "prefix", `String prefix;
      "position", mk_position pos;
      "with-doc", `Bool doc;
      "with-types", `Bool typ;
      "kind", kinds_to_json kind;
    ]

  | Expand_prefix (prefix, pos, kind, typ) ->
    mk "expand-prefix" [
      "prefix", `String prefix;
      "position", mk_position pos;
      "with-types", `Bool typ;
      "kind", kinds_to_json kind;
    ]
  | Document (identifier, pos) ->
    mk "document" [
      "identifier", (match identifier with
          | None -> `Null
          | Some ident -> `String ident
        );
      "position", mk_position pos;
    ]
  | Locate (prefix, look_for, pos) ->
    mk "locate" [
      "prefix", (match prefix with
          | None -> `Null
          | Some prefix -> `String prefix
        );
      "look-for", (match look_for with
          | `ML -> `String "implementation"
          | `MLI -> `String "interface"
        );
      "position", mk_position pos;
    ]
  | Jump (target, pos) ->
    mk "jump" [
      "target", `String target;
      "position", mk_position pos;
    ]
  | Phrase (target, pos) ->
    mk "phrase" [
      "target", `String (match target with `Next -> "next" | `Prev -> "prev");
      "position", mk_position pos;
    ]
  | Case_analysis (pos_start,pos_end) ->
    mk "case-analysis" [
      "start", mk_position pos_start;
      "end", mk_position pos_end;
    ]
  | Outline -> mk "outline" []
  | Errors { lexing; parsing; typing } ->
    let args =
      if lexing && parsing && typing
      then []
      else [
        "lexing", `Bool lexing;
        "parsing", `Bool parsing;
        "typing", `Bool typing;
      ]
    in
    mk "errors" args
  | Shape pos ->
    mk "shape" [
      "position", mk_position pos;
    ]
  | Dump args ->
    mk "dump" [
      "args", `List args
    ]
  | Path_of_source paths ->
    mk "path-of-source" [
      "paths", `List (List.map ~f:Json.string paths)
    ]
  | List_modules exts ->
    mk "list-modules" [
      "extensions", `List (List.map ~f:Json.string exts)
    ]
  | Findlib_list -> mk "findlib-list" []
  | Extension_list status ->
    mk "extension-list" [
      "filter", (match status with
          | `All -> `String "all"
          | `Enabled -> `String "enabled"
          | `Disabled -> `String "disabled"
        );
    ]
  | Path_list var ->
    mk "path-list" [
      "variable", (match var with
          | `Build -> `String "build"
          | `Source -> `String "source"
        );
    ]
  | Polarity_search (query, pos) ->
    mk "polarity-search" [
      "query", `String query;
      "position", mk_position pos;
    ]
  | Occurrences (`Ident_at pos) ->
    mk "occurrences" [
      "kind", `String "identifiers";
      "position", mk_position pos;
    ]
  | Refactor_open (action, pos) ->
    mk "refactor-open" [
      "action", `String (match action with `Qualify -> "qualify"
                                         | `Unqualify -> "unqualify");
      "position", mk_position pos;
    ]
  | Version -> mk "version" []

let string_of_completion_kind = function
  | `Value       -> "Value"
  | `Variant     -> "Variant"
  | `Constructor -> "Constructor"
  | `Label       -> "Label"
  | `Module      -> "Module"
  | `Modtype     -> "Signature"
  | `Type        -> "Type"
  | `Method      -> "Method"
  | `MethodCall  -> "#"
  | `Exn         -> "Exn"
  | `Class       -> "Class"

let with_location ?(skip_none=false) loc assoc =
  if skip_none && loc = Location.none then
    `Assoc assoc
  else
    `Assoc (("start", Lexing.json_of_position loc.Location.loc_start) ::
            ("end",   Lexing.json_of_position loc.Location.loc_end) ::
            assoc)

let json_of_type_loc (loc,desc,tail) =
  with_location loc [
    "type", (match desc with
        | `String _ as str -> str
        | `Index n -> `Int n);
    "tail", `String (match tail with
        | `No -> "no"
        | `Tail_position -> "position"
        | `Tail_call -> "call")
  ]

let json_of_error (error : Location.error) =
  let of_sub loc sub =
    let msg =
      Location.print_sub_msg Format.str_formatter sub;
      String.trim (Format.flush_str_formatter ())
    in
    with_location ~skip_none:true loc ["message", `String msg]
  in
  let loc = Location.loc_of_report error in
  let msg =
    Location.print_main Format.str_formatter error;
    String.trim (Format.flush_str_formatter ())
  in
  let typ =
    match error.source with
    | Location.Lexer   -> "lexer"
    | Location.Parser  -> "parser"
    | Location.Typer   -> "typer"
    | Location.Warning ->
      if String.is_prefixed ~by:"Error" msg then
        "typer" (* Handle warn-error (since 4.08) *)
      else
        "warning"
    | Location.Unknown -> "unknown"
    | Location.Env     -> "env"
    | Location.Config     -> "config"
  in
  let content = [
    "type"    , `String typ;
    "sub"     , `List (List.map ~f:(of_sub loc) error.sub);
    "valid"   , `Bool true;
    "message" , `String msg;
  ] in
  with_location ~skip_none:true loc content

let json_of_completion {Compl. name; kind; desc; info; deprecated} =
  `Assoc ["name", `String name;
          "kind", `String (string_of_completion_kind kind);
          "desc", `String desc;
          "info", `String info;
          "deprecated", `Bool deprecated]

let json_of_completions {Compl. entries; context } =
  `Assoc [
    "entries", `List (List.map ~f:json_of_completion entries);
    "context", (match context with
        | `Unknown -> `Null
        | `Application {Compl. argument_type; labels} ->
          let label (name,ty) = `Assoc ["name", `String name;
                                        "type", `String ty] in
          let a = `Assoc ["argument_type", `String argument_type;
                          "labels", `List (List.map ~f:label labels)] in
          `List [`String "application"; a])
  ]

let rec json_of_outline outline =
  let json_of_item { outline_name ; outline_kind ; outline_type; location ; children ; deprecated } =
    with_location location [
      "name", `String outline_name;
      "kind", `String (string_of_completion_kind outline_kind);
      "type", (match outline_type with
        | None -> `Null
        | Some typ -> `String typ);
      "children", `List (json_of_outline children);
      "deprecated", `Bool deprecated
    ]
  in
  List.map ~f:json_of_item outline

let rec json_of_shape { shape_loc; shape_sub } =
  with_location shape_loc [
    "children", `List (List.map ~f:json_of_shape shape_sub);
  ]

let json_of_locate resp =
  match resp with
  | `At_origin -> `String "Already at definition point"
  | `Builtin s ->
    `String (sprintf "%S is a builtin, and it is therefore impossible \
                      to jump to its definition" s)
  | `Invalid_context -> `String "Not a valid identifier"
  | `Not_found (id, None) -> `String ("didn't manage to find " ^ id)
  | `Not_found (i, Some f) ->
    `String
      (sprintf "%s was supposed to be in %s but could not be found" i f)
  | `Not_in_env str ->
    `String (Printf.sprintf "Not in environment '%s'" str)
  | `File_not_found msg ->
    `String msg
  | `Found (None,pos) ->
    `Assoc ["pos", Lexing.json_of_position pos]
  | `Found (Some file,pos) ->
    `Assoc ["file",`String file; "pos", Lexing.json_of_position pos]

let json_of_response (type a) (query : a t) (response : a) : json =
  match query, response with
  | Type_expr _, str -> `String str
  | Type_enclosing _, results ->
    `List (List.map ~f:json_of_type_loc results)
  | Enclosing _, results ->
    `List (List.map ~f:(fun loc -> with_location loc []) results)
  | Complete_prefix _, compl ->
    json_of_completions compl
  | Expand_prefix _, compl ->
    json_of_completions compl
  | Polarity_search _, compl ->
    json_of_completions compl
  | Refactor_open _, locations ->
    `List (List.map locations ~f:(fun (name,loc) ->
        with_location loc ["content",`String name]))
  | Document _, resp ->
    begin match resp with
      | `No_documentation -> `String "No documentation available"
      | `Invalid_context -> `String "Not a valid identifier"
      | `Builtin s ->
        `String (sprintf "%S is a builtin, no documentation is available" s)
      | `Not_found (id, None) -> `String ("didn't manage to find " ^ id)
      | `Not_found (i, Some f) ->
        `String
          (sprintf "%s was supposed to be in %s but could not be found" i f)
      | `Not_in_env str ->
        `String (Printf.sprintf "Not in environment '%s'" str)
      | `File_not_found msg ->
        `String msg
      | `Found doc ->
        `String doc
    end
  | Locate_type _, resp -> json_of_locate resp
  | Locate _, resp -> json_of_locate resp
  | Jump _, resp ->
    begin match resp with
      | `Error str ->
        `String str
      | `Found pos ->
        `Assoc ["pos", Lexing.json_of_position pos]
    end
  | Phrase _, pos ->
    `Assoc ["pos", Lexing.json_of_position pos]
  | Case_analysis _, ({ Location. loc_start ; loc_end; _ }, str) ->
    let assoc =
      `Assoc [
        "start", Lexing.json_of_position loc_start  ;
        "end", Lexing.json_of_position loc_end ;
      ]
    in
    `List [ assoc ; `String str ]
  | Outline, outlines ->
    `List (json_of_outline outlines)
  | Shape _, shapes ->
    `List (List.map ~f:json_of_shape shapes)
  | Errors _, errors ->
    `List (List.map ~f:json_of_error errors)
  | Dump _, json -> json
  | Path_of_source _, str -> `String str
  | List_modules _, strs -> `List (List.map ~f:Json.string strs)
  | Findlib_list, strs -> `List (List.map ~f:Json.string strs)
  | Extension_list _, strs -> `List (List.map ~f:Json.string strs)
  | Path_list _, strs -> `List (List.map ~f:Json.string strs)
  | Occurrences _, locations ->
    `List (List.map locations
             ~f:(fun loc -> with_location loc []))
  | Version, version ->
    `String version
