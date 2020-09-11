(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2019  Frédéric Bour  <frederic.bour(_)lakaban.net>
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
open Std.Result

module Directive = struct
  type include_path =
    [ `B of string | `S of string | `CMI of string | `CMT of string ]

  type no_processing_required =
    [ `EXT of string list
    | `FLG of string list
    | `STDLIB of string
    | `SUFFIX of string
    | `READER of string list
    | `EXCLUDE_QUERY_DIR ]

  module Processed = struct
    type acceptable_in_input = [ include_path | no_processing_required ]

    type t = [ acceptable_in_input | `ERROR_MSG of string ]
  end

  module Raw = struct
    type t =
      [ Processed.acceptable_in_input
      | `PKG of string list
      | `FINDLIB of string
      | `FINDLIB_PATH of string
      | `FINDLIB_TOOLCHAIN of string ]
  end
end

type directive = Directive.Processed.t

module Sexp = struct
  type t = Atom of string | List of t list

  let atoms_of_strings = List.map ~f:(fun s -> Atom s)

  let strings_of_atoms =
    List.filter_map ~f:(function Atom s -> Some s | _ -> None)

  let rec to_string = function
  | Atom s -> s
  | List l -> String.concat ~sep:" "
    ( List.concat [["("]; List.map ~f:to_string l;[")"]])

  let to_directive sexp =
    let make_error str =
      let str = Printf.sprintf "Unknown configuration tag \"%s\"" str in
      `ERROR_MSG str
    in
    match sexp with
    | List [ Atom tag; Atom value ] ->
      begin match tag with
        | "S" -> `S value
        | "B" -> `B value
        | "CMI" -> `CMI value
        | "CMT" -> `CMT value
        | "STDLIB" -> `STDLIB value
        | "SUFFIX" -> `SUFFIX value
        | "ERROR" -> `ERROR_MSG value
        | "FLG" ->
            (* This means merlin asked dune 2.6 for configuration.
              But the protocole evolved, only dune 2.8 should be used *)
            `ERROR_MSG "No .merlin file found. Try building the project."
        | tag -> make_error tag
      end
    | List [ Atom tag; List l ] ->
        let value = strings_of_atoms l in
        begin match tag with
        | "EXT" -> `EXT value
        | "FLG" -> `FLG value
        | "READER" -> `READER value
        | tag -> make_error tag
      end
    | List [ Atom "EXCLUDE_QUERY_DIR" ] -> `EXCLUDE_QUERY_DIR
    | _ -> `ERROR_MSG "Unexpect output from external config reader"

  let from_directives (directives : Directive.Processed.t list) =
    let f t =
      let tag, body =
        let single s = [ Atom s ] in
        match t with
        | `B s -> ("B", single s)
        | `S s -> ("S", single s)
        | `CMI s -> ("CMI", single s)
        | `CMT s -> ("CMT", single s)
        | `EXT ss -> ("EXT", [ List (atoms_of_strings ss) ])
        | `FLG ss -> ("FLG", [ List (atoms_of_strings ss) ])
        | `STDLIB s -> ("STDLIB", single s)
        | `SUFFIX s -> ("SUFFIX", single s)
        | `READER ss -> ("READER", [ List (atoms_of_strings ss) ])
        | `EXCLUDE_QUERY_DIR -> ("EXCLUDE_QUERY_DIR", [])
        | `ERROR_MSG s -> ("ERROR", single s)
      in
      List (Atom tag :: body)
    in
    List (List.map ~f directives)
end

module Csexp = Csexp.Make (Sexp)

module Commands = struct
  type t = File of string | Halt | Unknown

  let read_input in_channel =
    let open Sexp in
    match Csexp.input in_channel with
    | Ok (List [Atom "File"; Atom path]) -> File path
    | Ok (Atom "Halt") -> Halt
    | Ok _ -> Unknown
    | Error _msg -> Halt

  let send_file ~out_channel path =
    Sexp.(List [Atom "File"; Atom path])
    |> Csexp.to_channel out_channel
end

type read_error =
  | Unexpected_output of string
  | Csexp_parse_error of string

let read ~in_channel =
  match Csexp.input in_channel with
  | Ok (Sexp.List directives) ->
      Ok (List.map directives ~f:Sexp.to_directive)
  | Ok sexp ->
    let msg = Printf.sprintf
      "A list of directives was expected, instead got: \"%s\""
      (Sexp.to_string sexp)
    in
    Error (Unexpected_output msg)
  | Error msg -> Error (Csexp_parse_error msg)

let write ~out_channel (directives : directive list) =
  directives |> Sexp.from_directives |> Csexp.to_channel out_channel
