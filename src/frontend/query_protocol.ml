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

module Compl =
struct
  type 'desc raw_entry = {
    name: string;
    kind: [`Value|`Constructor|`Variant|`Label|
           `Module|`Modtype|`Type|`MethodCall|`Keyword];
    desc: 'desc;
    info: 'desc;
    deprecated: bool;
  }

  type entry = string raw_entry

  type application_context = {
    argument_type: string;
    labels : (string * string) list;
  }

  type t = {
    entries: entry list;
    context: [ `Unknown
             | `Application of application_context
             ]
  }

  type kind = [
    | `Constructor
    | `Labels
    | `Modules
    | `Modules_type
    | `Types
    | `Values
    | `Variants
    | `Keywords
  ]
end

type completions = Compl.t

type outline = item list
and item = {
  outline_name : string ;
  outline_kind : [
    | `Value
    | `Constructor
    | `Label
    | `Module
    | `Modtype
    | `Type
    | `Exn
    | `Class
    | `Method
  ];
  outline_type : string option ;
  deprecated : bool ;
  location : Location_aux.t ;
  children : outline ;
}

type shape = {
  shape_loc : Location_aux.t;
  shape_sub : shape list;
}

type error_filter = {
  lexing : bool;
  parsing : bool;
  typing : bool;
}

type is_tail_position = [`No | `Tail_position | `Tail_call]

type _ _bool = bool

type _ t =
  | Type_expr(* *)
    :  string * Msource.position
    -> string t
  | Type_enclosing(* *)
    :  (string * int) option * Msource.position * int option
    -> (Location.t * [`String of string | `Index of int] * is_tail_position) list t
  | Enclosing(* *)
    :  Msource.position
    -> Location.t list t
  | Complete_prefix(* *)
    :  string * Msource.position * Compl.kind list *
       [`with_documentation] _bool * [`with_types] _bool
    -> completions t
  | Expand_prefix(* *)
    :  string * Msource.position * Compl.kind list * [`with_types] _bool
    -> completions t
  | Polarity_search
    :  string * Msource.position
    -> completions t
  | Refactor_open
    :  [`Qualify | `Unqualify] * Msource.position
    -> (string * Location.t) list t
  | Document(* *)
    : string option * Msource.position
    -> [ `Found of string
       | `Invalid_context
       | `Builtin of string
       | `Not_in_env of string
       | `File_not_found of string
       | `Not_found of string * string option
       | `No_documentation
       ] t
  | Locate_type
    : Msource.position
      -> [ `Found of string option * Lexing.position
         | `Invalid_context
         | `Builtin of string
         | `Not_in_env of string
         | `File_not_found of string
         | `Not_found of string * string option
         | `At_origin
         ] t
  | Locate(* *)
    : string option * [ `ML | `MLI ] * Msource.position
    -> [ `Found of string option * Lexing.position
       | `Invalid_context
       | `Builtin of string
       | `Not_in_env of string
       | `File_not_found of string
       | `Not_found of string * string option
       | `At_origin
       ] t
  | Jump(* *)
    : string * Msource.position
    -> [ `Found of Lexing.position
       | `Error of string
       ] t
  | Phrase(* *)
    : [`Next | `Prev] * Msource.position
    -> Lexing.position t
  | Case_analysis(* *)
    : Msource.position * Msource.position -> (Location.t * string) t
  | Holes(* *)
    : (Location.t * string) list t
  | Construct
    : Msource.position * [`None | `Local] option * int option
    -> (Location.t * string list) t
  | Outline(* *)
    :  outline t
  | Shape(* *)
    :  Msource.position
    -> shape list t
  | Errors(* *)
    :  error_filter
    -> Location.error list t
  | Dump
    :  Std.json list
    -> Std.json t
  | Path_of_source(* *)
    :  string list
    -> string t
  | List_modules(* *)
    :  string list
    -> string list t
  | Findlib_list
    :  string list t
  | Extension_list
    :  [`All|`Enabled|`Disabled]
    -> string list t
  | Path_list
    :  [`Build|`Source]
    -> string list t
  | Occurrences(* *)
    : [`Ident_at of Msource.position]
    -> Location.t list t
  | Version
    : string t
