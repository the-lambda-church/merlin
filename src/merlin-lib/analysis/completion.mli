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

open Query_protocol

(* TODO: document all the following functions *)

type raw_info =
  [ `Constructor of Types.constructor_description
  | `Modtype of Types.module_type
  | `Modtype_declaration of Ident.t * Types.modtype_declaration
  | `None
  | `String of string
  | `Type_declaration of Ident.t * Types.type_declaration
  | `Type_scheme of Types.type_expr
  | `Variant of string * Types.type_expr option
  ]

val raw_info_printer : raw_info ->
  [ `String of string
  | `Print of Extend_protocol.Reader.outcometree
  | `Concat of string * Extend_protocol.Reader.outcometree
  ]

val map_entry : ('a -> 'b) ->
  'a Compl.raw_entry -> 'b Compl.raw_entry

val branch_complete
  :  Mconfig.t
  -> ?get_doc:([> `Completion_entry of [> `Type | `Vals ] * Path.t * Location.t ]
               -> [> `Found of string ])
  -> ?target_type:Types.type_expr
  -> ?kinds:Compl.kind list
  -> string
  -> Mbrowse.t
  -> raw_info Compl.raw_entry list

val expand_prefix
  : global_modules:string list
  -> ?kinds:Compl.kind list
  -> Env.t -> string
  -> raw_info Compl.raw_entry list

val application_context : prefix:Asttypes.label -> Mbrowse.t ->
  Types.type_expr option *
  [> `Application of Compl.application_context | `Unknown ]
