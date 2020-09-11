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

val log : 'a Logger.printf

module Namespace : sig
  type t = [ `Type | `Mod | `Modtype | `Vals | `Constr | `Labels ]
end

val from_path
  : config:Mconfig.t
  -> env:Env.t
  -> local_defs:Mtyper.typedtree
  -> pos:Lexing.position
  -> namespace:Namespaced_path.Namespace.t
  -> [ `ML | `MLI ]
  -> Path.t
  -> [> `File_not_found of string
     | `Found of string option * Lexing.position
     | `Builtin
     | `Not_in_env of string
     | `Not_found of string * string option ]

val from_string
  :  config:Mconfig.t
  -> env:Env.t
  -> local_defs:Mtyper.typedtree
  -> pos:Lexing.position
  -> ?namespaces:Namespace.t list
  -> [ `ML | `MLI ]
  -> string
  -> [> `File_not_found of string
      | `Found of string option * Lexing.position
      | `Builtin of string
      | `Missing_labels_namespace
      | `Not_found of string * string option
      | `Not_in_env of string
      | `At_origin ]

val get_doc
  :  config:Mconfig.t
  -> env:Env.t
  -> local_defs:Mtyper.typedtree
  -> comments:(string * Location.t) list
  -> pos:Lexing.position
  -> [ `User_input of string
     | `Completion_entry of (Namespaced_path.Namespace.t * Path.t * Location.t) ]
  -> [> `File_not_found of string
      | `Found of string
      | `Builtin of string
      | `Not_found of string * string option
      | `Not_in_env of string
      | `No_documentation ]
