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

type t =
  | Constructor of Types.constructor_description
    (* We attach the constructor description here so in the case of
      disambiguated constructors we actually directly look for the type
      path (cf. #486, #794). *)
  | Expr
  | Label of Types.label_description (* Similar to constructors. *)
  | Module_path
  | Module_type
  | Patt
  | Type
  | Unknown

val to_string : t -> string

(**
  [inspect_browse_tree lid ~cursor mbrowse] tries to provide contextual
  information given the selected identifier, the position of the cursor and the
  typed tree. It is used by Locate and Type_enclosing.

  The cursor position is used to distinguished wether a module path or an actual
  constructor name is pointed at when the cursor is in the middle of a
  longident, e.g. [Foo.B|ar.Constructor] (with | being the cursor).

  FIXME: when cursor at (M.|A 3), the enclosing node returned is const 3, thus
  breaking the context inference.
*)
val inspect_browse_tree :
  cursor:Std.Lexing.position -> Longident.t -> Mbrowse.t list -> t option
