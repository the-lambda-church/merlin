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

(** Destruct at the moment works in two contexts:

      - an expression context:
          It will replace the expression [e] under the cursor with

          {[
            match e with
            | p1 -> (??)
            | ...
          ]}

          This matching will be exhaustive.

          If [e] has a "package" type, it will be replaced by
          [let module M = (val e) in (??)]

      - a pattern context:
          Here two different behaviors can be observed:
          + if your matching is not exhaustive, it will be made exhaustive.
          + if your matching is exhaustive, it will refine the subpattern under
            the cursor if possible (i.e. if your cursor is on a variable or _ ).


                                * * *


    Final remarks:
      - Destruct will refuse to work on expression (resp. patterns) with a
        functional or polymorphic type.

      - Constructors of variant types will be prefixed by their path (if
        necessary) but record labels will not.
        The reason is that we don't control the way things are printed, we reuse
        [Pprintast] which will print things like:
        [{ Module.label1 = label1 ; Module.label2 = label2}] where one would
        rather have [{ Module.label1 ; label2 }]. Since qualifying one label is
        less annoying than rewriting the whole pattern, we decided to note
        qualify labels (understanding that the code inserted by merlin in the
        buffer will sometimes be wrong).

*)

(* TODO: document the following *)

exception Not_allowed of string
exception Useless_refine
exception Nothing_to_do
exception Wrong_parent of string

val node :
  Mconfig.t -> Msource.t -> Browse_raw.node ->
  Browse_raw.node list -> Location.t * string
(** [node ~env parents current_node] returns a location indicating which
    portion of the buffer must be replaced and the string to replace it with. *)

val log : 'a Logger.printf
