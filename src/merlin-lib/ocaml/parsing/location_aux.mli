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

type t
  = Location.t
  = { loc_start: Lexing.position; loc_end: Lexing.position; loc_ghost: bool }

val compare_pos: Lexing.position -> t -> int

(** Return the smallest location covered by both arguments,
    ghost if both are ghosts *)
val union : t -> t -> t

(** Like location_union, but keep loc_ghost'ness of first argument *)
val extend : t -> t -> t

(** Filter valid errors, log invalid ones *)
val prepare_errors : exn list -> Location.error list

(** {1 Dump} *)

val print : unit -> t -> string
val print_loc : (unit -> 'a -> string) -> unit -> 'a Location.loc -> string

val is_relaxed_location : string Location.loc -> bool
