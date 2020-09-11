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

(** Log module
  *
  * 1. Provide functions to log arbitrary messages, filtered according to a
  * section and a verbosity level.
  *
  * 2. Allow to setup a destination for these log messages.
  *
  **)

val log
  : section:string -> title:string -> ('b, unit, string, unit) format4 -> 'b

val fmt  : unit -> (Format.formatter -> unit) -> string
val json : unit -> (unit -> Std.json) -> string
val exn  : unit -> exn -> string

val log_flush : unit -> unit

type notification = {
  section: string;
  msg: string;
}

val notify : section:string -> ('b, unit, string, unit) format4 -> 'b
val with_notifications : notification list ref -> (unit -> 'a) -> 'a
val with_log_file : string option -> ?sections:string list -> (unit -> 'a) -> 'a

type 'a printf = title:string -> ('a, unit, string, unit) format4 -> 'a
type logger = { log : 'a. 'a printf }
val for_section : string -> logger
