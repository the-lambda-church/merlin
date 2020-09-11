(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Long identifiers, used in parsetree. *)

type t =
    Lident of string
  | Ldot of t * string
  | Lapply of t * t

val flatten: t -> string list
val unflatten: string list -> t option
val last: t -> string
val parse: string -> t

(* Merlin specific. *)

val keep_suffix : t -> t * bool
(** if [li', b = keep_suffix li] then:
    - the prefix of [li'] is a module path
    - [b = false] iff [li' = li].
    Corollary: [b = true] if [li] is a label access
               (i.e. [li = X.Y.z.Foo.Bar...]) *)
