(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Access paths *)

type t =
    Pident of Ident.t
  | Pdot of t * string * int
  | Papply of t * t

val same: t -> t -> bool
val isfree: Ident.t -> t -> bool
val binding_time: t -> int
val scope: t -> int

val nopos: int

val name: ?paren:(string -> bool) -> t -> string
    (* [paren] tells whether a path suffix needs parentheses *)
val head: t -> Ident.t

val last: t -> string

val compare : t -> t -> int

(* Backported from 4.08 *)

module Map : Map.S with type key = t
module Set : Set.S with type elt = t

(* Added for merlin *)

val to_string_list : t -> string list

module Path_tbl : Hashtbl.S with type key = t

module Nopos : sig
  type nopos = private
    | Pident of Ident.t
    | Pdot of t * string
    | Papply of t * t

  val view : t -> nopos
end
