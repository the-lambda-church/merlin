(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*        Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Std

(** If [restore = true] (the default), cookies set by external rewriters will be
    kept for later calls. *)

val apply_rewriters_str: ppx:string with_workdir list -> ?restore:bool -> tool_name:string -> Parsetree.structure -> Parsetree.structure
val apply_rewriters_sig: ppx:string with_workdir list -> ?restore:bool -> tool_name:string -> Parsetree.signature -> Parsetree.signature

val apply_rewriters: ppx:string with_workdir list -> ?restore:bool -> tool_name:string -> Mreader.parsetree -> Mreader.parsetree

val apply_pp : workdir:string -> filename:string -> source:string -> pp:string -> string
