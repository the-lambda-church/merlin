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

(***********************************************************************)
(**                                                                   **)
(**               WARNING WARNING WARNING                             **)
(**                                                                   **)
(** When you change this file, you must make the parallel change      **)
(** in config.mlbuild                                                 **)
(**                                                                   **)
(***********************************************************************)

open Local_store.Compiler


(* The main OCaml version string has moved to ../VERSION *)
let version = Sys.ocaml_version

let flambda = false

let exec_magic_number = "Caml1999X023"
and cmi_magic_number = "Caml1999I023"
and cmo_magic_number = "Caml1999O023"
and cma_magic_number = "Caml1999A023"
and cmx_magic_number =
  if flambda then
    "Caml1999y023"
  else
    "Caml1999Y023"
and cmxa_magic_number =
  if flambda then
    "Caml1999z023"
  else
    "Caml1999Z023"
and ast_impl_magic_number = "Caml1999M023"
and ast_intf_magic_number = "Caml1999N023"
and cmxs_magic_number = "Caml1999D023"
    (* cmxs_magic_number is duplicated in otherlibs/dynlink/natdynlink.ml *)
and cmt_magic_number = "Caml1999T023"

let load_path = srefk ([] : string list)

let interface_suffix = ref ".mli"

let max_tag = 245

let safe_string = true
let flat_float_array = false
