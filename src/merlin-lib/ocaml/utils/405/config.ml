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

let exec_magic_number = "Caml1999X011"
and cmi_magic_number = "Caml1999I021"
and cmo_magic_number = "Caml1999O011"
and cma_magic_number = "Caml1999A012"
and cmx_magic_number =
  (*if flambda then
    "Caml1999Y016"
  else*)
    "Caml1999Y015"
and cmxa_magic_number =
  (*if flambda then
    "Caml1999Z015"
  else*)
    "Caml1999Z014"
and ast_impl_magic_number = "Caml1999M020"
and ast_intf_magic_number = "Caml1999N018"
and cmxs_magic_number = "Caml2007D002"
and cmt_magic_number = "Caml2012T009"

let load_path = srefk ([] : string list)

let interface_suffix = ref ".mli"

let max_tag = 245

let safe_string = false
