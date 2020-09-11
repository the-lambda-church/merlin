(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Fabrice Le Fessant, INRIA Saclay                     *)
(*                                                                        *)
(*   Copyright 2012 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type pers_flags =
  | Rectypes
  | Alerts of Misc.alerts
  | Opaque
  | Unsafe_string

type cmi_infos = {
    cmi_name : string;
    cmi_sign : Types.signature_item list;
    cmi_crcs : (string * Digest.t option) list;
    cmi_flags : pers_flags list;
}

let input_cmi ic =
  let (name, sign) = input_value ic in
  let crcs = input_value ic in
  let flags = input_value ic in
  {
      cmi_name = name;
      cmi_sign = sign;
      cmi_crcs = crcs;
      cmi_flags = flags;
    }

let read_cmi filename =
  let open Magic_numbers.Cmi in
  let ic = open_in_bin filename in
  try
    let buffer =
      really_input_string ic (String.length Config.cmi_magic_number)
    in
    if buffer <> Config.cmi_magic_number then begin
      close_in ic;
      let pre_len = String.length Config.cmi_magic_number - 3 in
      if String.sub buffer 0 pre_len
          = String.sub Config.cmi_magic_number 0 pre_len then
      begin
        raise (Error (Wrong_version_interface (filename, buffer)))
      end else begin
        raise(Error(Not_an_interface filename))
      end
    end;
    let cmi = input_cmi ic in
    close_in ic;
    cmi
  with End_of_file | Failure _ ->
      close_in ic;
      raise(Error(Corrupted_interface(filename)))
    | Error e ->
      close_in ic;
      raise (Error e)

let output_cmi filename oc cmi =
(* beware: the provided signature must have been substituted for saving *)
  output_string oc Config.cmi_magic_number;
  output_value oc (cmi.cmi_name, cmi.cmi_sign);
  flush oc;
  let crc = Digest.file filename in
  let crcs = (cmi.cmi_name, Some crc) :: cmi.cmi_crcs in
  output_value oc crcs;
  output_value oc cmi.cmi_flags;
  crc

(* Error report moved to src/ocaml/typing/magic_numbers.ml *)
