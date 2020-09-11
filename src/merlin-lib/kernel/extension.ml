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

open Std
open Parser_raw

exception Unknown

type t = {
  name : string;
  private_def : string list;
  public_def : string list;
  packages : string list;
  keywords : (string * Parser_raw.token) list;
}

type set = string list

(* Private definitions are put in a fake module named "_" with the following
 * ident. Use it to test or find private definitions. *)
let ident = Ident.create_persistent "_"

(** Definition of each extension *)
let ext_lwt = {
  name = "lwt";
  private_def = [
    "module Lwt : sig
      val un_lwt : 'a Lwt.t -> 'a
      val in_lwt : 'a Lwt.t -> 'a Lwt.t
      val to_lwt : 'a -> 'a Lwt.t
      val finally' : 'a Lwt.t -> unit Lwt.t -> 'a Lwt.t
      val un_stream : 'a Lwt_stream.t -> 'a
      val unit_lwt : unit Lwt.t -> unit Lwt.t
    end"
  ];
  public_def = [
    "val (>>) : unit Lwt.t -> 'a Lwt.t -> 'a Lwt.t
     val raise_lwt : exn -> 'a Lwt.t
     val assert_lwt : bool -> unit Lwt.t"
  ];
  keywords = [
    "lwt", LET_LWT;
    "try_lwt", TRY_LWT;
    "match_lwt", MATCH_LWT;
    "finally", FINALLY_LWT;
    "for_lwt", FOR_LWT;
    "while_lwt", WHILE_LWT;
  ];
  packages = ["lwt.syntax"];
}

let ext_nonrec = {
  name = "nonrec";
  private_def = [];
  public_def = [];
  keywords = [
    "nonrec", NONREC;
  ];
  packages = [];
}

let ext_meta = {
  name = "meta";
  private_def = [
    "module Meta : sig
      val code : 'a -> 'a code
      val uncode : 'a code -> 'a
    end"
  ];
  public_def = [];
  keywords = [
    ">.", GREATERDOT;
  ];
  packages = [];
}

(* Known extensions *)
let registry = [ext_lwt;ext_meta]
let registry =
  List.fold_left registry ~init:String.Map.empty
    ~f:(fun map ext -> String.Map.add map ~key:ext.name ~data:ext)

let all = String.Map.keys registry

let lookup s =
  try Some (String.Map.find s registry)
  with Not_found -> None

let empty = []

(* Compute set of extensions from package names (used to enable support for
  "lwt" if "lwt.syntax" is loaded by user. *)
let from ~extensions ~packages =
  String.Map.fold registry ~init:[] ~f:(fun ~key:name ~data:ext set ->
      if List.mem name ~set:extensions ||
         List.exists ~f:(List.mem ~set:ext.packages) packages
      then name :: set
      else set
    )

(* Merlin expects a few extensions to be always enabled, otherwise error
   recovery may fail arbitrarily *)
let default = match My_config.ocamlversion with
              | `OCaml_4_02_2 | `OCaml_4_03_0 -> [ext_nonrec]
              | _ -> []

let default_kw = List.concat_map ~f:(fun ext -> ext.keywords) default

(* Lexer keywords needed by extensions *)
let keywords set =
  let add_kw kws ext =
    match lookup ext with
    | None -> kws
    | Some def -> def.keywords @ kws
  in
  let all = List.fold_left set ~init:default_kw ~f:add_kw in
  Lexer_raw.keywords all

(* Register extensions in typing environment *)
let parse_sig =
  let keywords = Lexer_raw.keywords [] in fun str ->
  let lexbuf = Lexing.from_string str in
  let state = Lexer_raw.make keywords in
  let rec lexer = function
    | Lexer_raw.Fail _ -> assert false
    | Lexer_raw.Return x -> x
    | Lexer_raw.Refill k -> lexer (k ())
  in
  let lexer lexbuf = lexer (Lexer_raw.token_without_comments state lexbuf) in
  (Parser_raw.interface lexer lexbuf : Parsetree.signature)

let type_sig env sg =
  let sg = Typemod.transl_signature env sg in
  sg.Typedtree.sig_type

(*
let add_hidden_signature env sign =
  let add_item env comp =
    match comp with
    | Types.Sig_value(id, decl)     -> Env.add_value (Ident.hide id) decl env
    | Types.Sig_type(id, decl, _)   -> Env.add_type ~check:false (Ident.hide id) decl env
    | Types.Sig_typext(id, decl, _) -> Env.add_extension ~check:false (Ident.hide id) decl env
    | Types.Sig_module(id, mty, _)  -> Env.add_module (Ident.hide id) mty.Types.md_type env
    | Types.Sig_modtype(id, decl)   -> Env.add_modtype (Ident.hide id) decl env
    | Types.Sig_class(id, decl, _)  -> Env.add_class (Ident.hide id) decl env
    | Types.Sig_class_type(id, decl, _) -> Env.add_cltype (Ident.hide id) decl env
  in
  List.fold_left ~f:add_item ~init:env sign
*)

let register exts env =
  (* Log errors ? *)
  let try_type sg' = try type_sig env sg' with _exn -> [] in
  let exts = List.filter_dup exts in
  let exts = List.filter_map ~f:(fun ext ->
      match String.Map.find ext registry with
      | ext -> Some ext
      | exception Not_found -> None
    ) exts
  in
  let process_ext e =
    let prv = List.concat_map ~f:parse_sig e.private_def in
    let pub = List.concat_map ~f:parse_sig e.public_def in
    try_type prv, try_type pub
  in
  let fakes, tops = List.split (List.map ~f:process_ext exts) in
  let env = Env.add_signature (List.concat tops) env in
  Env.add_merlin_extension_module ident
    (Types.Mty_signature (List.concat fakes)) env
