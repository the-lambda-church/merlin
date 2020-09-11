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

exception Unknown

(* Adjust typing environment for syntax extensions.
 * See [Fake] for AST part *)

(* Extension environment is composed of two part:
 * - private definitions, not exposed to user but accessed by AST rewriters,
 * - public definitions, those are made available to user in default scope,
 *   like the Pervasives module.
 * See [Typer.initial_env] for initial environment generation.
 *)

(** Definition of an extension (as seen from Lexer and Typer) *)
type t = {
  name : string;
  private_def : string list;
  public_def : string list;
  packages : string list;
  keywords : (string * Parser_raw.token) list;
}

(* Private definitions are put in a fake module named "_" with the following
 * ident. Use it to test or find private definitions. *)
val ident : Ident.t

(** Set of extension name *)
type set = string list

(* Lexer keywords needed by extensions *)
val keywords : set -> Lexer_raw.keywords
(* Register extensions in typing environment *)
val register : set -> Env.t -> Env.t

(* Known extensions *)
val all : set
val registry : t String.Map.t
val lookup : string -> t option

(* Compute set of extensions from package names (used to enable support for
  "lwt" if "lwt.syntax" package is loaded by user. *)
val from : extensions:string list -> packages:string list -> set

(* Merlin expects a few extensions to be always enabled, otherwise error
   recovery may fail arbitrarily *)
val empty : set
