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

val verbosity : int ref

module Printtyp : sig
  include module type of struct include Printtyp end

  val type_declaration :
    Env.t -> Ident.t -> Format.formatter -> Types.type_declaration -> unit

  val type_scheme : Env.t -> Format.formatter -> Types.type_expr -> unit

  val modtype : Env.t -> Format.formatter -> Types.module_type -> unit

  val wrap_printing_env : Env.t -> verbosity:int -> (unit -> 'a) -> 'a
end

val mod_smallerthan : int -> Types.module_type -> int option
(** Check if module is smaller (= has less definition, counting nested ones)
    than a particular threshold. Return (Some n) if module has size n, or None
    otherwise (module is bigger than threshold).
    Used to skip printing big modules in completion. *)

val type_in_env : ?verbosity:int -> ?keywords:Lexer_raw.keywords ->
  context: Context.t -> Env.t -> Format.formatter -> string -> bool
(** [type_in_env env ppf input] parses [input] and prints its type on [ppf].
    Returning true if it printed a type, false otherwise. *)

val print_type_with_decl : verbosity:int ->
  Env.t -> Format.formatter -> Types.type_expr -> unit
(** [print_type_or_decl] behaves like [Printtyp.type_scheme], it prints the
    type expression, except if it is a type constructor and verbosity is set then
    it also prints the type declaration. *)

val lookup_module : Longident.t ->
  Env.t -> Path.t * Types.module_type * Parsetree.attributes
(** [lookup_module] is a fancier version of [Env.lookup_module] that also
    returns the module type. *)

val read_doc_attributes : Parsetree.attributes -> (string * Location.t) option
(** [read_doc_attributes] looks for a docstring in an attribute list. *)

val is_deprecated : Parsetree.attributes -> bool
