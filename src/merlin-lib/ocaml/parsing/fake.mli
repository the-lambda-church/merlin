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

(* Definitions to help generating or rewriting pieces of AST,
 * used to simulate some CamlP4 extensions. *)

(* Generate AST faking value application *)
val app : Parsetree.expression ->
  Parsetree.expression -> Parsetree.expression
val pat_app : Parsetree.expression ->
  ('a * Parsetree.expression) -> ('a * Parsetree.expression )

(* Lwt extension *)
module Lwt : sig
  val un_lwt     : Parsetree.expression
  val to_lwt     : Parsetree.expression
  val in_lwt     : Parsetree.expression
  val unit_lwt   : Parsetree.expression
  val un_stream  : Parsetree.expression
  val finally_   : Parsetree.expression
  val raise_lwt_ : Longident.t
end

(* MetaOCaml support *)
module Meta : sig
  val code : Lexing.position -> Lexing.position ->
    Parsetree.expression -> Parsetree.expression
  val uncode : Lexing.position -> Lexing.position ->
    Parsetree.expression -> Parsetree.expression
end
