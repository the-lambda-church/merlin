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

open Parsetree

let app a b =
  let loc =
    if a.pexp_loc.Location.loc_ghost
    then {b.pexp_loc with Location.loc_ghost = true}
    else b.pexp_loc
  in
  Ast_helper.Exp.apply ~loc a [Ast_helper.no_label, b]

let pat_app f (pat,expr) = pat, app f expr

let prim_ident prim = Longident.parse ("_." ^ prim)
let prim ?(ghost=true) prim =
  let open Location in
  let ident = mknoloc (prim_ident prim) in
  let ident = if ghost
    then ident
    else {ident with loc = {ident.loc with loc_ghost = false}}
  in
  Ast_helper.Exp.ident ~loc:ident.loc ident

(* Lwt extension *)
module Lwt = struct
  let un_lwt = prim "Lwt.un_lwt"
  let to_lwt = prim "Lwt.to_lwt"
  let in_lwt = prim "Lwt.in_lwt"
  let unit_lwt = prim "Lwt.unit_lwt"
  let un_stream = prim "Lwt.un_stream"
  let finally_ = prim "Lwt.finally'"
  let raise_lwt_ = prim_ident "Lwt.raise_lwt'"
end

(* MetaOCaml support *)
module Meta = struct
  let prim_code = prim "Meta.code"
  let prim_uncode = prim "Meta.uncode"

  let code loc_start loc_end expr =
    let loc = {expr.pexp_loc with Location. loc_start; loc_end} in
    Ast_helper.Exp.apply ~loc prim_code [Ast_helper.no_label, expr]

  let uncode loc_start loc_end expr =
    let loc = {expr.pexp_loc with Location. loc_start; loc_end} in
    Ast_helper.Exp.apply ~loc prim_uncode [Ast_helper.no_label, expr]
end
