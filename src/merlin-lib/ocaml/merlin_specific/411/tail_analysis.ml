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
open Browse_raw
open Typedtree

let tail_operator = function
  | {exp_desc = Texp_ident
                  (_,_, {Types.val_kind =
                           Types.Val_prim
                             {Primitive.prim_name = "%sequand"|"%sequor"; _ }
                        ; _ })
    ; _ }
    -> true
  | _ -> false

let expr_tail_positions = function
  | Texp_apply (callee, args) when tail_operator callee ->
    begin match List.last args with
      | None | Some (_, None)-> []
      | Some (_, Some expr) -> [Expression expr]
    end
  | Texp_instvar _ | Texp_setinstvar _ | Texp_override _ | Texp_assert _
  | Texp_lazy _ | Texp_object _ | Texp_pack _
  | Texp_function _ | Texp_apply _ | Texp_tuple _
  | Texp_ident _ | Texp_constant _
  | Texp_construct _ | Texp_variant _ | Texp_record _
  | Texp_field _ | Texp_setfield _ | Texp_array _
  | Texp_while _ | Texp_for _ | Texp_send _ | Texp_new _
  | Texp_unreachable | Texp_extension_constructor _ | Texp_letop _
    -> []
  | Texp_match (_,cs,_)
    -> List.map cs ~f:(fun c -> Case c)
  | Texp_try (_,cs)
    -> List.map cs ~f:(fun c -> Case c)
  | Texp_letmodule (_,_,_,_,e) | Texp_letexception (_,e) | Texp_let (_,_,e)
  | Texp_sequence (_,e) | Texp_ifthenelse (_,e,None) | Texp_open (_, e)
    -> [Expression e]
  | Texp_ifthenelse (_,e1,Some e2)
    -> [Expression e1; Expression e2]


let tail_positions = function
  | Expression expr -> expr_tail_positions expr.exp_desc
  | Case case -> [Expression case.c_rhs]
  | _ -> []

(* If the expression is a function, return all of its entry-points (which are
   in tail-positions). Returns an empty list otherwise *)
let expr_entry_points = function
  | Texp_function {cases; _} -> List.map cases ~f:(fun c -> Case c)
  | _ -> []

let entry_points = function
  | Expression expr -> expr_entry_points expr.exp_desc
  | _ -> []

(* FIXME: what about method call? It should be translated to a Texp_apply,
   but I am not sure *)
let is_call = function
  | Expression {exp_desc = Texp_apply _; _} -> true
  | _ -> false
