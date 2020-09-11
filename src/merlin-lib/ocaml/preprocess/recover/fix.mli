(******************************************************************************)
(*                                                                            *)
(*                                   Menhir                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*              Yann Régis-Gianas, PPS, Université Paris Diderot              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU General Public License version 2, as described in the    *)
(*  file LICENSE.                                                             *)
(*                                                                            *)
(******************************************************************************)

(* This code is described in the paper ``Lazy Least Fixed Points in ML''. *)

(* -------------------------------------------------------------------------- *)

(* Maps. *)

(* We require imperative maps, that is, maps that can be updated in place.
   An implementation of persistent maps, such as the one offered by ocaml's
   standard library, can easily be turned into an implementation of imperative
   maps, so this is a weak requirement. *)

module type IMPERATIVE_MAPS = sig
  type key
  type 'data t
  val create: unit -> 'data t
  val clear: 'data t -> unit
  val add: key -> 'data -> 'data t -> unit
  val find: key -> 'data t -> 'data
  val iter: (key -> 'data -> unit) -> 'data t -> unit
end

(* -------------------------------------------------------------------------- *)

(* Properties. *)

(* Properties must form a partial order, equipped with a least element, and
   must satisfy the ascending chain condition: every monotone sequence
   eventually stabilizes. *)

(* [is_maximal] determines whether a property [p] is maximal with respect to
   the partial order. Only a conservative check is required: in any event, it
   is permitted for [is_maximal p] to return [false]. If [is_maximal p]
   returns [true], then [p] must have no upper bound other than itself. In
   particular, if properties form a lattice, then [p] must be the top
   element. This feature, not described in the paper, enables a couple of
   minor optimizations. *)

module type PROPERTY = sig
  type property
  val bottom: property
  val equal: property -> property -> bool
  val is_maximal: property -> bool
end

(* -------------------------------------------------------------------------- *)

(* The code is parametric in an implementation of maps over variables and in
   an implementation of properties. *)

module Make
  (M : IMPERATIVE_MAPS)
  (P : PROPERTY)
  : sig
    type variable = M.key
    type property = P.property

    (* A valuation is a mapping of variables to properties. *)
    type valuation = variable -> property

    (* A right-hand side, when supplied with a valuation that gives
       meaning to its free variables, evaluates to a property. More
       precisely, a right-hand side is a monotone function of
       valuations to properties. *)
    type rhs = valuation -> property

    (* A system of equations is a mapping of variables to right-hand
       sides. *)
    type equations = variable -> rhs

    (* [lfp eqs] produces the least solution of the system of monotone
       equations [eqs]. *)

    (* It is guaranteed that, for each variable [v], the application [eqs v] is
       performed at most once (whereas the right-hand side produced by this
       application is, in general, evaluated multiple times). This guarantee can
       be used to perform costly pre-computation, or memory allocation, when [eqs]
       is applied to its first argument. *)

    (* When [lfp] is applied to a system of equations [eqs], it performs no
       actual computation. It produces a valuation, [get], which represents
       the least solution of the system of equations. The actual fixed point
       computation takes place, on demand, when [get] is applied. *)
    val lfp: equations -> valuation
  end
