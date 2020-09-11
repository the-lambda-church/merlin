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

(* A compressed (or should we say sparse?) bit set is a list of pairs
   of integers. The first component of every pair is an index, while
   the second component is a bit field. The list is sorted by order
   of increasing indices. *)

type t =
  | N
  | C of int * int * t

type element =
    int

let word_size =
  Sys.word_size - 1

let empty =
  N

let is_empty = function
  | N ->
      true
  | C _ ->
      false

let add i s =
  let ioffset = i mod word_size in
  let iaddr = i - ioffset
  and imask = 1 lsl ioffset in
  let rec add = function
    | N ->
        (* Insert at end. *)
        C (iaddr, imask, N)
    | C (addr, ss, qs) as s ->
        if iaddr < addr then
          (* Insert in front. *)
          C (iaddr, imask, s)
        else if iaddr = addr then
          (* Found appropriate cell, update bit field. *)
          let ss' = ss lor imask in
          if ss' = ss then
            s
          else
            C (addr, ss', qs)
        else
          (* Not there yet, continue. *)
          let qs' = add qs in
          if qs == qs' then
            s
          else
            C (addr, ss, qs')
  in
  add s

let singleton i =
   add i N

let remove i s =
  let ioffset = i mod word_size in
  let iaddr = i - ioffset
  and imask = 1 lsl ioffset in
  let rec remove = function
    | N ->
        N
    | C (addr, ss, qs) as s ->
        if iaddr < addr then
          s
        else if iaddr = addr then
          (* Found appropriate cell, update bit field. *)
          let ss' = ss land (lnot imask) in
          if ss' = 0 then
            qs
          else if ss' = ss then
            s
          else
            C (addr, ss', qs)
        else
          (* Not there yet, continue. *)
          let qs' = remove qs in
          if qs == qs' then
            s
          else
            C (addr, ss, qs')
  in
  remove s

let rec fold f s accu =
  match s with
  | N ->
      accu
  | C (base, ss, qs) ->
      loop f qs base ss accu

and loop f qs i ss accu =
  if ss = 0 then
    fold f qs accu
  else
    (* One could in principle check whether [ss land 0x3] is zero and if
       so move to [i + 2] and [ss lsr 2], and similarly for various sizes.
       In practice, this does not seem to make a measurable difference. *)
    loop f qs (i + 1) (ss lsr 1) (if ss land 1 = 1 then f i accu else accu)

let iter f s =
  fold (fun x () -> f x) s ()

let is_singleton s =
  match s with
  | C (_, ss, N) ->
      (* Test whether only one bit is set in [ss]. We do this by turning
         off the rightmost bit, then comparing to zero. *)
      ss land (ss - 1) = 0
  | C (_, _, C _)
  | N ->
      false

let cardinal s =
  fold (fun _ m -> m + 1) s 0

let elements s =
  fold (fun tl hd -> tl :: hd) s []

let rec subset s1 s2 =
  match s1, s2 with
  | N, _ ->
      true
  | _, N ->
      false
  | C (addr1, ss1, qs1), C (addr2, ss2, qs2) ->
      if addr1 < addr2 then
        false
      else if addr1 = addr2 then
        if (ss1 land ss2) <> ss1 then
          false
        else
          subset qs1 qs2
      else
        subset s1 qs2

let mem i s =
  subset (singleton i) s

let rec union s1 s2 =
  match s1, s2 with
  | N, s
  | s, N ->
      s
  | C (addr1, ss1, qs1), C (addr2, ss2, qs2) ->
      if addr1 < addr2 then
        C (addr1, ss1, union qs1 s2)
      else if addr1 > addr2 then
        let s = union s1 qs2 in
        if s == qs2 then
          s2
        else
          C (addr2, ss2, s)
      else
        let ss = ss1 lor ss2 in
        let s = union qs1 qs2 in
        if ss == ss2 && s == qs2 then
          s2
        else
          C (addr1, ss, s)

let rec inter s1 s2 =
  match s1, s2 with
  | N, _
  | _, N ->
      N
  | C (addr1, ss1, qs1), C (addr2, ss2, qs2) ->
      if addr1 < addr2 then
        inter qs1 s2
      else if addr1 > addr2 then
        inter s1 qs2
      else
        let ss = ss1 land ss2 in
        let s = inter qs1 qs2 in
        if ss = 0 then
          s
        else
          if (ss = ss1) && (s == qs1) then
            s1
          else
            C (addr1, ss, s)

exception Found of int

let choose s =
  try
    iter (fun x ->
      raise (Found x)
    ) s;
    raise Not_found
  with Found x ->
    x

let rec compare s1 s2 =
  match s1, s2 with
      N, N ->  0
    | _, N ->  1
    | N, _ -> -1
    | C (addr1, ss1, qs1), C (addr2, ss2, qs2) ->
        if addr1 < addr2 then -1
        else if addr1 > addr2 then 1
        else if ss1 < ss2 then -1
        else if ss1 > ss2 then 1
        else compare qs1 qs2

let equal s1 s2 =
  compare s1 s2 = 0

let rec disjoint s1 s2 =
  match s1, s2 with
  | N, _
  | _, N ->
      true
  | C (addr1, ss1, qs1), C (addr2, ss2, qs2) ->
      if addr1 = addr2 then
        if (ss1 land ss2) = 0 then
          disjoint qs1 qs2
        else
          false
      else if addr1 < addr2 then
        disjoint qs1 s2
      else
        disjoint s1 qs2

