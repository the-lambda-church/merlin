(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

[@@@ocaml.warning "-27"]

type t =
    Pident of Ident.t
  | Pdot of t * string * int
  | Papply of t * t

let nopos = -1

let rec same p1 p2 =
  match (p1, p2) with
    (Pident id1, Pident id2) -> Ident.same id1 id2
  | (Pdot(p1, s1, pos1), Pdot(p2, s2, pos2)) -> s1 = s2 && same p1 p2
  | (Papply(fun1, arg1), Papply(fun2, arg2)) ->
       same fun1 fun2 && same arg1 arg2
  | (_, _) -> false

let rec isfree id = function
    Pident id' -> Ident.same id id'
  | Pdot(p, s, pos) -> isfree id p
  | Papply(p1, p2) -> isfree id p1 || isfree id p2

let rec binding_time = function
    Pident id -> Ident.binding_time id
  | Pdot(p, s, pos) -> binding_time p
  | Papply(p1, p2) -> max (binding_time p1) (binding_time p2)

let scope = binding_time

let kfalse x = false

let rec name ?(paren=kfalse) = function
    Pident id -> Ident.name id
  | Pdot(p, s, pos) ->
      name ~paren p ^ if paren s then ".( " ^ s ^ " )" else "." ^ s
  | Papply(p1, p2) -> name ~paren p1 ^ "(" ^ name ~paren p2 ^ ")"

let rec head = function
    Pident id -> id
  | Pdot(p, s, pos) -> head p
  | Papply(p1, p2) -> assert false

let rec last = function
  | Pident id -> Ident.name id
  | Pdot(_, s, _) -> s
  | Papply(_, p) -> last p

let rec compare p1 p2 =
  match (p1, p2) with
    (Pident id1, Pident id2) -> Pervasives.compare id1 id2
  | (Pdot(p1, s1, _pos1), Pdot(p2, s2, _pos2)) ->
      let h = compare p1 p2 in
      if h <> 0 then h else String.compare s1 s2
  | (Papply(fun1, arg1), Papply(fun2, arg2)) ->
      let h = compare fun1 fun2 in
      if h <> 0 then h else compare arg1 arg2
  | ((Pident _ | Pdot _), (Pdot _ | Papply _)) -> -1
  | ((Pdot _ | Papply _), (Pident _ | Pdot _)) -> 1


(* Backported from 4.08 *)

module T = struct
  type nonrec t = t
  let compare = compare

  let equal x y = compare x y = 0

  (* Added for merlin *)
  let rec hash = function
    | Pident id -> Hashtbl.hash id
    | Pdot (p, s, _) ->
      let h = hash p in
      Hashtbl.seeded_hash h (Hashtbl.hash s)
    | Papply (p1, p2) ->
      let h1 = hash p1 and h2 = hash p2 in
      Hashtbl.seeded_hash h1 h2
end

module Map = Map.Make (T)
module Set = Set.Make (T)

(* Added for merlin *)

let to_string_list p =
  let rec aux acc = function
  | Pident id -> Ident.name id :: acc
  | Pdot (p, str, _) -> aux (str :: acc) p
  | _ -> assert false
  in
  aux [] p

module Path_tbl = Hashtbl.Make (T)

module Nopos = struct
  type nopos =
    | Pident of Ident.t
    | Pdot of t * string
    | Papply of t * t

  let view : t -> nopos = function
    | Pident id -> Pident id
    | Pdot (t, s, _) -> Pdot (t, s)
    | Papply (p1, p2) -> Papply (p1, p2)
end
