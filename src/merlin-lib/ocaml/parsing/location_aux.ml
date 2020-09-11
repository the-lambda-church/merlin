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

type t
  = Location.t
  = { loc_start: Lexing.position; loc_end: Lexing.position; loc_ghost: bool }

let compare_pos pos loc =
  if Lexing.compare_pos pos loc.Location.loc_start < 0 then
    -1
  else if Lexing.compare_pos pos loc.Location.loc_end > 0 then
    1
  else
    0

let union l1 l2 =
  if l1 = Location.none then l2
  else if l2 = Location.none then l1
  else {
    Location.
    loc_start = Lexing.min_pos l1.Location.loc_start l2.Location.loc_start;
    loc_end   = Lexing.max_pos l1.Location.loc_end l2.Location.loc_end;
    loc_ghost = l1.Location.loc_ghost && l2.Location.loc_ghost;
  }

let extend l1 l2 =
  if l1 = Location.none then l2
  else if l2 = Location.none then l1
  else {
    Location.
    loc_start = Lexing.min_pos l1.Location.loc_start l2.Location.loc_start;
    loc_end   = Lexing.max_pos l1.Location.loc_end l2.Location.loc_end;
    loc_ghost = l1.Location.loc_ghost;
  }

(** Filter valid errors, log invalid ones *)
let prepare_errors exns =
  List.filter_map exns
    ~f:(fun exn ->
        match Location.error_of_exn exn with
        | None ->
          Logger.log ~section:"Mreader" ~title:"errors"
            "Location.error_of_exn (%a) = None"
            (fun () -> Printexc.to_string) exn;
          None
        | Some `Already_displayed ->  None
        | Some (`Ok err) -> Some err
      )

let print () {Location. loc_start; loc_end; loc_ghost}  =
  let l1, c1 = Lexing.split_pos loc_start in
  let l2, c2 = Lexing.split_pos loc_end in
  sprintf "%d:%d-%d:%d%s"
    l1 c1 l2 c2 (if loc_ghost then "{ghost}" else "")

let print_loc f () {Location. txt; loc} =
  sprintf "%a@%a" f txt print loc

let is_relaxed_location = function
  | { Location. txt = "merlin.relaxed-location" | "merlin.loc"; _ } -> true
  | _ -> false
