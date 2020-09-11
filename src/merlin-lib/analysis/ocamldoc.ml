(**************************************************************************)
(*                                                                        *)
(*  Copyright 2013 OCamlPro                                               *)
(*                                                                        *)
(*  All rights reserved.  This file is distributed under the terms of     *)
(*  the Lesser GNU Public License version 3.0.                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  Lesser GNU General Public License for more details.                   *)
(*                                                                        *)
(**************************************************************************)

(** Pops comments from a list of comments (string * loc) to find the ones that
   are associated to a given location. Also returns the remaining comments after
   the location. *)
let associate_comment ?(after_only=false) comments loc nextloc =
  let lstart = loc.Location.loc_start.Lexing.pos_lnum
  and lend =  loc.Location.loc_end.Lexing.pos_lnum in
  let isnext c =
    nextloc <> Location.none &&
    nextloc.Location.loc_start.Lexing.pos_cnum <
    c.Location.loc_end.Lexing.pos_cnum
  in
  let rec aux = function
    | [] -> None, []
    | (comment, cloc)::comments ->
        let cstart = cloc.Location.loc_start.Lexing.pos_lnum
        and cend =  cloc.Location.loc_end.Lexing.pos_lnum
        in
        let processed =
          (* It seems 4.02.3 remove ** from doc comment string, but not from
           * locations.  We can recognize doc comment by checking how the two
           * differ. *)
          (cloc.Location.loc_end.Lexing.pos_cnum -
           cloc.Location.loc_start.Lexing.pos_cnum) =
             String.length comment + 5
        in
        if cend < lstart - 1 || cstart < lend && after_only then
          aux comments
        else if cstart > lend + 1 ||
                isnext cloc ||
                cstart > lstart && cend < lend (* keep inner comments *)
        then
          None, (comment, cloc)::comments
        else if String.length comment < 2 ||
                (not processed && (comment.[0] <> '*' || comment.[1] = '*'))
        then
          aux comments
        else
        let comment =
          if processed then comment else
          String.sub comment 1 (String.length comment - 1)
        in
        let comment = String.trim comment in
        match aux comments with
        | None, comments -> Some comment, comments
        | Some c, comments -> Some (String.concat "\n" [comment; c]), comments
  in
  aux comments
