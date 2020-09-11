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

type t = {
  ppxs: string list;
  ppxopts: string list list String.Map.t;
}

let empty = { ppxs = []; ppxopts = String.Map.empty }

let add_ppx ppx t =
  if List.mem ppx ~set:t.ppxs
  then t
  else {t with ppxs = ppx :: t.ppxs}

let add_ppxopts ppx opts t =
  match opts with
  | [] -> t
  | opts ->
    let ppx = Filename.basename ppx in
    let optss =
      try String.Map.find ppx t.ppxopts
      with Not_found -> []
    in
    if not (List.mem ~set:optss opts) then
      let ppxopts = String.Map.add ~key:ppx ~data:(opts :: optss) t.ppxopts in
      {t with ppxopts}
    else t

let union ta tb =
  { ppxs = List.filter_dup (ta.ppxs @ tb.ppxs);
    ppxopts = String.Map.merge ~f:(fun _ a b -> match a, b with
        | v, None | None, v -> v
        | Some a, Some b -> Some (List.filter_dup (a @ b)))
        ta.ppxopts tb.ppxopts
  }

let command_line t =
  List.fold_right ~f:(fun ppx ppxs ->
      let basename = Filename.basename ppx in
      let opts =
        try String.Map.find basename t.ppxopts
        with Not_found -> []
      in
      let opts = List.concat (List.rev opts) in
      String.concat ~sep:" " (ppx :: opts) :: ppxs)
    t.ppxs ~init:[]

let dump t =
  let string k = `String k in
  let string_list l = `List (List.map ~f:string l) in
  `Assoc [
    "preprocessors",
    string_list t.ppxs;
    "options",
    `Assoc (
      String.Map.fold
        ~f:(fun ~key ~data:opts acc ->
          let opts = List.rev_map ~f:string_list opts in
          (key, `List opts) :: acc)
        ~init:[]
        t.ppxopts
    )
  ]
