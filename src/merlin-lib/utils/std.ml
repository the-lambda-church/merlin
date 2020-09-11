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

module Json = struct
  include Yojson.Basic
  let string x = `String x
  let int x = `Int x
  let bool x = `Bool x

  let option f = function
    | None -> `Null
    | Some x -> f x

  let list f x =
    `List (List.map f x)

  let print f () x =
    pretty_to_string (f x)

end

type json =
  [ `Assoc of (string * json) list
  | `Bool of bool
  | `Float of float
  | `Int of int
  | `List of json list
  | `Null
  | `String of string ]

module Hashtbl = struct
  include Hashtbl

  let find_some tbl key =
    try Some (find tbl key)
    with Not_found -> None

  let elements tbl = Hashtbl.fold (fun _key elt acc -> elt :: acc) tbl []

  let forall table f =
    match Hashtbl.iter (fun k v -> if not (f k v) then raise Exit) table with
    | () -> true
    | exception Exit -> false
end

module List = struct
  include ListLabels

  let init ~f n =
    let rec aux i = if i = n then [] else f i :: aux (succ i) in
    aux 0

  let index ~f l =
    let rec aux i = function
      | [] -> raise Not_found
      | x :: _ when f x -> i
      | _ :: xs -> aux (succ i) xs
    in
    aux 0 l

  let find_some ~f l =
    try Some (find ~f l)
    with Not_found -> None

  let rec rev_scan_left acc ~f l ~init = match l with
    | [] -> acc
    | x :: xs ->
      let init = f init x in
      rev_scan_left (init :: acc) ~f xs ~init

  let scan_left ~f l ~init =
      List.rev (rev_scan_left [] ~f l ~init)

  let rev_filter ~f lst =
    let rec aux acc = function
      | [] -> acc
      | x :: xs -> aux (if f x then x :: acc else acc) xs
    in
    aux [] lst

  let rec filter_map ~f = function
    | [] -> []
    | x :: xs ->
      match f x with
      | None -> filter_map ~f xs
      | Some x -> x :: filter_map ~f xs

  let rec find_map ~f = function
    | [] -> raise Not_found
    | x :: xs ->
      match f x with
      | None -> find_map ~f xs
      | Some x' -> x'

  let rec map_end ~f l1 l2 =
    match l1 with
    | [] -> l2
    | hd::tl -> f hd :: map_end ~f tl l2

  let concat_map ~f l = flatten (map ~f l)

  let replicate elem n =
    let rec aux acc elem n =
      if n <= 0 then acc else aux (elem :: acc) elem (n-1)
    in
    aux [] elem n

  let rec remove ?(phys=false) x =
    let check = if phys then (==) else (=) in
    function
    | [] -> []
    | hd :: tl when check x hd -> tl
    | hd :: tl -> hd :: remove ~phys x tl

  let rec remove_all x = function
    | [] -> []
    | hd :: tl when x = hd -> remove_all x tl
    | hd :: tl -> hd :: remove_all x tl

  let rec same ~f l1 l2 = match l1, l2 with
    | [], [] -> true
    | (hd1 :: tl1), (hd2 :: tl2) when f hd1 hd2 -> same ~f tl1 tl2
    | _, _ -> false

  (* [length_lessthan n l] returns
   *   Some (List.length l) if List.length l <= n
   *   None otherwise *)
  let length_lessthan n l =
    let rec aux i = function
      | _ :: xs when i < n -> aux (succ i) xs
      | [] -> Some i
      | _ -> None
    in
    aux 0 l

  let filter_dup' ~equiv lst =
    let tbl = Hashtbl.create 17 in
    let f a b =
      let b' = equiv b in
      if Hashtbl.mem tbl b'
      then a
      else (Hashtbl.add tbl b' (); b :: a)
    in
    rev (fold_left ~f ~init:[] lst)

  let filter_dup lst = filter_dup' ~equiv:(fun x -> x) lst

  let rec merge_cons ~f = function
    | a :: ((b :: tl) as tl') ->
      begin match f a b with
        | Some a' -> merge_cons ~f (a' :: tl)
        | None -> a :: merge_cons ~f tl'
      end
    | tl -> tl

  let rec take_while ~f = function
    | x :: xs when f x -> x :: take_while ~f xs
    | _ -> []

  let rec drop_while ~f = function
    | x :: xs when f x -> drop_while ~f xs
    | xs -> xs

  let rec take_n acc n = function
    | x :: xs when n > 0 -> take_n (x :: acc) (n - 1) xs
    | _ -> List.rev acc
  let take_n n l = take_n [] n l

  let rec drop_n n = function
    | _ :: xs when n > 0 -> drop_n (n - 1) xs
    | xs -> xs

  let rec split_n acc n = function
    | x :: xs when n > 0 -> split_n (x :: acc) (n - 1) xs
    | xs -> List.rev acc, xs
  let split_n n l = split_n [] n l

  let rec split3 xs ys zs = function
    | (x,y,z) :: tl -> split3 (x :: xs) (y :: ys) (z :: zs) tl
    | [] -> List.rev xs, List.rev ys, List.rev zs
  let split3 l = split3 [] [] [] l

  let rec unfold ~f a = match f a with
    | None -> []
    | Some a -> a :: unfold ~f a

  let rec rev_unfold acc ~f a = match f a with
    | None -> acc
    | Some a -> rev_unfold (a :: acc) ~f a

  let rec fold_n_map ~f ~init = function
    | [] -> init, []
    | x :: xs ->
      let acc, x' = f init x in
      let acc, xs' = fold_n_map ~f ~init:acc xs in
      acc, (x' :: xs')

  module Lazy = struct
    type 'a t =
      | Nil
      | Cons of 'a * 'a t lazy_t

    let rec map ~f = function
      | Nil -> Nil
      | Cons (hd,tl) ->
         Cons (f hd, lazy (map ~f (Lazy.force tl)))

    let rec to_strict = function
      | Nil -> []
      | Cons (hd, lazy tl) -> hd :: to_strict tl

    let rec unfold f a = match f a with
      | None -> Nil
      | Some a -> Cons (a, lazy (unfold f a))

    let rec filter_map ~f = function
      | Nil -> Nil
      | Cons (a, tl) -> match f a with
        | None -> filter_map ~f (Lazy.force tl)
        | Some a' -> Cons (a', lazy (filter_map ~f (Lazy.force tl)))
  end

  let rec last = function
    | [] -> None
    | [x] -> Some x
    | _ :: l -> last l

  let rec group_by pred group acc = function
    | [] -> List.rev acc
    | x :: xs ->
      match group with
      | (x' :: _) when pred x x' ->
        group_by pred (x :: group) acc xs
      | _ -> group_by pred [x] (group :: acc) xs

  let group_by pred xs =
    match group_by pred [] [] xs with
    | [] :: xs | xs -> xs

  (* Merge sorted lists *)
  let rec merge ~cmp l1 l2 = match l1, l2 with
    | l, [] | [], l -> l
    | (x1 :: _), (x2 :: x2s) when cmp x1 x2 > 0 ->
      x2 :: merge ~cmp l1 x2s
    | x1 :: x1s, _ ->
      x1 :: merge ~cmp x1s l2

  let rec uniq ~cmp = function
    | x1 :: (x2 :: _ as xs) when cmp x1 x2 = 0 -> uniq ~cmp xs
    | x :: xs  -> x :: uniq ~cmp xs
    | [] -> []

  let sort_uniq ~cmp l =
    uniq ~cmp (sort ~cmp l)

  let print f () l =
    "[ " ^ String.concat "; " (List.map (f ()) l) ^ " ]"
end

module Option = struct
  let bind opt ~f =
    match opt with
    | None -> None
    | Some x -> f x

  let map ~f = function
    | None -> None
    | Some x -> Some (f x)

  let get = function
    | None -> raise Not_found
    | Some x -> x

  let value ~default = function
    | None -> default
    | Some x -> x

  let value_map ~f ~default = function
    | None -> default
    | Some x -> f x

  let iter ~f = function
    | None -> ()
    | Some x -> f x

  let cons o xs = match o with
    | None -> xs
    | Some x -> x :: xs

  module Infix = struct
    let return x  = Some x
    let (>>=) x f = bind x ~f
    let (>>|) x f = map  x ~f
  end

  include Infix

  let to_list = function
    | None -> []
    | Some x -> [x]

  let is_some = function
    | None -> false
    | _ -> true

  let plus a b = match a with
    | Some _ -> a
    | None -> b

  let print f () = function
    | None -> "None"
    | Some s -> "Some (" ^ f () s ^ ")"
end

module Result = struct
  type ('a, 'e) t = ('a, 'e) Result.result =
  | Ok of 'a
  | Error of 'e
end

module String = struct
  include StringLabels

  let for_all f t =
    let len = String.length t in
    let rec loop i =
      i = len || (f t.[i] && loop (i + 1))
    in
    loop 0


  let reverse s1 =
    let len = length s1 in
    let s2  = Bytes.make len 'a' in
    for i = 0 to len - 1 do
      Bytes.set s2 i s1.[len - i - 1]
    done ;
    Bytes.to_string s2

  let common_prefix_len s1 s2 =
    let rec aux i =
      if i >= length s1 || i >= length s2 || s1.[i] <> s2.[i] then i else
      aux (succ i)
    in
    aux 0

  (* [is_prefixed ~by s] returns [true] iff [by] is a prefix of [s] *)
  let is_prefixed ~by =
    let l = String.length by in
    fun s ->
    let l' = String.length s in
    (l' >= l) &&
      (try for i = 0 to pred l do
             if s.[i] <> by.[i] then
               raise Not_found
           done;
           true
       with Not_found -> false)

  (* Drop characters from beginning of string *)
  let drop n s = sub s ~pos:n ~len:(length s - n)

  module Set = struct
    include MoreLabels.Set.Make (struct type t = string let compare = compare end)
    let of_list l = List.fold_left ~f:(fun s elt -> add elt s) l ~init:empty
    let to_list s = fold ~f:(fun x xs -> x :: xs) s ~init:[]
  end

  module Map = struct
    include MoreLabels.Map.Make (struct type t = string let compare = compare end)
    let of_list l =
      List.fold_left ~f:(fun m (k,v) -> add ~key:k ~data:v m) l ~init:empty
    let to_list m = fold ~f:(fun ~key ~data xs -> (key,data) :: xs) m ~init:[]

    let keys   m = fold ~f:(fun ~key ~data:_ xs -> key  :: xs) m ~init:[]
    let values m = fold ~f:(fun ~key:_ ~data xs -> data :: xs) m ~init:[]

    let add_multiple key data t =
      let current =
        try find key t
        with Not_found -> []
      in
      let data = data :: current in
      add ~key ~data t
  end

  let mem c s =
    try ignore (String.index s c : int); true
    with Not_found -> false

  let first_double_underscore_end s =
    let len = String.length s in
    let rec aux i =
      if i > len - 2 then raise Not_found else
      if s.[i] = '_' && s.[i + 1] = '_' then i + 1
      else aux (i + 1)
    in
    aux 0

  let no_double_underscore s =
    try ignore (first_double_underscore_end s); false
    with Not_found -> true

  let trim = function "" -> "" | str ->
    let l = String.length str in
    let is_space = function
      | ' ' | '\n' | '\t' | '\r' -> true
      | _ -> false
    in
    let r0 = ref 0 and rl = ref l in
    while !r0 < l && is_space str.[!r0] do incr r0 done;
    let r0 = !r0 in
    while !rl > r0 && is_space str.[!rl - 1] do decr rl done;
    let rl = !rl in
    if r0 = 0 && rl = l then str else sub str ~pos:r0 ~len:(rl - r0)

  let print () s = Printf.sprintf "%S" s

  (* FIXME: Remove once we drop support for 4.02 and replace the calls by their
     [_ascii] version. *)
  [@@@ocaml.warning "-3"]

  let capitalize = capitalize
  let uncapitalize = uncapitalize

  let lowercase = lowercase
  let uppercase = uppercase

  let split_on_char_ c s =
    match String.index s c with
    | exception Not_found -> [s]
    | p ->
      let rec loop i =
        match String.index_from s i c with
        | exception Not_found -> [String.sub s i (String.length s - i)]
        | j ->
          let s0 = String.sub s i (j - i) in
          s0 :: loop (j + 1)
      in
      let s0 = String.sub s 0 p in
      s0 :: loop (p + 1)

  let chop_prefix ~prefix text =
    let tlen = String.length text in
    let plen = String.length prefix in
    if tlen >= plen then
      try
        for i = 0 to plen - 1 do
          if prefix.[i] <> text.[i] then raise Not_found
        done;
        Some (String.sub text plen (tlen - plen))
      with Not_found -> None
    else
      None

  let next_occurrence ~pattern text from =
    let plen = String.length pattern in
    let last = String.length text - plen in
    let i = ref from and j = ref 0 in
    while !i <= last && !j < plen do
      if text.[!i + !j] <> pattern.[!j]
      then (incr i; j := 0)
      else incr j
    done;
    if !j < plen then
      raise Not_found
    else
      !i

  let replace_all ~pattern ~with_ text =
    if pattern = "" then text else
      match next_occurrence ~pattern text 0 with
      | exception Not_found -> text
      | j0 ->
        let buffer = Buffer.create (String.length text) in
        let rec aux i j =
          Buffer.add_substring buffer text i (j - i);
          Buffer.add_string buffer with_;
          let i' = j + String.length pattern in
          match next_occurrence ~pattern text i' with
          | exception Not_found ->
            Buffer.add_substring buffer text i' (String.length text - i')
          | j' -> aux i' j'
        in
        aux 0 j0;
        Buffer.contents buffer
end

let sprintf = Printf.sprintf

module Format = struct
  include Format

  let default_width = ref 0

  let to_string ?(width= !default_width) () =
    let b = Buffer.create 32 in
    let ppf = formatter_of_buffer b in
    let contents () =
      pp_print_flush ppf ();
      Buffer.contents b
    in
    pp_set_margin ppf width;
    ppf, contents
end

module Lexing = struct

  type position = Lexing.position = {
    pos_fname : string;
    pos_lnum : int;
    pos_bol : int;
    pos_cnum : int;
  }

  include (Lexing : module type of struct include Lexing end
           with type position := position)

  let move buf p =
    buf.lex_abs_pos <- (p.pos_cnum - buf.lex_curr_pos);
    buf.lex_curr_p <- p

  let from_strings ?empty ?position source refill =
    let pos = ref 0 in
    let len = ref (String.length source) in
    let source = ref source in
    let lex_fun buf size =
      let count = min (!len - !pos) size in
      let count =
        if count <= 0 then
          begin
            source := refill ();
            len := String.length !source;
            pos := 0;
            min !len size
          end
        else count
      in
      if count <= 0 then 0
      else begin
          String.blit ~src:!source ~src_pos:!pos ~dst:buf ~dst_pos:0 ~len:count;
          pos := !pos + count;
          (match empty with None -> () | Some r -> r := !pos >= !len);
          count
        end
    in
    let buf = from_function lex_fun in
    Option.iter ~f:(move buf) position;
    buf

  (* Manipulating position *)
  let make_pos ?(pos_fname="") (pos_lnum, pos_cnum) =
    { pos_fname ; pos_lnum ; pos_cnum ; pos_bol = 0 }

  let column pos = pos.pos_cnum - pos.pos_bol

  let set_column pos col = {pos with pos_cnum = pos.pos_bol + col}

  let split_pos pos = (pos.pos_lnum, column pos)

  let compare_pos p1 p2 =
    match compare p1.pos_lnum p2.pos_lnum with
    | 0 -> compare (column p1) (column p2)
    | n -> n

  let print_position () p =
    let l1, c1 = split_pos p in
    sprintf "%d:%d" l1 c1

  (* Current position in lexer, even if the buffer is in the middle of a refill
     operation *)
  let immediate_pos buf =
    {buf.lex_curr_p with pos_cnum = buf.lex_abs_pos + buf.lex_curr_pos}

  let json_of_position pos =
    let line, col = split_pos pos in
    `Assoc ["line", `Int line; "col", `Int col]

  let min_pos p1 p2 =
    if compare_pos p1 p2 <= 0 then p1 else p2

  let max_pos p1 p2 =
    if compare_pos p1 p2 >= 0 then p1 else p2
end

module Char = struct

  (* FIXME: Remove once we drop support for 4.02 and replace the calls to
     [uppercase] and [lowercase] by their [_ascii] version. *)
  [@@@ocaml.warning "-3"]

  include Char
  let is_lowercase c = lowercase c = c
  let is_uppercase c = uppercase c = c
  let is_strictly_lowercase c = not (is_uppercase c)
  let is_strictly_uppercase c = not (is_lowercase c)
end

module Glob : sig
  type pattern =
    | Wildwild
    | Exact of string
    | Regexp of Str.regexp
  val compile_pattern : string -> pattern
  val match_pattern : pattern -> string -> bool
end = struct
  type pattern =
    | Wildwild
    | Exact of string
    | Regexp of Str.regexp

  let compile_pattern = function
    | "**" -> Wildwild
    | pattern ->
      let regexp = Buffer.create 15 in
      let chunk = Buffer.create 15 in
      let flush () =
        if Buffer.length chunk > 0 then (
          Buffer.add_string regexp (Str.quote (Buffer.contents chunk));
          Buffer.clear chunk;
        )
      in
      let l = String.length pattern in
      let i = ref 0 in
      while !i < l do
        begin match pattern.[!i] with
          | '\\' -> incr i; if !i  < l then Buffer.add_char chunk pattern.[!i]
          | '*' -> flush (); Buffer.add_string regexp ".*";
          | '?' -> flush (); Buffer.add_char regexp '.';
          | x -> Buffer.add_char chunk x
        end;
        incr i
      done;
      if Buffer.length regexp = 0 then
        Exact (Buffer.contents chunk)
      else (
        flush ();
        Buffer.add_char regexp '$';
        Regexp (Str.regexp (Buffer.contents regexp))
      )

  let match_pattern re str =
    match re with
    | Wildwild -> true
    | Regexp re -> Str.string_match re str 0
    | Exact s -> s = str
end

let fprintf = Format.fprintf

let lazy_eq a b =
  match Lazy.is_val a, Lazy.is_val b with
  | true, true -> Lazy.force_val a == Lazy.force_val b
  | false, false -> a == b
  | _ -> false

let let_ref r v f =
  let v' = !r in
  r := v;
  match f () with
  | result -> r := v'; result
  | exception exn -> r := v'; raise exn

let failwithf fmt = Printf.ksprintf failwith fmt

module Shell = struct
  let split_command str =
    let comps = ref [] in
    let dirty = ref false in
    let buf   = Buffer.create 16 in
    let flush () =
      if !dirty then (
        comps := Buffer.contents buf :: !comps;
        dirty := false;
        Buffer.clear buf;
      )
    in
    let i = ref 0 and len = String.length str in
    let unescape = function
      | 'n' -> '\n'
      | 'r' -> '\r'
      | 't' -> '\t'
      |  x  -> x
    in
    while !i < len do
      let c = str.[!i] in
      incr i;
      match c with
      | ' ' | '\t' | '\n' | '\r' -> flush ()
      | '\\' ->
        dirty := true;
        if !i < len then (
          Buffer.add_char buf (unescape str.[!i]);
          incr i
        )
      | '\'' ->
        dirty := true;
        while !i < len && str.[!i] <> '\'' do
          Buffer.add_char buf str.[!i];
          incr i;
        done;
        incr i
      | '"' ->
        dirty := true;
        while !i < len && str.[!i] <> '"' do
          (match str.[!i] with
           | '\\' ->
             incr i;
             if !i < len then
               Buffer.add_char buf (unescape str.[!i]);
           | x -> Buffer.add_char buf x
          );
          incr i;
        done;
        incr i
      | x ->
        dirty := true;
        Buffer.add_char buf x
    done;
    flush ();
    List.rev !comps
end

  (* [modules_in_path ~ext path] lists ocaml modules corresponding to
   * filenames with extension [ext] in given [path]es.
   * For instance, if there is file "a.ml","a.mli","b.ml" in ".":
   * - modules_in_path ~ext:".ml" ["."] returns ["A";"B"],
   * - modules_in_path ~ext:".mli" ["."] returns ["A"] *)
let modules_in_path ~ext path =
  let seen = Hashtbl.create 7 in
  List.fold_left ~init:[] path
  ~f:begin fun results dir ->
    try
      Array.fold_left
      begin fun results file ->
        if Filename.check_suffix file ext
        then let name = Filename.chop_extension file in
             (if Hashtbl.mem seen name
              then results
              else
               (Hashtbl.add seen name (); String.capitalize name :: results))
        else results
      end results (Sys.readdir dir)
    with Sys_error _ -> results
  end

let file_contents filename =
  let ic = open_in filename in
  try
    let str = Bytes.create 1024 in
    let buf = Buffer.create 1024 in
    let rec loop () =
      match input ic str 0 1024 with
      | 0 -> ()
      | n ->
        Buffer.add_subbytes buf str 0 n;
        loop ()
    in
    loop ();
    close_in_noerr ic;
    Buffer.contents buf
  with exn ->
    close_in_noerr ic;
    raise exn

external reraise : exn -> 'a = "%reraise"

type 'a with_workdir = {
  workdir : string;
  workval : 'a;
}
(** Some value that must be interpreted with respect to a specific work
    directory. (e.g. for resolving relative paths or executing sub-commands *)

let dump_with_workdir f x : json =
  `Assoc [
    "workdir", `String x.workdir;
    "workval", f x.workval;
  ]
