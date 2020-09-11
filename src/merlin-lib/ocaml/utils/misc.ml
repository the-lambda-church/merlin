(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module CamlString = String

open Std

(* Errors *)

exception Fatal_error of string * Printexc.raw_backtrace

let () = Printexc.register_printer (function
    | Fatal_error (msg, bt) ->
      Some (Printf.sprintf "Fatal error: %s\n%s"
              msg (Printexc.raw_backtrace_to_string bt))
    | _ -> None
  )

let fatal_error msg =
  raise (Fatal_error (msg, Printexc.get_callstack 50))

let fatal_errorf fmt =
  (*Format.kasprintf is not available in 4.02.3 *)
  (*Format.kasprintf fatal_error fmt*)
  ignore (Format.flush_str_formatter ());
  Format.kfprintf
    (fun _ppf -> fatal_error (Format.flush_str_formatter ()))
    Format.str_formatter fmt

(* Exceptions *)

let try_finally ?(always=(fun () -> ())) ?(exceptionally=(fun () -> ())) work =
  match work () with
    | result ->
      begin match always () with
        | () -> result
        | exception always_exn ->
          (* raise_with_backtrace is not available before OCaml 4.05 *)
          (*let always_bt = Printexc.get_raw_backtrace () in*)
          exceptionally ();
          (*Printexc.raise_with_backtrace always_exn always_bt*)
          raise always_exn
      end
    | exception work_exn ->
      (*let work_bt = Printexc.get_raw_backtrace () in*)
      begin match always () with
        | () ->
          exceptionally ();
          (*Printexc.raise_with_backtrace work_exn work_bt*)
          raise work_exn
        | exception always_exn ->
          (*let always_bt = Printexc.get_raw_backtrace () in*)
          exceptionally ();
          (*Printexc.raise_with_backtrace always_exn always_bt*)
          raise always_exn
      end

type ref_and_value = R : 'a ref * 'a -> ref_and_value

let protect_refs =
  let set_refs l = List.iter ~f:(fun (R (r, v)) -> r := v) l in
  fun refs f ->
    let backup = List.map ~f:(fun (R (r, _)) -> R (r, !r)) refs in
    set_refs refs;
    match f () with
    | x           -> set_refs backup; x
    | exception e -> set_refs backup; raise e

(* List functions *)

let map_end f l1 l2 = List.map_end ~f l1 l2

let rec map_left_right f = function
    [] -> []
  | hd::tl -> let res = f hd in res :: map_left_right f tl

let for_all2 pred l1 l2 = List.for_all2 ~f:pred l1 l2

let replicate_list = List.replicate

let list_remove x = List.remove ~phys:false x

let rec split_last = function
    [] -> assert false
  | [x] -> ([], x)
  | hd :: tl ->
      let (lst, last) = split_last tl in
      (hd :: lst, last)

(* Options *)

let may f x = Option.iter ~f x
let may_map f x = Option.map ~f x

(* File functions *)

let remove_file filename =
  try  Sys.remove filename
  with Sys_error _msg -> ()

let rec split_path path acc =
  match Filename.dirname path, Filename.basename path with
  | dir, _ when dir = path -> dir :: acc
  | dir, base -> split_path dir (base :: acc)

(* Deal with case insensitive FS *)

external fs_exact_case : string -> string = "ml_merlin_fs_exact_case"

(* A replacement for sys_file_exists that makes use of stat_cache *)
module Exists_in_directory = File_cache.Make(struct
    let cache_name = "Exists_in_directory"
    type t = string -> bool
    let read dir =
      if Sys.file_exists dir &&
         Sys.is_directory dir
      then
        let cache = Hashtbl.create 4 in
        (fun filename ->
           match Hashtbl.find cache filename with
           | x -> x
           | exception Not_found ->
             let exists = Sys.file_exists (Filename.concat dir filename) in
             Hashtbl.add cache filename exists;
             exists)
      else (fun _ -> false)
  end)

let exact_file_exists ~dirname ~basename =
  Exists_in_directory.read dirname basename &&
  let path = Filename.concat dirname basename in
  let path' = fs_exact_case path in
  path == path' || basename = Filename.basename path'

let canonicalize_filename ?cwd path =
  let parts =
    match split_path path [] with
    | dot :: rest when dot = Filename.current_dir_name ->
      split_path (match cwd with None -> Sys.getcwd () | Some c -> c) rest
    | parts -> parts
  in
  let goup path = function
    | dir when dir = Filename.parent_dir_name ->
      (match path with _ :: t -> t | [] -> [])
    | dir when dir = Filename.current_dir_name ->
      path
    | dir -> dir :: path
  in
  let parts = List.rev (List.fold_left ~f:goup ~init:[] parts) in
  let filename_concats = function
    | [] -> ""
    | root :: subs -> List.fold_left ~f:Filename.concat ~init:root subs
  in
  fs_exact_case (filename_concats parts)

let rec expand_glob ~filter acc root = function
  | [] -> root :: acc
  | Glob.Wildwild :: _tl -> (* FIXME: why is tl not used? *)
    let rec append acc root =
      let items = try Sys.readdir root with Sys_error _ -> [||] in
      let process acc dir =
        let filename = Filename.concat root dir in
        if filter filename
        then append (filename :: acc) filename
        else acc
      in
      Array.fold_left process (root :: acc) items
    in
    append acc root
  | Glob.Exact component :: tl ->
    let filename = Filename.concat root component in
    expand_glob ~filter acc filename tl
  | pattern :: tl ->
    let items = try Sys.readdir root with Sys_error _ -> [||] in
    let process acc dir =
      if Glob.match_pattern pattern dir then
        let root' = Filename.concat root dir in
        if filter root' then
          expand_glob ~filter acc root' tl
        else acc
      else acc
    in
    Array.fold_left process acc items

let expand_glob ?(filter=fun _ -> true) path acc =
  match split_path path [] with
  | [] -> acc
  | root :: subs ->
    let patterns = List.map ~f:Glob.compile_pattern subs in
    expand_glob ~filter acc root patterns

let find_in_path path name =
  canonicalize_filename
  begin
    if not (Filename.is_implicit name) then
      if exact_file_exists
          ~dirname:(Filename.dirname name)
          ~basename:(Filename.basename name)
      then name
      else raise Not_found
    else List.find_map path ~f:(fun dirname ->
        if exact_file_exists ~dirname ~basename:name
        then Some (Filename.concat dirname name)
        else None
      )
  end

let find_in_path_rel path name =
  let rec simplify s =
    let open Filename in
    let base = basename s in
    let dir = dirname s in
    if dir = s then dir
    else if base = current_dir_name then simplify dir
    else concat (simplify dir) base
  in
  let rec try_dir = function
    | [] -> raise Not_found
    | dir::rem ->
      let dir = simplify dir in
      if Exists_in_directory.read dir name
      then Filename.concat dir name
      else try_dir rem
  in try_dir path

let find_in_path_uncap ?(fallback="") path name =
  let has_fallback = fallback <> "" in
  canonicalize_filename
  begin
    let uname = String.uncapitalize name in
    let ufallback = String.uncapitalize fallback in
    List.find_map path ~f:(fun dirname ->
        if exact_file_exists ~dirname ~basename:uname
        then Some (Filename.concat dirname uname)
        else if exact_file_exists ~dirname ~basename:name
        then Some (Filename.concat dirname name)
        else if has_fallback && exact_file_exists ~dirname ~basename:ufallback
        then Some (Filename.concat dirname ufallback)
        else if has_fallback && exact_file_exists ~dirname ~basename:fallback
        then Some (Filename.concat dirname fallback)
        else None
      )
  end

(* Expand a -I option: if it starts with +, make it relative to the standard
   library directory *)

let expand_directory alt s =
  if String.length s > 0 && s.[0] = '+'
  then Filename.concat alt
                       (String.sub s ~pos:1 ~len:(String.length s - 1))
  else s

(* Hashtable functions *)

let create_hashtable size init =
  let tbl = Hashtbl.create size in
  List.iter ~f:(fun (key, data) -> Hashtbl.add tbl key data) init;
  tbl

(* File copy *)

let copy_file ic oc =
  let buff = Bytes.create 0x1000 in
  let rec copy () =
    let n = input ic buff 0 0x1000 in
    if n = 0 then () else (output oc buff 0 n; copy())
  in copy()

let copy_file_chunk ic oc len =
  let buff = Bytes.create 0x1000 in
  let rec copy n =
    if n <= 0 then () else begin
      let r = input ic buff 0 (min n 0x1000) in
      if r = 0 then raise End_of_file else (output oc buff 0 r; copy(n-r))
    end
  in copy len

let string_of_file ic =
  let b = Buffer.create 0x10000 in
  let buff = Bytes.create 0x1000 in
  let rec copy () =
    let n = input ic buff 0 0x1000 in
    if n = 0 then Buffer.contents b else
      (Buffer.add_subbytes b buff 0 n; copy())
  in copy()

let output_to_file_via_temporary ?(mode = [Open_text]) filename fn =
  let (temp_filename, oc) =
    Filename.open_temp_file
       ~mode (*~perms:0o666*) ~temp_dir:(Filename.dirname filename)
       (Filename.basename filename) ".tmp" in
    (* The 0o666 permissions will be modified by the umask.  It's just
       like what [open_out] and [open_out_bin] do.
       With temp_dir = dirname filename, we ensure that the returned
       temp file is in the same directory as filename itself, making
       it safe to rename temp_filename to filename later.
       With prefix = basename filename, we are almost certain that
       the first generated name will be unique.  A fixed prefix
       would work too but might generate more collisions if many
       files are being produced simultaneously in the same directory. *)
  match fn temp_filename oc with
  | res ->
      close_out oc;
      begin try
        Sys.rename temp_filename filename; res
      with exn ->
        remove_file temp_filename; raise exn
      end
  | exception exn ->
      close_out oc; remove_file temp_filename; raise exn

(* Reading from a channel *)

let input_bytes ic n =
  let result = Bytes.create n in
  really_input ic result 0 n;
  result

(* Integer operations *)

let rec log2 n =
  if n <= 1 then 0 else 1 + log2(n asr 1)

let align n a =
  if n >= 0 then (n + a - 1) land (-a) else n land (-a)

let no_overflow_add a b = (a lxor b) lor (a lxor (lnot (a+b))) < 0

let no_overflow_sub a b = (a lxor (lnot b)) lor (b lxor (a-b)) < 0

(* Taken from Hacker's Delight, chapter "Overflow Detection" *)
let no_overflow_mul a b =
  not ((a = min_int && b < 0) || (b <> 0 && (a * b) / b <> a))

let no_overflow_lsl a k =
  0 <= k && k < Sys.word_size - 1 && min_int asr k <= a && a <= max_int asr k

module Int_literal_converter = struct
  (* To convert integer literals, allowing max_int + 1 (PR#4210) *)
  let cvt_int_aux str neg of_string =
    if String.length str = 0 || str.[0]= '-'
    then of_string str
    else neg (of_string ("-" ^ str))
  let int s = cvt_int_aux s (~-) int_of_string
  let int32 s = cvt_int_aux s Int32.neg Int32.of_string
  let int64 s = cvt_int_aux s Int64.neg Int64.of_string
  let nativeint s = cvt_int_aux s Nativeint.neg Nativeint.of_string
end

(* String operations *)

let chop_extension_if_any fname =
  try Filename.chop_extension fname with Invalid_argument _ -> fname

let chop_extensions file =
  let dirname = Filename.dirname file and basename = Filename.basename file in
  try
    let pos = String.index basename '.' in
    let basename = String.sub basename ~pos:0 ~len:pos in
    if Filename.is_implicit file && dirname = Filename.current_dir_name then
      basename
    else
      Filename.concat dirname basename
  with Not_found -> file

let search_substring pat str start =
  let rec search i j =
    if j >= String.length pat then i
    else if i + j >= String.length str then raise Not_found
    else if str.[i + j] = pat.[j] then search i (j+1)
    else search (i+1) 0
  in search start 0

let replace_substring ~before ~after str =
  let rec search acc curr =
    match search_substring before str curr with
      | next ->
         let prefix = String.sub str ~pos:curr ~len:(next - curr) in
         search (prefix :: acc) (next + String.length before)
      | exception Not_found ->
        let suffix = String.sub str ~pos:curr ~len:(String.length str - curr) in
        List.rev (suffix :: acc)
  in String.concat ~sep:after (search [] 0)


let rev_split_string cond s =
  let rec split1 res i =
    if i >= String.length s then res else begin
      if cond s.[i] then
        split1 res (i+1)
      else
        split2 res i (i+1)
    end
  and split2 res i j =
    if j >= String.length s then String.sub s ~pos:i ~len:(j-i) :: res else begin
      if cond s.[j] then
        split1 (String.sub s ~pos:i ~len:(j-i) :: res) (j+1)
      else
        split2 res i (j+1)
    end
  in split1 [] 0

let rev_split_words s =
  let helper = function
    | ' ' | '\t' | '\r' | '\n' -> true
    | _ -> false
  in
  rev_split_string helper s

let rev_string_split ~on s =
  rev_split_string ((=) on) s

let get_ref r =
  let v = !r in
  r := []; v

let set_or_ignore f opt x =
  match f x with
  | None -> ()
  | Some y -> opt := Some y

let fst3 (x, _, _) = x
let snd3 (_,x,_) = x
let thd3 (_,_,x) = x

let fst4 (x, _, _, _) = x
let snd4 (_,x,_, _) = x
let thd4 (_,_,x,_) = x
let for4 (_,_,_,x) = x


module LongString = struct
  type t = bytes array

  let create str_size =
    let tbl_size = str_size / Sys.max_string_length + 1 in
    let tbl = Array.make tbl_size Bytes.empty in
    for i = 0 to tbl_size - 2 do
      tbl.(i) <- Bytes.create Sys.max_string_length;
    done;
    tbl.(tbl_size - 1) <- Bytes.create (str_size mod Sys.max_string_length);
    tbl

  let length tbl =
    let tbl_size = Array.length tbl in
    Sys.max_string_length * (tbl_size - 1) + Bytes.length tbl.(tbl_size - 1)

  let get tbl ind =
    Bytes.get tbl.(ind / Sys.max_string_length) (ind mod Sys.max_string_length)

  let set tbl ind c =
    Bytes.set tbl.(ind / Sys.max_string_length) (ind mod Sys.max_string_length)
              c

  let blit src srcoff dst dstoff len =
    for i = 0 to len - 1 do
      set dst (dstoff + i) (get src (srcoff + i))
    done

  let output oc tbl pos len =
    for i = pos to pos + len - 1 do
      output_char oc (get tbl i)
    done

  let unsafe_blit_to_bytes src srcoff dst dstoff len =
    for i = 0 to len - 1 do
      Bytes.unsafe_set dst (dstoff + i) (get src (srcoff + i))
    done

  let input_bytes ic len =
    let tbl = create len in
    Array.iter (fun str -> really_input ic str 0 (Bytes.length str)) tbl;
    tbl
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

let edit_distance a b cutoff =
  let la, lb = String.length a, String.length b in
  let cutoff =
    (* using max_int for cutoff would cause overflows in (i + cutoff + 1);
       we bring it back to the (max la lb) worstcase *)
    min (max la lb) cutoff in
  if abs (la - lb) > cutoff then None
  else begin
    (* initialize with 'cutoff + 1' so that not-yet-written-to cases have
       the worst possible cost; this is useful when computing the cost of
       a case just at the boundary of the cutoff diagonal. *)
    let m = Array.make_matrix (la + 1) (lb + 1) (cutoff + 1) in
    m.(0).(0) <- 0;
    for i = 1 to la do
      m.(i).(0) <- i;
    done;
    for j = 1 to lb do
      m.(0).(j) <- j;
    done;
    for i = 1 to la do
      for j = max 1 (i - cutoff - 1) to min lb (i + cutoff + 1) do
        let cost = if a.[i-1] = b.[j-1] then 0 else 1 in
        let best =
          (* insert, delete or substitute *)
          min (1 + min m.(i-1).(j) m.(i).(j-1)) (m.(i-1).(j-1) + cost)
        in
        let best =
          (* swap two adjacent letters; we use "cost" again in case of
             a swap between two identical letters; this is slightly
             redundant as this is a double-substitution case, but it
             was done this way in most online implementations and
             imitation has its virtues *)
          if not (i > 1 && j > 1 && a.[i-1] = b.[j-2] && a.[i-2] = b.[j-1])
          then best
          else min best (m.(i-2).(j-2) + cost)
        in
        m.(i).(j) <- best
      done;
    done;
    let result = m.(la).(lb) in
    if result > cutoff
    then None
    else Some result
  end

let spellcheck env name =
  let cutoff =
    match String.length name with
      | 1 | 2 -> 0
      | 3 | 4 -> 1
      | 5 | 6 -> 2
      | _ -> 3
  in
  let compare target acc head =
    match edit_distance target head cutoff with
    | None -> acc
    | Some dist ->
      let (best_choice, best_dist) = acc in
      if dist < best_dist then ([head], dist)
      else if dist = best_dist then (head :: best_choice, dist)
      else acc
  in
  fst (List.fold_left ~f:(compare name) ~init:([], max_int) env)

let did_you_mean ppf get_choices =
  (* flush now to get the error report early, in the (unheard of) case
     where the search in the get_choices function would take a bit of
     time; in the worst case, the user has seen the error, she can
     interrupt the process before the spell-checking terminates. *)
  Format.fprintf ppf "@?";
  match get_choices () with
  | [] -> ()
  | choices ->
     let rest, last = split_last choices in
     Format.fprintf ppf "@\nHint: Did you mean %s%s%s?@?"
       (String.concat ~sep:", " rest)
       (if rest = [] then "" else " or ")
       last

let cut_at s c =
  let pos = String.index s c in
  String.sub s ~pos:0 ~len:pos,
  String.sub s ~pos:(pos+1) ~len:(String.length s - pos - 1)

let time_spent () =
  let open Unix in
  let t = times () in
  ((t.tms_utime +. t.tms_stime +. t.tms_cutime +. t.tms_cstime) *. 1000.0)

let normalise_eol s =
  let b = Buffer.create 80 in
    for i = 0 to String.length s - 1 do
      if s.[i] <> '\r' then Buffer.add_char b s.[i]
    done;
    Buffer.contents b

let unitname filename =
  let unitname =
    try String.sub filename ~pos:0 ~len:(String.index filename '.')
    with Not_found -> filename
  in
  String.capitalize unitname

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

module String = struct
  include CamlString
  module Ord = struct
    type t = string
    let compare = String.compare
  end
  module Set = Set.Make (Ord)
  module Map = Map.Make (Ord)
  module Tbl = Hashtbl.Make (struct
      type t = string
      let equal (x : string) (y : string) : bool = (x = y)
      let hash = Hashtbl.hash
    end)
end


type filepath = string
type modname = string
type crcs = (modname * Digest.t option) list

type alerts = string String.Map.t


module EnvLazy = struct
  type ('a,'b) t = ('a,'b) eval ref

  and ('a,'b) eval =
    | Done of 'b
    | Raise of exn
    | Thunk of 'a

  type undo =
    | Nil
    | Cons : ('a, 'b) t * 'a * undo -> undo

  type log = undo ref

  let force f x =
    match !x with
    | Done x -> x
    | Raise e -> raise e
    | Thunk e ->
        match f e with
        | y ->
          x := Done y;
          y
        | exception e ->
          x := Raise e;
          raise e

  let get_arg x =
    match !x with Thunk a -> Some a | _ -> None

  let create x =
    ref (Thunk x)

  let create_forced y =
    ref (Done y)

  let create_failed e =
    ref (Raise e)

  let log () =
    ref Nil

  let force_logged log f x =
    let open! Result_compat in (* merlin *)
    match !x with
    | Done x -> x
    | Raise e -> raise e
    | Thunk e ->
      match f e with
      | (Error _ as err : _ result) ->
          x := Done err;
          log := Cons(x, e, !log);
          err
      | Ok _ as res ->
          x := Done res;
          res
      | exception e ->
          x := Raise e;
          raise e

  let backtrack log =
    let rec loop = function
      | Nil -> ()
      | Cons(x, e, rest) ->
          x := Thunk e;
          loop rest
    in
    loop !log

  (* For compatibility with 4.02 and 4.03 *)

  let is_val t = match !t with
    | Done _ -> true
    | Raise _ | Thunk _ -> false

  let view t = !t

  (* For compatibility with 4.08 and 4.09 *)

  let force_logged_408 log f x =
    match !x with
    | Done x -> x
    | Raise e -> raise e | Thunk e ->
      match f e with
      | None ->
          x := Done None;
          log := Cons(x, e, !log);
          None
      | Some _ as y ->
          x := Done y;
          y
      | exception e ->
          x := Raise e;
          raise e


end
