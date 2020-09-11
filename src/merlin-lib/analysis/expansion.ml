open Std

type t = Trie of (string * Longident.t * t list lazy_t)

let rec explore_node lident env =
  let add_module name _ _ l =
    let lident = Longident.Ldot (lident, name) in
    Trie (name, lident, lazy (explore_node lident env)) :: l
  in
  Env.fold_modules add_module (Some lident) env []

let explore ?(global_modules=[]) env =
  let seen =
    let tbl = Hashtbl.create 7 in
    fun name -> Hashtbl.mem tbl name || (Hashtbl.add tbl name (); false)
  in
  let add_module l name =
    if seen name then l
    else
      let lident = Longident.Lident name in
      Trie (name, lident, lazy (explore_node lident env)) :: l
  in
  let add_module' name _ _ l = add_module l name in
  List.fold_left ~f:add_module global_modules
    ~init:(Env.fold_modules add_module' None env [])

(* This is a hacked up heuristic spell checking function.
   It checks only the prefix of the key.
   A proper damerau-levenshtein might be better but certainly not urgent.

   Implementation is a fork of
   https://github.com/c-cube/spelll/blob/master/src/spelll.ml
   Thanks companion-cube :) *)
let optimal_string_prefix_alignment key cutoff =
  let equal_char : char -> char -> bool = (=) in
  let min_int x y : int = if x < y then x else y in
  if String.length key = 0
  then (fun str -> String.length str)
  else
    (* distance vectors (v0=previous, v1=current) *)
    let v0 = Array.make (String.length key + 1) 0 in
    let v1 = Array.make (String.length key + 1) 0 in
    fun str ->
      let l1 = min (String.length str) (String.length key) in
      if l1 = 0 then
        String.length key
      else if str = key then
        0
      else
        try
          (* initialize v0: v0(i) = A(0)(i) = delete i chars from t *)
          for i = 0 to String.length key do
            v0.(i) <- i
          done;
          (* main loop for the bottom up dynamic algorithm *)
          for i = 0 to l1 - 1 do
            (* first edit distance is the deletion of i+1 elements from s *)
            v1.(0) <- i+1;

            let min = ref (i+1) in
            (* try add/delete/replace operations *)
            for j = 0 to String.length key - 1 do
              let cost = if equal_char str.[i] key.[j] then 0 else 1 in
              v1.(j+1) <- min_int (v1.(j) + 1) (min_int (v0.(j+1) + 1) (v0.(j) + cost));
              if i > 0 && j > 0 && str.[i] = key.[j-1] && str.[i-1] = key.[j] then
                v1.(j+1) <- min_int v1.(j+1) (v0.(j-1) + cost);

              min := min_int !min v1.(j+1)
            done;

            if !min > cutoff then raise Exit;

            (* copy v1 into v0 for next iteration *)
            Array.blit v1 0 v0 0 (String.length key + 1);
          done;
          let idx = String.length key in
          min v1.(idx-1) v1.(idx)
        with Exit -> cutoff + 1

let spell_index s1 =
  let cutoff = match String.length s1 with
    | 0 -> 0
    | 1 -> 0
    | 2 -> 0
    | 3 -> 1
    | _ -> 2
  in
  let f = optimal_string_prefix_alignment s1 cutoff in
  fun s2 -> (s1 = "" || s2 = "" || (s1.[0] = s2.[0] && (f s2 <= cutoff)))

let spell_match index str = index str

let filter path ts =
  let path = List.map ~f:spell_index path in
  let rec aux_ts ts = function
    | [] -> []
    | p0 :: ps -> List.filter_map ~f:(aux_t p0 ps) ts
  and aux_t p0 ps (Trie (name, ident, ts)) =
    if spell_match p0 name then
      Some (Trie (name, ident, lazy (aux_ts (Lazy.force ts) ps)))
    else
      None
  in
  aux_ts ts path

let rec to_lidents len acc = function
  | Trie (_, lident, _) :: ts when len = 0 ->
    to_lidents len (lident :: acc) ts
  | Trie (_, _, lazy ts') :: ts ->
    to_lidents len (to_lidents (len - 1) acc ts') ts
  | [] -> acc

let to_lidents len ts = to_lidents len [] ts

let get_lidents ts path =
  let open Longident in
  let lident = parse path in
  let lident, last = match lident with
    | Ldot (l, id) -> l, id
    | Lident id -> Lident "", id
    | Lapply _ -> assert false
  in
  let rec components acc = function
    | Lident "" -> acc
    | Lident id -> id :: acc
    | Lapply _ -> assert false
    | Ldot (l, id) -> components (id :: acc) l
  in
  let lidents = match components [] lident with
    | [] -> [None]
    | components ->
      let ts = filter components ts in
      let lidents = to_lidents (List.length components - 1) ts in
      List.map ~f:(fun x -> Some x) lidents
  in
  lidents, last
