open MenhirSdk
open Cmly_api
open Utils

module type S = sig
  module G : GRAMMAR

  type item = G.lr1 * G.production * int
  type recovery = G.lr1 -> int * (G.lr1 option * item list) list

  val recover : recovery
  val report : Format.formatter -> unit
end

module Make (G : GRAMMAR)
    (S : Synthesis.S with module G = G) : S with module G = G = struct
  module G = G
  open G

  type item = lr1 * production * int

  type recovery = lr1 -> int * (lr1 option * item list) list

(*
  let item_to_string (st, prod, p) =
    Printf.sprintf "(#%d, p%d, %d)" (Lr1.to_int st) (Production.to_int prod) p
*)

  type trace = Trace of float * item list

  module Trace = struct
    type t = trace
    let min = arg_min_float (fun (Trace (c,_)) -> c)

    let cat (Trace (c1, tr1)) (Trace (c2, tr2)) =
      Trace (c1 +. c2, tr1 @ tr2)

(*
    let to_string (Trace (c1, tr)) =
      Printf.sprintf "Trace (%f, %s)"
        c1 (list_fmt item_to_string tr)
*)
  end

  module State = struct
    type level = (nonterminal * Trace.t) list
    type t = level list

    let rec merge_level l1 l2 : level = match l1, l2 with
      | [], l -> l
      | l, [] -> l
      | ((nt1, c1) :: xs1), (x2 :: xs2) ->
          let (nt2, c2) = x2 in
          match compare nt1 nt2 with
          | 0 ->
              let x = (nt1, Trace.min c1 c2) in
              x :: merge_level xs1 xs2
          | n when n > 0 -> x2 :: merge_level l1 xs2
          | _ -> (nt1, c1) :: merge_level xs1 l2

    let rec merge l1 l2 : t = match l1, l2 with
      | [], l -> l
      | l, [] -> l
      | (x1 :: l1), (x2 :: l2) ->
          let x' = merge_level x1 x2 in
          x' :: merge l1 l2

(*
    let reduction_to_string (n, tr) =
      Printf.sprintf "(%s, %s)" (Nonterminal.name n) (Trace.to_string tr)

    let to_string (t : t) = list_fmt (list_fmt reduction_to_string) t
*)
  end

  let synthesize =
    let rec add_nt tr nt = function
      | [] -> [(nt, tr)]
      | x :: xs ->
          let c = compare nt (fst x) in
          if c = 0 then (nt, Trace.min tr (snd x)) :: xs
          else if c < 0 then
            (nt, tr) :: xs
          else
            x :: add_nt tr nt xs
    in
    let add_item cost item stack =
      let (_, prod, pos) = item in
      if cost = infinity then stack
      else
        let stack_hd = function
          | [] -> []
          | x :: _ -> x
        and stack_tl = function
          | [] -> []
          | _ :: xs -> xs
        in
        let rec aux stack = function
          | 0 -> add_nt (Trace (cost, [item])) (Production.lhs prod)
                   (stack_hd stack) :: stack_tl stack
          | n -> stack_hd stack :: aux (stack_tl stack) (n - 1)
        in
        aux stack pos
    in
    Lr1.tabulate (fun st ->
        List.fold_left (fun acc (prod, pos) ->
            if pos = 0 then (
              (*if prod.p_kind = `START then ( *)
              (* pos = 0 means we are on an initial state *)
              (*report "skipping %s at depth %d\n" prod.p_lhs.n_name pos;*)
              acc
            ) else (
              (*report "adding %s at depth %d\n" prod.p_lhs.n_name pos;*)
              add_item
                (S.cost_of (S.Tail (st, prod, pos)))
                (st, prod, pos) acc
            )
          )
          [] (Lr0.items (Lr1.lr0 st))
      )

  let step st ntss =
    let seen = ref CompressedBitSet.empty in
    let rec aux = function
      | [] -> []
      | ((nt, tr) :: x) :: xs
        when not (CompressedBitSet.mem (Nonterminal.to_int nt) !seen) &&
             not (Nonterminal.kind nt = `START) ->
          seen := CompressedBitSet.add (Nonterminal.to_int nt) !seen;
          let st' = List.assoc (N nt) (Lr1.transitions st) in
          let xs' = synthesize st' in
          let xs' = match xs' with
            | [] -> []
            | _ :: xs -> xs
          in
          let merge_trace (nt,tr') = (nt, Trace.cat tr' tr) in
          let xs' = List.map (List.map merge_trace) xs' in
          aux (State.merge xs' (x :: xs))
      | (_ :: x) :: xs -> aux (x :: xs)
      | [] :: xs -> xs
    in
    aux ntss

  let init st = ((st, [st]), step st (synthesize st))

  let expand ((st, sts), nts) =
    List.map (fun st' -> ((st', st' :: sts), step st' nts)) (S.pred st)

  let recover st =
    (* How big is the known prefix of the stack *)
    let pos =
      let items = Lr0.items (Lr1.lr0 st) in
      List.fold_left (fun pos (_, pos') -> max pos pos')
        (snd (List.hd items)) (List.tl items)
    in
    (* Walk this prefix *)
    let traces =
      let acc = ref [init st] in
      for _i = 1 to pos - 1 do
        acc := List.concat (List.map expand !acc)
      done;
      !acc
    in
    (* Last step *)
    let select_trace traces =
      (* Pick a trace with minimal cost, somewhat arbitrary *)
      match List.flatten traces with
      | [] ->
        (* FIXME: for release, empty list means recovery not possible
           (not enough annotations) *)
        assert false
      | (_, trace) :: alternatives ->
          List.fold_left
            (fun tr1 (_,tr2) -> Trace.min tr1 tr2)
            trace alternatives
    in
    let process_trace trace =
      match expand trace with
      | [] -> (* Initial state *)
          assert (snd trace = []); []
      | states ->
          let select_expansion ((st, _sts), trace') =
            if trace' = [] then
              (* Reached stack bottom *)
              (None, select_trace (snd trace))
            else
              (Some st, select_trace trace')
          in
          List.map select_expansion states
    in
    pos,
    List.flatten @@ List.map (fun trace ->
        List.map
          (fun (st, Trace (_, reductions)) -> st, reductions)
          (process_trace trace)
      ) traces

  let recover = Lr1.tabulate recover

  let report _ppf = ()
end
