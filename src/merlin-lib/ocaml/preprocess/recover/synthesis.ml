open MenhirSdk.Cmly_api
open Utils

module type S = sig
  module G : GRAMMAR

  type variable =
    | Head of G.lr1 * G.nonterminal
    | Tail of G.lr1 * G.production * int

  val variable_to_string : variable -> string

  type 'a paction =
    | Abort
    | Reduce of G.production
    | Shift  of G.symbol
    | Var    of 'a

  val paction_to_string : ('a -> string) -> 'a paction -> string

  type action = variable paction

  val action_to_string : action -> string

  val pred : G.lr1 -> G.lr1 list

  val cost_of  : variable -> float
  val cost_of_action  : action -> float
  val cost_of_actions : action list -> float
  val solution : variable -> action list
  val report   : Format.formatter -> unit
end

module Make (G : GRAMMAR) (A : Recover_attrib.S with module G = G)
  : S with module G = G =
struct
  module G = G
  open G

  let pred =
    (* Compute lr1 predecessor relation *)
    let tbl1 = Array.make Lr1.count [] in
    let revert_transition s1 (sym,s2) =
      assert (match Lr0.incoming (Lr1.lr0 s2) with
          | None -> false
          | Some sym' -> sym = sym');
      tbl1.(Lr1.to_int s2) <- s1 :: tbl1.(Lr1.to_int s2)
    in
    Lr1.iter
      (fun lr1 -> List.iter (revert_transition lr1) (Lr1.transitions lr1));
    (fun lr1 -> tbl1.(Lr1.to_int lr1))

  type variable =
    | Head of lr1 * nonterminal
    | Tail of lr1 * production * int

  let variable_to_string = function
    | Head (st, n) ->
        Printf.sprintf "Head (#%d, %s)"
          (Lr1.to_int st) (Nonterminal.name n)
    | Tail (st, prod, pos) ->
        Printf.sprintf "Tail (#%d, p%d, %d)"
          (Lr1.to_int st) (Production.to_int prod) pos

  type 'a paction =
    | Abort
    | Reduce of production
    | Shift  of symbol
    | Var    of 'a

  let paction_to_string variable_to_string = function
    | Abort -> "Abort"
    | Reduce prod -> "Reduce p" ^ string_of_int (Production.to_int prod)
    | Shift  sym -> "Shift " ^ (symbol_name sym)
    | Var v -> "Var (" ^ variable_to_string v ^ ")"

  type action = variable paction

  let action_to_string = paction_to_string variable_to_string

  let check_cost r =
    assert (r >= 0.); r

  let cost_of_prod p    = check_cost (1. +. A.cost_of_prod p)
  let cost_of_symbol s  = check_cost (1. +. A.cost_of_symbol s)
  let penalty_of_item i = check_cost (A.penalty_of_item i)

  let app var v = v var

  let var var = match var with
    | Head _ -> app var
    | Tail (_,prod,pos) ->
        if pos < Array.length (Production.rhs prod) then
          app var
        else
          let cost = cost_of_prod prod in
          const cost

(*
  let can_pop prod pos =
    pos > 1 &&
    (match (Production.rhs prod).(pos - 1) with
     | T t, _, _ -> Terminal.typ t = None
     | _ -> false)
*)

  let cost_of = function
    | Head (st, n) ->
        let acc = List.fold_left
            (fun acc (_sym, st') ->
               List.fold_left (fun acc (prod, pos) ->
                   if pos = 1 && Production.lhs prod = n then
                     var (Tail (st, prod, 0)) :: acc
                   else acc
                 ) acc (Lr0.items (Lr1.lr0 st'))
            ) [] (Lr1.transitions st)
        in
        let cost = List.fold_left
            (fun acc (_, prods) ->
               List.fold_left (fun acc prod ->
                   if Production.rhs prod = [||] && Production.lhs prod = n then
                     min_float (cost_of_prod prod) acc
                   else acc
                 ) acc prods
            ) infinity (Lr1.reductions st)
        in
        if cost < infinity || acc <> [] then
          (fun v -> List.fold_left (fun cost f -> min_float cost (f v)) cost acc)
        else const infinity

    | Tail (st, prod, pos) ->
        let penalty = penalty_of_item (prod, pos) in
        if penalty = infinity then
          const infinity
        else
        if pos >= Array.length (Production.rhs prod) then
          const (cost_of_prod prod)
        else
          let head =
            let sym, _, _ = (Production.rhs prod).(pos) in
            let cost = cost_of_symbol sym in
            if cost < infinity then const cost
            else match sym with
              | T _ -> const infinity
              | N n -> var (Head (st, n))
          in
          let tail =
            let sym, _, _ = (Production.rhs prod).(pos) in
            match List.assoc sym (Lr1.transitions st) with
            | st' -> var (Tail (st', prod, pos + 1))
            | exception Not_found ->
                (*report "no transition: #%d (%d,%d)\n" st.lr1_index prod.p_index pos;*)
                const infinity
          in
          (fun v -> head v +. tail v)

  let cost_of =
    let module Solver = Fix.Make (struct
        type key = variable
        type 'a t = (key, 'a) Hashtbl.t
        let create () = Hashtbl.create 7
        let find k tbl = Hashtbl.find tbl k
        let add k v tbl = Hashtbl.add tbl k v
        let iter f tbl = Hashtbl.iter f tbl
        let clear = Hashtbl.clear
      end) (struct
        type property = float
        let bottom = infinity
        let equal : float -> float -> bool = (=)
        let is_maximal f = f = 0.0
      end)
    in
    Solver.lfp cost_of

  let cost_of_action = function
    | Abort    -> infinity
    | Reduce p -> cost_of_prod p
    | Shift s  -> cost_of_symbol s
    | Var v    -> cost_of v

  let select var1 var2 =
    arg_min_float cost_of_action var1 var2

  let cost_of_actions actions =
    List.fold_left (fun cost act -> cost +. cost_of_action act) 0.0 actions

  let solution = function
    | Head (st, n) ->
        let acc = Abort in
        let acc = List.fold_left
            (fun acc (_sym, st') ->
               List.fold_left (fun acc (prod, pos) ->
                   if pos = 1 && Production.lhs prod = n then
                     select (Var (Tail (st, prod, 0))) acc
                   else acc
                 ) acc (Lr0.items (Lr1.lr0 st'))
            ) acc (Lr1.transitions st)
        in
        let acc = List.fold_left
            (fun acc (_, prods) ->
               List.fold_left (fun acc prod ->
                   if Production.rhs prod = [||] && Production.lhs prod = n then
                     select (Reduce prod) acc
                   else acc
                 ) acc prods
            ) acc (Lr1.reductions st)
        in
        [acc]

    | Tail (_st, prod, pos) when pos = Array.length (Production.rhs prod) ->
        [Reduce prod]

    | Tail (st, prod, pos) ->
        let penalty = penalty_of_item (prod, pos) in
        if penalty = infinity then
          [Abort]
        else
          let head =
            let sym, _, _ = (Production.rhs prod).(pos) in
            let cost = cost_of_symbol sym in
            if cost < infinity then
              Shift sym
            else match sym with
              | T _ -> Abort
              | N n -> Var (Head (st, n))
          in
          let tail =
            let sym, _, _ = (Production.rhs prod).(pos) in
            match List.assoc sym (Lr1.transitions st) with
            | st' -> Var (Tail (st', prod, pos + 1))
            | exception Not_found ->
                Abort
          in
          [head; tail]

  let report ppf =
    let open Format in
    let solutions = Lr1.fold
        (fun st acc ->
          match List.fold_left (fun (item, cost) (prod, pos) ->
              let cost' = cost_of (Tail (st, prod, pos)) in
              let actions = solution (Tail (st, prod, pos)) in
              assert (cost' = cost_of_actions actions);
              if cost' < cost then (Some (prod, pos), cost') else (item, cost)
            ) (None, infinity) (Lr0.items (Lr1.lr0 st))
          with
          | None, _ ->
              fprintf ppf "no synthesis from %d\n" (Lr1.to_int st);
              acc
          | Some item, cost -> (item, (cost, st)) :: acc
        ) []
    in
    List.iter (fun (item, states) ->
        fprintf ppf "# Item (%d,%d)\n" (Production.to_int (fst item)) (snd item);
        Print.item ppf item;
        List.iter (fun (cost, states) ->
            fprintf ppf "at cost %f from states %s\n\n"
              cost (list_fmt (fun x -> string_of_int (Lr1.to_int x)) states)
          ) (group_assoc states)
      ) (group_assoc solutions)
end
