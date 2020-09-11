open MenhirSdk.Cmly_api
open Utils

let menhir = "MenhirInterpreter"

(* Generation scheme doing checks and failing at runtime, or not ... *)
let safe = false

module Codeconsing (S : Synthesis.S) (R : Recovery.S with module G = S.G) : sig

  (* Step 1: record all definitions *)
  val record_item : R.item -> unit

  type instr =
    | Nil
    | Cons of instr S.paction * instr
    | Ref of int ref * instr

  (* Step 2: get prelude maximizing & serialization function *)
  val normalize : unit -> instr list * (R.item -> instr)

end = struct

  open S

  type fixed = A of fixed paction list

  let normalized_actions = Hashtbl.create 113

  let rec normalize_actions = function
    | [] -> []
    | [Var v] -> normalize_actions (S.solution v)
    | (x :: xs) as xxs ->
        try !(Hashtbl.find normalized_actions xxs)
        with Not_found ->
          let x' = normalize_action x in
          let xs' = normalize_actions xs in
          let xxs' = x' :: xs' in
          Hashtbl.add normalized_actions xxs (ref xxs');
          xxs'

  and normalize_action = function
    | Abort | Reduce _ | Shift _ as a -> a
    | Var v ->
        match normalize_actions (S.solution v) with
        | [x] -> x
        | xs -> Var (A xs)

  let item_to_actions (st, prod, pos) =
    normalize_actions [Var (Tail (st, prod, pos))]

  let roots : R.item list ref = ref []

  let record_item root =
    roots := root :: !roots

  let share () =
    let table = Hashtbl.create 113 in
    let rec get = function
      | [] -> []
      | (x :: xs) ->
          let xxs = (get_one x :: get xs) in
          try
            let r, v = Hashtbl.find table xxs in
            incr r; v
          with Not_found ->
            Hashtbl.add table xxs (ref 1, xxs);
            xxs
    and get_one = function
      | Var (A v) -> Var (A (get v))
      | x -> x
    in
    Hashtbl.iter (fun _k v -> v := get !v) normalized_actions;
    (* Return counter *)
    (fun v -> try !(fst (Hashtbl.find table v)) with Not_found -> 0)

  type instr =
    | Nil
    | Cons of instr paction * instr
    | Ref of int ref * instr

  let emitter () =
    let counter = share () in
    let table = Hashtbl.create 113 in
    let frozen = ref false in
    let values = ref [] in
    let rec emit = function
      | [] -> Nil
      | (x :: xs) as xxs ->
          try Hashtbl.find table xxs
          with Not_found ->
            let x = match x with
              | Var (A ys) -> Var (emit ys)
              | Abort | Reduce _ | Shift _ as a -> a
            in
            let value = Cons (x, emit xs) in
            if counter xxs = 1 then value else (
              assert (not !frozen);
              let value = Ref (ref (-1), value) in
              values := value :: !values;
              Hashtbl.add table xxs value;
              value
            )
    in
    frozen, values, emit

  let normalize () =
    let roots = List.map item_to_actions !roots in
    let frozen, values, emit = emitter () in
    let pass_2 item = ignore (emit item) in
    List.iter pass_2 roots;
    frozen := true;
    !values, (fun item -> emit (item_to_actions item))
end

module Make
    (G : GRAMMAR)
    (A : Recover_attrib.S with module G = G)
    (S : Synthesis.S with module G = G)
    (R : Recovery.S with module G = G) :
sig
  val emit : Format.formatter -> unit
end = struct

  open G
  open Format

  let emit_default_value ppf =
    fprintf ppf "open %s\n\n"
      (String.capitalize (Filename.basename Grammar.basename));
    fprintf ppf "module Default = struct\n";
    A.default_prelude ppf;

    fprintf ppf "  let value (type a) : a %s.symbol -> a = function\n" menhir;
    Terminal.iter (fun t ->
        match A.default_terminal t with
        | None -> ()
        | Some str ->
            fprintf ppf "    | %s.T %s.T_%s -> %s\n" menhir menhir (Terminal.name t) str
      );
    Nonterminal.iter (fun n ->
        match A.default_nonterminal n with
        | None -> ()
        | Some str ->
            fprintf ppf "    | %s.N %s.N_%s -> %s\n" menhir menhir (Nonterminal.mangled_name n) str
      );
    (*fprintf ppf "    | _ -> raise Not_found\n"; should be exhaustive*)
    fprintf ppf "end\n\n";
    fprintf ppf "let default_value = Default.value\n\n"

  let emit_defs ppf =
    fprintf ppf "open %s\n\n" menhir;
    fprintf ppf "type action =\n\
                \  | Abort\n\
                \  | R of int\n\
                \  | S : 'a symbol -> action\n\
                \  | Sub of action list\n\n";
    fprintf ppf "type decision =\n\
                \  | Nothing\n\
                \  | One of action list\n\
                \  | Select of (int -> action list)\n\n"

  module C = Codeconsing(S)(R)

  let emit_depth ppf =
    let open Format in
    fprintf ppf "let depth =\n  [|";
    Lr1.iter (fun st ->
        let depth, _ = R.recover st in
        fprintf ppf "%d;" depth
      );
    fprintf ppf "|]\n\n"

  let _code, get_instr, iter_entries =
    Lr1.iter (fun st ->
        let _depth, cases = R.recover st in
        List.iter (fun (_case, items) -> C.record_item (list_last items))
          cases
      );
    let code, get_instr = C.normalize () in
    let all_instrs =
      Lr1.tabulate (fun st ->
          let _depth, cases = R.recover st in
          List.map (fun (_case, items) -> get_instr (list_last items))
            cases
        )
    in
    code, get_instr,
    (fun f -> Lr1.iter (fun st -> List.iter f (all_instrs st)))

  let emit_can_pop ppf =
    Format.fprintf ppf "let can_pop (type a) : a terminal -> bool = function\n";
    G.Terminal.iter (fun t ->
        if G.Terminal.kind t = `REGULAR && G.Terminal.typ t = None then
          Format.fprintf ppf "  | T_%s -> true\n" (G.Terminal.name t));
    Format.fprintf ppf "  | _ -> false\n\n"

  let emit_recoveries ppf =
    let k = ref 0 in
    let instrs = ref [] in
    let rec alloc_entry = function
      | C.Nil -> ()
      | C.Cons (act, instr) -> alloc_entry_action act; alloc_entry instr
      | C.Ref (r, instr) ->
        if (!r = -1) then (
          alloc_entry instr;
          r := !k;
          instrs := (!k, instr) :: !instrs;
          incr k;
          )
    and alloc_entry_action = function
      | S.Abort | S.Reduce _ | S.Shift _ -> ()
      | S.Var instr -> alloc_entry instr
    in
    iter_entries alloc_entry;
    let open Format in

    let rec emit_action ppf = function
      | S.Abort -> fprintf ppf "Abort"
      | S.Reduce prod -> fprintf ppf "R %d" (Production.to_int prod)
      | S.Shift (T t) -> fprintf ppf "S (T T_%s)" (Terminal.name t)
      | S.Shift (N n) -> fprintf ppf "S (N N_%s)" (Nonterminal.mangled_name n)
      | S.Var instr -> fprintf ppf "Sub (%a)" emit_instr instr
    and emit_instr ppf = function
      | C.Nil -> fprintf ppf "[]"
      | C.Cons (act, C.Nil) ->
        fprintf ppf "[%a]" emit_action act
      | C.Cons (act, instr) ->
        fprintf ppf "%a :: %a" emit_action act emit_instr instr
      | C.Ref (r, _) -> fprintf ppf "r%d" !r
    in

    fprintf ppf "let recover =\n";

    let emit_shared (k, instr) =
      fprintf ppf "  let r%d = %a in\n" k emit_instr instr
    in
    List.iter emit_shared (List.rev !instrs);

    let all_cases =
      Lr1.fold (fun st acc ->
          let _, cases = R.recover st in
          let cases = List.map (fun (st', items) ->
              (get_instr (list_last items)),
              (match st' with None -> -1 | Some st' -> Lr1.to_int st')
            ) cases
          in
          let cases = match group_assoc cases with
            | [] -> `Nothing
            | [(instr, _)] -> `One instr
            | xs -> `Select xs
          in
          (cases, (Lr1.to_int st)) :: acc)
        []
    in
    let all_cases = group_assoc all_cases in

    fprintf ppf "  function\n";
    List.iter (fun (cases, states) ->
        fprintf ppf "  ";
        List.iter (fprintf ppf "| %d ") states;
        fprintf ppf "-> ";
        match cases with
        | `Nothing -> fprintf ppf "Nothing\n";
        | `One instr -> fprintf ppf "One (%a)\n" emit_instr instr
        | `Select xs ->
          fprintf ppf "Select (function\n";
          if safe then (
            List.iter (fun (instr, cases) ->
                fprintf ppf "    ";
                List.iter (fprintf ppf "| %d ") cases;
                fprintf ppf "-> %a\n" emit_instr instr;
              ) xs;
            fprintf ppf "    | _ -> raise Not_found)\n"
          ) else (
            match List.sort
                    (fun (_,a) (_,b) -> compare (List.length b) (List.length a))
                    xs
            with
            | (instr, _) :: xs ->
              List.iter (fun (instr, cases) ->
                  fprintf ppf "    ";
                  List.iter (fprintf ppf "| %d ") cases;
                  fprintf ppf "-> %a\n" emit_instr instr;
                ) xs;
              fprintf ppf "    | _ -> %a)\n" emit_instr instr
            | [] -> assert false
          )
      ) all_cases;

    fprintf ppf "  | _ -> raise Not_found\n"


  let emit ppf =
    emit_default_value ppf;
    emit_defs ppf;
    emit_depth ppf;
    emit_can_pop ppf;
    emit_recoveries ppf

end
