open Std

let {Logger. log} = Logger.for_section "Mreader_recover"

module Make
    (Parser : MenhirLib.IncrementalEngine.EVERYTHING)
    (Recovery : sig
       val default_value : Location.t -> 'a Parser.symbol -> 'a

       type action =
         | Abort
         | R of int
         | S : 'a Parser.symbol -> action
         | Sub of action list

       type decision =
         | Nothing
         | One of action list
         | Select of (int -> action list)

       val depth : int array

       val recover : int -> decision

       val guide : 'a Parser.symbol -> bool

       val token_of_terminal : 'a Parser.terminal -> 'a -> Parser.token

       val nullable : 'a Parser.nonterminal -> bool
     end)
    (Dump : sig
       val symbol  : unit -> Parser.xsymbol -> string
     end) =
struct

  type 'a candidate = {
    line: int;
    min_col: int;
    max_col: int;
    env: 'a Parser.env;
  }

  type 'a candidates = {
    popped: Parser.xsymbol list;
    shifted: Parser.xsymbol option;
    final: 'a option;
    candidates: 'a candidate list;
  }

  module T = struct
    (* FIXME: this is a bit ugly. We should ask for the type to be exported
       publicly by MenhirLib. *)

    [@@@ocaml.warning "-37"]

    type 'a checkpoint =
      | InputNeeded of 'a Parser.env
      | Shifting of 'a Parser.env * 'a Parser.env * bool
      | AboutToReduce of 'a Parser.env * Parser.production
      | HandlingError of 'a Parser.env
      | Accepted of 'a
      | Rejected
    external inj : 'a checkpoint -> 'a Parser.checkpoint = "%identity"
  end

  (*let env_state env =
    match Parser.top env with
    | None -> -1
    | Some (Parser.Element (state, _, _, _)) ->
      Parser.number state*)

  let feed_token ~allow_reduction token env =
    let rec aux allow_reduction = function
      | Parser.HandlingError _ | Parser.Rejected -> `Fail
      | Parser.AboutToReduce _ when not allow_reduction -> `Fail
      | Parser.Accepted v -> `Accept v
      | Parser.Shifting _ | Parser.AboutToReduce _ as checkpoint ->
        aux true (Parser.resume checkpoint)
      | Parser.InputNeeded env as checkpoint -> `Recovered (checkpoint, env)
    in
    aux allow_reduction (Parser.offer (T.inj (T.InputNeeded env)) token)

  let rec follow_guide col env = match Parser.top env with
    | None -> col
    | Some (Parser.Element (state, _, pos, _)) ->
      if Recovery.guide (Parser.incoming_symbol state) then
        match Parser.pop env with
        | None -> col
        | Some env -> follow_guide (snd (Lexing.split_pos pos)) env
      else
        col

  let candidate env =
    let line, min_col, max_col =
      match Parser.top env with
      | None -> 1, 0, 0
      | Some (Parser.Element (state, _, pos, _)) ->
        let depth = Recovery.depth.(Parser.number state) in
        let line, col = Lexing.split_pos pos in
        if depth = 0 then
          line, col, col
        else
          let col' = match Parser.pop_many depth env with
            | None -> max_int
            | Some env ->
              match Parser.top env with
              | None -> max_int
              | Some (Parser.Element (_, _, pos, _)) ->
                follow_guide (snd (Lexing.split_pos pos)) env
          in
          line, min col col', max col col'
    in
    { line; min_col; max_col; env }

  let attempt r token =
    let _, startp, _ = token in
    let line, col = Lexing.split_pos startp in
    let more_indented candidate =
      line <> candidate.line && candidate.min_col > col in
    let recoveries = List.drop_while ~f:more_indented r.candidates in
    let same_indented candidate =
      line = candidate.line ||
      (candidate.min_col <= col && col <= candidate.max_col)
    in
    let recoveries = List.take_while ~f:same_indented recoveries in
    let rec aux = function
      | [] -> `Fail
      | x :: xs -> match feed_token ~allow_reduction:true token x.env with
        | `Fail ->
          (*if not (is_closed k) then
            printf k "Couldn't resume %d with %S.\n"
              (env_state x.env) (let (t,_,_) = token in Dump.token t);*)
          aux xs
        | `Recovered (checkpoint, _) -> `Ok (checkpoint, x.env)
        | `Accept v ->
          begin match aux xs with
            | `Fail -> `Accept v
            | x -> x
          end
    in
    aux recoveries

  let decide env =
    let rec nth_state env n =
      if n = 0 then
        match Parser.top env with
        | None -> -1 (*allow giving up recovery on empty files*)
        | Some (Parser.Element (state, _, _, _)) -> Parser.number state
      else
        match Parser.pop env with
        | None -> assert (n = 1); -1
        | Some env -> nth_state env (n - 1)
    in
    let st = nth_state env 0 in
    match Recovery.recover st with
    | Recovery.Nothing -> []
    | Recovery.One actions -> actions
    | Recovery.Select f -> f (nth_state env Recovery.depth.(st))

  let generate (type a) (env : a Parser.env) =
    let module E = struct
      exception Result of a
    end in
    let shifted = ref None in
    let rec aux acc env =
      match Parser.top env with
      | None -> None, acc
      | Some (Parser.Element (state, _, _startp, endp)) ->
        (*Dump.element k elt;*)
        log ~title:"decide state" "%d" (Parser.number state);
        let actions = decide env in
        let candidate0 = candidate env in
        let rec eval (env : a Parser.env) : Recovery.action -> a Parser.env = function
          | Recovery.Abort ->
            log ~title:"eval Abort" "";
            raise Not_found
          | Recovery.R prod ->
            log ~title:"eval Reduce" "";
            let prod = Parser.find_production prod in
            Parser.force_reduction prod env
          | Recovery.S (Parser.N n as sym) ->
            let xsym = Parser.X sym in
            if !shifted = None && not (Recovery.nullable n) then
              shifted := Some xsym;
            log ~title:"eval Shift N" "%a" Dump.symbol xsym;
            (* FIXME: if this is correct remove the fixme, otherwise use
               [startp] *)
            let loc = {Location. loc_start = endp; loc_end = endp; loc_ghost = true} in
            let v = Recovery.default_value loc sym in
            Parser.feed sym endp v endp env
          | Recovery.S (Parser.T t as sym) ->
            let xsym = Parser.X sym in
            if !shifted = None then shifted := Some xsym;
            log ~title:"eval Shift T" "%a" Dump.symbol xsym;
            let loc = {Location. loc_start = endp; loc_end = endp; loc_ghost = true} in
            let v = Recovery.default_value loc sym in
            let token = (Recovery.token_of_terminal t v, endp, endp) in
            begin match feed_token ~allow_reduction:true token env with
              | `Fail -> assert false
              | `Accept v -> raise (E.Result v)
              | `Recovered (_,env) -> env
            end
          | Recovery.Sub actions ->
            log ~title:"enter Sub" "";
            let env = List.fold_left ~f:eval ~init:env actions in
            log ~title:"leave Sub" "";
            env
        in
        match
          List.rev_scan_left [] ~f:eval ~init:env actions
          |> List.map ~f:(fun env -> {candidate0 with env})
        with
        | exception Not_found -> None, acc
        | exception (E.Result v) -> Some v, acc
        | [] -> None, acc
        | (candidate :: _) as candidates ->
          aux (candidates @ acc) candidate.env
    in
    let popped = ref [] in
    (*let should_pop stack =
      let Parser.Element (state, _, _, _) = Parser.stack_element stack in
      match Parser.incoming_symbol state with
      | (Parser.T term) as t1 when Recovery.can_pop term ->
        log "Pop" "pop %s"
          (Dump.symbol (Parser.X t1));
        begin match Parser.stack_next stack with
          | None -> false
          | Some stack' ->
            let rec check_next = function
              | Recovery.S (Parser.T term' as t2) :: _
                when Parser.X t1 = Parser.X t2 ->
                false
              | Recovery.S sym :: _ ->
                log "Pop" "then push %s"
                  (Dump.symbol (Parser.X sym));
                popped := Parser.X t1 :: !popped;
                true
              | Recovery.Sub xs :: _ ->
                check_next xs
              | _ ->
                popped := Parser.X t1 :: !popped;
                true
            in
            check_next (decide stack')
        end
      | _ -> false
      in*)
    let final, candidates = aux [] env in
    (List.rev !popped, !shifted, final, candidates)

  let generate env =
    let popped, shifted, final, candidates = generate env in
    let candidates = List.rev_filter candidates
        ~f:(fun t -> not (Parser.env_has_default_reduction t.env))
    in
    { popped; shifted; final; candidates = (candidate env) :: candidates }

  (*let dump {Nav. nav; body; _} ~wrong:(t,s,_ as token) ~rest:tokens env =
    if not (is_closed body) then (
      let l, c = Lexing.split_pos s in
      printf body "Unexpected %S at %d:%d, " (Dump.token t) l c;
      link body "see recoveries"
        (fun _ -> Nav.push nav "Recoveries" @@ fun {Nav. body; _} ->
          let r = generate body env in
          let rec aux = function
            | [] -> ()
            | token :: tokens ->
              match attempt body r token with
              | `Fail -> aux tokens
              | `Accept _ ->
                text body "\nCouldn't resume, generated final AST.\n"
              | `Ok (_, recovered_from) ->
                printf body "\nResumed with %S from:\n"
                  (let (t,_,_) = token in Dump.token t);
                Dump.env body recovered_from
          in
          aux (token :: tokens)
        );
      text body ".\n";
      Dump.env body env;
      text body "\n"
    )*)
end
