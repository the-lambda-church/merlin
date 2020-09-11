open Parser_raw
open MenhirInterpreter

let opening (type a) : a terminal -> string option = function
  | T_STRUCT       -> Some "struct"
  | T_SIG          -> Some "sig"
  | T_OBJECT       -> Some "object"
  | T_BEGIN        -> Some "begin"
  | T_LPAREN       -> Some "("
  | T_LBRACKET     -> Some "["
  | T_LBRACE       -> Some "{"
  | T_LBRACKETBAR  -> Some "[|"
  | T_LBRACKETLESS -> Some "[<"
  | T_LBRACELESS   -> Some "{<"
  | _ -> None

let opening_st st =
  match incoming_symbol st with
  | T term -> opening term
  | _ -> None

let closing (type a) : a terminal -> bool = function
  | T_END             -> true
  | T_RPAREN          -> true
  | T_RBRACKET        -> true
  | T_RBRACE          -> true
  | T_BARRBRACKET     -> true
  | T_GREATERRBRACE   -> true
  | T_GREATERRBRACKET -> true
  | _ -> false

let closing_st st =
  match incoming_symbol st with
  | T term -> closing term
  | _ -> false

type explanation = {
  item: (string * Location.t) option;
  unclosed: (string * Location.t) option;
  location: Location.t;
  popped: MenhirInterpreter.xsymbol list;
  shifted: MenhirInterpreter.xsymbol option;
  unexpected: MenhirInterpreter.token;
}

let explain env (unexpected, startp, endp) popped shifted =
  let mkloc s e = {Location. loc_start = s; loc_end = e; loc_ghost = false} in
  let open MenhirInterpreter in
  let location = mkloc startp endp in
  let closed = ref 0 in
  let unclosed = ref None in
  let return item =
    { item; unclosed = !unclosed; location; popped; shifted; unexpected }
  in
  let rec process env = match top env with
    | None -> return None
    | Some (Element (st, _, startp, endp)) ->
      if closing_st st then incr closed;
      begin match opening_st st with
        | None -> ()
        | Some st ->
          if !closed = 0 && !unclosed = None then
            unclosed := Some (st, mkloc startp endp)
          else
            decr closed
      end;
      match Parser_explain.named_item_at (number st) with
      | name -> return (Some (name, mkloc startp endp))
      | exception Not_found ->
        match pop env with
        | None -> return None
        | Some env -> process env
  in
  process env

let to_error { item; unclosed; location; popped; shifted; unexpected = _ } =
  let inside = match item with
    | None -> ""
    | Some (name, _) -> " inside `" ^ name ^ "'" in
  let after = match unclosed with
    | None -> ""
    | Some (name, _) -> " after unclosed " ^ name in
  let friendly_name sym = match sym with
    | X (T _) -> "`" ^ Parser_printer.print_symbol sym ^ "'"
    | X (N _) -> Parser_printer.print_symbol sym
  in
  let popped = String.concat " " (List.rev_map friendly_name popped) in
  let expecting = match shifted with
    | None -> if popped = "" then "" else ", maybe remove " ^ popped
    | Some (X (T T_EOF)) -> ""
    | Some sym ->
      if popped = "" then ", expecting " ^ (friendly_name sym)
      else ", maybe replace " ^ popped ^ " by " ^ (friendly_name sym)
  in
  let msg = Printf.sprintf "Syntax error%s%s%s" inside after expecting in
  Location.error ~loc:location ~source:Location.Parser msg

exception Syntax_explanation of explanation

let syntax_explanation = function
  | Syntax_explanation explanation -> Some (to_error explanation)
  | _ -> None

let () = Location.register_error_of_exn syntax_explanation
