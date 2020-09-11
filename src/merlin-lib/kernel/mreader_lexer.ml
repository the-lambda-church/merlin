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

type keywords = Lexer_raw.keywords

type triple = Parser_raw.token * Lexing.position * Lexing.position

type item =
  | Triple of triple
  | Comment of (string * Location.t)
  | Error of Lexer_raw.error * Location.t

type t = {
  keywords: keywords;
  config: Mconfig.t;
  source: Msource.t;
  items: item list;
}

let get_tokens keywords pos text =
  let state = Lexer_raw.make keywords in
  let lexbuf = Lexing.from_string text in
  Lexing.move lexbuf pos;
  let rec aux items = function
    | Lexer_raw.Return (Parser_raw.COMMENT comment) ->
      continue (Comment comment :: items)
    | Lexer_raw.Refill k -> aux items (k ())
    | Lexer_raw.Return t ->
      let triple = (t, lexbuf.Lexing.lex_start_p, lexbuf.Lexing.lex_curr_p) in
      let items = Triple triple :: items in
      if t = Parser_raw.EOF
      then items
      else continue items
    | Lexer_raw.Fail (err, loc) ->
      continue (Error (err, loc) :: items)

  and continue items =
    aux items (Lexer_raw.token state lexbuf)

  in
  function
  | [] ->
    (* First line: skip #! ... *)
    aux [] (Lexer_raw.skip_sharp_bang state lexbuf)
  | items ->
    (* Resume *)
    continue items

let initial_position config =
  { Lexing.
    pos_fname = (Mconfig.filename config);
    pos_lnum = 1;
    pos_bol = 0;
    pos_cnum = 0;
  }

let make warnings keywords config source =
  Msupport.catch_errors warnings (ref []) @@ fun () ->
  let items =
    get_tokens keywords
    (initial_position config)
    (Msource.text source)
    []
  in
  { keywords; items; config; source }

let item_start = function
  | Triple (_,s,_) -> s
  | Comment (_, l) | Error (_, l) ->
    l.Location.loc_start

let item_end = function
  | Triple (_,_,e) -> e
  | Comment (_, l) | Error (_, l) ->
    l.Location.loc_end

let initial_position t =
  initial_position t.config

let rev_filter_map ~f lst =
  let rec aux acc = function
    | [] -> acc
    | x :: xs ->
      let acc =
        match f x with
        | Some x' -> x' :: acc
        | None -> acc
      in
      aux acc xs
  in
  aux [] lst

let tokens t =
  rev_filter_map t.items
    ~f:(function Triple t -> Some t | _ -> None)

let errors t =
  rev_filter_map t.items
    ~f:(function Error (err, loc) -> Some (Lexer_raw.Error (err, loc))
               | _ -> None)

let comments t =
  rev_filter_map t.items
    ~f:(function Comment t -> Some t | _ -> None)

open Parser_raw

let is_operator = function
  | PREFIXOP s
  | INFIXOP0 s | INFIXOP1 s | INFIXOP2 s | INFIXOP3 s | INFIXOP4 s -> Some s
  | BANG -> Some "!"
  | PERCENT -> Some "%"
  | PLUS -> Some "+"        | PLUSDOT -> Some "+."
  | MINUS -> Some "-"       | MINUSDOT -> Some "-."
  | STAR -> Some "*"        | EQUAL -> Some "="
  | LESS -> Some "<"        | GREATER -> Some ">"
  | OR -> Some "or"         | BARBAR -> Some "||"
  | AMPERSAND -> Some "&"   | AMPERAMPER -> Some "&&"
  | COLONEQUAL -> Some ":=" | PLUSEQ -> Some "+="
  | _ -> None

(* [reconstruct_identifier] is impossible to read at the moment, here is a
   pseudo code version of the function:
   (many thanks to Gabriel for this contribution)

        00| let h = parse (focus h) with
        01|   | . { h+1 }
        02|   | _ { h }
        03| in
        04| parse h with
        05| | BOF x=operator       { [x] }
        06| | ¬( x=operator        { [x] }
        07| | ' x=ident            { [] }
        08| | _ {
        09|   let acc, h = parse (h ! tail h) with
        10|     | x=ident !          { [x], h }
        11|     | ( ! x=operator )   { [x], h }
        12|     | ( x=operator ! )   { [x], h - 1 }
        13|     | ( x=operator ) !   { [x], h - 2 }
        14|     | _ { [], h }
        15|   in
        16|   let h = h - 1 in
        17|   let rec head acc = parse (h !) with
        18|     | tl x=ident . ! { head (x :: acc) tl }
        19|     | x=ident . !    { ident :: acc }
        20|     | _              { acc }
        21|   in head acc
        22| }

   Now for the explanations:
     line 0-3:  if we're on a dot, skip it and move to the right

     line 5,6:  if we're on an operator not preceded by an opening parenthesis,
                just return that.

     line 7:    if we're on a type variable, don't return anything.
                reconstruct_identifier is called when locating and getting the
                type of an expression, in both cases there's nothing we can do
                with a type variable.
                See #317

     line 8-22: two step approach:
       - line 9-15:  retrieve the identifier
                     OR retrieve the parenthesized operator and move before the
                        opening parenthesis

       - line 16-21: retrieve the "path" prefix of the identifier/operator we
                     got in the previous step.


   Additionnaly, the message of commit fc0b152 explains what we consider is an
   identifier:

     «
        Interpreting an OCaml identifier out of context is a bit ambiguous.

        A prefix of the form (UIDENT DOT)* is the module path,
        A UIDENT suffix is either a module name, a module type name (in case the
        whole path is a module path), or a value constructor.
        A LIDENT suffix is either a value name, a type constructor or a module
        type name.
        A LPAREN OPERATOR RPAREN suffix is a value name (and soon, maybe a
        value constructor if beginning by ':' ?!) .

        In the middle, LIDENT DOT (UIDENT DOT)* is projection of the field of a
        record.  In this case, merlin will drop everything up to the first
        UIDENT and complete in the scope of the (UIDENT DOT)* interpreted as a
        module path.
        Soon, the last UIDENT might also be the type of an inline record.
        (Module2.f.Module1.A <- type of the record of the value constructor named A of
        type f, defined in Module1 and aliased in Module2, pfffff).
     »
*)

let reconstruct_identifier_from_tokens tokens pos =
  let rec look_for_component acc = function

    (* Skip 'a and `A *)
    | ((LIDENT _ | UIDENT _), _, _) ::
      ((BACKQUOTE | QUOTE), _, _) :: items ->
      check acc items

    (* UIDENT is a regular a component *)
    | (UIDENT _, _, _) as item :: items ->
      look_for_dot (item :: acc) items

    (* LIDENT always begin a new identifier *)
    | (LIDENT _, _, _) as item :: items ->
      if acc = []
      then look_for_dot [item] items
      else check acc (item :: items)

    (* Reified operators behave like LIDENT *)
    | (RPAREN, _, _) :: (token, _, _ as item) :: (LPAREN, _, _) :: items
      when is_operator token <> None && acc = [] ->
      look_for_dot [item] items

    (* An operator alone is an identifier on its own *)
    | (token, _, _ as item) :: items
      when is_operator token <> None && acc = [] ->
      check [item] items

    (* Otherwise, check current accumulator and scan the rest of the input *)
    | _ :: items ->
      check acc items

    | [] -> raise Not_found

  and look_for_dot acc = function
    | (DOT,_,_) :: items -> look_for_component acc items
    | items -> check acc items

  and check acc items =
    if acc <> [] &&
       (let startp = match acc with
           | (_, startp, _) :: _ -> startp
           | _ -> assert false in
        Lexing.compare_pos startp pos <= 0) &&
       (let endp = match List.last acc with
           | Some ((_, _, endp)) -> endp
           | _ -> assert false in
        Lexing.compare_pos pos endp <= 0)
    then acc
    else match items with
      | [] -> raise Not_found
      | (_, _, endp) :: _ when Lexing.compare_pos endp pos < 0 ->
        raise Not_found
      | _ -> look_for_component [] items

  in
  match look_for_component [] tokens with
  | exception Not_found -> []
  | acc ->
    let fmt (token, loc_start, loc_end) =
      let id =
        match token with
        | UIDENT s | LIDENT s -> s
        | _ -> match is_operator token with
          | Some t -> t
          | None -> assert false
      in
      Location.mkloc id {Location. loc_start; loc_end; loc_ghost = false}
    in
    let before_pos = function
      | (_, s, _) ->
        Lexing.compare_pos s pos <= 0
    in
    List.map ~f:fmt (List.filter ~f:before_pos acc)

let reconstruct_identifier config source pos =
  let rec lex acc lexbuf =
    let token = Lexer_ident.token lexbuf in
    let item = (token, lexbuf.Lexing.lex_start_p, lexbuf.Lexing.lex_curr_p) in
    match token with
    | EOF -> (item :: acc)
    | EOL when Lexing.compare_pos lexbuf.Lexing.lex_curr_p pos > 0 ->
      (item :: acc)
    | EOL -> lex [] lexbuf
    | _ -> lex (item :: acc) lexbuf
  in
  let lexbuf = Lexing.from_string (Msource.text source) in
  Location.init lexbuf (Mconfig.filename config);
  let tokens = lex [] lexbuf in
  reconstruct_identifier_from_tokens tokens pos

let is_uppercase {Location. txt = x; _} =
  x <> "" && Char.is_uppercase x.[0]

let rec drop_lowercase acc = function
  | [x] -> List.rev (x :: acc)
  | x :: xs when not (is_uppercase x) -> drop_lowercase [] xs
  | x :: xs -> drop_lowercase (x :: acc) xs
  | [] -> List.rev acc

let for_completion t pos =
  let no_labels = ref false in
  let check_label = function
    | Triple ((LABEL _ | OPTLABEL _), _, _) -> no_labels := true
    | _ -> ()
  in
  let rec aux acc = function
    (* Cursor is before item: continue *)
    | item :: items when Lexing.compare_pos (item_start item) pos >= 0 ->
      aux (item :: acc) items

    (* Cursor is in the middle of item: stop *)
    | item :: _ when Lexing.compare_pos (item_end item) pos > 0 ->
      check_label item;
      raise Exit

    (* Cursor is at the end *)
    | ((Triple (token, _, loc_end) as item) :: _) as items
      when Lexing.compare_pos pos loc_end = 0 ->
      check_label item;
      begin match token with
        (* Already on identifier, no need to introduce *)
        | UIDENT _ | LIDENT _ -> raise Exit
        | _ -> acc, items
      end

    | items -> acc, items
  in
  let t =
    match aux [] t.items with
    | exception Exit -> t
    | acc, items ->
      {t with items =
                List.rev_append acc (Triple (LIDENT "", pos, pos) :: items)}
  in
  (!no_labels, t)

let identifier_suffix ident =
  match List.last ident with
  | Some x when is_uppercase x -> drop_lowercase [] ident
  | _ -> ident
