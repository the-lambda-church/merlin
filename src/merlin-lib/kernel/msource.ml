(* Merlin representation of a textual source code *)
open Std

let {Logger. log} = Logger.for_section "Msource"

type t = {
  text: string;
}

let dump t = `Assoc [
    "text"     , `String t.text;
  ]

let print_position () = function
  | `Start -> "start"
  | `Offset o -> string_of_int o
  | `Logical (l,c) -> string_of_int l ^ ":" ^ string_of_int c
  | `End -> "end"

let make text = {text}

(* Position management *)

type position = [
  | `Start
  | `Offset of int
  | `Logical of int * int
  | `End
]

exception Found of int

let find_line line {text} =
  if line <= 0 then
    Printf.ksprintf invalid_arg
      "Msource.find_line: invalid line number %d. \
       Numbering starts from 1" line;
  if line = 1 then 0 else
    let line' = ref line in
    try
      for i = 0 to String.length text - 1 do
        if text.[i] = '\n' then begin
          decr line';
          if !line' = 1 then
            raise (Found i);
        end
      done;
      log ~title:"find_line" "line %d out of bounds (max = %d)"
        line (line - !line');
      String.length text
    with Found n ->
      n + 1

let find_offset ({text} as t) line col =
  assert (col >= 0);
  let offset = find_line line t in
  if col = 0 then offset else
    try
      for i = offset to min (offset + col) (String.length text) - 1 do
        if text.[i] = '\n' then begin
          log ~title:"find_offset"
            "%d:%d out of line bounds, line %d only has %d columns"
            line col line (i - offset);
          raise (Found i)
        end
      done;
      if (offset + col) > (String.length text) then begin
        log ~title:"find_offset" "%d:%d out of file bounds" line col
      end;
      offset + col
    with Found off -> off

let get_offset t = function
  | `Start -> `Offset 0
  | `Offset x ->
    assert (x >= 0);
    if x <= String.length t.text then
      (`Offset x)
    else begin
      log ~title:"get_offset"
        "offset %d out of bounds (size is %d)" x (String.length t.text);
      (`Offset (String.length t.text))
    end
  | `End ->
    `Offset (String.length t.text)
  | `Logical (line, col) ->
    `Offset (find_offset t line col)

let get_logical {text} = function
  | `Start -> `Logical (1, 0)
  | `Logical _ as p -> p
  | `Offset _ | `End as r ->
    let len = String.length text in
    let offset = match r with
      | `Offset x when x > len ->
        log ~title:"get_logical" "offset %d out of bounds (size is %d)" x len;
        len
      | `Offset x ->
        assert (x >= 0);
        x
      | `End -> len
    in
    let line = ref 1 in
    let cnum = ref 0 in
    for i = 0 to offset - 1 do
      if text.[i] = '\n' then begin
        incr line;
        cnum := i + 1;
      end;
    done;
    `Logical (!line, offset - !cnum)

let get_lexing_pos t ~filename pos =
  let `Offset o = get_offset t pos in
  let `Logical (line, col) = get_logical t pos in
  { Lexing.
    pos_fname = filename;
    pos_lnum = line;
    pos_bol  = o - col;
    pos_cnum = o;
  }

let substitute t starting ending text =
  let len = String.length t.text in
  let `Offset starting = get_offset t starting in
  let `Offset ending = match ending with
    | `End -> `Offset len
    | `Length l ->
      if starting + l <= len then
        `Offset (starting + l)
      else begin
        log ~title:"substitute"
          "offset %d + length %d out of bounds (size is %d)" starting l len;
        `Offset len
      end
    | #position as p -> get_offset t p
  in
  if ending < starting then
    invalid_arg "Source.substitute: ending < starting";
  let text =
    String.sub t.text ~pos:0 ~len:starting ^
    text ^
    String.sub t.text ~pos:ending ~len:(len - ending)
  in
  {text}

(* Accessing content *)

let text t = t.text
