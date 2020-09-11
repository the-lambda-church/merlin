type t =
  | Cons   of t * t
  | Sym    of string
  | String of string
  | Int    of int
  | Float  of float

let nil = Sym "nil"

let escaped str =
  let len = String.length str in
  let extra_chars = ref 0 in
  for i = 0 to len - 1 do
    match str.[i] with
    | '\\' | '"' -> incr extra_chars
    | _ -> ()
  done;
  let buf = Buffer.create (len + !extra_chars + 2) in
  Buffer.add_char buf '"';
  if !extra_chars = 0 then (
    Buffer.add_string buf str
  ) else (
    for i = 0 to len - 1 do
      let c = str.[i] in
      if c = '"' || c = '\\' then
        Buffer.add_char buf '\\';
      Buffer.add_char buf c
    done;
  );
  Buffer.add_char buf '"';
  Buffer.contents buf

let unescaped str =
  (* Unescaped doesn't support unicode escaping and multibyte hex and octal
     escaping.
     Unicode escaping: '\uNNNN' or '\U00NNNNNN'
     Hex/octal escaping looks like '\xNN' or '\NNN'.
     '\xNNNN' and '\NNNNNN' are ambiguous, but emacs will try to parse them
     as multibyte
  *)
  match String.index str '\\' with
  | exception Not_found -> str
  | _ ->
    let len = String.length str in
    let buf = Buffer.create len in
    let i = ref 0 in
    while !i < len do
      match str.[!i] with
      | '\\' -> (
          incr i;
          begin match str.[!i] with
            | 'n' -> Buffer.add_char buf '\n'
            | 'r' -> Buffer.add_char buf '\r'
            | 't' -> Buffer.add_char buf '\t'
            | 'x' ->
              let c0 = Char.code str.[!i+1] in
              let c1 = Char.code str.[!i+2] in
              Buffer.add_char buf (Char.chr ((c0 * 16) lor c1));
              i := !i + 2;
            | '0'..'9' ->
              let c0 = Char.code str.[!i+1] in
              let c1 = Char.code str.[!i+2] in
              let c2 = Char.code str.[!i+3] in
              Buffer.add_char buf (Char.chr ((c0 * 64) lor (c1 * 8) lor c2));
              i := !i + 2;
            | c -> Buffer.add_char buf c
          end;
          incr i
        )
      | c ->
        Buffer.add_char buf c;
        incr i
    done;
    Buffer.contents buf

let rec of_list = function
  | [] -> nil
  | a :: tl -> Cons (a, of_list tl)

let rec tell_sexp tell = function
  | Cons (a,b) ->
    tell "(";
    tell_sexp tell a;
    tell_cons tell b
  | Sym s    -> tell s
  | String s -> tell (escaped s)
  | Int i    -> tell (string_of_int i)
  | Float f  -> tell (string_of_float f)

and tell_cons tell = function
  | Sym "nil" -> tell ")"
  | Cons (a,b) ->
    tell " ";
    tell_sexp tell a;
    tell_cons tell b
  | sexp ->
    tell " . ";
    tell_sexp tell sexp;
    tell ")"

let is_alpha c =
  (c >= 'a' && c <= 'z')
  || (c >= 'A' && c <= 'Z')

let is_num c =
  (c >= '0' && c <= '9')

let is_alphanum c = is_alpha c || is_num c

let read_sexp getch =
  let buf = Buffer.create 10 in
  let rec read_sexp getch = function
    | ' ' | '\t' | '\n' ->
      read_sexp getch (getch ())

    | c when is_num c ->
      read_num getch c

    | '\'' | ':' | '_' as c -> read_sym getch (Some c)
    | c when is_alpha c -> read_sym getch (Some c)

    | '"' ->
      read_string getch
    | '\000' -> raise End_of_file
    | '(' ->
      let lhs, next = read_sexp getch (getch ()) in
      read_cons getch (fun rhs -> Cons (lhs, rhs)) next
    | _ -> failwith "Invalid parse"

  and read_cons getch k next =
    match (match next with Some c -> c | None -> getch ()) with
    | ' ' | '\t' | '\n' -> read_cons getch k None
    | ')' -> k nil, None
    | '.' ->
      let rhs, next = read_sexp getch (getch ()) in
      let rec aux = function
        | ')' -> k rhs
        | ' ' | '\t' | '\n' -> aux (getch ())
        | _ -> failwith "Invalid parse"
      in
      begin match next with
        | Some c -> aux c
        | None -> aux (getch ())
      end, None
    | c ->
      let cell, next = read_sexp getch c in
      read_cons getch (fun rhs -> k (Cons (cell, rhs))) next

  and read_num getch c =
    Buffer.clear buf;
    Buffer.add_char buf c;
    let is_float = ref false in
    let rec aux () =
      match getch () with
      | c when c >= '0' && c <= '9' ->
        Buffer.add_char buf c; aux ()
      | '.' | 'e' | 'E' as c ->
        is_float := true;
        Buffer.add_char buf c; aux ()
      | c ->
        let s = Buffer.contents buf in
        (if !is_float
         then Float (float_of_string s)
         else Int (int_of_string s)),
        Some c
    in
    aux ()

  and read_string getch =
    Buffer.clear buf;
    let rec aux () =
      match getch () with
      | '\000' -> failwith "Unterminated string"
      | '\\' ->
        Buffer.add_char buf '\\';
        Buffer.add_char buf (getch ());
        aux ()
      | '"' ->
        String (unescaped (Buffer.contents buf)), None
      | c ->
        Buffer.add_char buf c;
        aux ()
    in
    aux ()

  and read_sym getch next =
    Buffer.clear buf;
    let rec aux next =
      match (match next with Some c -> c | None -> getch ()) with
      | ('\'' | '-' | ':' | '_') as c ->
        Buffer.add_char buf c;
        aux None
      | c when is_alphanum c ->
        Buffer.add_char buf c;
        aux None
      | c -> Sym (Buffer.contents buf), Some c
    in
    aux next
  in
  read_sexp getch (getch ())

let to_buf sexp buf =
  tell_sexp (Buffer.add_string buf) sexp

let to_string sexp =
  let buf = Buffer.create 100 in
  to_buf sexp buf;
  Buffer.contents buf

let getch_of_substring str pos len =
  let len = pos + len in
  if pos < 0 || len > String.length str then
    invalid_arg "Sexp.getch_of_substring";
  let pos = ref pos in
  let getch () =
    if !pos < len then
      let r = str.[!pos] in
      incr pos;
      r
    else '\000'
  in
  getch

let getch_of_string str =
  getch_of_substring str 0 (String.length str)

let of_string str =
  fst (read_sexp (getch_of_string str))

let getch_of_subbytes str pos len =
  let len = pos + len in
  if pos < 0 || len > Bytes.length str then
    invalid_arg "Sexp.getch_of_subbytes";
  let pos = ref pos in
  let getch () =
    if !pos < len then
      let r = Bytes.get str !pos in
      incr pos;
      r
    else '\000'
  in
  getch

let of_file_descr ?(on_read=ignore) fd =
  let getch = ref (fun () -> '\000') in
  let rest = ref None in
  let buffer = Bytes.create 1024 in
  let getch () =
    match !rest with
    | Some r ->
      rest := None;
      r
    | None ->
      match !getch () with
      | '\000' ->
        on_read fd;
        let read = Unix.read fd buffer 0 1024 in
        if read = 0 then '\000'
        else
          begin
            getch := getch_of_subbytes buffer 0 read;
            !getch ()
          end
      | c -> c
  in
  fun () ->
    try
      let sexp, rest' = read_sexp getch in
      rest := rest';
      Some sexp
    with End_of_file -> None

let of_channel ?on_read ic =
  of_file_descr ?on_read (Unix.descr_of_in_channel ic)

let rec of_json =
  let assoc_item (a,b) = Cons (Sym a, of_json b) in
  function
  | `Null       -> Sym "null"
  | `Int i      -> Int i
  | `Float f    -> Float f
  | `String s   -> String s
  | `Bool true  -> Sym "true"
  | `Bool false -> Sym "false"
  | `Assoc lst  -> Cons (Cons (Sym "assoc", Sym "nil"), of_list (List.map assoc_item lst))
  | `List lst   -> of_list (List.map of_json lst)

let rec to_json =
  let fail msg sexp =
    failwith (msg ^ ", got: \n" ^ to_string sexp)
  in
  let rec assoc_item = function
    | Cons (Cons (Sym a, b), c) -> (a, to_json b) :: assoc_item c
    | Sym "nil" -> []
    | sexp -> fail "expecting association (key . value)" sexp
  in
  let rec list_items = function
    | Sym "nil" -> []
    | Cons (hd, tl) -> to_json hd :: list_items tl
    | sexp -> fail "expecting list" sexp
  in
  function
  | Sym "null"  -> `Null
  | Sym "true"  -> `Bool true
  | Sym "false" -> `Bool false
  | Int i    -> `Int i
  | Float f  -> `Float f
  | String s -> `String s
  | Cons (Cons (Sym "assoc", Sym "nil"), assocs) ->
    `Assoc (assoc_item assocs)
  | Sym "nil" -> `List []
  | Cons (hd, tl) -> `List (to_json hd :: list_items tl)
  | Sym s -> `String s
