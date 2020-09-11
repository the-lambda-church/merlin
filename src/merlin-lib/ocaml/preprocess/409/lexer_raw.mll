(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* The lexer definition *)

{
open Misc
open Std
open Lexing
open Parser_raw

type keywords = (string, Parser_raw.token) Hashtbl.t

type error =
  | Illegal_character of char
  | Illegal_escape of string * string option
  | Reserved_sequence of string * string option
  | Unterminated_comment of Location.t
  | Unterminated_string
  | Unterminated_string_in_comment of Location.t * Location.t
  | Keyword_as_label of string
  | Invalid_literal of string

exception Error of error * Location.t

(* Monad in which the lexer evaluates *)
type 'a result =
  | Return of 'a
  | Refill of (unit -> 'a result)
  | Fail of error * Location.t

let return a = Return a

let fail lexbuf e = Fail (e, Location.curr lexbuf)
let fail_loc e l = Fail (e,l)

let rec (>>=) (m : 'a result) (f : 'a -> 'b result) : 'b result =
  match m with
  | Return a -> f a
  | Refill u ->
    Refill (fun () -> u () >>= f)
  | Fail _ as e -> e

type preprocessor = (Lexing.lexbuf -> Parser_raw.token) -> Lexing.lexbuf -> Parser_raw.token

type state = {
  keywords: keywords;
  mutable buffer: Buffer.t;
  mutable string_start_loc: Location.t;
  mutable comment_start_loc: Location.t list;
  mutable preprocessor: preprocessor option;
}

let make ?preprocessor keywords = {
  keywords;
  buffer = Buffer.create 17;
  string_start_loc = Location.none;
  comment_start_loc = [];
  preprocessor;
}

let lABEL m = m >>= fun v -> return (LABEL v)
let oPTLABEL m = m >>= fun v -> return (OPTLABEL v)

let rec catch m f = match m with
  | Fail (e,l) -> f e l
  | Refill next -> Refill (fun () -> catch (next ()) f)
  | Return _ -> m

(* The table of keywords *)

let keyword_table : keywords =
  create_hashtable 149 [
    "and", AND;
    "as", AS;
    "assert", ASSERT;
    "begin", BEGIN;
    "class", CLASS;
    "constraint", CONSTRAINT;
    "do", DO;
    "done", DONE;
    "downto", DOWNTO;
    "else", ELSE;
    "end", END;
    "exception", EXCEPTION;
    "external", EXTERNAL;
    "false", FALSE;
    "for", FOR;
    "fun", FUN;
    "function", FUNCTION;
    "functor", FUNCTOR;
    "if", IF;
    "in", IN;
    "include", INCLUDE;
    "inherit", INHERIT;
    "initializer", INITIALIZER;
    "lazy", LAZY;
    "let", LET;
    "match", MATCH;
    "method", METHOD;
    "module", MODULE;
    "mutable", MUTABLE;
    "new", NEW;
    "nonrec", NONREC;
    "object", OBJECT;
    "of", OF;
    "open", OPEN;
    "or", OR;
(*  "parser", PARSER; *)
    "private", PRIVATE;
    "rec", REC;
    "sig", SIG;
    "struct", STRUCT;
    "then", THEN;
    "to", TO;
    "true", TRUE;
    "try", TRY;
    "type", TYPE;
    "val", VAL;
    "virtual", VIRTUAL;
    "when", WHEN;
    "while", WHILE;
    "with", WITH;

    "lor", INFIXOP3("lor"); (* Should be INFIXOP2 *)
    "lxor", INFIXOP3("lxor"); (* Should be INFIXOP2 *)
    "mod", INFIXOP3("mod");
    "land", INFIXOP3("land");
    "lsl", INFIXOP4("lsl");
    "lsr", INFIXOP4("lsr");
    "asr", INFIXOP4("asr");
]

let keywords l = create_hashtable 11 l

(* To store the position of the beginning of a string and comment *)
let in_comment state = state.comment_start_loc <> []

(* Escaped chars are interpreted in strings unless they are in comments. *)
let store_escaped_uchar state lexbuf u =
  if in_comment state
  then Buffer.add_string state.buffer (Lexing.lexeme lexbuf)
  else Buffer.add_utf_8_uchar state.buffer u

(* to translate escape sequences *)

let digit_value c =
  match c with
  | 'a' .. 'f' -> 10 + Char.code c - Char.code 'a'
  | 'A' .. 'F' -> 10 + Char.code c - Char.code 'A'
  | '0' .. '9' -> Char.code c - Char.code '0'
  | _ -> assert false

let num_value lexbuf ~base ~first ~last =
  let c = ref 0 in
  for i = first to last do
    let v = digit_value (Lexing.lexeme_char lexbuf i) in
    assert(v < base);
    c := (base * !c) + v
  done;
  !c

let char_for_backslash = function
  | 'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c   -> c

let illegal_escape lexbuf reason =
  let error = Illegal_escape (Lexing.lexeme lexbuf, Some reason) in
  fail lexbuf error

let char_for_decimal_code state lexbuf i =
  let c = num_value lexbuf ~base:10 ~first:i ~last:(i+2) in
  if (c < 0 || c > 255) then
    if in_comment state
    then return 'x'
    else
      illegal_escape lexbuf
        (Printf.sprintf
          "%d is outside the range of legal characters (0-255)." c)
  else return (Char.chr c)

let char_for_octal_code state lexbuf i =
  let c = num_value lexbuf ~base:8 ~first:i ~last:(i+2) in
  if (c < 0 || c > 255) then
    if in_comment state
    then return 'x'
    else
      illegal_escape lexbuf
        (Printf.sprintf
          "o%o (=%d) is outside the range of legal characters (0-255)." c c)
  else return (Char.chr c)

let char_for_hexadecimal_code lexbuf i =
  Char.chr (num_value lexbuf ~base:16 ~first:i ~last:(i+1))

let uchar_for_uchar_escape lexbuf =
  let illegal_escape lexbuf reason =
    let error = Illegal_escape (Lexing.lexeme lexbuf, Some reason) in
    raise (Error (error, Location.curr lexbuf))
  in
  let len = Lexing.lexeme_end lexbuf - Lexing.lexeme_start lexbuf in
  let first = 3 (* skip opening \u{ *) in
  let last = len - 2 (* skip closing } *) in
  let digit_count = last - first + 1 in
  match digit_count > 6 with
  | true ->
      illegal_escape lexbuf
        "too many digits, expected 1 to 6 hexadecimal digits"
  | false ->
      let cp = num_value lexbuf ~base:16 ~first ~last in
      if Uchar.is_valid cp then Uchar.unsafe_of_int cp else
      illegal_escape lexbuf
        (Printf.sprintf "%X is not a Unicode scalar value" cp)

let keyword_or state s default =
  try Hashtbl.find state.keywords s
      with Not_found -> try Hashtbl.find keyword_table s
  with Not_found -> default

let is_keyword name = Hashtbl.mem keyword_table name

let check_label_name lexbuf name =
  if is_keyword name
  then fail lexbuf (Keyword_as_label name)
  else return name

(* Update the current location with file name and line number. *)

let update_loc lexbuf _file line absolute chars =
  let pos = lexbuf.lex_curr_p in
  let new_file = pos.pos_fname
    (*match file with
      | None -> pos.pos_fname
      | Some s -> s*)
  in
  lexbuf.lex_curr_p <- { pos with
    pos_fname = new_file;
    pos_lnum = if absolute then line else pos.pos_lnum + line;
    pos_bol = pos.pos_cnum - chars;
  }
;;

(* Warn about Latin-1 characters used in idents *)

let warn_latin1 lexbuf =
  Location.deprecated (Location.curr lexbuf)
    "ISO-Latin1 characters in identifiers"
;;

(* Error report *)

open Format

let prepare_error loc = function
  | Illegal_character c ->
      Location.errorf ~loc "Illegal character (%s)" (Char.escaped c)
  | Illegal_escape (s, explanation) ->
      Location.errorf ~loc
        "Illegal backslash escape in string or character (%s)%t" s
        (fun ppf -> match explanation with
           | None -> ()
           | Some expl -> fprintf ppf ": %s" expl)
  | Reserved_sequence (s, explanation) ->
      Location.errorf ~loc
        "Reserved character sequence: %s%t" s
        (fun ppf -> match explanation with
           | None -> ()
           | Some expl -> fprintf ppf " %s" expl)
  | Unterminated_comment _ ->
      Location.errorf ~loc "Comment not terminated"
  | Unterminated_string ->
      Location.errorf ~loc "String literal not terminated"
  | Unterminated_string_in_comment (_, literal_loc) ->
      Location.errorf ~loc
        "This comment contains an unterminated string literal"
        ~sub:[Location.msg ~loc:literal_loc "String literal begins here"]
  | Keyword_as_label kwd ->
      Location.errorf ~loc
        "`%s' is a keyword, it cannot be used as label name" kwd
  | Invalid_literal s ->
      Location.errorf ~loc "Invalid literal %s" s
(* FIXME: Invalid_directive? *)

let () =
  Location.register_error_of_exn
    (function
      | Error (err, loc) ->
        Some (prepare_error loc err)
      | _ ->
        None
    )

}

let newline = ('\013'* '\010')
let blank = [' ' '\009' '\012']
let lowercase = ['a'-'z' '_']
let uppercase = ['A'-'Z']
let identchar = ['A'-'Z' 'a'-'z' '_' '\'' '0'-'9' '\128'-'\255']
let lowercase_latin1 = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']
let uppercase_latin1 = ['A'-'Z' '\192'-'\214' '\216'-'\222']
let identchar_latin1 = identchar
  (*['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']*)
let symbolchar =
  ['!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~']
let symbolcharnopercent =
  ['!' '$' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~']
let dotsymbolchar =
  ['!' '$' '%' '&' '*' '+' '-' '/' ':' '=' '>' '?' '@' '^' '|']
let kwdopchar =
  ['$' '&' '*' '+' '-' '/' '<' '=' '>' '@' '^' '|']

let decimal_literal =
  ['0'-'9'] ['0'-'9' '_']*
let hex_digit =
  ['0'-'9' 'A'-'F' 'a'-'f']
let hex_literal =
  '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']['0'-'9' 'A'-'F' 'a'-'f' '_']*
let oct_literal =
  '0' ['o' 'O'] ['0'-'7'] ['0'-'7' '_']*
let bin_literal =
  '0' ['b' 'B'] ['0'-'1'] ['0'-'1' '_']*
let int_literal =
  decimal_literal | hex_literal | oct_literal | bin_literal
let float_literal =
  ['0'-'9'] ['0'-'9' '_']*
  ('.' ['0'-'9' '_']* )?
  (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']*) ?
let hex_float_literal =
  '0' ['x' 'X']
  ['0'-'9' 'A'-'F' 'a'-'f'] ['0'-'9' 'A'-'F' 'a'-'f' '_']*
  ('.' ['0'-'9' 'A'-'F' 'a'-'f' '_']* )?
  (['p' 'P'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']* )?
let literal_modifier = ['G'-'Z' 'g'-'z']


refill {fun k lexbuf -> Refill (fun () -> k lexbuf)}

rule token state = parse
  | ("\\" as bs) newline {
      match state.preprocessor with
      | None -> fail lexbuf (Illegal_character bs)
      | Some _ ->
        update_loc lexbuf None 1 false 0;
        token state lexbuf }
  | newline
      { update_loc lexbuf None 1 false 0;
        match state.preprocessor with
        | None -> token state lexbuf
        | Some _ -> return EOL
      }
  | blank +
      { token state lexbuf }
  | ".<"
      { return DOTLESS }
  | ">."
      { return (keyword_or state (Lexing.lexeme lexbuf) (INFIXOP0 ">.")) }
  | ".~"
      { return (keyword_or state (Lexing.lexeme lexbuf) DOTTILDE) }
  | "_"
      { return UNDERSCORE }
  | "~"
      { return TILDE }
      (*
  | ".~"
      { fail lexbuf
          (Reserved_sequence (".~", Some "is reserved for use in MetaOCaml")) }
      *)
  | "~" (lowercase identchar * as name) ':'
      { lABEL (check_label_name lexbuf name) }
  | "~" (lowercase_latin1 identchar_latin1 * as name) ':'
      { warn_latin1 lexbuf;
        return (LABEL name) }
  | "?"
      { return QUESTION }
  | "??"
      { return QUESTIONQUESTION }
  | "?" (lowercase identchar * as name) ':'
      { oPTLABEL (check_label_name lexbuf name) }
  | "?" (lowercase_latin1 identchar_latin1 * as name) ':'
      { warn_latin1 lexbuf; return (OPTLABEL name) }
  | lowercase identchar * as name
    { return (try Hashtbl.find state.keywords name
              with Not_found ->
              try Hashtbl.find keyword_table name
              with Not_found ->
                LIDENT name) }
  | lowercase_latin1 identchar_latin1 * as name
      { warn_latin1 lexbuf; return (LIDENT name) }
  | uppercase identchar * as name
    { (* Capitalized keywords for OUnit *)
      return (try Hashtbl.find state.keywords name
              with Not_found ->
              try Hashtbl.find keyword_table name
              with Not_found ->
                UIDENT name) }
  | uppercase_latin1 identchar_latin1 * as name
    { warn_latin1 lexbuf; return (UIDENT name) }
  | int_literal as lit { return (INT (lit, None)) }
  | (int_literal as lit) (literal_modifier as modif)
    { return (INT (lit, Some modif)) }
  | float_literal | hex_float_literal as lit
    { return (FLOAT (lit, None)) }
  | (float_literal | hex_float_literal as lit) (literal_modifier as modif)
    { return (FLOAT (lit, Some modif)) }
  | (float_literal | hex_float_literal | int_literal) identchar+ as invalid
    { fail lexbuf (Invalid_literal invalid) }
  | "\""
      { Buffer.reset state.buffer;
        state.string_start_loc <- Location.curr lexbuf;
        string state lexbuf >>= fun () ->
        lexbuf.lex_start_p <- state.string_start_loc.Location.loc_start;
        state.string_start_loc <- Location.none;
        return (STRING (Buffer.contents state.buffer, None))
      }
  | "{" (lowercase* as delim) "|"
      { Buffer.reset state.buffer;
        state.string_start_loc <- Location.curr lexbuf;
        quoted_string state delim lexbuf >>= fun () ->
        lexbuf.lex_start_p <- state.string_start_loc.Location.loc_start;
        state.string_start_loc <- Location.none;
        return (STRING (Buffer.contents state.buffer, Some delim)) }
  | "\'" newline "\'"
    { update_loc lexbuf None 1 false 1;
      (* newline is ('\013'* '\010') *)
      return (CHAR '\n') }
  | "\'" ([^ '\\' '\'' '\010' '\013'] as c) "\'"
    { return (CHAR c) }
  | "\'\\" (['\\' '\'' '\"' 'n' 't' 'b' 'r' ' '] as c) "\'"
    { return (CHAR (char_for_backslash c)) }
  | "\'\\" 'o' ['0'-'3'] ['0'-'7'] ['0'-'7'] "\'"
    { char_for_octal_code state lexbuf 3 >>= fun c -> return (CHAR c) }
  | "\'\\" ['0'-'9'] ['0'-'9'] ['0'-'9'] "\'"
    { char_for_decimal_code state lexbuf 2 >>= fun c -> return (CHAR c) }
  | "\'\\" 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] "\'"
    { return (CHAR (char_for_hexadecimal_code lexbuf 3)) }
  | "\'" ("\\" _ as esc)
      { fail lexbuf (Illegal_escape (esc, None)) }
  | "(*"
      { let start_loc = Location.curr lexbuf in
        state.comment_start_loc <- [start_loc];
        Buffer.reset state.buffer;
        comment state lexbuf >>= fun end_loc ->
        let s = Buffer.contents state.buffer in
        Buffer.reset state.buffer;
        return (COMMENT (s, { start_loc with
                              Location.loc_end = end_loc.Location.loc_end }))
      }
  | "(*)"
      { let loc = Location.curr lexbuf in
        Location.prerr_warning loc Warnings.Comment_start;
        state.comment_start_loc <- [loc];
        Buffer.reset state.buffer;
        comment state lexbuf >>= fun end_loc ->
        let s = Buffer.contents state.buffer in
        Buffer.reset state.buffer;
        return (COMMENT (s, { loc with Location.loc_end = end_loc.Location.loc_end }))
      }
  | "*)"
      { let loc = Location.curr lexbuf in
        Location.prerr_warning loc Warnings.Comment_not_end;
        lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_curr_pos - 1;
        let curpos = lexbuf.lex_curr_p in
        lexbuf.lex_curr_p <- { curpos with pos_cnum = curpos.pos_cnum - 1 };
        return STAR
      }
  | "#" [' ' '\t']* (['0'-'9']+ as num) [' ' '\t']*
        ("\"" ([^ '\010' '\013' '\"' ] * as name) "\"")?
        [^ '\010' '\013'] * newline
      { update_loc lexbuf name (int_of_string num) true 0;
        token state lexbuf
      }
  | "#"  { return HASH }
  | "&"  { return AMPERSAND }
  | "&&" { return AMPERAMPER }
  | "`"  { return BACKQUOTE }
  | "\'" { return QUOTE }
  | "("  { return LPAREN }
  | ")"  { return RPAREN }
  | "*"  { return STAR }
  | ","  { return COMMA }
  | "->" { return MINUSGREATER }
  | "."  { return DOT }
  | "." (dotsymbolchar symbolchar* as op) { return (DOTOP op) }
  | ".." { return DOTDOT }
  | ":"  { return COLON }
  | "::" { return COLONCOLON }
  | ":=" { return COLONEQUAL }
  | ":>" { return COLONGREATER }
  | ";"  { return SEMI }
  | ";;" { return SEMISEMI }
  | "<"  { return LESS }
  | "<-" { return LESSMINUS }
  | "="  { return EQUAL }
  | "["  { return LBRACKET }
  | "[|" { return LBRACKETBAR }
  | "[<" { return LBRACKETLESS }
  | "[>" { return LBRACKETGREATER }
  | "]"  { return RBRACKET }
  | "{"  { return LBRACE }
  | "{<" { return LBRACELESS }
  | "|"  { return BAR }
  | "||" { return BARBAR }
  | "|]" { return BARRBRACKET }
  | ">"  { return GREATER }
  | ">]" { return GREATERRBRACKET }
  | "}"  { return RBRACE }
  | ">}" { return GREATERRBRACE }
  | "[@" { return LBRACKETAT }
  | "[@@"  { return LBRACKETATAT }
  | "[@@@" { return LBRACKETATATAT }
  | "[%" { return LBRACKETPERCENT }
  | "[%%" { return LBRACKETPERCENTPERCENT }
  | "!"  { return BANG }
  | "!=" { return (INFIXOP0 "!=") }
  | "+"  { return PLUS }
  | "+." { return PLUSDOT }
  | "+=" { return PLUSEQ }
  | "-"  { return MINUS }
  | "-." { return MINUSDOT }

  | "!" symbolchar + as op
            { return (PREFIXOP op) }
  | ['~' '?'] symbolchar + as op
            { return (PREFIXOP op) }
  | ['=' '<' '|' '&' '$' '>'] symbolchar * as op
            { return (keyword_or state op
                       (INFIXOP0 op)) }
  | ['@' '^'] symbolchar * as op
            { return (INFIXOP1 op) }
  | ['+' '-'] symbolchar * as op
            { return (INFIXOP2 op) }
  | "**" symbolchar * as op
            { return (INFIXOP4 op) }
  | '%'     { return PERCENT }
  | ['*' '/' '%'] symbolchar * as op
            { return (INFIXOP3 op) }
  | '#' (symbolchar | '#') + as op
            { return (try Hashtbl.find state.keywords op
                      with Not_found -> HASHOP op) }
  (* Old style js_of_ocaml support is implemented by generating a custom token *)
  | '#' (symbolchar | '#') + as op
            { return (try Hashtbl.find state.keywords op
                      with Not_found -> HASHOP op) }
  | "let" kwdopchar dotsymbolchar * as op
            { return (LETOP op) }
  | "and" kwdopchar dotsymbolchar * as op
            { return (ANDOP op) }
  | eof { return EOF }

  | _ as illegal_char
      { fail lexbuf (Illegal_character illegal_char) }

and comment state = parse
    "(*"
      { state.comment_start_loc <- (Location.curr lexbuf) :: state.comment_start_loc;
      Buffer.add_string state.buffer (Lexing.lexeme lexbuf);
      comment state lexbuf
    }
  | "*)"
      { match state.comment_start_loc with
        | [] -> assert false
        | [_] -> state.comment_start_loc <- []; return (Location.curr lexbuf)
        | _ :: l -> state.comment_start_loc <- l;
                  Buffer.add_string state.buffer (Lexing.lexeme lexbuf);
                  comment state lexbuf
       }
  | "\""
      {
        state.string_start_loc <- Location.curr lexbuf;
        Buffer.add_char state.buffer '\"';
        let buffer = state.buffer in
        state.buffer <- Buffer.create 15;
        (catch (string state lexbuf) (fun e l -> match e with
             | Unterminated_string ->
               begin match state.comment_start_loc with
                 | [] -> assert false
                 | loc :: _ ->
                   let start = List.hd (List.rev state.comment_start_loc) in
                   state.comment_start_loc <- [];
                   fail_loc (Unterminated_string_in_comment (start, l)) loc
               end
             | e -> fail_loc e l
           )
        ) >>= fun () ->
      state.string_start_loc <- Location.none;
      Buffer.add_string buffer (String.escaped (Buffer.contents state.buffer));
      state.buffer <- buffer;
      Buffer.add_char state.buffer '\"';
      comment state lexbuf }
  | "{" lowercase* "|"
      {
        let delim = Lexing.lexeme lexbuf in
        let delim = String.sub delim ~pos:1 ~len:(String.length delim - 2) in
        state.string_start_loc <- Location.curr lexbuf;
        Buffer.add_string state.buffer (Lexing.lexeme lexbuf);
        (catch (quoted_string state delim lexbuf) (fun e l -> match e with
             | Unterminated_string ->
               begin match state.comment_start_loc with
                 | [] -> assert false
                 | loc :: _ ->
                   let start = List.hd (List.rev state.comment_start_loc) in
                   state.comment_start_loc <- [];
                   fail_loc (Unterminated_string_in_comment (start, l)) loc
               end
             | e -> fail_loc e l
           )
        ) >>= fun () ->
        state.string_start_loc <- Location.none;
        Buffer.add_char state.buffer '|';
        Buffer.add_string state.buffer delim;
        Buffer.add_char state.buffer '}';
        comment state lexbuf }

  | "''"
      { Buffer.add_string state.buffer (Lexing.lexeme lexbuf); comment state lexbuf }
  | "'" newline "'"
      { update_loc lexbuf None 1 false 1;
        Buffer.add_string state.buffer (Lexing.lexeme lexbuf);
        comment state lexbuf
      }
  | "'" [^ '\\' '\'' '\010' '\013' ] "'"
      { Buffer.add_string state.buffer (Lexing.lexeme lexbuf); comment state lexbuf }
  | "'\\" ['\\' '\"' '\'' 'n' 't' 'b' 'r' ' '] "'"
      { Buffer.add_string state.buffer (Lexing.lexeme lexbuf); comment state lexbuf }
  | "'\\" ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
      { Buffer.add_string state.buffer (Lexing.lexeme lexbuf); comment state lexbuf }
  | "'\\" 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] "'"
      { Buffer.add_string state.buffer (Lexing.lexeme lexbuf); comment state lexbuf }
  | eof
      { match state.comment_start_loc with
        | [] -> assert false
        | loc :: _ ->
          let start = List.hd (List.rev state.comment_start_loc) in
          state.comment_start_loc <- [];
          fail_loc (Unterminated_comment start) loc
      }
  | newline
      { update_loc lexbuf None 1 false 0;
        Buffer.add_string state.buffer (Lexing.lexeme lexbuf);
        comment state lexbuf
      }
  | _
      { Buffer.add_string state.buffer (Lexing.lexeme lexbuf); comment state lexbuf }

and string state = parse
    '\"'
      { return () }
  | '\\' newline ([' ' '\t'] * as space)
      { update_loc lexbuf None 1 false (String.length space);
        string state lexbuf
      }
  | '\\' ['\\' '\'' '\"' 'n' 't' 'b' 'r' ' ']
      { Buffer.add_char state.buffer
          (char_for_backslash (Lexing.lexeme_char lexbuf 1));
        string state lexbuf }
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9']
      { char_for_decimal_code state lexbuf 1 >>= fun c ->
        Buffer.add_char state.buffer c;
        string state lexbuf }
  | '\\' 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F']
      { Buffer.add_char state.buffer (char_for_hexadecimal_code lexbuf 2);
        string state lexbuf }
  | '\\' 'u' '{' hex_digit+ '}'
      { store_escaped_uchar state lexbuf (uchar_for_uchar_escape lexbuf);
        string state lexbuf }
  | '\\' _
      { if in_comment state
        then string state lexbuf
        else begin
(*  Should be an error, but we are very lax.
                  fail (Illegal_escape (Lexing.lexeme lexbuf),
                        (Location.curr lexbuf)
*)
          let loc = Location.curr lexbuf in
          Location.prerr_warning loc Warnings.Illegal_backslash;
          Buffer.add_char state.buffer (Lexing.lexeme_char lexbuf 0);
          Buffer.add_char state.buffer (Lexing.lexeme_char lexbuf 1);
          string state lexbuf
        end
      }
  | newline
      { if not (in_comment state) then
          Location.prerr_warning (Location.curr lexbuf) Warnings.Eol_in_string;
        update_loc lexbuf None 1 false 0;
        Buffer.add_string state.buffer (Lexing.lexeme lexbuf);
        string state lexbuf
      }
  | eof
      { let loc = state.string_start_loc in
        state.string_start_loc <- Location.none;
        fail_loc Unterminated_string loc }
  | _
      { Buffer.add_char state.buffer (Lexing.lexeme_char lexbuf 0);
        string state lexbuf }

and quoted_string state delim = parse
  | newline
      { update_loc lexbuf None 1 false 0;
        Buffer.add_string state.buffer (Lexing.lexeme lexbuf);
        quoted_string state delim lexbuf
      }
  | eof
      { let loc = state.string_start_loc in
        state.string_start_loc <- Location.none;
        fail_loc Unterminated_string loc }
  | "|" lowercase* "}"
      {
        let edelim = Lexing.lexeme lexbuf in
        let edelim = String.sub edelim ~pos:1 ~len:(String.length edelim - 2) in
        if delim = edelim then return ()
        else (Buffer.add_string state.buffer (Lexing.lexeme lexbuf);
              quoted_string state delim lexbuf)
      }
  | _
      { Buffer.add_char state.buffer (Lexing.lexeme_char lexbuf 0);
        quoted_string state delim lexbuf }

and skip_sharp_bang state = parse
  | "#!" [^ '\n']* '\n' [^ '\n']* "\n!#\n"
      { update_loc lexbuf None 3 false 0; token state lexbuf }
  | "#!" [^ '\n']* '\n'
      { update_loc lexbuf None 1 false 0; token state lexbuf }
  | "" { token state lexbuf }

{
  type comment = string * Location.t

  (* preprocessor support not implemented, not compatible with monadic
     interface *)

  let rec token_without_comments state lexbuf =
    token state lexbuf >>= function
    | COMMENT _ ->
      token_without_comments state lexbuf
    | tok -> return tok
}
