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

open Std
open Format

(* Possible errors *)
type error =
  | Illegal_character of char
  | Illegal_escape of string
  | Unterminated_comment of Location.t
  | Unterminated_string
  | Unterminated_string_in_comment of Location.t * Location.t
  | Keyword_as_label of string
  | Invalid_literal of string
exception Error of error * Location.t
val report_error : formatter -> error -> unit

(* Keywords, manipulated by extensions *)
type keywords
val keywords: (string * Parser_raw.token) list -> keywords

(* Monad in which the lexer evaluates *)
type 'a result =
  | Return of 'a
  | Refill of (unit -> 'a result)
  | Fail of error * Location.t

type preprocessor = (Lexing.lexbuf -> Parser_raw.token) -> Lexing.lexbuf -> Parser_raw.token

type state = {
  keywords: keywords;
  mutable buffer: Buffer.t;
  mutable string_start_loc: Location.t;
  mutable comment_start_loc: Location.t list;
  mutable preprocessor: preprocessor option;
}

val make: ?preprocessor:preprocessor -> keywords -> state

(* The lexical analyzer *)

val skip_sharp_bang: state -> Lexing.lexbuf -> Parser_raw.token result
val token: state -> Lexing.lexbuf -> Parser_raw.token result

(* Comments are filtered out from the token rule and stored in a global
   variable. *)
type comment = string * Location.t

(* If you want to get the raw output, including comments, from the lexer, use
   the [token_with_comments] entry point. *)
val token_without_comments : state -> Lexing.lexbuf -> Parser_raw.token result
