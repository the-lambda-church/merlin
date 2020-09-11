type parsetree = [
  | `Interface of Parsetree.signature
  | `Implementation of Parsetree.structure
]

type comment = (string * Location.t)

type result = {
  config        : Mconfig.t;
  lexer_errors  : exn list;
  parser_errors : exn list;
  comments      : comment list;
  parsetree     : parsetree;
  no_labels_for_completion : bool;
}

type pretty_parsetree = Extend_protocol.Reader.pretty_parsetree
type outcometree = Extend_protocol.Reader.outcometree

(* Ambient reader.

   Some actions need to interact with an external process.
   `with_ambient_reader' will setup this process to speed up later calls.
*)

val with_ambient_reader : Mconfig.t -> Msource.t -> (unit -> 'a) -> 'a

(* Main functions *)

val parse :
  ?for_completion:Msource.position -> Mconfig.t -> Msource.t -> result

val print_pretty :
  Mconfig.t -> Msource.t -> pretty_parsetree -> string

val print_outcome :
  Mconfig.t -> Msource.t -> outcometree -> string

val print_batch_outcome :
  Mconfig.t -> Msource.t -> outcometree list -> string list

val reconstruct_identifier:
  Mconfig.t -> Msource.t -> Lexing.position -> string Location.loc list

(* Update config after parse *)

val apply_directives: Mconfig.t -> parsetree -> Mconfig.t
