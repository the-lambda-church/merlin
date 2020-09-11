type t

val stop : t -> unit

val start : string -> string list -> Mconfig.t -> Msource.t -> t option

val parse :
  ?for_completion:Msource.position -> t ->
  ([`No_labels of bool ] *
   [`Implementation of Parsetree.structure | `Interface of Parsetree.signature])
  option

val reconstruct_identifier :
  Lexing.position -> t -> string Location.loc list option

val print_pretty :
  Extend_protocol.Reader.pretty_parsetree -> t -> string option

val print_outcomes :
  Extend_protocol.Reader.outcometree list -> t -> string list option

val print_outcome :
  Extend_protocol.Reader.outcometree -> t -> string option
