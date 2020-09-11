open Std

type t =
    Cons of t * t
  | Sym of string
  | String of string
  | Int of int
  | Float of float

val nil : t
val of_list : t list -> t

val tell_sexp : (string -> unit) -> t -> unit
val tell_cons : (string -> unit) -> t -> unit

val to_buf : t -> Buffer.t -> unit

val to_string : t -> string

val of_string : string -> t

val of_file_descr :
  ?on_read:(Unix.file_descr -> unit) -> Unix.file_descr -> unit -> t option
val of_channel :
  ?on_read:(Unix.file_descr -> unit) -> in_channel -> unit -> t option

val of_json : json -> t
val to_json : t -> json
