(** Helper for the driver (Merlin) *)
open Extend_protocol

type t

exception Extension of string * string * string

val run : ?notify:(string -> unit) -> ?debug:(string -> unit) -> string -> t

val stop : t -> unit

val capabilities : t -> capabilities

val reader : t ->
  Reader.request ->
  Reader.response
