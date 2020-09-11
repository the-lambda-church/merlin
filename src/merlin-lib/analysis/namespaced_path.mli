module Namespace : sig
  type t = [
    | `Vals
    | `Type
    | `Constr
    | `Mod
    | `Modtype
    | `Functor
    | `Labels
    | `Unknown
    | `Apply
  ]

  val to_string : t -> string
end

module Id : sig
  type t = private
    | Id of Ident.t
    | String of string

  val name : t -> string
end

type t (* = private elt list *)
and elt = private
  | Ident of Id.t * Namespace.t
  | Applied_to of t

val to_string : t -> string
val to_unique_string : t -> string

val head : t -> elt option
val head_exn : t -> elt

val peal_head : t -> t option
val peal_head_exn : t -> t

val equal : t -> t -> bool

val rewrite_head : new_prefix:t -> t -> t

val strip_stamps : t -> t

val of_path : namespace:Namespace.t -> Path.t -> t

val empty : t

val subst_prefix : old_prefix:t -> new_prefix:t -> t -> t option
