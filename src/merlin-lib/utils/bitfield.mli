type t

val max_idx: int
val empty: t
val full: t
val set: int -> t -> t
val unset: int -> t -> t
val is_set: t -> int -> bool
