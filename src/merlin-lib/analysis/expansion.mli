type t

val explore : ?global_modules:string list -> Env.t -> t list

val get_lidents : t list -> string -> Longident.t option list * string

val spell_index : string -> string -> bool

val spell_match : (string -> bool) -> string -> bool
