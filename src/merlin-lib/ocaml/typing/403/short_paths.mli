
module Desc = Short_paths_graph.Desc

module Basis : sig

  type t

  val create : unit -> t

  val add : t -> string -> unit

  val load : t -> string -> string list -> string list ->
    Desc.Module.t -> Desc.deprecated -> unit

end

type t

val initial : Basis.t -> t

val add : t -> Desc.t list Lazy.t -> t

type type_result =
  | Nth of int
  | Path of int list option * Path.t

val find_type : t -> Path.t -> type_result

type type_resolution =
  | Nth of int
  | Subst of int list
  | Id

val find_type_resolution : t -> Path.t -> type_resolution

val find_type_simple : t -> Path.t -> Path.t

type class_type_result = int list option * Path.t

val find_class_type : t -> Path.t -> class_type_result

val find_class_type_simple : t -> Path.t -> Path.t

val find_module_type : t -> Path.t -> Path.t

val find_module : t -> Path.t -> Path.t
