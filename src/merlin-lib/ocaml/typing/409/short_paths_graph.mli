(** [Short_path_graph] is a representation of the environment (as a graph,
    using [Graph.t]) that is more suitable to answer short path queries.

    The only structures shared with the typechecker are [Ident.t] and [Path.t].
    [Graph.t] is pure and doesn't hook into the [Env.t].
    Context has to be rebuilt by outside code using [Graph.add].
*)

(* Generic definitions *)

module String_map : Map.S with type key = string

module Ident : sig

  type t = Ident.t

  val equal : t -> t -> bool

  val compare : t -> t -> int

  val name : t -> string

  val global : string -> t

end

module Ident_map : Map.S with type key = Ident.t

module Ident_set : Set.S with type elt = Ident.t

module Path : sig

  type t = Path.t =
    | Pident of Ident.t
    | Pdot of t * string
    | Papply of t * t

  val equal : t -> t -> bool

  val compare : t -> t -> int

end

module Path_map : Map.S with type key = Path.t

module Path_set : Set.S with type elt = Path.t

(* Subset of the type algebra that is relevant to short path *)

module Desc : sig

  type deprecated =
    | Deprecated
    | Not_deprecated

  module Type : sig

    type t =
      | Fresh
      (** type t *)
      | Nth of int
      (** The n'th projection of type parameters.
          E.g. for n < m, [type ('x_0,'x_1,...,'x_m-1) t = 'x_n]
          is represented as [Nth n]. *)
      | Subst of Path.t * int list
      (** An alias to some other type after substitution of type parameters.
          E.g. [type ('x_0, 'x_1', 'x_2, 'x_3) t = ('x_3, 'x_2) p]
          is represented as [Subst (p, [3,2])]. *)
      | Alias of Path.t
      (** A direct alias to another type, preserving parameters.
          E.g [type t = p], [type 'a t = 'a p], ...
          are represented as [Alias p]. *)
  end

  module Class_type : sig

    type t =
      | Fresh
      | Subst of Path.t * int list
      | Alias of Path.t

  end

  module Module_type : sig

    type t =
      | Fresh
      | Alias of Path.t

  end

  module Module : sig

    type component =
      | Type of string * Type.t * deprecated
      | Class_type of string * Class_type.t * deprecated
      | Module_type of string * Module_type.t * deprecated
      | Module of string * t * deprecated

    and components = component list

    and kind =
      | Signature of components Lazy.t
      | Functor of (Path.t -> t)

    and t =
      | Fresh of kind
      | Alias of Path.t

  end

  type source =
    | Local
    | Open

  type t =
    | Type of Ident.t * Type.t * source * deprecated
    | Class_type of Ident.t * Class_type.t * source * deprecated
    | Module_type of Ident.t * Module_type.t * source * deprecated
    | Module of Ident.t * Module.t * source * deprecated
    | Declare_type of Ident.t
    | Declare_class_type of Ident.t
    | Declare_module_type of Ident.t
    | Declare_module of Ident.t

end

module Sort : sig

  type t =
    | Defined
    | Declared of Ident_set.t

end

module Age : Natural.S

module Dependency : Natural.S

module Origin : sig

  type t =
    | Dependency of Dependency.t
    | Dependencies of Dependency.t list
    | Environment of Age.t

  val equal : t -> t -> bool

  val hash : t -> int

end

type graph

module Type : sig

  type t

  val origin : graph -> t -> Origin.t

  val path : graph -> t -> Path.t

  val hidden : t -> bool

  val sort : graph -> t -> Sort.t

  type resolved =
    | Nth of int
    | Path of int list option * t

  val resolve : graph -> t -> resolved

end

module Class_type : sig

  type t

  val origin : graph -> t -> Origin.t

  val path : graph -> t -> Path.t

  val hidden : t -> bool

  val sort : graph -> t -> Sort.t

  type resolved = int list option * t

  val resolve : graph -> t -> resolved

end

module Module_type : sig

  type t

  val origin : graph -> t -> Origin.t

  val path : graph -> t -> Path.t

  val hidden : t -> bool

  val sort : graph -> t -> Sort.t

end

module Module : sig

  type t

  val origin : graph -> t -> Origin.t

  val path : graph -> t -> Path.t

  val hidden : t -> bool

  val sort : graph -> t -> Sort.t

  val types : graph -> t -> Type.t String_map.t option

  val class_types : graph -> t -> Class_type.t String_map.t option

  val module_types : graph -> t -> Module_type.t String_map.t option

  val modules : graph -> t -> t String_map.t option

end

module Diff : sig

  module Item : sig

    type t =
      | Type of Ident.t * Type.t * Origin.t option
      | Class_type of Ident.t * Class_type.t * Origin.t option
      | Module_type of Ident.t * Module_type.t * Origin.t option
      | Module of Ident.t * Module.t * Origin.t option

    val origin : graph -> t -> Origin.t

    val id : graph -> t -> Ident.t

    val previous : graph -> t -> Origin.t option

  end

  type t = Item.t list

end

module Component : sig

  type source =
    | Global
    | Local
    | Open

  type t =
    | Type of
        Origin.t * Ident.t * Desc.Type.t * source * Desc.deprecated
    | Class_type of
        Origin.t * Ident.t * Desc.Class_type.t * source * Desc.deprecated
    | Module_type of
        Origin.t * Ident.t * Desc.Module_type.t * source * Desc.deprecated
    | Module of
        Origin.t * Ident.t * Desc.Module.t * source * Desc.deprecated
    | Declare_type of Origin.t * Ident.t
    | Declare_class_type of Origin.t * Ident.t
    | Declare_module_type of Origin.t * Ident.t
    | Declare_module of Origin.t * Ident.t

end

module Graph : sig

  type t = graph

  val empty : t

  val add : t -> Component.t list -> t * Diff.t

  val merge : t -> Diff.t -> t

  val find_type : t -> Path.t -> Type.t

  val find_class_type : t -> Path.t -> Class_type.t

  val find_module_type : t -> Path.t -> Module_type.t

  val find_module : t -> Path.t -> Module.t

  val is_type_path_visible : t -> Path.t -> bool

  val is_class_type_path_visible : t -> Path.t -> bool

  val is_module_type_path_visible : t -> Path.t -> bool

  val is_module_path_visible : t -> Path.t -> bool

  val is_type_ident_visible : t -> Ident.t -> bool

  val is_class_type_ident_visible : t -> Ident.t -> bool

  val is_module_type_ident_visible : t -> Ident.t -> bool

  val is_module_ident_visible : t -> Ident.t -> bool

end
